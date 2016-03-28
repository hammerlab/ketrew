(**************************************************************************)
(*    Copyright 2014, 2015:                                               *)
(*          Sebastien Mondet <seb@mondet.org>,                            *)
(*          Leonid Rozenberg <leonidr@gmail.com>,                         *)
(*          Arun Ahuja <aahuja11@gmail.com>,                              *)
(*          Jeff Hammerbacher <jeff.hammerbacher@gmail.com>               *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)

open Ketrew_pure
open Internal_pervasives
open Unix_io

module Error = struct
  include Error
  type t =
    [ `Http of
        [ `Call of [ `GET | `POST ] * Uri.t
        | `Targets
        | `Target_query of Unique_id.t * string
        | `Process_holder
        ] *
        [ `Exn of exn
        | `Json_parsing of string * [ `Exn of exn ]
        | `Unexpected_message of Protocol.Down_message.t
        | `Wrong_json of Yojson.Safe.json
        | `Wrong_response of Cohttp.Response.t * string ]
    | `Server_error_response of
        [ `Call of [ `GET | `POST ] * Uri.t ] * string ]

  let log error_value = 
    Error.log_client_error error_value

end

module Standalone = struct
  type t = {
    configuration: Configuration.standalone;
    engine: Engine.t;
  }
  let create configuration =
    Engine.load 
      ~configuration:(Configuration.standalone_engine configuration)
    >>= fun engine ->
    return { engine; configuration }

  let release t =
    Engine.unload t.engine

end
module Http_client = struct
  type t = {
    configuration: Configuration.client;
    base_uri: Uri.t;
  }
  let create configuration =
    let open Configuration in
    let conn_string = connection configuration in
    of_result (
      try `Ok (Uri.of_string conn_string) with
      | e ->
        `Error (`Wrong_configuration (`Found conn_string, `Exn  e)))
    >>= fun uri ->
    let base_uri = Uri.add_query_param' uri ("token", token configuration) in
    return { configuration; base_uri; }

  let client_error ~where ~what = `Client (`Http (where, what) : Error.t)
  let client_error_exn where exn = `Client (`Http (where, `Exn exn) : Error.t)
  let fail_client (e : Error.t) = fail (`Client e)


  let call_json ?(args=[]) t ~meta_meth ~path =
    let uri =
      Uri.with_path t.base_uri path
      |> (fun u -> Uri.add_query_param' u ("format", "json"))
      |> (fun init ->
          List.fold args ~init ~f:(fun u arg -> Uri.add_query_param' u arg))
    in 
    let meth, body =
      match meta_meth with
      | `Get -> `GET, `Empty
      | `Post_message m ->
        `POST, `String (Protocol.Up_message.serialize m)
    in
    let where = `Call (meth, uri) in
    wrap_deferred
      ~on_exn:(fun e -> client_error_exn where e) (fun () ->
          let uri_ = uri in
          Log.(s "HTTP call: " % uri uri_ @ very_verbose);
          Cohttp_lwt_unix.Client.call ~body meth uri)
    >>= fun (response, body) ->
    wrap_deferred ~on_exn:(fun e -> client_error ~where ~what:(`Exn e))
      (fun () -> Cohttp_lwt_body.to_string body)
    >>= fun body_str ->
    begin match Cohttp.Response.status response with
    | `OK ->
      begin try
        return (Yojson.Safe.from_string body_str)
      with e ->
        fail_client (`Http (where, `Json_parsing (body_str, `Exn e)))
      end
    | `Not_found ->
        fail_client (`Server_error_response (where, body_str))
    | other ->
      fail_client (`Http (where, `Wrong_response (response, body_str)))
    end

  let filter_down_message json ~f ~loc =
    begin try
      let message = (Protocol.Down_message.of_json_exn json) in
      begin match f message with
      | Some x -> return x
      | None ->
        fail_client (`Http (loc, `Unexpected_message message))
      end
    with _ -> fail_client (`Http (loc, `Wrong_json json))
    end


  let get_targets_and_filter t ~ids ~filter =
    call_json t ~path:"/api" ~meta_meth:(`Post_message (`Get_targets ids))
    >>= filter_down_message
      ~loc:`Targets
      ~f:(function
        | `List_of_targets tl -> filter tl
        | _ -> None)

  let get_current_targets t =
    get_targets_and_filter t ~ids:[] ~filter:(fun s -> Some s)
  let get_target t ~id =
    get_targets_and_filter t ~ids:[id] ~filter:(function [one] -> Some one | _ -> None)
  let get_targets t ~id_list =
    get_targets_and_filter t ~ids:id_list ~filter:(fun s -> Some s)

  let add_targets t ~targets =
    let msg = `Submit_targets targets in
    call_json t ~path:"/api" ~meta_meth:(`Post_message msg) 
    >>= fun (_: Json.t) ->
    return ()


  let kill_or_restart t what =
    call_json t ~path:"/api" ~meta_meth:(`Post_message what)
    >>= fun (_: Json.t) ->
    return ()

  let kill t id_list = kill_or_restart t (`Kill_targets id_list)
  let restart t id_list = kill_or_restart t (`Restart_targets id_list)

  let call_query t ~target query =
    let id = Target.id target in
    let message = `Call_query (id, query) in
    call_json t ~path:"/api" ~meta_meth:(`Post_message message)
    >>= fun json ->
    filter_down_message json ~loc:(`Target_query (id, query))
      ~f:(function `Query_result s -> Some s | _ -> None)

  let get_list_of_target_ids t query =
    let rec call_up_msg msg =
      call_json t ~path:"/api" ~meta_meth:(`Post_message msg)
      >>= filter_down_message
        ~loc:`Targets
        ~f:(function
          | `List_of_target_ids (tl, (_ : Time.t)) -> Some (`Done tl)
          | `Deferred_list_of_target_ids (id, total) -> (* id × total-length *)
            Log.(s "Deferred_list_of_target_ids: " % s id % sf "(total: %d)" total
                 @ verbose);
            (Some (`Get_less (id, total)))
          | _ -> None)
      >>= begin function
      | `Done l -> return l
      | `Get_less (id, total) ->
        call_up_msg (`Get_deferred (id, 0, 200))
      end
    in
    call_up_msg (`Get_target_ids query)

  let call_process_holder t message =
    call_json t ~path:"/api" ~meta_meth:(`Post_message (`Process message))
    >>= fun down ->
    filter_down_message down ~loc:`Process_holder
      ~f:(function `Process p -> Some p | other -> None)


end

type t = [
  | `Standalone of Standalone.t
  | `Http_client of Http_client.t
]

let create configuration =
  match Configuration.mode configuration with
  | `Standalone st -> 
    Standalone.create st
    >>= fun standalone ->
    return (`Standalone standalone)
  | `Client c ->
    Http_client.create c
    >>= fun client ->
    return (`Http_client client)
  | `Server s ->
    Standalone.create (Configuration.standalone_of_server s)
    >>= fun standalone ->
    return (`Standalone standalone)

let release  = function
| `Standalone s -> Standalone.release s
| `Http_client c -> return ()

let as_client ~configuration ~f =
  create configuration
  >>= fun client ->
  begin try f ~client with
  | e -> 
    release client
    >>= fun () ->
    fail (`Failure (fmt "as_client: client function threw exception: %s" 
                      (Printexc.to_string e)))
  end
  >>< begin function
  | `Ok o ->
    release client
    >>= fun () ->
    return o
  | `Error e ->
    release client >>< fun _ ->
    fail e
  end

let add_targets t tlist =
  match t with
  | `Standalone s ->
    let open Standalone in
    Engine.add_targets s.engine tlist
  | `Http_client c ->
    Http_client.add_targets c tlist

let all_visible_targets = function
| `Standalone s ->
  let open Standalone in
  Engine.all_visible_targets s.engine
| `Http_client c ->
  Http_client.get_current_targets c

let kill t id_list =
  match t with
  | `Standalone s ->
    let open Standalone in
    Deferred_list.while_sequential id_list (fun id -> Engine.kill s.engine ~id)
    >>= fun (_ : unit list) ->
    return ()
  | `Http_client c ->
    Http_client.kill c id_list

let get_target t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Engine.get_target s.engine id
  | `Http_client c ->
    Http_client.get_target c ~id

let get_targets t ~id_list =
  match t with
  | `Standalone s ->
    let open Standalone in
    Deferred_list.while_sequential id_list ~f:(fun id ->
        Engine.get_target s.engine id)
  | `Http_client c ->
    Http_client.get_targets c ~id_list

let get_list_of_target_ids t ~query =
  match t with
  | `Standalone s ->
    Engine.get_list_of_target_ids s.Standalone.engine query
  | `Http_client c ->
    Http_client.get_list_of_target_ids c (query, [])

let call_query t ~target query =
  match t with
  | `Standalone s ->
    let host_io = Engine.host_io s.Standalone.engine in
    Plugin.call_query ~target query ~host_io
  | `Http_client c ->
    Http_client.call_query c ~target query
    >>< begin function 
    | `Ok s -> return s
    | `Error (`Failure e) -> fail (Log.s e)
    | `Error (`Client e) -> fail (Error.log e)
    end

let restart t ids =
  match t with
  | `Standalone s ->
    let open Standalone in
    Deferred_list.while_sequential ids (Engine.restart_target s.engine)
    >>= fun (_ : Target.id list) ->
    return ()
  | `Http_client c ->
    Http_client.restart c ids

let get_local_engine = function
| `Standalone s -> (Some s.Standalone.engine)
| `Http_client _ -> None

let configuration = function
| `Standalone s ->
  Configuration.create (`Standalone s.Standalone.configuration)
| `Http_client h ->
  Configuration.create (`Client h.Http_client.configuration)

let call_process_holder t message =
  match t with
  | `Standalone _ ->
    fail (`Failure "Cannot use process-holder in standalone mode")
  | `Http_client c ->
    Http_client.call_process_holder c message


(*
  Submit a workflow:

   - make sure the target is active,
   - render all the dependencies/failure-callbacks/success-callbacks,
   - writes errors to Log
*)
let flatten_to_pure_targets ?add_tags t =
  let module T = Ketrew_pure.Target in
  t#activate;
  let todo = ref [t] in
  let to_return = ref [] in
  let add_todos ~more_tags l =
    List.iter l (* not_already_done *) ~f:(fun x ->
        match
          List.exists !todo ~f:(fun y -> x#id = y#id)
          || List.exists !to_return ~f:(fun y -> T.id y = x#id)
        with
        | true -> ()
        | false ->
          x#add_recursive_tags more_tags;
          todo := x :: !todo
      ) in
  let add_to_return ~more_tags t =
    t#add_tags more_tags;
    begin match add_tags with
    | None -> ()
    | Some tags -> t#add_tags tags
    end;
    to_return := t#render :: !to_return in
  let rec go_through_deps () =
    match !todo with
    | t :: more ->
      let more_tags = t#get_recursive_tags in
      add_to_return ~more_tags t; (* In !todo, so was not in !to_return. *)
      add_todos ~more_tags t#depends_on;
      add_todos ~more_tags t#on_failure_activate;
      add_todos ~more_tags t#on_success_activate;
      todo := List.filter !todo ~f:(fun x -> x#id <> t#id);
      go_through_deps ();
    | [] -> () in
  go_through_deps ();
  !to_return

let submit_generic
    ?override_configuration ?add_tags (t : EDSL.Internal_representation.t) =
  let targets = flatten_to_pure_targets t ?add_tags in
  let configuration =
    Configuration.load_exn
      (match override_configuration with
      | Some c -> `Override c | None -> `Guess) in
  match Lwt_main.run (
      as_client ~configuration ~f:(fun ~client -> add_targets client targets)
    ) with
  | `Ok () -> ()
  | `Error e ->
    Log.(s "Run-error: " % s (Error.to_string e) @ error);
    failwith (Error.to_string e)

let submit ?override_configuration ?add_tags (t : EDSL.user_target)  =
  submit_generic ?override_configuration ?add_tags
    (t :> EDSL.Internal_representation.t)

let submit_workflow ?override_configuration ?add_tags (t : _ EDSL.workflow_node) =
  submit_generic ?override_configuration ?add_tags (t#render)
