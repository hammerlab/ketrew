(**************************************************************************)
(*  Copyright 2014, Sebastien Mondet <seb@mondet.org>                     *)
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

open Ketrew_pervasives

module Standalone = struct
  type t = {
    configuration: Ketrew_configuration.standalone;
    engine: Ketrew_engine.t;
  }
  let create configuration =
    Ketrew_engine.load 
      ~configuration:(Ketrew_configuration.standalone_engine configuration)
    >>= fun engine ->
    return { engine; configuration }

  let release t =
    Ketrew_engine.unload t.engine

end
module Http_client = struct
  type t = {
    configuration: Ketrew_configuration.client;
    base_uri: Uri.t;
  }
  let create configuration =
    let open Ketrew_configuration in
    let conn_string = connection configuration in
    of_result (
      try `Ok (Uri.of_string conn_string) with
      | e ->
        `Error (`Wrong_configuration (`Found conn_string, `Exn  e)))
    >>= fun uri ->
    let base_uri = Uri.add_query_param' uri ("token", token configuration) in
    return { configuration; base_uri; }

  let client_error ~where ~what = `Client (`Http (where, what))
  let client_error_exn where exn = `Client (`Http (where, `Exn exn))

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
        `POST, `String (Ketrew_protocol.Post_message.serialize m)
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
    begin match Cohttp_lwt_unix.Client.Response.status response with
    | `OK ->
      begin try
        return (Yojson.Basic.from_string body_str)
      with e ->
        fail (`Client (`Http (where, `Json_parsing (body_str, `Exn e))))
      end
    | `Not_found ->
        fail (`Client (`Server_error_response (where, body_str)))
    | other ->
      fail (`Client (`Http (where, `Wrong_response (response, body_str))))
    end

  let filter_down_message json ~f ~loc =
    begin try
      let message = (Ketrew_protocol.Down_message.of_json_exn json) in
      begin match f message with
      | Some x -> return x
      | None ->
        fail (`Client (`Http (loc, `Unexpected_message message)))
      end
    with _ -> fail (`Client (`Http (loc, `Wrong_json json)))
    end


  let get_targets t ~ids ~filter =
    call_json t ~path:"/api" ~meta_meth:(`Post_message (`Get_targets ids))
    >>= filter_down_message
      ~loc:`Targets
      ~f:(function
        | `List_of_targets tl ->
          filter (List.map tl ~f:Ketrew_target.of_serializable)
        | _ -> None)

  let get_current_targets t = get_targets t ~ids:[] ~filter:(fun s -> Some s)
  let get_target t ~id =
    get_targets t ~ids:[id] ~filter:(function [one] -> Some one | _ -> None)

  let add_targets t ~targets =
    let msg =
      `List_of_targets (List.map targets ~f:Ketrew_target.to_serializable) in
    call_json t ~path:"/add-targets" ~meta_meth:(`Post_message msg) 
    >>= fun (_: Json.t) ->
    return ()


  let kill_or_archive t what =
    let ids, error_loc, path =
      match what with
      | `Kill_targets i as e -> i, e, "/kill-targets" 
      | `Archive_targets i as e -> i, e, "/archive-targets"
      | `Restart_targets i as e -> i, e, "/restart-targets"
    in
    let msg = (`List_of_target_ids ids) in
    call_json t ~path ~meta_meth:(`Post_message msg)
    >>= filter_down_message
      ~loc:error_loc
      ~f:(function `Ok -> Some () | _ -> None)

  let kill t id_list = kill_or_archive t (`Kill_targets id_list)
  let archive t id_list = kill_or_archive t (`Archive_targets id_list)
  let restart t id_list = kill_or_archive t (`Restart_targets id_list)

  let call_query t ~target query =
    let id = Ketrew_target.id target in
    let args = [
      "id", id;
      "query", query;
    ] in
    call_json t ~path:"/target-call-query" ~meta_meth:`Get ~args
    >>= fun json ->
    filter_down_message json ~loc:(`Target_query (id, query))
      ~f:(function `Query_result s -> Some s | _ -> None)

  let call_cleanable_targets ~how_much t =
    let args = [
      "howmuch", (match how_much with `Soft -> "soft" | `Hard -> "hard");
    ] in
    let loc = (`Cleanable_targets how_much) in
    call_json t ~path:"/cleanable-targets" ~meta_meth:`Get ~args
    >>= fun json ->
    filter_down_message json ~loc
      ~f:(function `List_of_target_ids s -> Some s | _ -> None)

end

type t = [
  | `Standalone of Standalone.t
  | `Http_client of Http_client.t
]

let create configuration =
  match Ketrew_configuration.mode configuration with
  | `Standalone st -> 
    Standalone.create st
    >>= fun standalone ->
    return (`Standalone standalone)
  | `Client c ->
    Http_client.create c
    >>= fun client ->
    return (`Http_client client)
  | `Server s ->
    Standalone.create (Ketrew_configuration.standalone_of_server s)
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
  | `Ok () ->
    release client
  | `Error e ->
    release client >>< fun _ ->
    fail e
  end

let add_targets t tlist =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.add_targets s.engine tlist
  | `Http_client c ->
    Http_client.add_targets c tlist

let current_targets = function
| `Standalone s ->
  let open Standalone in
  Ketrew_engine.current_targets s.engine
| `Http_client c ->
  Http_client.get_current_targets c

let kill t id_list =
  match t with
  | `Standalone s ->
    let open Standalone in
    Deferred_list.while_sequential id_list (fun id ->
        Ketrew_engine.kill s.engine ~id)
    >>= fun (_ : unit list) ->
    return ()
  | `Http_client c ->
    Http_client.kill c id_list

let get_target t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.get_target s.engine id
  | `Http_client c ->
    Http_client.get_target c ~id


let targets_to_clean_up t ~how_much =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.Target_graph.(
      get_current s.engine
      >>= fun graph ->
      return (targets_to_clean_up graph how_much)
    )
  | `Http_client c ->
    Http_client.call_cleanable_targets ~how_much c

let call_query t ~target query =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_plugin.call_query ~target query
  | `Http_client c ->
    Http_client.call_query c ~target query
    >>< begin function 
    | `Ok s -> return s
    | `Error (`Failure e) -> fail (Log.s e)
    | `Error (`Client e) -> fail (Ketrew_error.log_client_error e)
    end

let restart_target t ids =
  match t with
  | `Standalone s ->
    let open Standalone in
    Deferred_list.while_sequential ids (Ketrew_engine.restart_target s.engine)
    >>= fun (_ : Ketrew_target.id list) ->
    return ()
  | `Http_client c ->
    Http_client.restart c ids

let get_local_engine = function
| `Standalone s -> (Some s.Standalone.engine)
| `Http_client _ -> None

(*
  Submit a workflow:

   - make sure the target is active,
   - render all the dependencies/fallbacks/success-triggers,
   - writes errors to Log
*)
let user_command_list t =
  t#activate;
  let rec go_through_deps t =
    t#render ::
    List.concat_map t#dependencies ~f:go_through_deps
    @ List.concat_map t#if_fails_activate ~f:go_through_deps
    @ List.concat_map t#success_triggers ~f:go_through_deps
  in
  let targets =
    (go_through_deps t)
    |> List.dedup ~compare:Ketrew_target.(fun ta tb -> compare (id ta) (id tb))
  in
  match targets with
  | first :: more -> (first, more)
  | [] -> assert false (* there is at least the argument one *)

let submit ?override_configuration t =
  let active, dependencies = user_command_list t in
  let config_path = Ketrew_configuration.get_path () in
  match Lwt_main.run (
    Ketrew_configuration.get_configuration ?override_configuration config_path
    >>= fun configuration ->
          as_client ~configuration ~f:(fun ~client ->
              add_targets client (active :: dependencies))
  ) with
  | `Ok () -> ()
  | `Error e ->
    Log.(s "Run-error: " % s (Ketrew_error.to_string e) @ error);
    failwith (Ketrew_error.to_string e)

