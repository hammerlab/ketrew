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
      | `Post_string s -> `POST, `String s
      | `Post_json s -> `POST, `String (Json.to_string s) in
    let where = `Call (meth, uri) in
    wrap_deferred
      ~on_exn:(fun e -> client_error_exn where e) (fun () ->
          let uri_ = uri in
          Log.(s "HTTP call: " % uri uri_ @ very_verbose);
          Cohttp_lwt_unix.Client.call ~body meth uri)
    >>= fun (response, body) ->
    begin match Cohttp_lwt_unix.Client.Response.status response with
    | `OK ->
      begin match body with
      | `Empty ->
        fail (client_error ~where ~what:(`Wrong_response (response, body)))
      | `String s -> return s
      | `Stream s -> lwt_stream_to_string s
      end
      >>= fun body_str ->
      begin try
        return (Yojson.Basic.from_string body_str)
      with e ->
        fail (`Client (`Http (where, `Json_parsing (body_str, `Exn e))))
      end
    | other ->
      fail (`Client (`Http (where, `Wrong_response (response, body))))
    end

  let get_current_targets ~archived t =
    call_json t ~path:"/targets" ~meta_meth:`Get 
      ~args:(if archived then ["archived", "true"] else [])
    >>= fun json ->
    begin match json with
    | `List jsons ->
      Deferred_list.while_sequential jsons (fun json ->
          let s = Yojson.Basic.to_string json in
          of_result (Ketrew_target.deserialize s))
    | other -> 
      fail (`Client (`Http (`Targets, `Wrong_json other)))
    end

  let add_targets t ~targets =
    let body_string =
      List.map targets ~f:Ketrew_target.serialize
      |> String.concat ~sep:", "
      |> fmt "[%s]" in
    call_json t ~path:"/add-targets" ~meta_meth:(`Post_string body_string) 
    >>= fun (_: Json.t) ->
    return ()

  let get_target t ~id =
    call_json t ~path:"/targets" ~meta_meth:`Get ~args:["id", id]
    >>= fun json ->
    begin match json with
    | `List [json] ->
      let s = Yojson.Basic.to_string json in
      of_result (Ketrew_target.deserialize s)
    | other -> 
      fail (`Client (`Http (`Targets, `Wrong_json other)))
    end

  let kill_or_archive t what =
    let ids, error_loc, path =
      match what with
      | `Kill_targets i as e -> i, e, "/kill-targets" 
      | `Archive_targets i as e -> i, e, "/archive-targets"
      | `Restart_targets i as e -> i, e, "/restart-targets"
    in
    let json = `List (List.map ids (fun id -> `String id)) in
    call_json t ~path ~meta_meth:(`Post_json json)
    >>= fun json ->
    begin try
      Serialize_happenings.of_json_exn json |> return
    with e ->
      fail (`Client (`Http (error_loc, `Wrong_json json)))
    end

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
    begin match json with
    | `List [`String s] ->
      return s
    | other -> 
      fail (`Client (`Http (`Target_query (id, query), `Wrong_json other)))
    end

  let call_cleanable_targets ~how_much t =
    let args = [
      "howmuch", (match how_much with `Soft -> "soft" | `Hard -> "hard");
    ] in
    let error = client_error ~where:(`Cleanable_targets how_much) in
    call_json t ~path:"/cleanable-targets" ~meta_meth:`Get ~args
    >>= fun json ->
    begin match json with
    | `Assoc ["to-kill", `List kill; "to-archive", `List archive] ->
      let to_strings strs = 
        Deferred_list.while_sequential strs ~f:(function
          | `String s -> return s
          | other -> fail (error ~what:(`Wrong_json json)))
      in
      to_strings kill
      >>= fun to_kill ->
      to_strings archive
      >>= fun to_archive ->
      return (`To_kill to_kill, `To_archive to_archive)
    | other -> 
      fail (error ~what:(`Wrong_json other))
    end
end

type t = [
  | `Standalone of Standalone.t
  | `Http of Http_client.t
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
    >>= fun _ ->
    return ()
  | `Http_client c ->
    Http_client.add_targets c tlist

let current_targets ?(archived=false) = function
| `Standalone s ->
  let open Standalone in
  Ketrew_engine.current_targets s.engine
  >>= fun current ->
  begin if archived
    then
      Ketrew_engine.archived_targets s.engine
      >>| (@) current
    else return current
  end
| `Http_client c ->
  Http_client.get_current_targets ~archived c

let kill t id_list =
  match t with
  | `Standalone s ->
    let open Standalone in
    Deferred_list.while_sequential id_list (fun id ->
        Ketrew_engine.kill s.engine ~id)
    >>| List.concat
  | `Http_client c ->
    Http_client.kill c id_list

let archive t id_list =
  match t with
  | `Standalone s ->
    let open Standalone in
    Deferred_list.while_sequential id_list (fun id ->
        Ketrew_engine.archive_target s.engine id)
    >>| List.concat
  | `Http_client c ->
    Http_client.archive c id_list

let is_archived t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.is_archived s.engine id
  | `Http_client c ->
    Log.(s "Function is_archived not implemented over HTTP: returning `false`"
         @ warning);
    return false

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
    >>| List.concat
  | `Http_client c ->
    Http_client.restart c ids

let get_local_engine = function
| `Standalone s -> (Some s.Standalone.engine)
| `Http_client _ -> None

