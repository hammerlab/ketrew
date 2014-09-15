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
    let error_loc = `Call (meth, uri) in
    wrap_deferred
      ~on_exn:(fun e -> `Client (`Http (error_loc, `Exn e))) (fun () ->
          Cohttp_lwt_unix.Client.call ~body meth uri)
    >>= fun (response, body) ->
    begin match Cohttp_lwt_unix.Client.Response.status response with
    | `OK ->
      begin match body with
      | `Empty ->
        fail (`Client (`Http (error_loc, `Wrong_response (response, body))))
      | `String s -> return s
      | `Stream s -> lwt_stream_to_string s
      end
      >>= fun body_str ->
      begin try
        return (Yojson.Basic.from_string body_str)
      with e ->
        fail (`Client (`Http (error_loc, `Json_parsing (body_str, `Exn e))))
      end
    | other ->
      fail (`Client (`Http (error_loc, `Wrong_response (response, body))))
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
    let id, error_loc, path =
      match what with
      | `Kill_target i as e -> i, e, "/kill-targets" 
      | `Archive_target i as e -> i, e, "/archive-targets"
    in
    let json = `List [`String id] in
    call_json t ~path ~meta_meth:(`Post_json json) 
    >>= fun json ->
    begin try
      Serialize_happenings.of_json_exn json |> return
    with e ->
      fail (`Client (`Http (error_loc, `Wrong_json json)))
    end

  let kill t ~id = kill_or_archive t (`Kill_target id)
  let archive t ~id = kill_or_archive t (`Archive_target id)

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
  | `Server c ->
    fail (`Wrong_configuration (`Found "server", `Expected "client/standalone"))

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
    Deferred_list.while_sequential tlist ~f:(Ketrew_engine.add_target s.engine)
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

let kill t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.kill s.engine ~id
  | `Http_client c ->
    Http_client.kill c ~id

let archive t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.archive_target s.engine id
  | `Http_client c ->
    Http_client.archive c ~id

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


let get_current_graph t =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.Target_graph.get_current s.engine
  | `Http_client c ->
    fail (`Failure "get_current_graph not implemented")

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

let restart_target t ~target =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_engine.restart_target s.engine target
  | `Http_client c ->
    fail (`Failure "restart_target not implemented")

let get_local_engine = function
| `Standalone s -> (Some s.Standalone.engine)
| `Http_client _ -> None

