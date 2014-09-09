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
    engine: Ketrew_state.t;
  }
  let create configuration =
    Ketrew_state.load 
      ~configuration:(Ketrew_configuration.standalone_engine configuration)
    >>= fun engine ->
    return { engine; configuration }

  let release t =
    Ketrew_state.unload t.engine

end
module Http_client = struct
  type t = {
    configuration: Ketrew_configuration.client;
  }
  let create configuration =
    return { configuration }
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
| `Http_client c -> fail (`Failure "not implemented")

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
    Deferred_list.while_sequential tlist ~f:(Ketrew_state.add_target s.engine)
    >>= fun _ ->
    return ()
  | `Http_client c -> fail (`Failure "not implemented")

let current_targets = function
| `Standalone s ->
  let open Standalone in
  Ketrew_state.current_targets s.engine
| `Http_client c -> fail (`Failure "not implemented")

let archived_targets = function
| `Standalone s ->
  let open Standalone in
  Ketrew_state.archived_targets s.engine
| `Http_client c -> fail (`Failure "not implemented")

let kill t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_state.kill s.engine ~id
  | `Http_client c ->
    fail (`Failure "not implemented")

let archive t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_state.archive_target s.engine id
  | `Http_client c ->
    fail (`Failure "not implemented")

let is_archived t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_state.is_archived s.engine id
  | `Http_client c ->
    fail (`Failure "not implemented")

let get_target t ~id =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_state.get_target s.engine id
  | `Http_client c ->
    fail (`Failure "not implemented")


let get_current_graph t =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_state.Target_graph.get_current s.engine
  | `Http_client c ->
    fail (`Failure "not implemented")

let call_query t ~target query =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_plugin.call_query ~target query
  | `Http_client c ->
    fail (Log.s "NOT IMPLEMENTED")

let restart_target t ~target =
  match t with
  | `Standalone s ->
    let open Standalone in
    Ketrew_state.restart_target ~state:s.engine target
  | `Http_client c ->
    fail (`Failure "not implemented")

let get_local_engine = function
| `Standalone s -> (Some s.Standalone.engine)
| `Http_client _ -> None

