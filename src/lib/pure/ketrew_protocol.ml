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

module Down_message = struct
  type t = Ketrew_gen_protocol_v0.Down_message.t
  include Json.Make_versioned_serialization
      (Ketrew_gen_protocol_v0.Down_message)
      (Ketrew_gen_versioned.Down_message)

  (*
    let added_target ~original_id ~fresh_id =
    let open Ketrew_gen_protocol_v0.Added_target in
    {original_id; fresh_id}
 *)

  let log : t -> Log.t =
    fun _ ->
    assert false
      (*
    function
  | `List_of_targets ts -> 
    Log.(s "List_of_targets: " % OCaml.list Ketrew_target.log ts)
  | `List_of_query_descriptions tsl ->
    Log.(s "List_of_query_descriptions: "
         % OCaml.list (fun (a, b) -> sf "%s: %s" a b) tsl)
  | `Query_result str ->
    Log.(s "Query_result: " % quote str)
  | `Targets_added tl ->
    let open Ketrew_gen_protocol_v0.Added_target in
    Log.(s "Targets_added: "
         % OCaml.list (fun {original_id; fresh_id} ->
             s original_id % sp % s fresh_id) tl)
  | `Happens _ as hl ->
    Log.(s "Happening-list: " 
         % s (serialize hl))
  | `Clean_up todo ->
    let open Ketrew_gen_protocol_v0.Clean_up_todo_list in
    Log.(s "Clean_up_todo_list: " % n
         % s "to-kill: " % OCaml.list string todo.to_kill 
         % s "to-archive: " % OCaml.list string todo.to_archive) 
  *)
end

module Post_message = struct
  type t = Ketrew_gen_protocol_v0.Post_message.t
  include Json.Make_versioned_serialization
      (Ketrew_gen_protocol_v0.Post_message)
      (Ketrew_gen_versioned.Post_message)

  let log : t -> Log.t =
    fun _ ->
    assert false
      (*
    function
  | `List_of_targets ts -> 
    Log.(s "List_of_targets: " % OCaml.list Ketrew_target.log ts)
  | `List_of_target_ids ts ->
    Log.(s "List_of_target_ids: " % OCaml.list string ts)
  *)

  let to_string_hum t = log t |> Log.to_long_string
end

