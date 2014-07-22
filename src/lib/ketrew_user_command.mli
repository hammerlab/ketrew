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

(** Commands that can be fed to the engine by end-users. *)

open Ketrew_pervasives

type t = [
  | `Fail of Ketrew_pervasives.Log.t
  | `Make of Ketrew_target.t * Ketrew_target.t list
]
(** A user-todo-item, is either asking Ketrew to fail with a message, or
   to start a workflow. *)

val log : t -> Ketrew_pervasives.Log.t
(** Convert a command into a [Log.t] document. *)

val run_list :
  state:Ketrew_state.t ->
  t list ->
  (unit,
   [> `Database of Ketrew_database.error
   | `Database_unavailable of Ketrew_target.id
   | `Failure of string
   | `Missing_data of Ketrew_target.id
   | `Target of [> `Deserilization of string ]
   | `Persistent_state of [> `Deserilization of string ] ])
    Deferred_result.t
(** Run a todo-list with the given [state] instance. *)
