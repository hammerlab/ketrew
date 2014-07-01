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

(** The “application” state; the Engine. *)

open Ketrew_pervasives

type t
(** The contents of the application state. *)

val default_plugins :
  (string * (module Ketrew_long_running.LONG_RUNNING)) list
(** The “long-running” plugins loaded by default. *)

val with_state: 
  ?plugins:(string * (module Ketrew_long_running.LONG_RUNNING)) list ->
  configuration:Ketrew_configuration.t ->
  (state:t ->
   (unit, [> `Database of Ketrew_database.error 
          | `Failure of string] as 'merge_error) Deferred_result.t) ->
  (unit, 'merge_error) Deferred_result.t
(** Create a {!State.t}, run the function passed as argument, and properly dispose of it. *)


val add_target :
  t ->
  Ketrew_target.t ->
  (unit,
   [> `Database of Ketrew_database.error
   | `Database_unavailable of Ketrew_target.id
   | `Persistent_state of [> `Deserilization of string ] ])
  Deferred_result.t
(** Add a target to the state. *)

val archive_target: t ->
  Ketrew_target.id ->
  (unit,
   [> `Database of Ketrew_database.error
   | `Database_unavailable of Ketrew_target.id
   | `Missing_data of Ketrew_target.id
   | `Target of [> `Deserilization of string ]
   | `Persistent_state of [> `Deserilization of string ] ])
    Deferred_result.t
(** Move a target to the “archived” list. *)

val get_target: t -> Unique_id.t ->
  (Ketrew_target.t,
   [> `Database of [> `Get of string | `Load of string ] * string
   | `Missing_data of string
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Get a target from its id. *)

val current_targets :
  t ->
  (Ketrew_target.t list,
   [> `Database of Ketrew_database.error
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of Ketrew_target.id
    | `Persistent_state of [> `Deserilization of string ]
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
(** Get the list of targets currently handled. *)
  
val archived_targets :
  t ->
  (Ketrew_target.t list,
   [> `Database of Ketrew_database.error
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of Ketrew_target.id
    | `Persistent_state of [> `Deserilization of string ]
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
(** Get the list of targets that have been archived. *)

val is_archived: t -> Unique_id.t -> 
  (bool, 
   [> `Database of [> `Get of string | `Load of string ] * string
   | `Missing_data of Ketrew_target.id
   | `Persistent_state of [> `Deserilization of string ]
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Check whether a target is in the “archive”. *)
  
type happening =
  [ `Target_activated of Ketrew_target.id * [ `Dependency ]
  | `Target_died of
      Ketrew_target.id  *
      [ `Dependencies_died
      | `Plugin_not_found of string
      | `Wrong_type
      | `Killed
      | `Long_running_unrecoverable of string * string
      | `Process_failure ]
  | `Target_started of Ketrew_target.id * string
  | `Target_succeeded of
      Ketrew_target.id *
      [ `Artifact_literal | `Artifact_ready | `Process_success ] ]
(** Structured log of what can happen during {!step} or {!kill}. *)

val what_happened_to_string : happening -> string
(** Transform an item of the result of {!step} to a human-readable string. *)

val log_what_happened : happening -> Log.t
(** Transform a {!happening} into {!Log.t} document. *)

val step :
  t ->
  (happening list,
   [> `Database of Ketrew_database.error
    | `Database_unavailable of Ketrew_target.id
    | `Host of _ Ketrew_host.Error.execution
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of Ketrew_target.id
    | `Persistent_state of [> `Deserilization of string ]
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
(** Run one step of the engine; [step] returns a list of “things that
    happened”. *)

val get_status : t -> Ketrew_target.id ->
  (Ketrew_target.workflow_state,
   [> `Database of Ketrew_database.error
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of string
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
(** Get the state description of a given target (by “id”). *)

val kill:  t -> id:Ketrew_target.id ->
  ([> `Target_died of Unique_id.t *
                      [> `Killed 
                      | `Long_running_unrecoverable of string * string
                      | `Plugin_not_found of string ] ]
     list,
   [> `Database of Ketrew_database.error
   | `Failed_to_kill of string
   | `Database_unavailable of string
   | `IO of
        [> `Read_file_exn of string * exn
        | `Write_file_exn of string * exn ]
   | `Missing_data of string
   | `Not_implemented of string
   | `System of [> `File_info of string ] * [> `Exn of exn ]
   | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** Kill a target *)

val long_running_log: state:t -> string -> string -> (string * Log.t) list
(** [long_running_log ~state plugin_name serialized_run_params]
    calls {!Ketrew_long_running.LONG_RUNNING.log} with the right plugin. *)

val additional_queries: state:t -> Ketrew_target.t -> (string * Log.t) list
(** Get the potential additional queries ([(key, description)] pairs) that can
    be called on the target. *)

val call_query: state:t -> target:Ketrew_target.t -> string ->
  (string, Log.t) Deferred_result.t
(** Call a query on a target. *)

val restart_target: state:t -> Ketrew_target.t -> 
  (Ketrew_target.t * Ketrew_target.t list, 
   [> `Database of Ketrew_database.error
   | `Database_unavailable of Ketrew_target.id
   | `Missing_data of Ketrew_target.id
   | `Persistent_state of [> `Deserilization of string ]
   | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** Make new activated targets out of a given target and its “transitive
    reverse dependencies” *)
