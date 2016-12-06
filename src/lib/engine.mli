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

(** The engine of the actual Workflow Engine. *)

open Ketrew_pure.Internal_pervasives
open Unix_io

type t
(** The contents of the application engine. *)

val with_engine:
  configuration:Configuration.engine ->
  (engine:t ->
   (unit, [> `Database of Persistent_data.Error.database
          | `Failure of string
          | `Dyn_plugin of
               [> `Dynlink_error of Dynlink.error | `Findlib of exn ]
          ] as 'merge_error) Deferred_result.t) ->
  (unit, 'merge_error) Deferred_result.t
(** Create a {!engine.t}, run the function passed as argument, and properly dispose of it. *)

val load:
  configuration:Configuration.engine ->
  (t,
   [> `Database of Persistent_data.Error.database
   | `Failure of string
   | `Dyn_plugin of
        [> `Dynlink_error of Dynlink.error | `Findlib of exn ]
   ]) Deferred_result.t

val unload: t ->
  (unit, [>
      | `Database of  Persistent_data.Error.database
    ]) Deferred_result.t

val configuration: t -> Configuration.engine
(** Retrieve the configuration. *)

val add_targets :
  t ->
  Ketrew_pure.Target.t list ->
  (unit,
   [> `Database of Persistent_data.Error.database
   ]) Deferred_result.t
(** Add a list of targets to the engine. *)

val get_target: t -> Unique_id.t ->
  (Ketrew_pure.Target.t,
   [> `Database of Persistent_data.Error.database]) Deferred_result.t
(** Get a target from its id. *)


val get_list_of_target_ids: t ->
  Ketrew_pure.Protocol.Up_message.target_query ->
  (Ketrew_pure.Target.id list,
   [> `Database of Persistent_data.Error.database ]) Deferred_result.t
(** Get only the Ids of the targets for a given “query”:

- [`All] for all the targets visible to the engine.
- [`Not_finished_before _] for the targets that were not finished at a given date.
*)

val next_changes: t -> (Persistent_data.Change.t list, 'a) Deferred_result.t
(** Block until the next batch of changes happening at the persistence-level,
    this stream is itself rate-limited. *)

module Run_automaton : sig

  type step_allowed_errors = [
    | `Database of Persistent_data.Error.database 
    | `List of step_allowed_errors list
  ]
  (** This type represents the errors that are allowed to escape the {!step}
      function.
  
      Those errors are so fatal that the engine should not really continue:
      the database is not responding, or some data is missing or has a wrong
      format.

      The type system makes sure that all other errors are dealt with by the
      state machine.
  *)

  val step :
    t ->
    (bool, step_allowed_errors ) Deferred_result.t
  (** Run one step of the engine; [step] returns [true] if something happened. *)

  val fix_point: t ->
    ([ `Steps of int], step_allowed_errors) Deferred_result.t
  (** Run {!step} many times until nothing happens or nothing “new” happens. *)


  val try_to_fix_step_error: t ->
    info:string ->
    step_allowed_errors ->
    (unit,
     [> `Database of Persistent_data.Error.database
     | `Not_fixable of step_allowed_errors ])
      Deferred_result.t
end

val get_status : t -> Ketrew_pure.Target.id ->
  (Ketrew_pure.Target.State.t,
   [> `Database of Persistent_data.Error.database
   | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
   | `System of [> `File_info of string ] * [> `Exn of exn ]])
    Deferred_result.t
(** Get the state description of a given target (by “id”). *)

val kill : t -> id:Unique_id.t ->
  (unit,
   [> `Database of Persistent_data.Error.database ]) Deferred_result.t
(** Kill a target *)

val restart_target: t -> Ketrew_pure.Target.id ->
  (Ketrew_pure.Target.id,
   [> `Database of Persistent_data.Error.database ]) Deferred_result.t
(** Make new activated targets out of a given target and its “transitive
    reverse dependencies” *)

val host_io: t -> Host_io.t
