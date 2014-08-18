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

(** Definition of the basic building bloc of a workflow. *)
open Ketrew_pervasives

(** Definition of command-lines to run on a given {!Ketrew_host.t}. *)
module Command : sig

    type t = Ketrew_gen_target_v0_t.command
    (** The type of commands. *)

    val shell : ?host:Ketrew_host.t -> string -> t
    (** Create a “shell” command for a given [Host.t]. *)

    val program: ?host:Ketrew_host.t -> Ketrew_program.t -> t
    (** Create a [Command.t] that runs a {!Ketrew_program.t}. *)

    val get_host : t -> Ketrew_host.t
    (** Get the host. *)

    val log: t -> Log.t
    (** Get a display document. *)

    val to_string_hum : t -> string
    (** Get a Human-readable string. *)

    val get_output :
      t ->
      (string * string,
       [> `Host of _ Ketrew_host.Error.non_zero_execution ])
      Deferred_result.t
    (** Run the command and get its [(stdout, stderr)] pair. *)

    val run :
      t ->
      (unit,
       [> `Host of _ Ketrew_host.Error.non_zero_execution ])
      Deferred_result.t
    (** Run the command and ignore its [(stdout, stderr)] pair. *)

  end

type build_process = [
  | `Artifact of Ketrew_artifact.t (** Literal, already-built, artifact *)
  | `Direct_command of Command.t (** [Command.t] to run. *)
  | `Long_running of (string * string) 
  (** Use a long-running plugin: [(plugin_name, initial_run_parameters)].  *)
]
(** Specification of how to build a target. {ul
   {li  [`Artifact a]: literal, already-built, artifact, }
   {li [`Direct_command c]: a [Command.t] to run (should produce a [Volume.t]), }
   {li [`Get_output c]: a [Command.t] to run and get its [stdout] (should
       produce a value), }
   {li [`Long_running (plugin_name, initial_run_parameters)]:
    Use a long-running plugin. }
    } 
*)

val nop : build_process
(** A build process that does nothing. *)

type submitted_state = [ `Created of Time.t ]
type activated_state =
    [ `Activated of Time.t * submitted_state * [ `Dependency | `User ] ]
type run_bookkeeping = {
  plugin_name : string;
  run_parameters : string;
  run_history : string list;
}
type running_state = [ `Running of run_bookkeeping * activated_state ]
type death_reason = [ `Failed of string | `Killed of string ]
type finished_state = [
  | `Dead of Time.t * [ activated_state | running_state ] * death_reason
  | `Successful of
      Time.t * [ activated_state | running_state ] * Ketrew_artifact.t 
]
(** The potential cases of a “finished” target (defined as {[
type finished_state = [
  | `Dead of Time.t * [ activated_state | running_state ] * death_reason
  | `Successful of
      Time.t * [ activated_state | running_state ] * Ketrew_artifact.t ] ]}).
    ]} — ocamldoc wrong display). *)

type workflow_state = [
  | submitted_state
  | activated_state
  | running_state
  | finished_state
]
(** The all the potential cases of the state of a target (defined as {[
type workflow_state = [
  | submitted_state
  | activated_state
  | running_state
  | finished_state
]
    ]} — ocamldoc wrong display). *)

type id = Unique_id.t
(** The identifiers of targets. *)

module Condition : sig
  type t = [
    | `True
    | `False
    | `Volume_exists of Ketrew_artifact.Volume.t
    | `Volume_size_bigger_than of Ketrew_artifact.Volume.t * int
    | `Command_returns of Command.t * int
    | `And of t list
  ]
  (** A execution anti-condition, the condition defines when a target is
    (already) ready: {ul
    {li with [`False] the target always runs (because never “ready”),}
    {li with [`True] the target never runs (a bit useless),}
    {li with [`Volume_exists v] the target runs if the volume does not exist
    ([make]-like behavior).}
    {li with [`Volume_size_bigger_than (v, sz)] Ketrew will get the total size
    of the volume (in bytes) and check that it is bigger.}
    {li with [`Command_returns (c, v)] Ketrew will run the {!Command.t} and
    check its return value.}
    {li [`And list_of_conditions] is a conjunction of conditions.}
      }
  *)

  val log: t -> Log.t
  val to_string_hum: t -> string

end

module Equivalence: sig
  type t = Ketrew_gen_target_v0_t.equivalence

end

type t = {
  id : id;
  name : string;
  persistance : [ `Input_data | `Recomputable of float | `Result ];
  metadata : Ketrew_artifact.Value.t;
  dependencies : id list;
  if_fails_activate : id list;
  make : build_process;
  condition : Condition.t option;
  equivalence: Equivalence.t;
  history : workflow_state;
}
(** The fat record holding targets. *)

val create :
  ?id:id -> ?name:string ->
  ?persistance:[ `Input_data | `Recomputable of float | `Result ] ->
  ?metadata:Ketrew_artifact.Value.t ->
  ?dependencies:id list ->
  ?if_fails_activate:id list ->
  ?make:build_process -> 
  ?condition:Condition.t ->
  ?equivalence: Equivalence.t ->
  unit ->
  t
(** Create a target value (not stored in the DB yet). *)

val activate_exn : t -> by:[ `Dependency | `User ] -> t
(** Get an activated target out of a “submitted” one, 
    raises [Invalid_argument _] if the target is in a wrong state. *)

val make_succeed_exn : t -> Ketrew_artifact.t -> t
(** Get a successfully finished target out of an activated  or running one, 
    raises [Invalid_argument _] if the target is in a wrong state. *)

val kill_exn : ?msg:string -> t -> t
(** Get dead target out of an activated  or running one, 
    raises [Invalid_argument _] if the target is in a wrong state. *)

val make_fail_exn : ?msg:string -> t -> t
(** Get dead target out of an activated  or running one, 
    raises [Invalid_argument _] if the target is in a wrong state. *)

val set_running_exn : t -> plugin_name:string -> run_parameters:string -> t
(** Get a running target out of an activated “long-running-plugin” one,
    raises [Invalid_argument _] if the target is in a wrong state. *)

val update_running_exn : t -> run_parameters:string -> t
(** Get a new target updated with new run paramters (from a plugin), 
    raises [Invalid_argument _] if the target is in a wrong state. *)

val active :
  ?id:id -> ?name:string ->
  ?persistance:[ `Input_data | `Recomputable of float | `Result ] ->
  ?metadata:Ketrew_artifact.Value.t ->
  ?dependencies:id list ->
  ?if_fails_activate:id list ->
  ?make:build_process ->
  ?condition:Condition.t ->
  ?equivalence: Equivalence.t ->
  unit -> t
(** Like {!create} but set as already activated. *)

val reactivate: 
  ?with_id:string ->
  ?with_name:string ->
  ?with_metadata:Ketrew_artifact.Value.t ->
  t -> t
(** “Clone” the target as an activated new target. *)

val is_equivalent: t -> t -> bool
(** Tell whether the first on is equivalent to the second one. This not
    a commutative operation: the function does not look at
    the second target's [Equivalence] field. *)

val id : t -> Unique_id.t
(** Get a target's id. *)

val name : t -> string
(** Get a target's user-defined name. *)

val serialize : t -> string
(** Serialize a target (for the database). *)

val deserialize :
  string ->
  (t, [> `Target of [> `Deserilization of string ] ])
  Result.t
(** Deserilize a target from a string. *)

val log : t -> Log.t
(** Get a [Log.t] “document” to display the target. *)

val should_start:
  t ->
  (bool, [> `Host of _ Ketrew_host.Error.non_zero_execution 
         | `Volume of [> `No_size of Log.t] ]) Deferred_result.t
(** Check whether a target is ready or should start, given its condition.  *)

val did_ensure_condition:
  t ->
  (bool, [> `Host of _ Ketrew_host.Error.non_zero_execution 
         | `Volume of [> `No_size of Log.t] ]) Deferred_result.t
(** Check whether a target actually did its job, given its condition.  *)

(** Basic boolean queries on targets. *)
module Is: sig
  val created: t -> bool
  val activated: t -> bool
  val running: t -> bool
  val finished: t -> bool
  val failed: t -> bool
  val successful: t -> bool
  val killable: t -> bool
  val activated_by_user: t -> bool
end

(** Get the most recent serialized 
    [run_parameters] if the target is a “long-running”,
    [None] otherwise. *)
val latest_run_parameters: t -> string option
