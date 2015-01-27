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

    type t
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

module Build_process: sig
  type t = [
    | `No_operation
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

  val nop : t
  (** A build process that does nothing. *)
end

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

  val eval: t -> (bool,
                  [> `Host of
                       [> `Execution of
                            < host : string; message : string;
                              stderr : string option; stdout : string option >
                       | `Non_zero of string * int
                       | `Ssh_failure of
                            [> `Wrong_log of string
                            | `Wrong_status of Ketrew_unix_process.Exit_code.t ] *
                            string
                       | `System of [> `Sleep of float ] * [> `Exn of exn ]
                       | `Timeout of float
                       | `Unix_exec of string ]
                         Ketrew_host.Error.execution
                  | `Volume of [> `No_size of Ketrew_pervasives.Log.t ] ]) Deferred_result.t

end

module Equivalence: sig
  type t = [
    | `None
    | `Same_active_condition
  ]
end

module State : sig
  type t
  type history = Ketrew_gen_target_v0.State.t
  val simplify: t -> [
      | `Activable
      | `In_progress
      | `Successful
      | `Failed
    ]
  val history: t -> history

  val name: t -> string

  val summary :
    t ->
    [ `Time of Time.t ] * [ `Log of string option ] * [ `Info of string list ]

  val log: ?depth:int ->  t -> Log.t

  module Is : sig
    val building : t -> bool
    val tried_to_start : t -> bool
    val started_running : t -> bool
    val starting : t -> bool
    val still_building : t -> bool
    val still_running : t -> bool
    val ran_successfully : t -> bool
    val successfully_did_nothing : t -> bool
    val active : t -> bool
    val verified_success : t -> bool
    val already_done : t -> bool
    val dependencies_failed : t -> bool
    val failed_running : t -> bool
    val failed_to_kill : t -> bool
    val failed_to_start : t -> bool
    val killing : t -> bool
    val tried_to_kill : t -> bool
    val did_not_ensure_condition : t -> bool
    val killed : t -> bool
    val finished : t -> bool
    val passive : t -> bool
    val killable: t -> bool
  end
end

type t
(** The thing holding targets. *)

val create :
  ?id:id -> ?name:string ->
  ?metadata:[ `String of string ] ->
  ?dependencies:id list ->
  ?if_fails_activate:id list ->
  ?success_triggers:id list ->
  ?make:Build_process.t -> 
  ?condition:Condition.t ->
  ?equivalence: Equivalence.t ->
  ?tags: string list ->
  unit ->
  t
(** Create a target value (not stored in the DB yet). *)

val to_serializable: t -> Ketrew_gen_target_v0.Target.t
val of_serializable: Ketrew_gen_target_v0.Target.t -> t


val id : t -> Unique_id.t
(** Get a target's id. *)

val name : t -> string
(** Get a target's user-defined name. *)

val dependencies: t -> id list
val fallbacks: t -> id list
val success_triggers: t -> id list
val metadata: t -> [`String of string] option
val build_process: t -> Build_process.t
val condition: t -> Condition.t option
val equivalence: t -> Equivalence.t
val additional_log: t -> (Time.t * string) list
val tags: t -> string list
val state: t -> State.t


module Automaton : sig

  (** A {i pure} automaton *)

  type failure_reason = Ketrew_gen_target_v0.Process_failure_reason.t
  type progress = [ `Changed_state | `No_change ]
  type 'a transition_callback = ?log:string -> 'a -> t * progress
  type severity = [ `Try_again | `Fatal ]
  (* type 'a io_action = [ `Succeeded of 'a | `Failed of 'a ] *)
  type bookkeeping = Ketrew_gen_target_v0.Run_bookkeeping.t =
    { plugin_name: string; run_parameters: string}
  type long_running_failure = severity * string * bookkeeping
  type long_running_action =  (bookkeeping, long_running_failure) Pvem.Result.t
  type process_check =
    [ `Successful of bookkeeping | `Still_running of bookkeeping ]
  type process_status_check = (process_check, long_running_failure) Pvem.Result.t
  type condition_evaluation = (bool, severity * string) Pvem.Result.t
  type dependencies_status = 
    [ `All_succeeded | `At_least_one_failed of id list | `Still_processing ]
  type transition = [
    | `Do_nothing of unit transition_callback
    | `Activate of id list * unit transition_callback
    | `Check_and_activate_dependencies of dependencies_status transition_callback
    | `Start_running of bookkeeping * long_running_action transition_callback
    | `Eval_condition of Condition.t * condition_evaluation transition_callback
    | `Check_process of bookkeeping * process_status_check transition_callback
    | `Kill of bookkeeping * long_running_action transition_callback
  ]
  val transition: t -> transition
end

val activate_exn :
  ?log:string -> t -> reason:[ `Dependency of id | `User ] -> t
(** Get an activated target out of a “submitted” one, 
    raises [Invalid_argument _] if the target is in a wrong state. *)

val kill : ?log:string -> t -> t option
(** Get dead target out of a killable one, 
    or [None] if not killable. *)

val reactivate :
  ?with_id:id -> ?with_name:string ->
  ?with_metadata:[`String of string] option  ->
  ?log:string -> t -> t
(** *)

val is_equivalent: t -> t -> bool
(** Tell whether the first on is equivalent to the second one. This not
    a commutative operation: the function does not look at
    the second target's [Equivalence] field. *)

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


(** Get the most recent serialized 
    [run_parameters] if the target is a “long-running”,
    [None] otherwise. *)
val latest_run_parameters: t -> string option


module Stored_target : sig
  type target = t
  type t
  val to_json: t -> Json.t
  (** Serialize a target to [Json.t] intermediate representation. *)

  val serialize : t -> string
  (** Serialize a target (for the database). *)

  val deserialize :
    string ->
    (t, [> `Target of [> `Deserilization of string ] ])
      Result.t
      (** Deserilize a target from a string. *)

  val get_target: t -> [ `Target of target | `Pointer of id ]
  val of_target: target -> t

  val id: t -> id

  val make_pointer: from:target -> pointing_to:target -> t
end


