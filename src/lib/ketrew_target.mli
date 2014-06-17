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
  | `Get_output of Command.t (** [Command.t] to run and get its [stdout]. *) 
  | `Long_running of (string * string) 
  (** Use a long-running plugin: [(plugin_name, initial_run_parameters)].
  *)
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

type t = {
  id : id;
  name : string;
  persistance : [ `Input_data | `Recomputable of float | `Result ];
  metadata : Ketrew_artifact.value;
  dependencies : id list;
  make : build_process;
  result_type : Ketrew_artifact.Type.t;
  history : workflow_state;
}
(** The fat record holding targets. *)

val create :
  ?id:id -> ?name:string ->
  ?persistance:[ `Input_data | `Recomputable of float | `Result ] ->
  ?metadata:Ketrew_artifact.value ->
  ?dependencies:id list -> ?make:build_process -> Ketrew_artifact.Type.t -> t
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
  ?metadata:Ketrew_artifact.value ->
  ?dependencies:id list -> ?make:build_process -> Ketrew_artifact.Type.t -> t
(** Like {!create} but set as already activated. *)

val id : t -> Unique_id.t
(** Get a target's id. *)

val serialize : t -> string
(** Serialize a target (for the database). *)

val deserialize :
  string ->
  (t, [> `Target of [> `Deserilization of string ] ])
  Result.t
(** Deserilize a target from a string. *)

val log : t -> Log.t
(** Get a [Log.t] “document” to display the target. *)

