open Ketrew_pervasives

module Command :
  sig
    type t
    val shell : ?host:Ketrew_host.t -> string -> t
    val get_host : t -> Ketrew_host.t
    val to_string_hum : t -> string
    val get_output :
      t ->
      (string * string,
       [> `Host of [> `Execution of string * string * string * string ] ])
      Deferred_result.t
    val run :
      t ->
      (unit,
       [> `Host of [> `Execution of string * string * string * string ] ])
      Deferred_result.t
  end
type build_process =
    [ `Artifact of Ketrew_artifact.t
    | `Direct_command of Command.t
    | `Get_output of Command.t
    | `Long_running of string * string ]
val nop : build_process
type submitted_state = [ `Created of Time.t ]
type activated_state =
    [ `Activated of
        Time.t * submitted_state * [ `Dependency | `User ] ]
type run_bookkeeping = {
  plugin_name : string;
  run_parameters : string;
  run_history : string list;
}
type running_state = [ `Running of run_bookkeeping * activated_state ]
type death_reason = [ `Failed of string | `Killed of string ]
type finished_state =
    [ `Dead of
        Time.t *
        [ `Activated of
            Time.t * submitted_state *
            [ `Dependency | `User ]
        | `Running of run_bookkeeping * activated_state ] * death_reason
    | `Successful of
        Time.t *
        [ `Activated of
            Time.t * submitted_state *
            [ `Dependency | `User ]
        | `Running of run_bookkeeping * activated_state ] * Ketrew_artifact.t ]
type workflow_state =
    [ `Activated of
        Time.t * submitted_state * [ `Dependency | `User ]
    | `Created of Time.t
    | `Dead of
        Time.t *
        [ `Activated of
            Time.t * submitted_state *
            [ `Dependency | `User ]
        | `Running of run_bookkeeping * activated_state ] * death_reason
    | `Running of run_bookkeeping * activated_state
    | `Successful of
        Time.t *
        [ `Activated of
            Time.t * submitted_state *
            [ `Dependency | `User ]
        | `Running of run_bookkeeping * activated_state ] * Ketrew_artifact.t ]
type id = Unique_id.t
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
val create :
  ?name:string ->
  ?persistance:[ `Input_data | `Recomputable of float | `Result ] ->
  ?metadata:Ketrew_artifact.value ->
  ?dependencies:id list -> ?make:build_process -> Ketrew_artifact.Type.t -> t
val activate_exn : t -> by:[ `Dependency | `User ] -> t
val make_succeed_exn : t -> Ketrew_artifact.t -> t
val kill_exn : ?msg:string -> t -> t
val make_fail_exn : ?msg:string -> t -> t
val set_running_exn : t -> plugin_name:string -> run_parameters:string -> t
val update_running_exn : t -> run_parameters:string -> t
val active :
  ?name:string ->
  ?persistance:[ `Input_data | `Recomputable of float | `Result ] ->
  ?metadata:Ketrew_artifact.value ->
  ?dependencies:id list -> ?make:build_process -> Ketrew_artifact.Type.t -> t
val id : t -> Unique_id.t
val serialize : 'a -> string
val deserialize :
  string ->
  (t, [> `Target of [> `Deserilization of string ] ])
  Result.t
val log : t -> Log.t
