(** The “application” state; the Engine. *)

open Ketrew_pervasives

(** Definition of the configuration (input to state creation; contents of the
    future config-file). *)
module Configuration :
  sig
    type t
    (** The contents of the configuration. *)

    val create :
      ?persistent_state_key:string -> database_parameters:string -> unit -> t
    (** Create a configuration, [persistent_state_key] is the “key” of the
        state storage in the database, [database_parameters] are used to call
        {!Ketrew_database.load}. *)

  end

type t
(** The contents of the application state. *)

val default_plugins :
  (string * (module Ketrew_long_running.LONG_RUNNING)) list
(** The “long-running” plugins loaded by default. *)

val create :
  ?plugins:(string * (module Ketrew_long_running.LONG_RUNNING)) list ->
  Configuration.t ->
  (t, 'a) Deferred_result.t
(** Initialize the state. *)

val add_target :
  t ->
  Ketrew_target.t ->
  (unit,
   [> `Database of [> `Load ] * string
    | `Database_unavailable of Ketrew_target.id
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Persistent_state of [> `Deserilization of string ]
    | `System of [> `File_info of string ] * [> `Exn of exn ] ])
  Deferred_result.t
(** Add a target to the state. *)

val current_targets :
  t ->
  (Ketrew_target.t list,
   [> `Database of [> `Load ] * string
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of Ketrew_target.id
    | `Persistent_state of [> `Deserilization of string ]
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
(** Get the list of targets currently handled. *)
  
val what_happened_to_string :
  [< `Target_activated of string * [< `Dependency ]
   | `Target_died of
       string *
       [< `Dependencies_died
        | `Failed_to_start of string * string
        | `Failed_to_update of string * string
        | `Plugin_not_found of string
        | `Process_failure ]
   | `Target_started of string * string
   | `Target_succeeded of
       string * [< `Artifact_literal | `Artifact_ready | `Process_success ] ] ->
  string
(** Transform an item of the result of {!step} to a human-readable string. *)

val step :
  t ->
  ([ `Target_activated of Ketrew_target.id * [ `Dependency ]
   | `Target_died of
       Ketrew_target.id  *
       [ `Dependencies_died
       | `Failed_to_start of string * string
       | `Failed_to_update of string * string
       | `Plugin_not_found of string
       | `Process_failure ]
   | `Target_started of Ketrew_target.id * string
   | `Target_succeeded of
       Ketrew_target.id *
       [ `Artifact_literal | `Artifact_ready | `Process_success ] ]
   list,
   [> `Database of [> `Load ] * string
    | `Database_unavailable of Ketrew_target.id
    | `Host of [> `Execution of string * string * string * string ]
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
   [> `Database of [> `Load ] * string
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of string
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
(** Get the state description of a given target (by “id”). *)
