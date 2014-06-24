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
