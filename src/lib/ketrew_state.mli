open Ketrew_pervasives
(*
module Persistent_state :
  sig
    type t = { current_targets : Target.id list; }
    val create : unit -> t
    val serialize : 'a -> string
    val unserialize :
      string ->
      (t, [> `Persistent_state of [> `Deserilization of string ] ])
      Ketrew_pervasives.t
    val add : t -> Target.t -> t
    val current_targets : t -> Target.id list
  end
*)
module Configuration :
  sig
    type t
    val create :
      ?persistent_state_key:string -> database_parameters:string -> unit -> t
  end

type t

val default_plugins :
  (string * (module Ketrew_long_running.LONG_RUNNING)) list

val create :
  ?plugins:(string * (module Ketrew_long_running.LONG_RUNNING)) list ->
  Configuration.t ->
  (t, 'a) Deferred_result.t

(*
val database :
  t ->
  (Database.t,
   [> `Database of [> `Load ] * string
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `System of [> `File_info of string ] * [> `Exn of exn ] ])
  Ketrew_pervasives.t
val get_persistent :
  t ->
  (Persistent_state.t,
   [> `Database of [> `Load ] * string
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Persistent_state of [> `Deserilization of string ]
    | `System of [> `File_info of string ] * [> `Exn of exn ] ])
  Ketrew_pervasives.t
val save_persistent :
  t ->
  'a ->
  (unit,
   [> `Database of [> `Load ] * string
    | `Database_unavailable of string
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `System of [> `File_info of string ] * [> `Exn of exn ] ])
  Ketrew_pervasives.t
val add_or_update_target :
  t ->
  Target.t ->
  (unit,
   [> `Database of [> `Load ] * string
    | `Database_unavailable of Target.id
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `System of [> `File_info of string ] * [> `Exn of exn ] ])
  Ketrew_pervasives.t
*)
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
(*
val get_target :
  Database.t ->
  string ->
  (Target.t,
   [> `Missing_data of string | `Target of [> `Deserilization of string ] ])
  Ketrew_pervasives.t
*)
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
  (*
val _check_and_activate_dependencies :
  t:t ->
  string list ->
  ([> `Go_now | `Some_dependencies_died of string list | `Wait ] *
   [> `Target_activated of Ketrew_pervasives.Unique_id.t * [> `Dependency ] ]
   list,
   [> `Database of [> `Load ] * string
    | `Database_unavailable of Target.id
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of string
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Ketrew_pervasives.t
val with_plugin_or_kill_target :
  t ->
  target:Target.t ->
  plugin_name:string ->
  ((module Ketrew_long_running.LONG_RUNNING) ->
   (([> `Target_died of
          Ketrew_pervasives.Unique_id.t * [> `Plugin_not_found of string ] ]
     as 'a)
    list,
    [> `Database of [> `Load ] * string
     | `Database_unavailable of Target.id
     | `IO of
         [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
     | `System of [> `File_info of string ] * [> `Exn of exn ] ]
    as 'b)
   Ketrew_pervasives.t) ->
  ('a list, 'b) Ketrew_pervasives.t
val _start_running_target :
  t ->
  Target.t ->
  ([> `Target_died of
        Ketrew_pervasives.Unique_id.t *
        [> `Failed_to_start of string * string
         | `Plugin_not_found of string
         | `Process_failure ]
    | `Target_started of Ketrew_pervasives.Unique_id.t * string
    | `Target_succeeded of
        Ketrew_pervasives.Unique_id.t *
        [> `Artifact_literal | `Process_success ] ]
   list,
   [> `Database of [> `Load ] * string
    | `Database_unavailable of Target.id
    | `Host of [> `Execution of string * string * string * string ]
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `System of [> `File_info of string ] * [> `Exn of exn ] ])
  Ketrew_pervasives.t
val _update_status :
  t ->
  target:Target.t ->
  bookkeeping:Target.run_bookkeeping ->
  ([> `Target_died of
        Ketrew_pervasives.Unique_id.t *
        [> `Failed_to_update of string * string
         | `Plugin_not_found of string
         | `Process_failure ]
    | `Target_succeeded of
        Ketrew_pervasives.Unique_id.t * [> `Process_success ] ]
   list,
   [> `Database of [> `Load ] * string
    | `Database_unavailable of Target.id
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `System of [> `File_info of string ] * [> `Exn of exn ] ])
  Ketrew_pervasives.t
val log_what_happened :
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
  Ketrew_pervasives.Log.t
*)
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

val get_status : t -> Ketrew_target.id ->
  (Ketrew_target.workflow_state,
   [> `Database of [> `Load ] * string
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of string
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
