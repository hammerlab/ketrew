(** Implementation of the {!LONG_RUNNING} API with the LSF batch processing
    scheduler.
*)

include Ketrew_long_running.LONG_RUNNING

val create :
  ?host:Ketrew_host.t ->
  ?queue:string ->
  ?name:string ->
  ?wall_limit:string ->
  ?processors:[ `Min of int | `Min_max of int * int ] ->
  string list ->
  [> `Long_running of string  * string ]
(** Create a “long-running” {!Ketrew_target.build_process} (run parameters
    already serialized). *)

