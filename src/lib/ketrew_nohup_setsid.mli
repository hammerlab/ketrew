(** Implementation of the {!LONG_RUNNING} API with [nohup setsid] unix
    processes. *)

include Ketrew_long_running.LONG_RUNNING

val create: ?host:Ketrew_host.t -> string list -> [> `Long_running of string * string ]
(** Create a “long-running” {!Ketrew_target.build_process} (run parameters
    already serialized). *)

