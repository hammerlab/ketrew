
include Ketrew_long_running.LONG_RUNNING

val create: ?host:Ketrew_host.t -> string list -> [> `Long_running of string * string ]

