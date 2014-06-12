(** Implementation of the {!LONG_RUNNING} API with [nohup setsid] unix
    processes. *)

(** This module implements {!Ketrew_long_running.LONG_RUNNING} plugin-API.

    Shell commands are put in a {!Ketrew_monitored_script.t}, and
    started with ["nohup setsid bash <script> &"].

    The {!update} function uses the log-file of the monitored-script, and the
    command ["ps -p <Group-PID>"].

    The {!kill} function kills the process group (created thanks to ["setsid"])
    with ["kill -- <N>"] (where ["<N>"] is the negative PID of the group).

    This seems quite portable among {i unices} except MacOSX which seems
    to have dropped support for ["setsid"], and has broken terminal detachment
    (see people having
    {{:https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard}TMux problems}, 
    {{:http://stackoverflow.com/questions/23898623/nohup-cant-detach-from-console}Nohup problems}.)

*)

(** The “standard” plugin-API. *)
include Ketrew_long_running.LONG_RUNNING

val create: ?host:Ketrew_host.t -> string list -> [> `Long_running of string * string ]
(** Create a “long-running” {!Ketrew_target.build_process} (run parameters
    already serialized). *)

