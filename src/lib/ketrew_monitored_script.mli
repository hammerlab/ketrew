(** Generate Shell scripts that “monitor” commands. *)

(** The goal of this module is the create shell scripts from a high-level
      representation; the scripts are “monitored” in the sense that code is
      added to log every returned value or failure in a parsable [log] file.
*)

open Ketrew_pervasives

type t = Ketrew_gen_base_v0_t.monitored_script =
  {playground: Ketrew_path.absolute_directory; program: Ketrew_program.t}
(** The definition of a monitored script. *)

val create:  playground:Ketrew_path.absolute_directory -> Ketrew_program.t -> t
(** Create a new script, which will run the list of commands, and store state
    values in the [playground] directory. *)

val log_file : t -> Ketrew_path.absolute_file
(** Path to the log file of the script. *)

val pid_file : t -> Ketrew_path.absolute_file
(** Path to the “PID” file: where the script stores the PID of the process
    running the script, [- pid] will be the process id of the process group
    created by `setsid` (useful for killing the whole process tree). *)

val to_string : ?write_pid:bool -> t -> string
(** Render the [monitored_script] to a shell-script string;
    if [write_pid] is [true] (the default), the script writes the pid to
    [pid_file t]. *)

val parse_log : string ->
  [ `After of string * string * string
  | `Before of string * string * string
  | `Error of string list
  | `Failure of string * string * string
  | `Start of string
  | `Success of string ] list
(** Parse the log file of a [monitored_script]. *)
