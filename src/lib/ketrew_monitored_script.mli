
open Ketrew_pervasives

(** The goal of this module is the create shell scripts from a high-level
      representation; the scripts are “monitored” in the sense that code is
      added to log every returned value or failure in a parsable [log] file.
*)

type t

val create:  playground:Ketrew_path.absolute_directory -> string list -> t

val log_file : t -> Ketrew_path.absolute_file
val pid_file : t -> Ketrew_path.absolute_file

val to_string : t -> string
(** Render the [monitored_script] to a shell-script string. *)

val parse_log : string ->
  [ `After of string * string * string
  | `Before of string * string * string
  | `Error of string list
  | `Failure of string * string * string
  | `Start of string
  | `Success of string ] list
(** Parse the log file of a [monitored_script] (the return type should change
    soon …). *)
