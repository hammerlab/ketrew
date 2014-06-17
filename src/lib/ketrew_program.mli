(** The “things” to run on a given host. *)

open Ketrew_pervasives

type t = Ketrew_gen_base_v0_t.program
(** A program. *)

val to_shell_commands: t -> string list
(** Convert a program to a list of shell commands. *)

val to_single_shell_command: t -> string
(** Convert a program to a shell command. *)

val log: t -> Log.t
(** Create a {!Log.t} document to display a program. *)

val to_string_hum: t -> string
(** Get a display-friendly string of a program. *)
