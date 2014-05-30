(** Manage calls to Unix processes *)

open Ketrew_pervasives

(** High-level representation of Unix exit codes. *)
module Exit_code: sig
  type t = [
    | `Exited of int
    | `Signaled of int
    | `Stopped of int
  ]
  val to_string: t -> string
end

val exec :
  ?bin:string ->
  string list ->
  (string * string * Exit_code.t,
   [> `Process of
        [> `Exec of string * string list ] * [> `Exn of exn ] ])
    Deferred_result.t
(** Execute a process with a given list of strings as “[argv]”, if you can
    provide the [~bin] argument to specify the actual file to be executed. The
    function returns the tuple [(stdout, stderr, exit_code)]. *)

val succeed :
  ?bin:string ->
  string list ->
  (string * string,
   [> `Process of
        [> `Exec of string * string list ] *
        [> `Exn of exn | `Non_zero of string ] ])
    Deferred_result.t
(** Do like {!exec} but fail if the process does not exit with [0] status. *)

val error_to_string :
  [< `Process of
       [< `Exec of string * string list ] *
       [< `Exn of exn | `Non_zero of string ] ] ->
  string
(** Display-friendly version of the errors of this module. *)
