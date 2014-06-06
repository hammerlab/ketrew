(** Command line interface to the engine. *)

open Ketrew_pervasives

type user_todo = [
  | `Fail of Log.t
]
(** User-provided commands. *)

val run_main :
  ?plugins:(string * (module Ketrew_long_running.LONG_RUNNING)) list ->
  ?argv:string array ->
  configuration:Ketrew_state.Configuration.t ->
  user_todo list Cmdliner.Term.t -> [ `Never_returns ]
(** The “main” function, it will [exit n] with [n = 0] if succeed or [n > 0] if
    fails. *)

