(** Command line interface to the engine. *)

open Ketrew_pervasives

type user_todo = [
  | `Fail of Log.t
  | `Make of Ketrew_target.t * Ketrew_target.t list
]
(** User-provided commands. *)

val run_main :
  ?plugins:(string * (module Ketrew_long_running.LONG_RUNNING)) list ->
  ?argv:string array ->
  configuration:Ketrew_state.Configuration.t ->
  ?additional_term:user_todo list Cmdliner.Term.t ->
  unit ->
  [ `Never_returns ]
(** The “main” function, it will [exit n] with [n = 0] if succeed or [n > 0] if
    fails. *)

