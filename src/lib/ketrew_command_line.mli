(** Command line interface to the engine. *)

open Ketrew_pervasives


val run_main :
  ?plugins:(string * (module Ketrew_long_running.LONG_RUNNING)) list ->
  ?argv:string array ->
  configuration:Ketrew_state.Configuration.t ->
  ?additional_term:Ketrew_user_command.t list Cmdliner.Term.t ->
  unit ->
  [ `Never_returns ]
(** The “main” function, it will [exit n] with [n = 0] if succeed or [n > 0] if
    fails. *)

