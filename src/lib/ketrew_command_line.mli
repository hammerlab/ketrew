(** Command line interface to the engine. *)

open Ketrew_pervasives

val run_main :
  ?plugins:(string * (module Ketrew_long_running.LONG_RUNNING)) list ->
  ?argv:string array ->
  ?override_configuration:Ketrew_state.Configuration.t ->
  unit ->
  [ `Never_returns ]
(** The “main” function, it will [exit n] with [n = 0] if succeed or [n > 0] if
    fails. *)

