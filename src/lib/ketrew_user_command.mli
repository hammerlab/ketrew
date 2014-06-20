(** Commands that can be fed to the engine by end-users. *)

open Ketrew_pervasives

type t = [
  | `Fail of Ketrew_pervasives.Log.t
  | `Make of Ketrew_target.t * Ketrew_target.t list
]
(** A user-todo-item, is either asking Ketrew to fail with a message, or
   to start a workflow. *)

val log : t -> Ketrew_pervasives.Log.t
(** Convert a command into a [Log.t] document. *)

val run_list :
  state:Ketrew_state.t ->
  t list ->
  (unit,
   [> `Database of Ketrew_database.error
   | `Database_unavailable of Ketrew_target.id
   | `Failure of string
   | `Persistent_state of [> `Deserilization of string ] ])
    Deferred_result.t
(** Run a todo-list with the given [state] instance. *)
