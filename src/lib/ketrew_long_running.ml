(** Definition of the interface required from “long-running task” plugins. *)

open Ketrew_pervasives

type error = [
  | `Fatal of string
  | `Recoverable of string
]
(** The “imposed” error types for “long-running” plugins. *)

(** The module type [LONG_RUNNING] defines the interface for plugins. *)
module type LONG_RUNNING = sig

  type run_parameters
  (** Hidden type kept serialized by the engine. *)

  val name: string
  (** The (unique) name of the plugin. *)

  val serialize: run_parameters -> string
  (** Serialize the run parameters for storage by the engine. *)

  val deserialize_exn: string -> run_parameters
  (** Deserialize the run parameters from a string; the engine guaranties
      that [deserialize_exn] will be called onthe result of {!serialize};
      and assumes that no exception will be thrown in that case. *)

  val start: run_parameters ->
    (run_parameters, error) Deferred_result.t
  (** Start the long-running computation, the returned [run_parameters] will be
      stored and used for the first call to {!update}. *)

  val update: run_parameters ->
    ([`Succeeded of run_parameters
     | `Failed of run_parameters * string
     | `Still_running of run_parameters],
     [> `Failed_to_update of string]) Deferred_result.t
  (** Check and update the status of the long-running job. Again, is
      [`Still_running rp] is returned, the next call to {!update} (or {!kill})
      will receive those parameters. *)

  val kill: run_parameters ->
    ([`Killed of run_parameters],
     [> `Failed_to_kill of string]) Deferred_result.t
  (** Kill the long-running computation. *)

end
