open Ketrew_pervasives

module type LONG_RUNNING = sig

  type run_parameters

  val name: string

  val serialize: run_parameters -> string
  val deserialize_exn: string -> run_parameters

  val start: run_parameters ->
    (run_parameters, [>  `Failed_to_start of string]) Deferred_result.t
  val update: run_parameters ->
    ([`Succeeded of run_parameters
     | `Failed of run_parameters * string
     | `Still_running of run_parameters],
     [> `Failed_to_update of string]) Deferred_result.t

  val kill: run_parameters ->
    ([`Killed of run_parameters],
     [> `Failed_to_kill of string]) Deferred_result.t

end
