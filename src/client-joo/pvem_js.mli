(** The error-monad embedded in a [Lwt.t]. *)

include Pvem.DEFERRED_RESULT
  with type 'a deferred = 'a Lwt.t
   and type ('ok, 'error) t = ('ok, 'error) Pvem.Result.t Lwt.t

val sleep : float -> (unit, [> `Exn of exn ]) t
