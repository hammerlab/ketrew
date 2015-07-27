
(* Similar to `Pvem_lwt_unix`, we embed the error monad into `Lwt_js`.  *)
include Pvem.With_deferred(Lwt)
let sleep f =
  wrap_deferred ~on_exn:(fun e -> `Exn e) begin fun () ->
    Lwt_js.sleep f
  end
