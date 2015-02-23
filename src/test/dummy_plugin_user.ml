
(*M

This is a workflow script using `Dummy_plugin` to create a (local) target.

M*)
open Printf
let () =
  let open Ketrew.EDSL in
  Ketrew_client.submit (
    target (sprintf "%S with dummy-plugin" Sys.argv.(1))
      ~make:(Dummy_plugin.create
               ~host:(Host.parse "/tmp")
               (Program.sh Sys.argv.(1)))
  )
