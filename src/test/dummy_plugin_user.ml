
open Printf
let () =
  let open Ketrew.EDSL in
  run (
    target (sprintf "%S with dummy-plugin" Sys.argv.(1))
      ~make:(Dummy_plugin.create
               ~host:(parse_host "/tmp")
               (Program.sh Sys.argv.(1)))
  )
