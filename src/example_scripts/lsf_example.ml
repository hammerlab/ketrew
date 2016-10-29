#use "topfind"
#thread
#require "ketrew"
let run_command_with_lsf cmd =
  let module KEDSL = Ketrew.EDSL in
  let host =
    (* `Host.parse` takes an URI and creates a “Host” datastructue: a place to
       run stuff.  *)
    KEDSL.Host.parse
      "ssh://user42@MyLSFCluster/home/user42/ketrew-playground/?shell=bash"
    (* This one is an SSH host, named `MyLSFCluster`.
       The directory `/home/user42/ketrew-playground/` will be used by Ketrew
       to monitor the jobs. *)
  in
  let program =
    (* A “program” is a datastructure representing “extended shell scripts”.
       `Program.sh` creates one out a shell command. *)
    KEDSL.Program.sh cmd in
  let lsf_build_process =
    (* “build process” is a method for making things:
       `lsf` creates a datastructure that represents a job running a `program`
       with the LSF scheduling engine, on the host `host`.  *)
    KEDSL.lsf
      ~queue:"normal-people" ~wall_limit:"1:30"
      ~processors:(`Min_max (1,1)) ~host program
  in
  (* The function `KEDSL.workflow_node` creates a node in the workflow graph.
     This one is very simple, it has a name and a build-process,
     and since it doesn't have dependencies or fallbacks, it is a
     “single-node” workflow: *)
  KEDSL.workflow_node KEDSL.without_product
     ~name:"run_command_with_lsf"
     ~make:lsf_build_process

let () =
  let workflow =
     (* Create the  workflow with the first argument of the command line: *)
     run_command_with_lsf Sys.argv.(1) in
  (* Then, `Client.submit` is the only function that “does” something, it
     submits the workflow to the engine: *)
  Ketrew.Client.submit_workflow workflow
  (* This means sending the workflow to the server over HTTP(S). The
     server will start running the workflow right away.  *)
