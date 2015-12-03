#use "topfind"
#thread
#require "ketrew"

let run_command_with_daemonize ~cmd ~email =
  let module KEDSL = Ketrew.EDSL in

  (* Where to run stuff *)
  let host = KEDSL.Host.tmp_on_localhost in

  (* A “program” is a datastructure representing an “extended shell script”. *)
  let program = KEDSL.Program.sh cmd in

  (* A “build process” is a method for making things.

     In this case, `daemonize` creates a datastructure that represents a job
     running our program on the host. *)
  let build_process = KEDSL.daemonize ~host program in
  (* On Mac OSX
  let build_process = KEDSL.daemonize ~using:`Python_daemon ~host program in
  *)

  (* A target that Ketrew will activate after cmd completes *)
  let email_target ~success =
    let sstring = if success then "succeeded" else "failed" in
    let e_program =
      KEDSL.Program.shf "echo \"'%s' %s\" | mail -s \"Status update\" %s"
        cmd sstring
        email
    in
    let e_process =
      KEDSL.daemonize ~using:`Python_daemon ~host e_program in
    KEDSL.workflow_node KEDSL.without_product
      ~name:("email result " ^ sstring)
      ~make:e_process
  in

  (* The function `KEDSL.workflow_node` creates a node in the workflow graph.
     The value `KEDSL.without_product` means this node does not
     “produce” anything, it is like a `.PHONY` target in `make`. *)
  KEDSL.workflow_node KEDSL.without_product
    ~name:"daemonize command"
    ~make:build_process
    ~edges:[
      KEDSL.on_success_activate (email_target true);
      KEDSL.on_failure_activate (email_target false);
    ]

let () =
  (* Grab the command line arguments. *)
  let cmd   = Sys.argv.(1) in
  let email = Sys.argv.(2) in

  (* Create the  workflow with the first argument of the command line: *)
  let workflow = run_command_with_daemonize ~cmd ~email in

  (* Then, `Client.submit_workflow` is the only function that “does”
     something, it submits the workflow to the engine: *)
  Ketrew.Client.submit_workflow workflow
