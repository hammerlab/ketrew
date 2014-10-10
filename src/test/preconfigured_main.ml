
(*M

Preconfigured Main Ketrew CLI App
=================================


Create The Configuration
------------------------

Overriding the configuration from a main file:

M*)
let override_configuration =
  let open Ketrew_configuration in
  let engine =
    engine ~database_parameters:"/tmp/somepath" ()
  in
  let server =
    server ~authorized_tokens_path:"/tmp/tokens"
      ~return_error_messages:false
      ?command_pipe:None
      ~engine
      (`Tls ("somecert.pem", "somekey.pem", 4242))
  in
  create server
    ~debug_level:42
    ~plugins:[`OCamlfind "lwt.react"; `OCamlfind "lwt.unix" ]

(*M

A Cmdliner Command To Add
-------------------------

This just shows how to add one or more sub-commands to the command-line
interface:

M*)
let additional_cmdliner_command =
  let open Ketrew_pervasives in
  let open Cmdliner in
  let open Term in
  let term =
    pure (fun stuff_to_say ->
        Log.(s "From custom-main: " % OCaml.list quote stuff_to_say @ normal);
        return ())
    $ Arg.(non_empty @@ pos_all string [] @@
           info [] ~docv:"STUFF-TO-SAY" 
             ~doc:"Tell Ketrew to say $(docv)") 
  in
  (term, info "say-stuff" ~doc:"Say stuff")


(*M

Start The Application
---------------------

Calling `Ketrew.Command_line.run_main`
just like `src/app/main.ml` but preconfigured:

M*)
let `Never_returns =
  Ketrew.Command_line.run_main
    ~additional_commands:[additional_cmdliner_command]
    ~override_configuration ()
