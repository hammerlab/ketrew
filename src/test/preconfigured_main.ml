
(*M

Preconfigured Main Ketrew CLI App
=================================


Create The Configuration
------------------------

Overriding the configuration from a main file:

M*)
let override_configuration =
  let open Ketrew_configuration in
  let server =
    create_server ~authorized_tokens_path:"/tmp/tokens"
      ~return_error_messages:false
      ?command_pipe:None
      (`Tls ("somecert.pem", "somekey.pem", 4242))
  in
  create
    ~debug_level:42
    ~with_color:true
    ~database_parameters:"/tmp/somepath"
    ~plugins:[`OCamlfind "lwt.react"; `OCamlfind "lwt.unix" ]
    ~server ()

(*M

Start The Application
---------------------

Calling `Ketrew.Command_line.run_main`
just like `src/app/main.ml` but preconfigured:

M*)
let `Never_returns =
  Ketrew.Command_line.run_main ~override_configuration ()
