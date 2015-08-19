(**************************************************************************)
(*    Copyright 2014, 2015:                                               *)
(*          Sebastien Mondet <seb@mondet.org>,                            *)
(*          Leonid Rozenberg <leonidr@gmail.com>,                         *)
(*          Arun Ahuja <aahuja11@gmail.com>,                              *)
(*          Jeff Hammerbacher <jeff.hammerbacher@gmail.com>               *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)

(*M

Preconfigured Main Ketrew CLI App
=================================


Create The Configuration
------------------------

Overriding the configuration from a main file:

M*)
let override_configuration =
  let open Ketrew.Configuration in
  let engine =
    engine ~database_parameters:"/tmp/somepath" ()
  in
  let server =
    server
      ~authorized_tokens:[
        authorized_tokens_path "/tmp/tokens"
      ]
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
  let open Ketrew_pure.Internal_pervasives in
  let open Ketrew.Unix_io.Deferred_result in
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
