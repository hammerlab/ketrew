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

open Ketrew_pure
open Internal_pervasives
open Unix_io

(* Generate a big random string with URL-friendly printable characters. *)
let generate_token () =
  let () = Random.self_init () in
  List.init 32 ~f:(fun _ -> (Random.bits ()) land 255)
  |> List.map ~f:char_of_int
  |> String.of_character_list
  |> B64.encode ~alphabet:B64.uri_safe_alphabet

(* The first bytes of an ocaml script that loads the Ketrew installed
   by opam. *)
let ocaml_config_file_header ~debug_level =
  fmt {ocaml|
let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ();;
#use "topfind"
#thread
#require "ketrew"
open Ketrew.Configuration
let debug_level = %d
|ocaml} debug_level

(* Forge or return the user's database specification URI (Trakeva). *)
let come_up_with_database_parameters ~config_path =
  begin function
  | `Default ->
    let default = "sqlite" in
    let set = Trakeva_of_uri.available_backends in
    begin match List.mem default ~set with
    | true ->
      return (config_path // "database")
    | false ->
      Log.(s "The " % quote default % s " backend was not available at \
                                         Trakeva's compile time" %sp
           % parens (s "available backends: "
                     % OCaml.list quote set) % n
           % s "Please use for example: \
                -use-database \
                postgresql://pg.example.com:4242/database" % n
           % s "see also \
                http://seb.mondet.org/software/ketrew/Database_Backends.html"
           @ error);
      fail (`Failure "Cannot create configuration")
    end
  | `User_set s -> return s
  end

(* Forge a string that is the OCaml value expect by `Configuration.server`. *)
let ocaml_server_connection ~config_path ~port tls =
  begin match tls with
  | `Use (cert, key) -> return (fmt {o|`Tls (%S, %S, %d)|o} cert key port)
  | `Create_self_signed ->
    let cert = config_path // "certificate.pem" in
    let key = config_path // "privkey-nopass.pem" in
    System.Shell.do_or_fail
      (fmt "openssl req -x509 -newkey rsa:2048 \
            -keyout %s -out %s \
            -days 10 -nodes -subj \"/CN=test_ketrew\" 2> /dev/null" key cert)
    >>= fun () ->
    return (fmt {o|`Tls (%S, %S, %d)|o} cert key port)
  | `Don't -> return (fmt "`Tcp %d" port)
  end

let merlin_file ~config_path =
  System.Shell.do_or_fail (fmt "echo 'PKG ketrew' > %s"
                             ((config_path // ".merlin") |> Filename.quote))

let ocaml_engine_and_server_functions
    ~config_path ~database_parameters
    ~tokens ~auth_tokens_file ~tls ~server_listen =
  let engine =
    fmt {ocaml|
let engine =
  engine ~database_parameters:%S ()
|ocaml}
      database_parameters
  in
  let server =
    fmt {ocaml|
let server ~daemon =
  server ~daemon ~engine
    ~authorized_tokens:[
       %s
       authorized_tokens_path %S
     ]
    ~return_error_messages:true
    ~log_path:%S
    ~command_pipe:%S
    (%s)
|ocaml}
      (List.map tokens ~f:(fun tok ->
           fmt "authorized_token ~name:\"%s\" %S;" tok tok)
       |> String.concat ~sep:"\n")
      auth_tokens_file
      (config_path // "server-log")
      (config_path // "command.pipe")
      server_listen
  in
  engine, server

let ocaml_let_unit_output_profiles () =
  {ocaml|
let () =
  output [
    profile "standalone"
      (create ~debug_level (standalone () ~engine));
    profile "server" (create ~debug_level (server ~daemon:false));
    profile "daemon" (create ~debug_level (server ~daemon:true));
    profile "default" (create ~debug_level client);
    profile "client" (create ~debug_level client);
  ]
|ocaml}

let generate_configuration_directory
    ~use_database ~tokens ~tls ~port ~debug_level config_path =
  let tokens = if tokens = [] then [generate_token ()] else tokens in
  System.ensure_directory_path config_path
  >>= fun () ->
  ocaml_server_connection ~config_path ~port tls
  >>= fun server_listen ->
  let auth_tokens_file = config_path // "authorized_tokens" in
  System.Shell.do_or_fail (fmt "touch %s" (Filename.quote auth_tokens_file))
  >>= fun () ->
  merlin_file ~config_path
  >>= fun () ->
  come_up_with_database_parameters ~config_path use_database
  >>= fun database_parameters ->
  let engine, server =
    ocaml_engine_and_server_functions ~config_path ~database_parameters
      ~tokens ~auth_tokens_file ~tls ~server_listen
  in
  let client =
    fmt {ocaml|
let client =
  client ~token:%S "http%s://127.0.0.1:%d"
|ocaml}
      (match tokens with one :: _ -> one | [] -> "TODO:set-tokens")
      (match tls with `Don't -> "" | _ -> "s")
      port
  in
  let config =
    ocaml_config_file_header ~debug_level
    ^ engine
    ^ server
    ^ client
    ^ ocaml_let_unit_output_profiles ()
  in
  IO.write_file ~content:config (config_path // "configuration.ml")
