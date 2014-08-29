(**************************************************************************)
(*  Copyright 2014, Sebastien Mondet <seb@mondet.org>                     *)
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

open Ketrew_pervasives

type server = {
  authorized_tokens_path: string option; 
  listen_to: [ `Tls of (string * string * int) ];
  return_error_messages: bool;
  command_pipe: string option;
  daemon: bool;
  log_path: string option;
}
type plugin = [ `Compiled of string | `OCamlfind of string ]
type t = {
  database_parameters: string;
  persistent_state_key: string;
  turn_unix_ssh_failure_into_target_failure: bool;
  debug_level: int;
  with_color: bool;
  host_timeout_upper_bound: float;
  server: server option;
  plugins: plugin list;
}

let log t =
  Log.([
      s "Database: " % sf "%S" t.database_parameters;
      s "State-key: " % sf "%S" t.persistent_state_key;
      s "Unix-failure "
      % (if t.turn_unix_ssh_failure_into_target_failure
         then s "turns"
         else s "does not turn")
      % s " into target failure";
      s "Debug-level: " % i t.debug_level;
      s "Client " % s (if t.with_color then "with" else "without") % s " colors";
      s "Timeout-upper-bound: " % f t.host_timeout_upper_bound % s " seconds";
      s "Plugins: "
      % begin match t.plugins with
      | [] -> s "None"
      | more -> n % indent (separate n (List.map more ~f:(function
        | `Compiled path -> s "* Compiled: " % quote path
        | `OCamlfind pack -> s "* OCamlfind package: " % quote pack
        ))) % n
      end
      % s "Server: "
      % (match t.server with
        | None -> s "Not configured"
        | Some srv -> n % indent (
            s "Authorized tokens: " 
            % OCaml.(option string) srv.authorized_tokens_path % n
            % s  "Listen: "
            % (match srv.listen_to with
              | `Tls (cert, key, port) -> 
                s "TLS:" % i port % sp
                % parens (indent (s "Certificate: " % quote cert % s ", " % n
                                      % s "Key: " % quote key))
              ) % n
            % s "Return-error-messages: "
            % OCaml.bool srv.return_error_messages %n))
    ])


let database_parameters t = t.database_parameters
let persistent_state_key t = t.persistent_state_key
let is_unix_ssh_failure_fatal t = t.turn_unix_ssh_failure_into_target_failure

let default_persistent_state_key = "ketrew_persistent_state"

let default_configuration_path = 
  Sys.getenv "HOME" ^ "/.ketrew/configuration.toml"

let default_database_path = 
  Sys.getenv "HOME" ^ "/.ketrew/database_dbm"

let host_timeout_upper_bound t = t.host_timeout_upper_bound

let create_server
    ?authorized_tokens_path ?(return_error_messages=false)
    ?command_pipe ?(daemon=false) ?log_path
    listen_to =
  {authorized_tokens_path; listen_to; return_error_messages;
   command_pipe; daemon; log_path; }

let create 
    ?(debug_level=2)
    ?(with_color=true)
    ?(turn_unix_ssh_failure_into_target_failure=false)
    ?(persistent_state_key=default_persistent_state_key)
    ?(host_timeout_upper_bound=60.)
    ?(plugins=[])
    ?server
    ~database_parameters () =
  {
    database_parameters; persistent_state_key;
    turn_unix_ssh_failure_into_target_failure; plugins;
    debug_level; with_color; host_timeout_upper_bound; server;
  }

let parse_exn str =
  let toml = Toml.from_string str in
  let toml_option ?(table=toml) get name =
    try Some (get table name) with _ -> None in
  let toml_mandatory ?(table=toml) get name =
    try get table name
    with _ -> failwith (fmt "Mandatory field %S missing" name) in
  let turn_unix_ssh_failure_into_target_failure =
    toml_option Toml.get_bool "turn-unix-ssh-failure-into-target-failure" in
  let debug_level = toml_option Toml.get_int "debug-level" in
  let host_timeout_upper_bound =
    toml_option Toml.get_float "host-timeout-upper-bound" in
  let with_color =
    Option.(
      toml_option Toml.get_table "client"
      >>= fun table ->
      toml_option ~table Toml.get_bool "color") in
  let database_parameters, persistent_state_key =
    let table = toml_mandatory Toml.get_table "database" in
    toml_mandatory ~table Toml.get_string  "path",
    toml_option ~table Toml.get_string "state-key" in
  let plugins =
    toml_option Toml.get_table "plugins"
    |> Option.value_map ~default:[] ~f:Toml.toml_to_list
    (* Toml.toml_to_list seems to reverse the order of the table, so 
       we reverse back: *)
    |> List.rev_map ~f:(function
      | ("compiled", TomlType.TString path) -> [`Compiled path]
      | ("ocamlfind", TomlType.TString pack) -> [`OCamlfind pack]
      | ("compiled", TomlType.TArray (TomlType.NodeString paths)) ->
         List.map paths ~f:(fun p -> `Compiled p)
      | ("ocamlfind", TomlType.TArray (TomlType.NodeString packs)) ->
         List.map packs ~f:(fun p -> `OCamlfind p)
      | (other, _) ->
        failwith (fmt "Expecting “compiled” plugins only")
      )
    |> List.concat
  in
  let server =
    let open Option in
    toml_option Toml.get_table "server"
    >>= fun table ->
    let return_error_messages =
      toml_option ~table Toml.get_bool "return-error-messages" in
    let authorized_tokens_path =
      toml_option ~table Toml.get_string "authorized-tokens-path" in
    let command_pipe =
      toml_option ~table Toml.get_string "command-pipe-path" in
    let daemon =
      toml_option ~table Toml.get_bool "daemonize" in
    let log_path =
      toml_option ~table Toml.get_string "log-path" in
    let listen_to =
      let cert = toml_mandatory ~table Toml.get_string "certificate" in
      let key = toml_mandatory ~table Toml.get_string "private-key" in
      let port = toml_mandatory ~table Toml.get_int "port" in
      `Tls (cert, key, port) in
    return (create_server ?return_error_messages ?command_pipe
              ?daemon ?log_path ?authorized_tokens_path listen_to)
  in
  create 
    ?turn_unix_ssh_failure_into_target_failure 
    ?debug_level 
    ?with_color
    ?persistent_state_key 
    ?host_timeout_upper_bound
    ~plugins
    ?server
    ~database_parameters ()

let parse s =
  let open Result in
  try parse_exn s |> return 
  with e -> fail (`Configuration (`Parsing (Printexc.to_string e)))

let apply_globals t =
  global_debug_level := t.debug_level;
  global_with_color := t.with_color;
  Ketrew_host.default_timeout_upper_bound := t.host_timeout_upper_bound;
  ()

let get_configuration ?(and_apply=true) ?override_configuration path =
  begin match override_configuration with
  | Some c -> 
    return c
  | None ->
    IO.read_file path
    >>= fun content ->
    of_result (parse content)
  end
  >>= fun conf ->
  if and_apply then apply_globals conf;
  return conf

let plugins t = t.plugins

let server_configuration t = t.server

let listen_to s = s.listen_to
let return_error_messages s = s.return_error_messages
let authorized_tokens_path s = s.authorized_tokens_path
let command_pipe s = s.command_pipe
let daemon       s = s.daemon
let log_path     s = s.log_path
