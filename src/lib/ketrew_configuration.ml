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

type plugin = [ `Compiled of string | `OCamlfind of string ]

type engine = {
  database_parameters: string;
  persistent_state_key: string;
  turn_unix_ssh_failure_into_target_failure: bool;
  host_timeout_upper_bound: float option;
}
type ui = {
  with_color: bool;
}
type server = {
  authorized_tokens_path: string option; 
  listen_to: [ `Tls of (string * string * int) ];
  return_error_messages: bool;
  command_pipe: string option;
  daemon: bool;
  log_path: string option;
  server_engine: engine;
  server_ui: ui;
}
type client = {
  (* client_engine: engine; *)
  connection: string;
  token: string;
  client_ui: ui;
}
type standalone = {
  standalone_engine: engine;
  standalone_ui: ui;
}
type mode = [
  | `Standalone of standalone
  | `Client of client
  | `Server of server
]

type t = {
  debug_level: int;
  plugins: plugin list;
  mode: mode;
}

let log t =
  let open Log in
  let item name l = s name % s ": " % l % n in
  let sublist l = n % indent (separate empty l) in
  let common = 
    sublist [
      item "Debug-level" (i t.debug_level);
      item "Plugins" (match t.plugins with
        | [] -> s "None"
        | more -> sublist (List.map more ~f:(function
          | `Compiled path -> item "Compiled"  (quote path)
          | `OCamlfind pack -> item "OCamlfind package" (quote pack))))
    ] in
  let ui t =
    item "Colors"
      (s (if t.with_color then "with" else "without") % s " colors")
  in
  let engine t = 
    sublist [
      item "Database" (quote t.database_parameters);
      item "State-key" (quote t.persistent_state_key);
      item "Unix-failure"
        ((if t.turn_unix_ssh_failure_into_target_failure
          then s "turns"
          else s "does not turn") % s " into target failure");
    ] in
  match t.mode with
  | `Standalone {standalone_engine; standalone_ui} ->
    item "Mode" (s "Standalone")
    % item "Engine" (engine standalone_engine)
    % item "UI" (ui standalone_ui)
    % item "Misc" common
  | `Client client ->
    item "Mode" (s "Client")
    % item "Connection" (quote client.connection)
    % item "Auth-token" (quote client.token)
    % item "UI" (ui client.client_ui)
    % item "Misc" common
  | `Server srv ->
    item "Mode" (s "Server")
    % item "Engine" (engine srv.server_engine)
    % item "UI" (ui srv.server_ui)
    % item "HTTP-server" (sublist [
        item "Authorized tokens" 
          OCaml.(option string srv.authorized_tokens_path);
        item "Daemonize" (OCaml.bool srv.daemon);
        item "Command Pipe" (OCaml.option quote srv.command_pipe);
        item "Log-path" (OCaml.option quote srv.log_path);
        item "Return-error-messages" (OCaml.bool srv.return_error_messages);
        item "Listen"
          (let `Tls (cert, key, port) = srv.listen_to in
           sublist [
             item "Port" (i port);
             item "Certificate" (quote cert);
             item "Key" (quote key); 
           ])
      ])
    % item "Misc" common

let default_persistent_state_key = "ketrew_persistent_state"

let default_configuration_path = 
  Sys.getenv "HOME" ^ "/.ketrew/configuration.toml"

let default_database_path = 
  Sys.getenv "HOME" ^ "/.ketrew/database"

let create ?(debug_level=0) ?(plugins=[]) mode =
  {debug_level; plugins; mode;}


let ui ?(with_color=true) () = {with_color}
let default_ui = ui ()

let engine 
    ?(database_parameters=default_database_path)
    ?(persistent_state_key=default_persistent_state_key)
    ?(turn_unix_ssh_failure_into_target_failure=false)
    ?host_timeout_upper_bound () = {
  database_parameters;
  persistent_state_key;
  turn_unix_ssh_failure_into_target_failure;
  host_timeout_upper_bound;
}
let default_engine = engine ()

let standalone ?(ui=default_ui) ?engine () =
  let standalone_engine = Option.value engine ~default:default_engine in
  (`Standalone {standalone_engine; standalone_ui = ui}) 

let client ?(ui=default_ui) ~token connection =
  (`Client {client_ui = ui; connection; token})

let server
    ?ui ?engine
    ?authorized_tokens_path ?(return_error_messages=false)
    ?command_pipe ?(daemon=false) ?log_path listen_to =
  let server_engine = Option.value engine ~default:default_engine in
  let server_ui = Option.value ui ~default:default_ui in
  (`Server {server_engine; authorized_tokens_path; listen_to; server_ui;
            return_error_messages; command_pipe; daemon; log_path; })



let parse_exn str =
  let toml = Toml.from_string str in
  let toml_option ?(table=toml) get name =
    try Some (get table name) with _ -> None in
  let toml_mandatory ?(table=toml) get name =
    try get table name
    with _ -> failwith (fmt "Mandatory field %S missing" name) in
  let debug_level = toml_option Toml.get_int "debug-level" in
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
  let engine =
    Option.(
      toml_option Toml.get_table "engine"
      >>= fun table ->
      let database_parameters =
        toml_option ~table Toml.get_string "database-path" in
      let persistent_state_key =
        toml_option ~table Toml.get_string "state-key" in
      let turn_unix_ssh_failure_into_target_failure =
        toml_option ~table Toml.get_bool "turn-unix-ssh-failure-into-target-failure" in
      let host_timeout_upper_bound =
        toml_option ~table Toml.get_float "host-timeout-upper-bound" in
      return (engine ?database_parameters ?persistent_state_key
                ?turn_unix_ssh_failure_into_target_failure
                ?host_timeout_upper_bound ()))
  in
  let ui =
    Option.(
      toml_option Toml.get_table "ui"
      >>= fun table ->
      let with_color = toml_option ~table Toml.get_bool "color" in
      return (ui ?with_color ())
    ) in
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
    return (server ?return_error_messages ?command_pipe ?engine ?daemon
              ?ui ?log_path ?authorized_tokens_path listen_to)
  in
  let client =
    Option.(
      toml_option Toml.get_table "client"
      >>= fun table ->
      let connection = toml_mandatory ~table Toml.get_string "connection" in
      let token = toml_mandatory ~table Toml.get_string "token" in
      return (client ?ui ~token connection)
    ) in
  let mode =
    match client, server with
    | Some s, None | None, Some s -> s
    | None, None ->  standalone ?engine ?ui ()
    | _, _ -> failwith "Cannot handle multiple configurations in the same file"
  in
  create ?debug_level ~plugins mode


let parse s =
  let open Result in
  try parse_exn s |> return 
  with e -> fail (`Configuration (`Parsing (Printexc.to_string e)))

let apply_globals t =
  global_debug_level := t.debug_level;
  let color, host_timeout =
    match t.mode with
    | `Client {client_ui; connection} -> (client_ui.with_color, None)
    | `Standalone {standalone_ui; standalone_engine} ->
      (standalone_ui.with_color, standalone_engine.host_timeout_upper_bound)
    | `Server {server_engine; server_ui; _} ->
      (server_ui.with_color, server_engine.host_timeout_upper_bound)
  in
  global_with_color := color;
  Log.(s "Configuration: setting globals: "
       % indent (n
         % s "debug_level: " % i !global_debug_level % n
         % s "with_color: " % OCaml.bool !global_with_color % n
         % s "timeout upper bound: " % OCaml.(option float host_timeout)
       ) @ very_verbose);
  begin match host_timeout with
  | Some ht -> Ketrew_host.default_timeout_upper_bound := ht
  | None -> ()
  end

let get_path () =
  try Sys.getenv "KETREW_CONFIGURATION" with _ ->
    (try Sys.getenv "KETREW_CONFIG" with _ ->
      default_configuration_path)

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
  begin if and_apply then (
    apply_globals conf;
    Ketrew_plugin.load_plugins conf.plugins
  ) else
    return ()
  end
  >>= fun () ->
  return conf

let plugins t = t.plugins

let server_configuration t =
  match t.mode with
  | `Server s -> Some s
  | other -> None
let listen_to s = s.listen_to
let return_error_messages s = s.return_error_messages
let authorized_tokens_path s = s.authorized_tokens_path
let command_pipe s = s.command_pipe
let daemon       s = s.daemon
let log_path     s = s.log_path
let database_parameters e = e.database_parameters
let persistent_state_key e = e.persistent_state_key
let is_unix_ssh_failure_fatal e = e.turn_unix_ssh_failure_into_target_failure
let mode t = t.mode
let standalone_engine st = st.standalone_engine
let server_engine s = s.server_engine
let connection c = c.connection
let token c = c.token

let standalone_of_server s = 
  {standalone_ui = s.server_ui;
   standalone_engine = s.server_engine;}

let get_configuration_for_daemon_exn
    ?override_configuration path =
  let conf =
    begin match override_configuration with
    | Some c -> c
    | None ->
      begin
        let i = open_in path in
        let content =
          let buf = Buffer.create 1023 in
          let rec get_all () =
            begin try
              let line = input_line i in
              Buffer.add_string buf (line ^ "\n");
              get_all ()
            with e -> ()
            end;
          in
          get_all ();
          Buffer.contents buf in
        close_in i;
        let conf = parse_exn content in
        conf
      end
    end
  in
  match server_configuration conf with
  | Some server_config when daemon server_config ->
    `Daemonize_with (log_path server_config)
  | _ -> `Do_not_daemonize
