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

type t = {
  database_parameters: string;
  persistent_state_key: string;
  turn_unix_ssh_failure_into_target_failure: bool;
  debug_level: int;
  with_color: bool;
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
    ])


let database_parameters t = t.database_parameters
let persistent_state_key t = t.persistent_state_key
let is_unix_ssh_failure_fatal t = t.turn_unix_ssh_failure_into_target_failure

let default_persistent_state_key = "ketrew_persistent_state"

let default_configuration_path = 
  Sys.getenv "HOME" ^ "/.ketrew/client.toml"

let default_database_path = 
  Sys.getenv "HOME" ^ "/.ketrew/database_dbm"


let create 
    ?(debug_level=2)
    ?(with_color=true)
    ?(turn_unix_ssh_failure_into_target_failure=false)
    ?(persistent_state_key=default_persistent_state_key)
    ~database_parameters () =
  {
    database_parameters; persistent_state_key;
    turn_unix_ssh_failure_into_target_failure;
    debug_level; with_color;
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
  let with_color =
    Option.(
      toml_option Toml.get_table "client"
      >>= fun table ->
      toml_option ~table Toml.get_bool "color") in
  let database_parameters, persistent_state_key =
    let table = toml_mandatory Toml.get_table "database" in
    toml_mandatory ~table Toml.get_string  "path",
    toml_option ~table Toml.get_string "state-key" in
  create 
    ?turn_unix_ssh_failure_into_target_failure 
    ?debug_level 
    ?with_color
    ?persistent_state_key 
    ~database_parameters ()

let parse s =
  let open Result in
  try parse_exn s |> return 
  with e -> fail (`Configuration (`Parsing (Printexc.to_string e)))

let apply_globals t =
  global_debug_level := t.debug_level;
  global_with_color := t.with_color;
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
