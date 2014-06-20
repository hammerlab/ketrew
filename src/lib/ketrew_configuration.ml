
open Ketrew_pervasives

type t = {
  database_parameters: string;
  persistent_state_key: string;
  turn_unix_ssh_failure_into_target_failure: bool;
}

let database_parameters t = t.database_parameters
let persistent_state_key t = t.persistent_state_key
let is_unix_ssh_failure_fatal t = t.turn_unix_ssh_failure_into_target_failure

let default_persistent_state_key = "ketrew_persistent_state"

let default_configuration_path = 
  Sys.getenv "HOME" ^ "/.ketrew/client.toml"

let default_database_path = 
  Sys.getenv "HOME" ^ "/.ketrew/database_dbm"


let create 
    ?(turn_unix_ssh_failure_into_target_failure=false)
    ?(persistent_state_key=default_persistent_state_key) ~database_parameters () =
  {
    database_parameters; persistent_state_key;
    turn_unix_ssh_failure_into_target_failure;
  }

let parse_exn str =
  let toml = Toml.from_string str in
  let turn_unix_ssh_failure_into_target_failure =
    try 
      Toml.get_bool toml "unix-ssh-make-targets-fail"
      || Toml.get_bool toml "turn-unix-ssh-failure-into-target-failure"
    with
    | _ -> false
  in
  let table =
    try Toml.get_table toml "database" with
    | Not_found -> failwith "Configuration file without [database]" in
  let database_parameters = 
    try Toml.get_string table "path" with
    | _ -> failwith "missing database path" in
  let persistent_state_key =
    try Toml.get_string table "state-key" with 
    | _ -> default_persistent_state_key in
  create ~turn_unix_ssh_failure_into_target_failure
    ~persistent_state_key ~database_parameters ()

let parse s =
  let open Result in
  try parse_exn s |> return 
  with e -> fail (`Configuration (`Parsing (Printexc.to_string e)))


let get_configuration ?override_configuration path =
  match override_configuration with
  | Some c -> return c
  | None ->
    IO.read_file path
    >>= fun content ->
    of_result (parse content)
