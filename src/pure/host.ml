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

open Internal_pervasives

module Ssh = struct

  let _configuration_ssh_batch_option = ref ""

  let configure_ssh_batch_option spec =
    let op =
      match spec with
      | `Openssh -> "-oBatchMode=yes"
      | `Dropbear -> "-s"
      | `Custom s -> s
    in
    _configuration_ssh_batch_option := op

  let () = configure_ssh_batch_option `Openssh

  type t ={
    address: string;
    port: int option;
    user: string option;
    add_ssh_options: string list;
  } [@@deriving yojson]

  let ssh_batch_option _ = !_configuration_ssh_batch_option
      (* in the future `_` should really be `t` *)

end

type connection = [
  | `Localhost
  | `Ssh of Ssh.t
] [@@deriving yojson]
type default_shell ={
  binary: string option;
  command_name: string;
  options: string list;
  command_option: string;
} [@@deriving yojson]

type t = {
  name: string;
  connection: connection;
  playground: Path.t option;
  default_shell: default_shell;
  execution_timeout: Time.t option;
} [@@deriving yojson]

let default_shell ?binary ?(options=[]) ?(command_option="-c") command_name =
  {binary; command_name; options; command_option}

let shell_sh_minus_c = default_shell "sh"

let shell_of_default_shell t cmd =
  t.default_shell.command_name ::
  t.default_shell.options
  @ [t.default_shell.command_option; cmd]

let create ~connection ?execution_timeout ?(default_shell=shell_sh_minus_c) ?playground name =
  {name; connection; playground; default_shell; execution_timeout}

let localhost 
    ?execution_timeout ?default_shell ?playground ?(name="localhost") () = 
  create ~connection:`Localhost ?default_shell ?playground name

let tmp_on_localhost = 
  localhost ~playground:(Path.absolute_directory_exn "/tmp")
    ~name:"file://tmp" ()

let ssh 
    ?execution_timeout ?(add_ssh_options=[]) ?default_shell ?playground
    ?port ?user ?name address =
  create ?playground ?default_shell Option.(value name ~default:address)
    ~connection:(`Ssh {Ssh. address; port; user; add_ssh_options})

let of_uri uri =
  let connection =
    Option.value_map ~default:`Localhost (Uri.host uri) ~f:(fun address ->
        let add_ssh_options =
          Option.value ~default:[] (Uri.get_query_param' uri "ssh-option") in
        let user = Uri.userinfo uri in
        `Ssh {Ssh.address; port = Uri.port uri; user; add_ssh_options})
  in
  let playground =
    match Uri.path uri with
    | "" -> None
    | rel when Filename.is_relative rel ->
      let cwd = Sys.getcwd () in
      Some (Path.absolute_directory_exn (Filename.concat cwd rel))
    | p -> Some (Path.absolute_directory_exn p)
  in
  let default_shell =
    Uri.get_query_param uri "shell"
    |> Option.bind ~f:(fun s -> 
        match String.split ~on:(`Character ',') s with
        | [] -> None
        | one :: [] -> Some (default_shell one)
        | one :: two :: [] -> Some (default_shell ~command_option:two one)
        | one :: more -> 
          let command_option = List.last more |> Option.value_exn ~msg:"bug" in
          let options = List.split_n more (List.length more - 1) |> fst in
          Some (default_shell ~command_option one ~options)
      )
  in
  let execution_timeout =
    Uri.get_query_param uri "timeout" 
    |> Option.bind ~f:Float.of_string in
  create ?playground ~connection ?default_shell ?execution_timeout
    (Uri.to_string uri)

let to_uri t =
  let scheme, host, port, userinfo =
    match t.connection with
    | `Ssh {Ssh.address; port; user;} -> Some "ssh", Some address, port, user
    | `Localhost -> None, None, None, None
  in
  let query =
    let {binary; command_name; options; command_option} =
      t.default_shell in
    let shell_spec = [command_name] @ options @ [command_option] in
    ["shell", [String.concat ~sep:"," shell_spec]]
  in
  Uri.make ?scheme ?userinfo ?host ?port 
    ?path:(Option.map ~f:Path.to_string t.playground)
    ~query ()

let of_string s =
  let uri = Uri.of_string s in
  of_uri uri

let log t = 
  Log.(brakets ( s "Host " % s (Uri.to_string (to_uri t))))

let to_string_hum t = Log.to_long_string (log t)

let execution_timeout t = t.execution_timeout
let connection t = t.connection
let playground t = t.playground


