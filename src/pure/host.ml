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
  | `Named of string
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

let markup {name; connection; playground; default_shell; execution_timeout} =
  let open Display_markup in
  description_list [
    "Name", Text name;
    "Connection",
    begin match connection with
    | `Localhost -> Text "Local-host"
    | `Named n -> textf "Named: %S" n
    | `Ssh {Ssh. address; port; user; add_ssh_options} ->
      concat [
        path (fmt "ssh://%s%s%s"
                (Option.value_map user ~default:"" ~f:(fmt "%s@"))
                address
                (Option.value_map port ~default:"" ~f:(fmt ":%d")));
        (match add_ssh_options with
        | [] -> Text " (no options)"
        | more ->
          concat [textf " (options: ";
                  flat_list more ~f:command;
                  textf ")"])
      ]
    end;
    "Playground", option ~f:(fun p -> Path.to_string p |> path) playground;
    "Default-shell",
    (let {binary; command_name; options; command_option} = default_shell in
      description_list [
        "Binary", option ~f:command binary;
        "Command-name", command command_name;
        "Options", flat_list options ~f:command;
        "Command-option", command command_option;
      ]);
    "Execution-timeout", option ~f:time_span execution_timeout;
  ]


  
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

let of_uri_exn uri =
  let connection =
    match Uri.scheme uri, Uri.host uri with
    | None, None -> `Localhost
    | Some "named", Some name -> `Named name
    | Some "ssh", Some address
    | None, Some address ->
        let add_ssh_options =
          Option.value ~default:[] (Uri.get_query_param' uri "ssh-option")
          @ Option.value ~default:[] (Uri.get_query_param' uri "ssh-options")
        in
        let user = Uri.userinfo uri in
        `Ssh {Ssh.address; port = Uri.port uri; user; add_ssh_options}
    | Some other, _ -> 
      failwith (fmt "Unkown scheme: %S" other)
  in
  let playground =
    match Uri.path uri with
    | "" -> None
    | rel when Filename.is_relative rel ->
      (* It seems `path` returns a relative path only when there is no host
         Uri.(of_string "path" |> path) = "path"
         Uri.(of_string "ssh:path" |> path)  = "path"
         The second one should have failed earlier (scheme without host)
      *)
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

let of_uri uri =
  let open Result in
  try of_uri_exn uri |> return with
  | Failure f -> fail (`Host_uri_parsing_error (Uri.to_string uri, f))
  | e ->
    fail (`Host_uri_parsing_error (Uri.to_string uri, Printexc.to_string e))

let to_uri t =
  let scheme, host, port, userinfo, add_ssh_options =
    match t.connection with
    | `Ssh {Ssh.address; port; user; add_ssh_options} ->
      Some "ssh", Some address, port, user, add_ssh_options
    | `Localhost -> None, None, None, None, []
    | `Named n -> Some "named", Some n, None, None, []
  in
  let query =
    let {binary; command_name; options; command_option} =
      t.default_shell in
    let shell_spec = [command_name] @ options @ [command_option] in
    "shell", [String.concat ~sep:"," shell_spec];
    ::
    begin match add_ssh_options with
    | [] -> []
    | more -> [ "ssh-options", more ]
    end
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


