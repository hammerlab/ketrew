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

type token = {name: string; value: string; comments : string list}
type t = {
  valid_tokens: token list;
  authentication_input: [`Path of string | `Inline of (string * string)] list;
}

let log {valid_tokens; authentication_input} =
  let paths =
    List.filter_map authentication_input ~f:(function
      | `Path p -> Some p | `Inline _ -> None) in
  let inlines =
    List.filter_map authentication_input ~f:(function
      | `Path _ -> None | `Inline (name, _) -> Some name) in
  Log.(
    s "From paths: " % OCaml.list quote paths
    % s "; inline: " % OCaml.list string inlines)

let valid {value; _} =
  let valid_chars = B64.uri_safe_alphabet ^ "=" in
  let invalid_char = fun c -> not (String.exists valid_chars ((=) c)) in
  match String.find ~f:invalid_char value with
  | Some _ -> false
  | None -> true

let split_and_trim line =
  String.split line ~on:(`Character ' ')
  |> List.map ~f:(fun t -> String.strip ~on:`Both t)
  |> List.filter ~f:(fun s -> s <> "")

let load_file file =
  Log.(s "Authentication: loading " % quote file @ verbose);
  IO.read_file file
  >>= fun content ->
  let valid_tokens =
    String.split content ~on:(`Character '\n')
    |> List.filter_map ~f:(fun line ->
        match split_and_trim line with
        | comment :: _ when String.get comment ~index:1 = Some '#' -> None
        | name :: value :: comments ->
            let token =  {name; value; comments} in
            begin match valid token with
            | true -> Some token
            | false ->  Log.(s "Invalid character(s) in token: " % OCaml.string value % s " in file "
                            % OCaml.string file @ warning);
                        None
            end
        | [] -> None
        | other ->
          Log.(s "Ignoring line: " % OCaml.string line % s " of file "
                % OCaml.string file @ warning);
          None)
  in
  Log.(s "Loaded auth from " % OCaml.string file
        % OCaml.list (fun t ->
            OCaml.list OCaml.string [t.name; t.value;
                                    String.concat ~sep:" "  t.comments])
          valid_tokens
        @ verbose);
  return valid_tokens

let load meta_tokens =
  Deferred_list.while_sequential meta_tokens ~f:(function
    | `Path p -> load_file p
    | `Inline (name, value) ->
        let token = {name; value; comments = []} in
        let tokens =
          match valid token with
          | true -> [token]
          | false ->
            Log.(s "Invalid character(s) in token: " % OCaml.string value @ warning);
            [] in
        return tokens)
  >>| List.concat
  >>= fun valid_tokens ->
  return {valid_tokens; authentication_input = meta_tokens}

let reload {authentication_input; _} = load authentication_input

type capabilities = [
  | `Browse_gui
  | `See_targets
  | `Query_targets
  | `See_server_status
  | `Restart_targets
  | `Submit_targets
  | `Kill_targets
  | `Play_with_process_holder
]


let able_to_do ~read_only_mode = function
  | `Browse_gui
  | `See_server_status
  | `See_targets
  | `Query_targets  -> true
  | `Kill_targets
  | `Restart_targets
  | `Play_with_process_holder
  | `Submit_targets -> not read_only_mode

let can t ~read_only_mode ?token stuff =
  let token_is_valid tok =
    List.exists t.valid_tokens ~f:(fun x -> x.value = tok) in
  let valid_token =
    Option.value ~default:false (Option.map ~f:token_is_valid token) in
  valid_token && able_to_do stuff ~read_only_mode


