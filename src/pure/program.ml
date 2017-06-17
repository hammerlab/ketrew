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

type t = [
  | `And of t list
  | `Exec of string list
  | `Shell_command of string
] [@@deriving yojson]


let exec_to_command sl =
  fmt "%s" (List.map sl ~f:Filename.quote |> String.concat ~sep:" ")

let rec to_shell_commands = function
| `Shell_command s -> [s]
| `Exec sl -> [exec_to_command sl]
| `And l -> List.concat_map l ~f:to_shell_commands

let to_single_shell_command t = 
  String.concat ~sep:" && " (to_shell_commands t)

let rec log = function
| `Shell_command str -> Log.(s "Sh:" % brakets (s str))
| `Exec sl -> Log.(s "Exec:" % OCaml.list (sf "%S") sl)
| `And l -> Log.(separate (s " && ") (List.map ~f:log l))

let to_string_hum p = Log.to_long_string (log p)

let flatten p =
  let rec to_leaves =
    function
    | `Exec _ | `Shell_command _ as leaf -> [leaf]
    | `And l ->
      List.concat_map l ~f:to_leaves
  in
  match p with
  | `And l -> `And (to_leaves p)
  | other -> other

let rec markup =
  let open Display_markup in
  function
  | `And l ->
    List.map l ~f:markup
    |> itemize
    |> description "Chain (&&)"
  | `Shell_command c ->
    description "Sh" (command c)
  | `Exec ex ->
    description "Exec" (exec_to_command ex |> command)

let flatten_program = flatten
let markup ?(flatten = true) p =
  if flatten then markup (flatten_program p) else markup p

