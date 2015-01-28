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

type t = Ketrew_gen_base_v0.Program.t

let rec to_shell_commands = function
| `Shell_command s -> [s]
| `Exec sl -> 
  [fmt "%s" (List.map sl ~f:Filename.quote |> String.concat ~sep:" ")]
| `And l -> List.concat_map l ~f:to_shell_commands

let to_single_shell_command t = 
  String.concat ~sep:" && " (to_shell_commands t)

let rec log = function
| `Shell_command str -> Log.(s "Sh:" % brakets (s str))
| `Exec sl -> Log.(s "Exec:" % OCaml.list (sf "%S") sl)
| `And l -> Log.(separate (s " && ") (List.map ~f:log l))

let to_string_hum p = Log.to_long_string (log p)



