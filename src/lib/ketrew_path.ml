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

type t =
  Ketrew_gen_base_v0.Path.t
open Ketrew_gen_base_v0.Path

let file path :  t =
  {kind = `File; path}

let directory path :  t  =
  {kind = `Directory; path}

let root : t = directory "/"

let absolute_file_exn s : t =
  if Filename.is_relative s
  then invalid_argument_exn ~where:"Path" "absolute_file_exn"
  else file s
let absolute_directory_exn s : t =
  if Filename.is_relative s
  then invalid_argument_exn ~where:"Path" "absolute_directory_exn"
  else directory s
let relative_directory_exn s : t =
  if Filename.is_relative s
  then directory s
  else invalid_argument_exn ~where:"Path" "relative_directory_exn"
let relative_file_exn s : t =
  if Filename.is_relative s
  then file s
  else invalid_argument_exn ~where:"Path" "relative_file_exn"

let concat = fun x y -> { kind = y.kind; path = Filename.concat x.path y.path}

let to_string: t -> string = fun x -> x.path
let to_string_quoted: t -> string = fun x -> Filename.quote x.path

let size_shell_command = function
| {kind = `File; path } ->
  fmt "\\ls -nl %s | awk '{print $5}'" (Filename.quote path)
| {kind = `Directory; path } ->  "echo '0'"

let exists_shell_condition = function
| {kind = `File; path } ->  fmt "[ -f %s ]" (Filename.quote path)
| {kind = `Directory; path } ->  fmt "[ -d %s ]" (Filename.quote path)

