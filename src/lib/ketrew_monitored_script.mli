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

(** Generate Shell scripts that “monitor” commands. *)

(** The goal of this module is to create shell scripts from a high-level
    representation. The scripts are “monitored” in the sense that code is
    added to log every returned value or failure in a parsable [log] file.
*)

open Ketrew_pervasives

type t = Ketrew_gen_base_v0.Monitored_script.t =
  {playground: Ketrew_path.t; program: Ketrew_program.t}
(** The definition of a monitored script. *)

val create:  playground:Ketrew_path.t -> Ketrew_program.t -> t
(** Create a new script, which will run the list of commands, and store state
    values in the [playground] directory. *)

val log_file : t -> Ketrew_path.t
(** Path to the log file of the script. *)

val pid_file : t -> Ketrew_path.t
(** Path to the “PID” file: where the script stores the PID of the process
    running the script, [- pid] will be the process id of the process group
    created by `setsid` (useful for killing the whole process tree). *)

val to_string : ?write_pid:bool -> t -> string
(** Render the [monitored_script] to a shell-script string;
    if [write_pid] is [true] (the default), the script writes the pid to
    [pid_file t]. *)

val parse_log : string ->
  [ `After of string * string * string
  | `Before of string * string * string
  | `Error of string list
  | `Failure of string * string * string
  | `Start of string
  | `Success of string ] list
(** Parse the log file of a [monitored_script]. *)
