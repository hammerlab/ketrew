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

val fail_fatal : string -> ('b, [> `Fatal of string ]) Deferred_result.t
(** Call {!Deferred_result.fail} with a “fatal error” (Mandatory in the
    Long-running API). *)

val out_file_path : playground:Ketrew_path.t -> Ketrew_path.t
(** Standard path for [stdout] files (given a fresh playground). *)

val err_file_path : playground:Ketrew_path.t -> Ketrew_path.t
(** Standard path for [stderr] files. *)

val script_path : playground:Ketrew_path.t -> Ketrew_path.t
(** Standard path for monitored-script files. *)

val classify_and_transform_errors :
  ('a,
   [< `Fatal of string
   | `Host of
        [ `Execution of
            < host : string; message : string; stderr : string option;
              stdout : string option >
        | `Non_zero of string * int
        | `Ssh_failure of
            [ `Wrong_log of string
            | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string
        | `System of [ `Sleep of float ] * [ `Exn of exn ]
        | `Timeout of float
        | `Unix_exec of string ]
   | `IO of
        [< `Exn of exn
        | `File_exists of string
        | `Read_file_exn of string * exn
        | `Write_file_exn of string * exn
        | `Wrong_path of string ]
   | `System of
        [< `Copy of string
        | `File_info of string
        | `File_tree of string
        | `List_directory of string
        | `Make_directory of string
        | `Make_symlink of string * string
        | `Move of string
        | `Remove of string ] *
        [< `Already_exists
        | `Exn of exn
        | `File_exists of string
        | `File_not_found of string
        | `IO of
            [< `Exn of exn
            | `File_exists of string
            | `Read_file_exn of string * exn
            | `Write_file_exn of string * exn
            | `Wrong_path of string ]
        | `Not_a_directory of string
        | `Wrong_access_rights of int
        | `Wrong_file_kind of string * Ketrew_pervasives.System.file_info
        | `Wrong_path of string ]
   | `Timeout of 'b ]) Result.t ->
  ('a, [ `Fatal of string | `Recoverable of string ]) Deferred_result.t
(** Transform most known errors into long-running plugin API errors; using
    {!Ketrew_host.Error.classify}.  *)

val fresh_playground_or_fail :
  Ketrew_host.t -> (Ketrew_path.t, [> `Fatal of string ]) Deferred_result.t
(** Get a fresh-playground from a [Host.t]. *)

val get_log_of_monitored_script :
  host:Ketrew_host.t ->
  script:Ketrew_monitored_script.t ->
  ([ `After of string * string * string
   | `Before of string * string * string
   | `Error of string list
   | `Failure of string * string * string
   | `Start of string
   | `Success of string ] list option,
   [> `Timeout of Time.t ])
  Deferred_result.t
(** Fetch and parse the [log] file of a monitored-script. *)

val get_pid_of_monitored_script :
  host:Ketrew_host.t ->
  script:Ketrew_monitored_script.t ->
  (int option, [> `Timeout of Time.t ]) Deferred_result.t
(** Fetch and parse the [pid] file of a monitored-script. *)
