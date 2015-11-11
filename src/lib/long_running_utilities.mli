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

open Ketrew_pure.Internal_pervasives
open Unix_io

val fail_fatal : string -> ('b, [> `Fatal of string ]) Deferred_result.t
(** Call {!Deferred_result.fail} with a “fatal error” (Mandatory in the
    Long-running API). *)

val out_file_path : playground:Ketrew_pure.Path.t -> Ketrew_pure.Path.t
(** Standard path for [stdout] files (given a fresh playground). *)

val err_file_path : playground:Ketrew_pure.Path.t -> Ketrew_pure.Path.t
(** Standard path for [stderr] files. *)

val script_path : playground:Ketrew_pure.Path.t -> Ketrew_pure.Path.t
(** Standard path for monitored-script files. *)

val classify_and_transform_errors :
  ('a,
   [< `Fatal of string
   | `Host of 
        [ `Execution of
            < host : bytes; message : bytes; stderr : bytes option;
              stdout : bytes option >
        | `Named_host_not_found of bytes
        | `Non_zero of bytes * int
        | `Ssh_failure of
            [ `Wrong_log of bytes
            | `Wrong_status of Unix_process.Exit_code.t ] * 
            bytes
        | `System of [ `Sleep of float ] * [ `Exn of exn ]
        | `Timeout of float
        | `Unix_exec of bytes ]
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
        | `Wrong_file_kind of string * System.file_info
        | `Wrong_path of string ]
   | `Timeout of 'b ]) Result.t ->
  ('a, [ `Fatal of string | `Recoverable of string ]) Deferred_result.t
(** Transform most known errors into long-running plugin API errors; using
    {!Ketrew_pure.Host.Error.classify}.  *)

val fresh_playground_or_fail :
  host_io:Host_io.t ->
  Ketrew_pure.Host.t ->
  (Ketrew_pure.Path.t, [> `Fatal of string ]) Deferred_result.t
(** Get a fresh-playground from a [Host.t]. *)

val get_log_of_monitored_script :
  host_io:Host_io.t ->
  host:Ketrew_pure.Host.t ->
  script:Ketrew_pure.Monitored_script.t ->
  ([ `After of string * string * string
   | `Before of string * string * string
   | `Error of string list
   | `Failure of string * string * string
   | `Start of string
   | `Success of string ] list option,
   [> `Host of [> `Named_host_not_found of bytes ]
   | `Timeout of float ]) Deferred_result.t
(** Fetch and parse the [log] file of a monitored-script. *)

val get_pid_of_monitored_script :
  host_io:Host_io.t ->
  host:Ketrew_pure.Host.t ->
  script:Ketrew_pure.Monitored_script.t ->
  (int option,
   [> `Host of [> `Named_host_not_found of bytes ]
   | `Timeout of float ]) Deferred_result.t
(** Fetch and parse the [pid] file of a monitored-script. *)

val shell_command_output_or_log :
  host_io:Host_io.t ->
  host:Ketrew_pure.Host.t ->
  string -> (string, Log.t) Deferred_result.t
(** Call {!Host_io.get_shell_command_output} and transform errors
    into a {!Log.t}. *)
