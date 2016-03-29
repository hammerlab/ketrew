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

(** Implementation of the {!LONG_RUNNING} API asking Aapache Yarn
    for resources and using {!Ketrew_daemonize} to “keep” the
    process group together. *)

(** This module implements {!Ketrew_long_running.LONG_RUNNING} plugin-API.
*)


(** The “standard” plugin-API. *)
include Long_running.LONG_RUNNING


type distributed_shell_parameters

val distributed_shell_program :
  ?hadoop_bin:string ->
  ?distributed_shell_shell_jar:string ->
  ?container_vcores: int ->
  container_memory:[ `GB of int | `MB of int | `Raw of string ] ->
  timeout:[ `Raw of string | `Seconds of int ] ->
  application_name:string ->
  Ketrew_pure.Program.t ->
  [> `Distributed_shell of distributed_shell_parameters * Ketrew_pure.Program.t ]
(** Create a value [`Distributed_shell _] to feed to {!create},
    see {!Edsl.yarn_distributed_shell}. *)

val create :
  ?host:Ketrew_pure.Host.t ->
  ?daemonize_using:[ `Nohup_setsid | `Python_daemon ] ->
  ?daemon_start_timeout: float ->
  [ `Distributed_shell of distributed_shell_parameters * Ketrew_pure.Program.t
  | `Yarn_application of Ketrew_pure.Program.t ] ->
  [> `Long_running of string * string ]
(** Create a “long-running” {!Ketrew_pure.Target.build_process} (run parameters
    are already serialized), see {!Edsl.yarn_application}. *)

(** {3 Advanced Global Configuration} *)

val max_name_length : int ref
(* The distributed-shell applications create files (and read them); we have
   seen it go wrong on some special characters or when reaching the maximal
   filename length (of the filesystem, often 255 bytes).
   As part of their sanitization, application-names are truncated to this value
   (default [200]).
*)
