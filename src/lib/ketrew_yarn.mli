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
include Ketrew_long_running.LONG_RUNNING


type distributed_shell_parameters

val distributed_shell_program :
  ?hadoop_bin:string ->
  ?distributed_shell_shell_jar:string ->
  container_memory:[ `GB of int | `MB of int | `Raw of string ] ->
  timeout:[ `Raw of string | `Seconds of int ] ->
  application_name:string ->
  Ketrew_program.t ->
  [> `Distributed_shell of distributed_shell_parameters * Ketrew_program.t ]
(** Create a value [`Distributed_shell _] to feed to {!create},
    see {!Ketrew_edsl.yarn_distributed_shell}. *)

val create :
  ?host:Ketrew_host.t ->
  ?daemonize_using:[ `Nohup_setsid | `Python_daemon ] ->
  ?daemon_start_timeout: float ->
  [ `Distributed_shell of distributed_shell_parameters * Ketrew_program.t
  | `Yarn_application of Ketrew_program.t ] ->
  [> `Long_running of string * string ]
(** Create a “long-running” {!Ketrew_target.build_process} (run parameters
    are already serialized), see {!Ketrew_edsl.yarn_application}. *)
