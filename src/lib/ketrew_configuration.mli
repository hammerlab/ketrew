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

(** Definition of the configuration (input to state creation; contents of the
    config-file). *)

open Ketrew_pervasives

type server
type t
(** The contents of the configuration. *)

val create_server: 
  ?authorized_tokens_path: string ->
  ?return_error_messages: bool ->
  ?command_pipe: string ->
  ?daemon: bool ->
  ?log_path: string ->
  [ `Tls of string * string * int ] ->
  server
(** Create a server configuration (to pass as optional argument to the
      {!create} function). *)

val create :
  ?debug_level:int ->
  ?with_color:bool ->
  ?turn_unix_ssh_failure_into_target_failure: bool ->
  ?persistent_state_key:string -> 
  ?host_timeout_upper_bound: float ->
  ?server:server ->
  database_parameters:string -> unit -> t
(** Create a configuration, [persistent_state_key] is the “key” of the
    state storage in the database, [database_parameters] are used to call
    {!Ketrew_database.load}.

    The parameter [turn_unix_ssh_failure_into_target_failure] tells
    Ketrew whether it should kill targets when a failure is not
    assuredly “their fault” (e.g. a call to [ssh] may fail
    because of network settings, and succeed when tried again later);
    the default value is [false].
*)

val default_configuration_path: string
(** Default path to the configuration file. *)

val default_database_path: string
(** Default path to the database (used when generating custom configuration
    files). *)

val database_parameters: t -> string
(** Get the database parameters. *)

val persistent_state_key: t -> string
(** Get the “key” of the state values in the database. *)

val is_unix_ssh_failure_fatal: t -> bool
(** Should we kill targets on ssh/unix errors. *)

val parse :
  string ->
  (t, [> `Configuration of [> `Parsing of string ] ]) Result.t
(** Parse the contents of a configuration file. *)

val apply_globals: t -> unit
(** Apply options that have global impact. *)

val get_configuration :
  ?and_apply:bool ->
  ?override_configuration:t ->
  string ->
  (t,
   [> `Configuration of [> `Parsing of string ]
   | `IO of [> `Read_file_exn of string * exn ] ]) Deferred_result.t
(** The call [get_configuration file] reads and parses the file [f], unless
    [override_configuration] is provided.
    if [and_apply] is [true] (the default), then {!apply_globals} is called.
*)


val server_configuration: t -> server option
val authorized_tokens_path: server -> string option 
val listen_to: server -> [ `Tls of (string * string * int) ]
val return_error_messages: server -> bool
val command_pipe: server -> string option
val daemon: server -> bool
val log_path: server -> string option

val log: t -> Log.t list
(** Get a display-friendly list of configuration items. *)
