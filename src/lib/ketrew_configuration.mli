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
open Ketrew_unix_io

type t
(** The contents of the configuration. *)

type plugin = [ `Compiled of string | `OCamlfind of string ]
(** The 2 kinds of dynamically loaded “plugins” accepted by Ketrew:

    - [`Compiled path]: path to a `.cma` or `.cmxs` compiled file.
    - [`OCamlfind package]: name of a Findlib package.

*)
type ui
val ui: ?with_color:bool -> unit -> ui

type engine
val engine: 
  ?database_parameters:string ->
  ?persistent_state_key:string -> 
  ?turn_unix_ssh_failure_into_target_failure: bool ->
  ?host_timeout_upper_bound: float ->
  unit -> engine

type server
val server: 
  ?ui:ui ->
  ?engine:engine ->
  ?authorized_tokens_path: string ->
  ?return_error_messages: bool ->
  ?command_pipe: string ->
  ?daemon: bool ->
  ?log_path: string ->
  [ `Tls of string * string * int ] ->
  [> `Server of server]
(** Create a server configuration (to pass as optional argument to the
    {!create} function).

    - [authorized_tokens_path] is a path to a file similar to an SSH
    [authorized_keys] file.
    - [return_error_messages]: whether the server should return explicit error
    messages to clients (default [false]).
    - [command_pipe]: path to a named-piped for the server to listen to
    commands.
    - [daemon]: whether to daemonize the server or not  (default [false]).
    - [log_path]: path to write logs.
    - [`Tls ("certificate.pem", "privatekey.pem", port)]: configure the OpenSSL
    server to listen on [port].
*)

type standalone
val standalone: ?ui:ui -> ?engine:engine -> unit -> [> `Standalone of standalone]
type client
val client: ?ui:ui -> token:string -> string -> [> `Client of client]

type mode = [
  | `Standalone of standalone
  | `Server of server
  | `Client of client
]

val create : ?debug_level:int -> ?plugins: plugin list -> mode  -> t
(** Create a configuration, [persistent_state_key] is the “key” of the
    state storage in the database, [database_parameters] are used to call
    {!Ketrew_database.load}.

    The parameter [turn_unix_ssh_failure_into_target_failure] tells
    Ketrew whether it should kill targets when a failure is not
    assuredly “their fault” (e.g. a call to [ssh] may fail
    because of network settings, and succeed when tried again later);
    the default value is [false].

    See the documentation on the configuration file for further explanations on
    the parameters.
*)

val default_database_path: string
(** Default path to the database (used when generating custom configuration
    files). *)

val database_parameters: engine -> string
(** Get the database parameters. *)

val persistent_state_key: engine -> string
(** Get the “key” of the state values in the database. *)

val is_unix_ssh_failure_fatal: engine -> bool
(** Should we kill targets on ssh/unix errors. *)

val plugins: t ->  plugin list
(** Get the configured list of plugins. *)

val mode: t -> mode

val standalone_engine: standalone -> engine
val server_engine: server -> engine

val server_configuration: t -> server option
(** Get the potentiel server configuration. *)

val authorized_tokens_path: server -> string option 
(** The path to the [authorized_tokens] file. *)

val listen_to: server -> [ `Tls of (string * string * int) ]
(** Get the OpenSSL server configuration. *)

val return_error_messages: server -> bool
(** Get the value of [return_error_messages]. *)

val command_pipe: server -> string option
(** Get the path to the “command” named pipe. *)

val daemon: server -> bool
(** Tell whether the server should detach. *)

val log_path: server -> string option
(** Get the path to the server's log file. *)

val log: t -> Log.t
(** Get a display-friendly list of configuration items. *)

val connection: client -> string
val token: client -> string

val standalone_of_server: server -> standalone

(** Check environment variable KETREW_CONFIGURATION before returning
    [default_configuration_path]. *)

(** The call [get_configuration file] reads and parses the file [f], unless
    [override_configuration] is provided.
    if [and_apply] is [true] (the default), then global settings are applied
    and plugins are loaded.
*)

(** Do like {!get_configuration} but in a dirty Lwt-less way and
    return only partial information: whether to daemonize or not (the [string
    option] is the potential log-file path). *)

val load_exn:
  ?and_apply:bool ->
  ?profile:string ->
  [ `From_path of string
  | `Guess
  | `In_directory of string
  | `Override of t ] ->
  t
(** Load a configuration.

    If [and_apply] is [true] (the default), then global settings are applied
    and plugins are loaded.

    When the configuration comes from a file, the argument [profile]
    allows to load a given profile. If [None] then the loading process
    will try the ["KETREW_PROFILE"] environment variable, or use the name
    ["default"].
    
    The last argument tells the functions how to load the configuration:
    
    - [`Override c]: use [c] as configuration
    - [`From_path path]: parse the file [path]
    - [`In_directory root]: look for configuration files in the [root]
      directory
    - [`Guess]: use environment variables and/or default values to
      find the configuration file.

   *)


type profile
(** A profile is a name associated with a configuraton. *)

val profile: string -> t -> profile
(** Create a profile value. *)

val output: profile list -> unit
(** Output a configuration file containing a list of profile to [stdout]. *)
