(**************************************************************************)
(*  Copyright 2015, Sebastien Mondet <seb@mondet.org>                     *)
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

(**
   The “client” is the frontend to either a standalone engine or an
   HTTP client talking to a server/engine.
*)

open Ketrew_pervasives
open Ketrew_unix_io


(** [Error.t] is the type of the error kinds that this module introduces. *)
module Error : sig

  type t =
    [ `Http of
        [ `Call of [ `GET | `POST ] * Uri.t
        | `Targets
        | `Target_query of Ketrew_pervasives.Unique_id.t * string
        ] *
        [ `Exn of exn
        | `Json_parsing of string * [ `Exn of exn ]
        | `Unexpected_message of Ketrew_protocol.Down_message.t
        | `Wrong_json of Yojson.Safe.json
        | `Wrong_response of Cohttp_lwt_unix.Client.Response.t * string ]
    | `Server_error_response of
        [ `Call of [ `GET | `POST ] * Uri.t ] * string ]

  val log : t -> Log.t
end

type t
(** The handle of the client. *)

val as_client:
  configuration:Ketrew_configuration.t ->
  f:(client:t ->
     (unit,
      [> `Database of Trakeva.Error.t
      | `Database_unavailable of Ketrew_target.id
      | `Dyn_plugin of [> `Dynlink_error of Dynlink.error | `Findlib of exn ]
      | `Failure of string
      | `Wrong_configuration of [> `Found of string ] * [> `Exn of exn ] ]
      as 'a)
       Deferred_result.t) ->
  (unit, 'a) Deferred_result.t
(** Run the function [f] with a fresh-client created with the [configuration].
    
    If the configuration can be for an HTTP client, for a standalone
    engine, or for a server (the client behaves like a local standalone
    engine, using {!Ketrew_configuration.standalone_of_server}).
*)

val configuration: t -> Ketrew_configuration.t
(** Retrieve the configuration used to create the client. *)

val get_local_engine: t -> Ketrew_engine.t option
(** Get the handle to the engine (returns [None] if the client is
    an HTTP one). *)

val current_targets: t -> 
  (Ketrew_target.t list,
   [> `Client of Error.t
   | `Database of Trakeva.Error.t
   | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
   | `Missing_data of Ketrew_target.id
   | `Persistent_state of [> `Deserilization of string ]
   | `System of [> `File_info of string ] * [> `Exn of exn ]
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Get all the current targets. *)

val get_list_of_target_ids : t ->
  query:Ketrew_protocol.Up_message.target_query ->
  (Ketrew_target.id list,
   [> `Client of Error.t
   | `Database of Trakeva.Error.t
   | `Missing_data of string
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Get a list of target IDs given the [query]. *)

val get_target: t ->
  id:Ketrew_target.id ->
  (Ketrew_target.t,
   [> `Client of Error.t
   | `Database of Trakeva.Error.t
   | `Missing_data of string
   | `Persistent_state of [> `Deserilization of string ]
   | `Target of [> `Deserilization of string ] ])
   Deferred_result.t
(** The latest contents of a given target.  *)

val get_targets: t ->
  id_list:Ketrew_target.id list ->
  (Ketrew_target.t list,
   [> `Client of Error.t
   | `Database of Trakeva.Error.t
   | `Missing_data of string
   | `Persistent_state of [> `Deserilization of string ]
   | `Target of [> `Deserilization of string ] ])
   Deferred_result.t
(** Same as {!get_target} but “in bulk.” *)

val call_query: t -> target:Ketrew_target.t -> string ->
  (string, Log.t) Deferred_result.t
(** Call a target's plugin query by name.  *)

val kill: t ->
  Ketrew_target.id list ->
  (unit,
   [> `Client of Error.t
   | `Database of Trakeva.Error.t
   | `Database_unavailable of Ketrew_target.id
   | `Missing_data of Ketrew_target.id
   | `Persistent_state of [> `Deserilization of string ]
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Kill a set of targets. *)
    
val restart_target: t ->
  Ketrew_target.id list ->
  (unit,
   [> `Client of Error.t
   | `Database of Trakeva.Error.t
   | `Database_unavailable of Ketrew_target.id
   | `Missing_data of Ketrew_target.id
   | `Persistent_state of [> `Deserilization of string ]
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Restart a set of targets. *)

val submit:
  ?override_configuration:Ketrew_configuration.t ->
  Ketrew_edsl.user_target ->
  unit
(** Submit a high-level workflow description to the engine; this
    function calls [Lwt_main.run]. *)
