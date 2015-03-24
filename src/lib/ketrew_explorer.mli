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

(** The “Target Explorer™“ *)

type t

val create : client:Ketrew_client.t -> unit -> t

val explore : t ->
  (unit, [> `Client of
            [> `Http of
              [> `Archive_targets of string list
              | `Call of [> `GET | `POST ] * Uri.t
              | `Kill_targets of string list
              | `Restart_targets of string list
              | `Targets ] *
              [> `Exn of exn
              | `Json_parsing of string * [> `Exn of exn ]
              | `Unexpected_message of Ketrew_gen_protocol_v0.Down_message.t
              | `Wrong_json of Json.t
              | `Wrong_response of Cohttp.Response.t * string ]
            | `Server_error_response of [> `Call of [> `GET | `POST ] * Uri.t ]
            * string ]
        | `Database of Trakeva.Error.t
        | `Database_unavailable of string
        | `Failure of string
        | `IO of [> `Read_file_exn of string * exn
                 | `Write_file_exn of string * exn ]
        | `Missing_data of string
        | `Persistent_state of [> `Deserilization of string ]
        | `System of [> `File_info of string ] * [> `Exn of exn ]
        | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** [explore ~client exploration_states] runs a read-eval loop to explore and
    interact with targets.*)
