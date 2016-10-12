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


(**
Implementation of the HTTP server.
*)

open Ketrew_pure.Internal_pervasives

open Unix_io


val start :
  just_before_listening:(
    unit ->
    (unit,
     [> `Database of Persistent_data.Error.database
     | `Database_unavailable of string
     | `Dyn_plugin of
          [> `Dynlink_error of Dynlink.error
          | `Findlib of exn ]
     | `Failure of string
     | `IO of
          [> `Read_file_exn of string * exn ]
     | `Fetching_node of Persistent_data.Error.fetching_node
     | `Server_status_error of string
     | `Start_server_error of string
     | `System of
          [> `File_info of string
          | `List_directory of string
          | `Remove of string ] *
          [> `Exn of exn ]
     | `Target of
          [> `Deserilization of string ] ]
     as 'propagated_error) Unix_io.t) ->
  configuration:Configuration.server ->
  (unit, 'propagated_error) Unix_io.t
(** Start the server according to its configuration.  *)


val status: configuration:Configuration.server ->
  ([ `Not_responding of string
   | `Running
   | `Wrong_response of Cohttp.Response.t ],
   [> `Failure of string | `Server_status_error of string ]) Deferred_result.t
(** Ask for the status of the server running locally by calling
    ["https://127.0.0.1:<port>/hello"]. *)



val stop: configuration:Configuration.server ->
  ([ `Done | `Timeout ],
   [> `IO of [> `Exn of exn | `File_exists of string | `Wrong_path of string ]
   | `Stop_server_error of string
   | `System of [> `File_info of string ] * [> `Exn of exn ] ]) Deferred_result.t
(** Stop the server by calling the commad ["die"] on the configured
    command-pipe, stopping will fail with [`Stop_server_error _] if
    that path is not configured. *)
