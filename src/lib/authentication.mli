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

(** Server Authentication.

  Module dealing with access tokens and access rights. There are no
  “sessions” here; just a file that looks like SSH's `authorized_keys`, and a
  function: token × capability → bool.

  Capabilities are defined with polymorphic variants.
*)

open Ketrew_pure
open Internal_pervasives
open Unix_io

type t

val log : t -> Log.t
(** Describe the source of the authentication. *)

val load : [ `Inline of string * string | `Path of String.t ] list ->
    (t, [> `IO of [> `Read_file_exn of IO.path * exn ] ]) Deferred_result.t
(** Load tokens that represent your authentication.*)

val reload : t ->
    (t, [> `IO of [> `Read_file_exn of IO.path * exn ] ]) Deferred_result.t
(** Reload tokens based upon their original source.
    
    This makes sense if the they were originall loaded from a specific `Path. *)

(** The capabilities that are validated, these are grouped according to the
    {{!type:Ketrew_pure.Protocol.Up_message.t} Up_message.t} that is received.*)
type capabilities = [
  | `Browse_gui                 (** Can we open up web gui *)
  | `See_targets                (** Can we show the targets tracked by the server. *)
  | `Query_targets              (** Can we perform individual query targets, see {{!val:Ketrew_pure.Protocol.Up_message.`Call_query} `Call_query}*)
  | `See_server_status          (** Can we inspect the servers status. *)
  | `Restart_targets            (** Can the server restart targets. *)
  | `Submit_targets             (** Is the server accepting new targets. *)
  | `Kill_targets               (** Can the server kill targets. *)
  | `Play_with_process_holder   (** Modify the process controling targets. *)
]

(** Determine if we have the desired capabilities *)
val can : t -> read_only_mode:bool -> ?token:string -> capabilities -> bool
