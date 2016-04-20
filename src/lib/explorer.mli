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

(** The “Target Explorer™“ *)

type t

val create : client:Client.t -> unit -> t

val explore : t ->
  (unit, [> `Client of Client.Error.t 
         | `Database of Trakeva.Error.t
         | `Database_unavailable of string
         | `Failure of string
         | `IO of [> `Read_file_exn of string * exn
                  | `Write_file_exn of string * exn ]
         | `Fetching_node of Persistent_data.Error.fetching_node
         | `System of [> `File_info of string ] * [> `Exn of exn ]
         | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** [explore ~client exploration_states] runs a read-eval loop to explore and
    interact with targets.*)
