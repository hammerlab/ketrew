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

(** The inner-workings of the command `ketrew init`. *)

open Ketrew_pure.Internal_pervasives

open Unix_io

val generate_configuration_directory :
  use_database:[ `Default | `User_set of string ] ->
  tokens:string list ->
  tls:[ `Create_self_signed | `Don't | `Use of string * string ] ->
  port:int ->
  debug_level:int ->
  string ->
  (unit,
   [> `Failure of string
   | `IO of [> `Write_file_exn of Unix_io.IO.path * exn ]
   | `Shell of
        string *
        [> `Exited of int | `Exn of exn | `Signaled of int | `Stopped of int ]
   | `System of
        [> `Make_directory of string ] *
        [> `Exn of exn | `Wrong_access_rights of int ] ])
    Deferred_result.t
(** Generate a configurated directory at a given path (last argument). *)
