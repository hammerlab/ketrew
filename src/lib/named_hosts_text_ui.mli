(**************************************************************************)
(*    Copyright 2014, 2015, 2105:                                         *)
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

open Ketrew_pure
open Unix_io

val sub_commands:
  version:string ->
  prefix:string ->
  configuration_arg:Configuration.t Cmdliner.Term.t ->
  unit ->
  ((unit,
    [> `Client of Client.Error.t
    | `Database of Persistent_data.Error.database
    | `Dyn_plugin of [> `Dynlink_error of Dynlink.error | `Findlib of exn ]
    | `Failure of string
    | `IO of [> `Write_file_exn of string * exn ]
    | `Wrong_configuration of [> `Found of string ] * [> `Exn of exn ] ])
     Deferred_result.t Cmdliner.Term.t * Cmdliner.Term.info)
    list
(** Create a list of cmdliner sub-commands to integrate in a greater command
    line application.

    The command names will start with [prefix].
*)
