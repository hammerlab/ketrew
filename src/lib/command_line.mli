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

(** Command line interface to the engine. *)

open Ketrew_pure.Internal_pervasives

open Unix_io

val run_main :
  ?argv:string array ->
  ?override_configuration:Configuration.t ->
  ?additional_commands: ((unit, string) Deferred_result.t Cmdliner.Term.t * Cmdliner.Term.info) list ->
  unit ->
  [ `Never_returns ]
(** The “main” function for the application, it will [exit n] with [n = 0] if
    succeed or [n > 0] if an error occurs.

    - [argv]: one can provide an array of arguments to be used instead of
    {!Sys.argv}.
    - [override_configuration]: providing a custom configuration will prevent
    Ketrew from looking up a configuration file.
    - [additional_commands]: a list of {!Cmdliner} commands to add to the
    interface.

*)


