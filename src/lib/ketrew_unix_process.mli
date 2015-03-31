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

(** Manage calls to Unix processes *)

open Ketrew_pervasives

(** Higher-level representation of Unix exit codes. *)
module Exit_code: sig
  type t = [
    | `Exited of int
    | `Signaled of int
    | `Stopped of int
  ]
  val to_string: t -> string
  val to_log: t -> Log.t
end

val exec :
  ?bin:string ->
  string list ->
  (string * string * Exit_code.t,
   [> `Process of
        [> `Exec of string * string list ] * [> `Exn of exn ] ])
    Deferred_result.t
(** Execute a process with a given list of strings as “[argv]”, if you can
    provide the [~bin] argument to specify the actual file to be executed. The
    function returns the tuple [(stdout, stderr, exit_code)]. *)

val succeed :
  ?bin:string ->
  string list ->
  (string * string,
   [> `Process of
        [> `Exec of string * string list ] *
        [> `Exn of exn | `Non_zero of string ] ])
    Deferred_result.t
(** Do like {!exec} but fail if the process does not exit with [0] status. *)

val error_to_string :
  [< `Process of
       [< `Exec of string * string list ] *
       [< `Exn of exn | `Non_zero of string ] ] ->
  string
(** Display-friendly version of the errors of this module. *)
