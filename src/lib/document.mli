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

open Internal_pervasives

(** Transform complex Ketrew values into display-friendly {!Log.t} values. *)
val build_process : ?with_details:bool ->
    [< `Long_running of string * string | `No_operation ] ->
    SmartPrint.t

val target_for_menu : Ketrew_pure.Target.t -> Log.t

val metadata: full:bool -> [ `String of string ] -> Log.t

val target : ?build_process_details:bool ->
  ?condition_details:bool ->
  ?metadata_details:bool ->
  Ketrew_pure.Target.t ->
  Log.t
