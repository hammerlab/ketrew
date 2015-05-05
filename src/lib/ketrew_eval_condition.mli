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

(** Evaluation of {!Ketrew_target.Condition.t} values. *)

open Ketrew_pervasives

open Ketrew_unix_io

val bool: Ketrew_target.Condition.t ->
    (bool,
     [> `Host of
          [> `Execution of
               < host : string; message : string;
                 stderr : string option; stdout : string option >
          | `Non_zero of string * int
          | `Ssh_failure of
               [> `Wrong_log of string
               | `Wrong_status of Ketrew_unix_process.Exit_code.t ] *
               string
          | `System of [> `Sleep of float ] * [> `Exn of exn ]
          | `Timeout of float
          | `Unix_exec of string ]
            Ketrew_host_io.Error.execution
     | `Volume of [> `No_size of Ketrew_pervasives.Log.t ] ]) Deferred_result.t




