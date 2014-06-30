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

(** Implementation of the {!LONG_RUNNING} API with the LSF batch processing
    scheduler.
*)

(**
    “Long-running” plugin based on the
    {{:http://en.wikipedia.org/wiki/Platform_LSF}LSF} batch scheduler.

    Shell commands are put in a {!Ketrew_monitored_script.t}, and
    started with ["bsub [OPTIONS] < <script>"] (we gather the job-id while
    submitting).

    The {!update} function uses the log-file of the monitored-script, and the
    command ["bjobs [OPTIONS] <job-ID>"].

    The {!kill} function kills the job with ["bkill <job-ID>"].

*)


include Ketrew_long_running.LONG_RUNNING
(** The “standard” plugin API. *)

val create :
  ?host:Ketrew_host.t ->
  ?queue:string ->
  ?name:string ->
  ?wall_limit:string ->
  ?processors:[ `Min of int | `Min_max of int * int ] ->
  Ketrew_program.t ->
  [> `Long_running of string  * string ]
  (** Create a “long-running” {!Ketrew_target.build_process} to run a 
    {!Ketrew_program.t} on a given LSF-enabled host (run parameters
    already serialized): {ul
      {li [?queue] is the name of the LSF queue requested (["-q"] option). }
      {li [?name] is the job name (["-J"] option). }
      {li [?wall_limit] is the job's Wall-time timeout (["-W"] option). }
      {li [?processors] is the “processors” request (["-n"] option). }
    }

*)

