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

(** Implementation of the {!LONG_RUNNING} API with the PBS batch processing
    scheduler.
*)

(**
    “Long-running” plugin based on the
    {{:http://en.wikipedia.org/wiki/Portable_Batch_System}PBS}
    batch scheduler.

    Shell commands are put in a {!Ketrew_monitored_script.t}, and
    started with ["qsub [OPTIONS] <script>"] (we gather the job-id while
    submitting).

    The {!update} function uses the log-file of the monitored-script, and the
    command ["qstat [OPTIONS] <job-ID>"].

    The {!kill} function kills the job with ["qdel <job-ID>"].

*)


include Ketrew_long_running.LONG_RUNNING
(** The “standard” plugin API. *)

val create :
  ?host:Ketrew_host.t ->
  ?queue:string ->
  ?name:string ->
  ?wall_limit:[ `Hours of float ] ->
  ?processors:int ->
  ?email_user:[ `Always of string | `Never ] ->
  ?shell:string ->
  Ketrew_gen_pbs_v0.Program.t ->
  [> `Long_running of string * string ]
(** Create a “long-running” {!Ketrew_target.build_process} to run a 
    {!Ketrew_program.t} on a given PBS-enabled host (run parameters
    already serialized): {ul
    {li [?queue] is the name of the PBS queue requested (["-q"] option). }
    {li [?name] is the job name (["-N"] option). }
    {li [?wall_limit] is the job's Wall-time timeout (["-l"] option, default: 24 H). }
    {li [?processors] is the “processors” request (["-l"] option). }
    {li [?email_user] tell PBS to send emails to the given address. }
    {li [?shell] sets the shell used for the ["#!"] of the PBS script. }
    }

*)

