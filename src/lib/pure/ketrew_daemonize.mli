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

(** Implementation of the {!LONG_RUNNING} API with [nohup setsid] unix
    processes or generated Python scripts. *)

(** This module implements {!Ketrew_long_running.LONG_RUNNING} plugin-API.

    Shell commands are put in a {!Ketrew_monitored_script.t}, and
    run in the background (detached in a new process group).

    There are two methods for starting/detaching the computation
    (set with the [~using] parameter): 

    - [`Nohup_setsid] (the default) means that the script will be started with
    ["nohup setsid bash <script> &"].
    This method is the {i POSIX-ly} portable one; but, sadly,
    it is broken on MacOSX
    (see people having
    {{:https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard}TMux problems}, 
    {{:http://stackoverflow.com/questions/23898623/nohup-cant-detach-from-console}Nohup problems}).

    - [`Python_daemon] means that the script will be started by
    a generated Python script.
    Obviously, this works only when the host can run Python scripts (which
    includes MacOSX).


    The {!update} function uses the log-file of the monitored-script, and the
    command ["ps -p <Group-PID>"].

    The {!kill} function kills the process group (created thanks to ["setsid"])
    with ["kill -- <N>"] (where ["<N>"] is the negative PID of the group).

*)

(** The “standard” plugin-API. *)
include Ketrew_long_running.LONG_RUNNING

val create:
  ?starting_timeout:float ->
  ?using:[ `Nohup_setsid | `Python_daemon] ->
  ?host:Ketrew_host.t -> Ketrew_program.t ->
  [> `Long_running of string * string ]
(** Create a “long-running” {!Ketrew_target.build_process} (run parameters
    are already serialized). *)

