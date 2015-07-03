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


val default_plugins :
  (string * (module Long_running.LONG_RUNNING)) list
(** The “long-running” plugins loaded by default. *)

val register_long_running_plugin :
  name:string -> (module Long_running.LONG_RUNNING) -> unit
(** Function to be called from dynamically loaded plugins. *)

val long_running_log: string -> string -> (string * Log.t) list
(** [long_running_log ~state plugin_name serialized_run_params]
    calls {!Long_running.LONG_RUNNING.log} with the right plugin. *)

val additional_queries: Ketrew_pure.Target.t -> (string * Log.t) list
(** Get the potential additional queries ([(key, description)] pairs) that can
    be called on the target. *)

val call_query:  target:Ketrew_pure.Target.t -> string ->
  (string, Log.t) Deferred_result.t
(** Call a query on a target. *)

val find_plugin: string -> (module Long_running.LONG_RUNNING) option

val load_plugins :
  [ `Compiled of string | `OCamlfind of string ] list ->
  (unit,
   [> `Dyn_plugin of
        [> `Dynlink_error of Dynlink.error | `Findlib of exn ]
   | `Failure of string ]) Deferred_result.t

val load_plugins_no_lwt_exn :
  [ `Compiled of string | `OCamlfind of string ] list -> unit
(** Dynamically load a list of plugins, this function is not
    cooperative (with Lwt) and may raise [Failure].

    The specification is (structurally) the same type as
    {!Ketrew_pure.Configuration.plugin}.
*)
