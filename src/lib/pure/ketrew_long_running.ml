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

(** Definition of the interface required from “long-running task” plugins. *)

open Ketrew_pervasives

type error = [
  | `Fatal of string
  | `Recoverable of string
]
(** The “imposed” error types for “long-running” plugins.
    A [`Fatal _] error will make the target die with the error,
    whereas if an error is [`Recoverable _] Ketrew will keep trying
    (for example, a networking error which may not happen later).
*)

(** The module type [LONG_RUNNING] defines the interface for plugins. *)
module type LONG_RUNNING = sig

  type run_parameters
  (** Hidden type kept serialized by the engine. *)

  val name: string
  (** The (unique) name of the plugin. *)

  val serialize: run_parameters -> string
  (** Serialize the run parameters for storage by the engine. *)

  val deserialize_exn: string -> run_parameters
  (** Deserialize the run parameters from a string; the engine guaranties
      that [deserialize_exn] will be called on the result of {!serialize};
      and assumes that no exception will be thrown in that case. *)

  val start: run_parameters ->
    (run_parameters, error) Deferred_result.t
  (** Start the long-running computation, the returned [run_parameters] will be
      stored and used for the first call to {!update}. *)

  val update: run_parameters ->
    ([`Succeeded of run_parameters
     | `Failed of run_parameters * string
     | `Still_running of run_parameters], error) Deferred_result.t
  (** Check and update the status of the long-running job. Again, is
      [`Still_running rp] is returned, the next call to {!update} (or {!kill})
      will receive those parameters. *)

  val kill: run_parameters ->
    ([`Killed of run_parameters], error) Deferred_result.t
  (** Kill the long-running computation. *)

  val log: run_parameters -> (string * Log.t) list
  (** Get a list of things to display. *)

  val additional_queries : run_parameters -> (string * Log.t) list
  (** List of potential [(query, description)] pairs that can be passed
      to {!query}. *)

  val query: run_parameters -> string -> (string, Log.t) Deferred_result.t
  (** Perform a query. *)
end
