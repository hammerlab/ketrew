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

(** The engine of the actual Workflow Engine. *)

open Ketrew_pervasives
open Ketrew_unix_io

type t
(** The contents of the application engine. *)

val with_engine: 
  configuration:Ketrew_configuration.engine ->
  (engine:t ->
   (unit, [> `Database of Trakeva.Error.t
          | `Failure of string
          | `Database_unavailable of Ketrew_target.id
          | `Dyn_plugin of
               [> `Dynlink_error of Dynlink.error | `Findlib of exn ]
          ] as 'merge_error) Deferred_result.t) ->
  (unit, 'merge_error) Deferred_result.t
(** Create a {!engine.t}, run the function passed as argument, and properly dispose of it. *)

val load: 
  configuration:Ketrew_configuration.engine ->
  (t,
   [> `Database of Trakeva.Error.t
   | `Failure of string
   | `Dyn_plugin of
        [> `Dynlink_error of Dynlink.error | `Findlib of exn ]
   ]) Deferred_result.t

val unload: t -> 
  (unit, [>
      | `Database_unavailable of Ketrew_target.id
      | `Database of  Trakeva.Error.t
    ]) Deferred_result.t

val configuration: t -> Ketrew_configuration.engine
(** Retrieve the configuration. *)

val add_targets :
  t ->
  Ketrew_target.t list ->
  (unit,
   [> `Database of Trakeva.Error.t
   | `Database_unavailable of Ketrew_target.id
   | `Missing_data of Ketrew_target.id
   | `Target of [> `Deserilization of string ]
   ]) Deferred_result.t
(** Add a list of targets to the engine. *)

val get_target: t -> Unique_id.t ->
  (Ketrew_target.t,
   [> `Database of Trakeva.Error.t
   | `Missing_data of string
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Get a target from its id. *)

val all_targets :
  t ->
  (Ketrew_target.t list,
   [> `Database of Trakeva.Error.t
    | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
    | `Missing_data of Ketrew_target.id
    | `System of [> `File_info of string ] * [> `Exn of exn ]
    | `Target of [> `Deserilization of string ] ])
  Deferred_result.t
(** Get the list of targets currently handled. *)

val get_list_of_target_ids: t ->
  [ `All | `Not_finished_before of Time.t | `Created_after of Time.t ] ->
  (Ketrew_target.id list,
   [> `Database of Trakeva.Error.t
   | `Missing_data of string
   | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** Get only the Ids of the targets for a given “query”:
    
- [`All] for all the targets visible to the engine.
- [`Not_finished_before _] for the targets that were not finished at a given date.
*)

module Run_automaton : sig
  val step :
    t ->
    (bool,
     [> `Database of  Trakeva.Error.t
     | `Database_unavailable of Ketrew_target.id
     | `Missing_data of Ketrew_target.id
     | `Target of [> `Deserilization of string ] ])
      Deferred_result.t
  (** Run one step of the engine; [step] returns [true] if something happened. *)

  val fix_point: t ->
    ([ `Steps of int],
     [> `Database of Trakeva.Error.t
     | `Database_unavailable of Ketrew_target.id
     | `Missing_data of Ketrew_target.id
     | `Target of [> `Deserilization of string ] ])
      Deferred_result.t
      (** Run {!step} many times until nothing happens or nothing “new” happens. *)
end

val get_status : t -> Ketrew_target.id ->
  (Ketrew_target.State.t,
   [> `Database of Trakeva.Error.t
   | `IO of
        [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
   | `Missing_data of string
   | `System of [> `File_info of string ] * [> `Exn of exn ]
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Get the state description of a given target (by “id”). *)

val kill :
  t ->
  id:Ketrew_pervasives.String.t ->
  (unit,
   [> `Database of
        [> `Act of Trakeva.Action.t | `Load of string ] * string
   | `Database_unavailable of string ])
    Deferred_result.t
(** Kill a target *)

val restart_target: t -> Ketrew_target.id -> 
  (Ketrew_target.id, 
   [> `Database of Trakeva.Error.t
   | `Database_unavailable of Ketrew_target.id
   | `Missing_data of Ketrew_target.id
   | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** Make new activated targets out of a given target and its “transitive
    reverse dependencies” *)


module Measure: sig

  val incomming_request:
    t ->
    connection_id:string ->
    request:Cohttp.Request.t ->
    unit
  val end_of_request:
    t ->
    connection_id:string ->
    request:Cohttp.Request.t ->
    response_log: string ->
    body_length: int ->
    unit
  val tag: t -> string -> unit
end
module Measurements: sig

  val flush: t ->
    (unit, [>
        | `Database of Trakeva.Error.t
        | `Database_unavailable of Ketrew_target.id
      ]) Deferred_result.t

  val get_all: t ->
    (Ketrew_measurement.Collection.t,
     [> `Database of Trakeva.Error.t
     | `Deserialization of exn * string
     | `Missing_data of string]) Deferred_result.t

end
