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

open Ketrew_pure
open Internal_pervasives
open Unix_io

type t

module Error : sig
  type fetching_node = [
    | `Get_stored_target
    | `Pointer_loop_max_depth of int
    | `Target_to_add 
  ] * [ `Id of string ]

  type database =
    [
      | `Exec of string * [ `Blob of string | `Null ] array
      | `Load of string
      | `Parsing of string
      | `Close
    ]
    * [ `Exn of string ]

  val database_to_string: database -> string
end

val create :
  database_parameters:string ->
  (t,
   [> `Database of Error.database
   | `Database_unavailable of string
   | `Fetching_node of Error.fetching_node
   | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** Connect to the Postgresql database and initialize it (if needed). *) 

val unload: t ->
  (unit, [> `Database of Error.database]) Deferred_result.t
(** Close the connection to the DB. *)

val get_target:
  t ->
  Target.id ->
  (Ketrew_pure.Target.t,
   [> `Database of Error.database
   | `Database_unavailable of string
   | `Fetching_node of Error.fetching_node
   | `Target of [> `Deserilization of string ] ]) Deferred_result.t
(** Get the node at a given ID, the function actually “follows
    pointers” i.e.  it gets the node considered by Ketrew after
    equivalence matching.  *)

val all_visible_targets :
  t ->
  (Ketrew_pure.Target.t list,
   [>  `Database of Error.database
   | `Database_unavailable of string
   | `Fetching_node of Error.fetching_node
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t


val activate_target :
  t ->
  target:Target.t ->
  reason:[ `Dependency of Target.id | `User ] ->
  (unit,
   [> `Database of Error.database
   | `Database_unavailable of string
   | `Fetching_node of Error.fetching_node
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** Register a node as “Active” (i.e. to be picked-up by the engine at
    the next step, using {!fold_active_targets}). *)

val fold_active_targets :
  t ->
  init:'a ->
  f:('a ->
     target:Target.t ->
     ('a,
      [> `Database of Error.database
      | `Fetching_node of Error.fetching_node
      | `Target of [> `Deserilization of string ] ]
      as 'combined_errors)
       Deferred_result.t) ->
  ('a, 'combined_errors) Deferred_result.t
(** Go through all the active nodes. *)

val update_target :
  t ->
  Target.t ->
  (unit,
   [> `Database of Error.database
   | `Database_unavailable of string ])
    Deferred_result.t

val find_all_orphans: 
  t ->
  (Ketrew_pure.Target.t list,
   [> `Database of Error.database
   | `Database_unavailable of string
   | `Fetching_node of Error.fetching_node
   | `Target of [> `Deserilization of string ] ])
    Deferred_result.t
(** [find_all_orphans] goes through the cache and returns all the targets that
    are passive but not reachable, i.e. that can't be activated, ever. *)

module Change : sig
  type t = [ `Started | `New_nodes of string list | `Nodes_changed of string list ]
    [@@deriving show]
end
val next_changes: t -> (Change.t list, 'a) Deferred_result.t

module Killing_targets: sig

  val proceed_to_mass_killing :
    t ->
    (bool,
     [> `Database of Error.database
     | `Database_unavailable of string
     | `Fetching_node of Error.fetching_node
     | `Target of [> `Deserilization of string ] ])
      Deferred_result.t
  val add_target_ids_to_kill_list :
    t ->
    string list ->
    (unit,
     [> `Database of Error.database
     | `Database_unavailable of string ])
      Deferred_result.t
end

module Adding_targets: sig
  val register_targets_to_add :
    t ->
    Target.t list ->
    (unit,
     [> `Database of Error.database
     | `Database_unavailable of string ])
      Deferred_result.t
  val check_and_really_add_targets :
    t ->
    (bool,
     [> `Database of Error.database
     | `Database_unavailable of string
     | `Fetching_node of Error.fetching_node
     | `Target of [> `Deserilization of string ] ])
      Deferred_result.t

  (** Bypass the normal flow of target addition and put a target in the DB. *)
  val force_add_passive_target: t ->
    Ketrew_pure.Target.Stored_target.target ->
    (unit,
     [> `Database of Error.database
     | `Database_unavailable of string ]) Deferred_result.t

end

module Synchronize: sig
  val copy :
    string ->
    string ->
    (unit,
     [> `Syncronize of
          string * string *
          [> `Database of Error.database
          | `IO of
               [> `Read_file_exn of string * exn
               | `Write_file_exn of string * exn ]
          | `System of
               [> `File_info of string
               | `List_directory of string
               | `Make_directory of string ] *
               [> `Exn of exn | `Wrong_access_rights of int ]
          | `Target of [> `Deserilization of string ]
          | `Unknown_uri_scheme of string * string option
          | `Weird_file of string ] ])
      Deferred_result.t
  module Error: sig
    val to_string :
      string * string *
      [< `Database of Error.database
      | `IO of
           [< `Exn of exn
           | `File_exists of string
           | `Read_file_exn of string * exn
           | `Write_file_exn of string * exn
           | `Wrong_path of string ]
      | `Target of [< `Deserilization of string ]
      | `System of
           [< `Copy of string
           | `File_info of string
           | `File_tree of string
           | `List_directory of string
           | `Make_directory of string
           | `Make_symlink of string * string
           | `Move of string
           | `Remove of string ] *
           [< `Already_exists
           | `Exn of exn
           | `File_exists of string
           | `File_not_found of string
           | `IO of
               [< `Exn of exn
               | `File_exists of string
               | `Read_file_exn of string * exn
               | `Write_file_exn of string * exn
               | `Wrong_path of string ]
           | `Not_a_directory of string
           | `Wrong_access_rights of int
           | `Wrong_file_kind of
               string * Pvem_lwt_unix.System.file_info
           | `Wrong_path of string ]
      | `Unknown_uri_scheme of string * string option
      | `Weird_file of string ] ->
      string
  end
end
