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

open Ketrew_pervasives

(** Artifacts are inputs such as values or files, or outputs such as the
    results of computations. *)

(** Module defining “Volumes” which are definitions of a given file structure
    on a given {!Ketrew_host.t}. *)
module Volume : sig

    type structure = Ketrew_gen_base_v0.Volume_structure.t
    (** The structure of a volume defines the hierarchy of files relative to
        a [root] path, one constructs structures with {!file} and {!dir}:
        {[
          let src_dir_structure =
            dir "src" [
              dir "lib" [
                file "ketrew.ml";
                file "ketrew_artifact.ml";
                file "ketrew_artifact.mli";
                (* ... *)
              ];
              dir "test" [
                file "main.ml";
              ];
            ]
          ]}
    
    *)

    val file : string -> structure
    (** Define a “file” structure. *)

    val dir : string -> structure list -> structure
    (** Define a “directory” structure. *)

    type t = Ketrew_gen_base_v0.Volume.t
    (** The container of volumes. *)

    val create : host:Ketrew_host.t -> root:Ketrew_path.t -> structure -> t
    (** Create a volume. Example {[
          let sources =
            Volume.create ~host:deployment_server ~root:git_repository
              src_dir_structure ]}
    *)

    val all_paths : t -> Ketrew_path.t list
    (** Get all the paths of the given Volume (files and directories). *) 

    val exists :
      t ->
      (bool, [> `Host of _ Ketrew_host.Error.execution ]) Deferred_result.t
    (** Check whether the whole structure of the Volume exists on the host. *)

    val get_size: t ->
      (int, [> `Host of _ Ketrew_host.Error.non_zero_execution 
            | `Volume of [> `No_size of Log.t]]) Deferred_result.t
    (** Get the total size of a volume in bytes. Returns 0 if the volume does
        not exist (actually calls {!exists}). *)

    val to_string_hum : t -> string
    (** Get a Human-readable string. *)

    val log : t -> Log.t
    (** Get a Human-readable document. *)

  end

