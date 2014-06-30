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

(** Artifacts are input values or files, or results of computations. *)

(** Module defining “Volumes” which are definitions of a given file structure
    on a given {!Ketrew_host.t}. *)
module Volume : sig

    type structure = Ketrew_gen_base_v0_t.volume_structure
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

    type t = Ketrew_gen_base_v0_t.volume
    (** The container of volumes. *)

    val create :
      host:Ketrew_host.t -> root:Ketrew_path.absolute_directory -> structure ->
      t
    (** Create a volume. Example {[
          let sources =
            Volume.create ~host:deployment_server ~root:git_repository
              src_dir_structure ]}
    *)

    val all_paths : t -> <relativity: Ketrew_path.absolute; ..> Ketrew_path.t list
    (** Get all the paths of the given Volume (files and directories). *) 

    val exists :
      t ->
      (bool, [> `Host of _ Ketrew_host.Error.execution ]) Deferred_result.t
    (** Check whether the whole structure of the Volume exists on the host. *)

    val to_string_hum : t -> string
    (** Get a Human-readable string. *)

    val log : t -> Log.t
    (** Get a Human-readable document. *)

  end

module Value : sig
  type t = [ `Number of float | `String of string | `Unit ]
      (** Literal values. *)

  val log: t -> Log.t

  val unit : t
  (** Alias for [`Unit]. *)
end


type t = [ `Value of Value.t | `Volume of Volume.t ]
(* Literal artifact, i.e. a literal value, or the specification of a volume
   (used for now also to represent its “value”). *)

val log: t -> Log.t
(** Get a [Log] document. *)
