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

(** File-path handling *)

type t = Ketrew_gen_base_v0.Path.t
(** General type of file-paths.  *)

val file : string -> t
(** Create a path to a file. *)

val directory : string -> t
(** Create a path to a directory. *)

val root: t
(** The root directory (i.e. ["/"] on Unix). *)

val absolute_file_exn : string -> t
(** Create an absolute path to a file, raises [Invalid_argument _] if the path
    is not absolute. *)

val absolute_directory_exn : string -> t
(** Create an absolute path to a directory, raises [Invalid_argument _] if the
    path is not absolute. *)

val relative_directory_exn : string -> t
(** Create a relative  path to a directory, raises [Invalid_argument _] if the
    path is not relative. *)

val relative_file_exn : string -> t
(** Create a relative to a file, raises [Invalid_argument _] if the path
    is not relative. *)

val concat : t -> t -> t
(** Safely concatenate two paths (calls [Filename.concat]). *)

val to_string : t -> string
(** Convert the path to a “Unix” path. *)

val to_string_quoted : t -> string
(** Convert the path to a “Unix” path quoted for a shell command (c.f. [Filename.quoted]). *)

val exists_shell_condition: t -> string
(** Create a ["/bin/sh"] command that checks if the file or directory exists. *)

val size_shell_command: t -> string
(** Create a ["/bin/sh"] command that outputs ["0"] for directories and
    their size for files. *)
