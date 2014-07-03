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

(** File-path handling with precise phantom-types *)

type relative
(** Abstract phantom type. *)

type absolute
(** Abstract phantom type. *)

type file
(** Abstract phantom type. *)

type directory
(** Abstract phantom type. *)

type 'a t = Ketrew_gen_base_v0_t.path
  constraint 'a = < kind : 'file_kind; relativity : 'relativity >
(**
   General type of file-paths, [kind] may be [file] or [directory],
   [relativity] may be [relative] or [absolute].
*)

type absolute_directory = < kind : directory; relativity : absolute > t
(** Readable alias. *) 

type absolute_file = < kind : file; relativity : absolute > t
(** Readable alias. *) 

type relative_directory = < kind : directory; relativity : relative > t
(** Readable alias. *) 

type relative_file = < kind : file; relativity : relative > t
(** Readable alias. *) 

val file : string -> < kind : file; relativity : 'any > t
(** Create a path to a file. *)

val directory : string -> < kind : directory; relativity : 'any > t
(** Create a path to a directory. *)

val root : < kind : directory; relativity : absolute > t
(** The root directory (i.e. ["/"] on Unix). *)

val absolute_file_exn : string -> < kind : file; relativity : absolute > t
(** Create an absolute path to a file, raises [Invalid_argument _] if the path
    is not absolute. *)

val absolute_directory_exn :
  string -> < kind : directory; relativity : absolute > t
(** Create an absolute path to a directory, raises [Invalid_argument _] if the
    path is not absolute. *)

val relative_directory_exn :
  string -> < kind : directory; relativity : relative > t
(** Create a relative  path to a directory, raises [Invalid_argument _] if the
    path is not relative. *)

val relative_file_exn : string -> < kind : file; relativity : relative > t
(** Create a relative to a file, raises [Invalid_argument _] if the path
    is not relative. *)

val concat :
  < kind : directory; relativity : 'a > t ->
  < kind : 'b; relativity : relative > t -> < kind : 'b; relativity : 'a > t
(** Safely concatenate two paths (calls [Filename.concat]). *)

val to_string : < kind : 'a; relativity : 'b > t -> string
(** Convert the path to a “Unix” path. *)

val to_string_quoted : < kind : 'a; relativity : 'b > t -> string
(** Convert the path to a “Unix” path quoted for a shell command (c.f. [Filename.quoted]). *)

val any_kind :
  < kind : 'some_kind; relativity : 'relativity > t -> 
  < kind : 'any_kind;  relativity : 'relativity > t
(** Coerce a path to any kind of path (file or directory). *)
     
val exists_shell_condition:
  < kind : 'any_kind; relativity : 'any_relativity > t -> string
(** Create a ["/bin/sh"] command that checks if the file or directory exists. *)

val size_shell_command:
  < kind : 'any_kind; relativity : 'any_relativity > t -> string
(** Create a ["/bin/sh"] command that outputs ["0"] for directories and
    their size for files. *)
