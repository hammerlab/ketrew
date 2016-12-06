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

(** Keyboard interaction functions (build “menus”, ask questions, etc.) *)


open Ketrew_pure.Internal_pervasives
open Unix_io


val init : unit -> unit
(** Initialize the module. *)

val toggle_verbose : unit -> unit
(** Turn on or off messages about which key is pressed. *)

type +'a menu_item
(** The type of a menu item. *)

val menu_item : ?char:char -> ?log:SmartPrint.t -> 'a -> 'a menu_item
(** Represent a menu item. *)

val menu : ?max_per_page:int ->
           ?always_there:'a menu_item list ->
           sentence:SmartPrint.t ->
           'a menu_item list ->
             ('a, [> `Failure of string ]) t
(** Display a menu given the specified [menu_items] *)

val open_in_dollar_editor : string -> (unit, 'a) Deferred_result.t
(** Open a file in ["$EDITOR"]. *)

val view_in_dollar_editor : ?extension:string -> string ->
  (unit, [> `IO of [> `Write_file_exn of string * exn ] ]) Deferred_result.t
(** View a string in ["$EDITOR"]. *)

val ask_for_edition : ?extension:string -> string ->
  (string,
   [> `IO of
        [> `Read_file_exn of IO.path * exn
        | `Write_file_exn of IO.path * exn ] ])
    Deferred_result.t
(** Edit content in ["$EDITOR"]. *)

val get_key : unit -> (char, [> `Failure of string ]) t
(** Get a key from the terminal input. *)

val build_sublist_of_targets :
  client: Client.t ->
  list_name:string ->
  all_log:SmartPrint.t ->
  go_verb:SmartPrint.t ->
  filter:Ketrew_pure.Protocol.Up_message.filter ->
  ([> `Cancel | `Go of string list ],
   [> `Client of Client.Error.t
   | `Database of Persistent_data.Error.database
   | `Failure of string
   | `IO of [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
   | `System of [> `File_info of string ] * [> `Exn of exn ]]) t
(** Figure out the targets to be displayed. *)

val make_target_menu : targets:Ketrew_pure.Target.t list ->
    ?filter_target:(Ketrew_pure.Target.t -> bool) ->
    unit ->
      ([> `Go of string ] menu_item) list
(** Create a menu with the targets. *)

val run_with_quit_key :
  < start : (unit, [> `Failure of string ] as 'start_error) Deferred_result.t;
    stop : unit > ->
  (unit, 'start_error) Deferred_result.t
(** Start and run an action until it finishes or unitl the key
    ["q"] is pressed. *)

val read_password_exn : unit -> string
(** Read a line form [stdin] without echoing to the terminal. *)
