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
(** Keyboard interaction functions (build “menus”, ask questions, etc.) *)

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
  client:[< `Http_client of Ketrew_client.Http_client.t
         | `Standalone of Ketrew_client.Standalone.t ] ->
  list_name:string ->
  all_log:SmartPrint.t ->
  go_verb:SmartPrint.t ->
  filter:(Ketrew_target.t -> bool) ->
    ([> `Cancel | `Go of string list ],
     [> `Client of
          [> `Http of
               [> `Call of [> `GET | `POST ] * Uri.t | `Targets ] *
               [> `Exn of exn
                | `Json_parsing of string * [> `Exn of exn ]
                | `Unexpected_message of Ketrew_protocol.Down_message.t
                | `Wrong_json of Ketrew_pervasives.Json.t
                | `Wrong_response of Cohttp.Response.t * string ]
           | `Server_error_response of [> `Call of [> `GET | `POST ] * Uri.t ] * string ]
      | `Database of Trakeva.Error.t
      | `Failure of string
      | `IO of [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
      | `Missing_data of string
      | `Persistent_state of [> `Deserilization of string ]
      | `System of [> `File_info of string ] * [> `Exn of exn ]
      | `Target of [> `Deserilization of string ] ]) t
(** Figure out the targets to be displayed. *)

val make_target_menu : targets:Ketrew_target.t list ->
    ?filter_target:(Ketrew_target.t -> bool) ->
    unit ->
      ([> `Go of string ] menu_item) list
(** Create a menu with the targets. *)
