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

(**
   Measurements are values that the Engine will keep about its
   performance; those can be “flushed” to the database, and exported
   later.
*)

open Ketrew_pervasives


type response_log
type http_request
type measurement

(** This module allows to construct and transform measurmeent items. *)
module Item: sig

  type t

  val create : measurement -> t

  val make_http_request :
    string -> Cohttp.Request.t -> http_request

  val make_reponse_log : string -> int -> response_log

  val incoming_request: http_request -> t
  val end_of_request: http_request -> response_log -> t
  val tag: string -> t

  val time : t -> Time.t
  (** Get the date recorded for the item. *)

  val compare_by_time : t -> t -> int
  (** Comparison function that can be given to functions like [List.sort]. *)

  val to_strings : t -> string list
  (** Convert an item to a list of displayable functions. *)

  val collect_http_requests :
    t list ->
    < body_length : int; date : Time.t; duration : float; uri : string > list
    (** Go through a list of Items and find pairs of incoming/ending HTTP requests,
        return statistics about them. *)

end

(** A mutable collection of {!Item.t} values. *)
module Collection: sig

  type t

  val serialize : t -> string
  val deserialize_exn : string -> t

  val create : unit -> t

  val add : t -> Item.t -> unit

  val clear : t -> unit

  val concat : t list -> t

  val to_list : t -> Item.t list

end
