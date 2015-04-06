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

(** [Pervasives] module for the library. *)

module Result = Pvem.Result
module String = struct
  include Sosa.Native_string
end

module Array = Nonstd.Array
module Option = Nonstd.Option
module Int = Nonstd.Int
module Float = Nonstd.Float
module List = struct
  (** This is a hack around
      https://github.com/whitequark/ppx_deriving/issues/28 *)
  include Nonstd.List
  let map ~f l = map ~f l
end

let (//) = Filename.concat
(** A very standard operator. *)

let printf = `No
(** We disable [printf]. *)

let sprintf = `No
(** We disable [sprintf], renamed as: *)

let fmt = Printf.sprintf
(** The only function dealing with “formats.” *)

let global_debug_level = ref 2
(** Global reference. *)

let global_with_color = ref true
(** Global reference. *)

let global_log_print_string = ref (Printf.printf "%s%!")

(** Application of the functor [Docout.Make_logger] to write to [stderr]
    without buffering. *)
module Log =  struct
  include Docout.Make_logger (struct
    type ('a, 'b) result = 'a
    let debug_level () = !global_debug_level
    let with_color () = !global_with_color
    let line_width = 72
    let indent = 4
    let print_string s =
      !global_log_print_string s 
    let do_nothing () = ()
    let name = "ketrew"
  end)
  let a f x = s (f x)
  let to_long_string = to_string ~line_width:max_int ~indent:4
  let if_color color log = 
    if !global_with_color then color log else log
  let nbsp = s " "
  let quote = sf "%S"
  let sexp sexp_of_t t = s (sexp_of_t t |> Sexplib.Sexp.to_string)
  let uri theuri = s (Uri.to_string theuri)
end

module Json = struct
  type t = Yojson.Safe.json
  let to_string t = Yojson.Safe.pretty_to_string ~std:true t
  let log t = 
    let str = to_string t in
    Log.(indent (s str))

  module Versioned = struct

    module type WITH_VERSIONED_SERIALIZATION = sig
      type t
      val to_json : t -> Yojson.Safe.json
      val of_json_exn : Yojson.Safe.json -> t
      val serialize : t -> string
      val deserialize_exn : string -> t
    end
    module Of_v0 (T: sig
        type t
        val to_yojson : t -> Yojson.Safe.json
        val of_yojson : Yojson.Safe.json -> [ `Error of bytes | `Ok of t ]
      end) : WITH_VERSIONED_SERIALIZATION with type t := T.t = struct
      type 'a versioned = V0 of 'a [@@deriving yojson]
      let to_json t =
        versioned_to_yojson T.to_yojson (V0 t)
      let serialize t =
        to_json t |> Yojson.Safe.pretty_to_string ~std:true
      let of_json_exn json : T.t =
        match versioned_of_yojson T.of_yojson json with
        | `Ok (V0 t) -> t
        | `Error str ->
          failwith (fmt "deserialization error: %s" str)

      let deserialize_exn s =
        Yojson.Safe.from_string s |> of_json_exn

    end
  end
end

(** Function that have a documented, easy to check contract, can raise
    [Invalid_argument _] (their name should end in [_exn]). *)
let invalid_argument_exn ?(where="pervasives") what =
  raise (Invalid_argument (fmt "[%S]: %s" where what))

(** Handle timestamps. *)
module Time = struct
  type t = float
    [@@deriving yojson]

  let now () : t = Unix.gettimeofday ()

  let to_filename f =
    let open Unix in
    let tm = gmtime f in
    fmt "%04d-%02d-%02d-%02dh%02dm%02ds%03dms-UTC"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      (tm.tm_mday)
      (tm.tm_hour + 1)
      (tm.tm_min + 1)
      (tm.tm_sec)
      ((f -. (floor f)) *. 1000. |> int_of_float)

  let log f = Log.s (to_filename f)
end


(** Provide pseudo-unique identifiers. *)
module Unique_id = struct

  type t = string [@@deriving yojson]

  (** Create a fresh filename-compliant identifier. *)
  let create () =
    fmt "ketrew_%s_%09d"
      Time.(now () |> to_filename) (Random.int 1_000_000_000)
end

                  
