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

include Nonstd
module Result = Pvem.Result
include  Pvem_lwt_unix
include  Pvem_lwt_unix.Deferred_result
module String = struct
  include Sosa.Native_string
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
  type t = Yojson.Basic.json
  let to_string t = Yojson.Basic.pretty_to_string ~std:true t
  let log t = 
    let str = to_string t in
    Log.(indent (s str))

  module Make_versioned_serialization
      (T : sig
         type t
       end) 
      (T_VERSIONED : sig
         type t = [`V0 of T.t]
         val source: unit -> t CConv.Source.t
         val sink: unit -> t CConv.Sink.t
       end) 
  = struct

    let to_json (t : T.t) =
      let versioned = `V0 t in
      CConv.into (T_VERSIONED.source ()) CConvYojson.sink versioned

    let of_json_exn (json : t) =
      match CConv.from CConvYojson.source (T_VERSIONED.sink ()) json with
      | `V0 t -> t

    let serialize t =
      Yojson.Basic.pretty_to_string ~std:true (to_json t)

    let deserialize_exn s =
      Yojson.Basic.from_string s |> of_json_exn

  end

end

(** Function that have a documented, easy to check contract, can raise
    [Invalid_argument _] (their name should end in [_exn]). *)
let invalid_argument_exn ?(where="pervasives") what =
  raise (Invalid_argument (fmt "[%S]: %s" where what))

(** Handle timestamps. *)
module Time = struct
  include Ketrew_gen_base_v0.Time

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

  include Ketrew_gen_base_v0.Unique_id

  (** Create a fresh filename-compliant identifier. *)
  let create () =
    fmt "ketrew_%s_%09d"
      Time.(now () |> to_filename) (Random.int 1_000_000_000)
end

let wrap_preemptively ~on_exn f =
  wrap_deferred (fun () -> 
      Lwt_preemptive.detach f ())
    ~on_exn

let lwt_stream_to_string lwt_stream =
  let buf = Buffer.create 42 in
  wrap_deferred ~on_exn:(fun e -> `Failure (Printexc.to_string e))
    Lwt.(fun () ->
        Lwt_stream.iter_s 
          (fun s -> Buffer.add_string buf s; return ()) lwt_stream)
  >>= fun () ->
  return (Buffer.contents buf)

