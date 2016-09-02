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

(** [Pervasives] module for the library. *)

module Result = Pvem.Result
module String = struct
  let legacy_lowercase = String.lowercase
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

let global_executable_path =
  begin match Filename.is_relative Sys.executable_name with
  | false -> Sys.executable_name
  | true -> Filename.concat (Sys.getcwd ()) Sys.executable_name
  end
(** Full path to the executable that is running (makes no sense in the
    javascript backend).

    This will be executed before the server has the chance to
    daemonize (and hence change the current directory).
    
    When called from a shell that uses ["$PATH"],
    [Sys.executable_name] should absolute.
*)

let global_debug_level = ref 0
(** Global reference. *)

let global_with_color = ref true
(** Global reference. *)

let global_log_print_string = ref (Printf.printf "%s%!")


module Uri = struct
  include Uri
  let to_yojson u =
    `String (Uri.to_string u)
  let of_yojson =
    let open Ppx_deriving_yojson_runtime.Result in
    function
    | `String u ->
      begin
        try Ok (Uri.of_string u)
        with e -> Error "Wrong URI string in Yojson input"
      end
    | other -> Error "Wrong JSON for Uri.t"
end

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

  let big_byte_sequence ?(string=quote) ?(max_length=20) str =
    match String.sub str ~index:0 ~length:max_length with
    | None -> string str
    | Some substr ->
      braces (
        string substr % s "…" % nbsp %
        (match String.length str with
        | n when n / 1024 > 10 ->
          i (n / 1024) % nbsp % s "KB"
        | n -> i n % nbsp % s "B")
      )
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
        val of_yojson : Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
      end) : WITH_VERSIONED_SERIALIZATION with type t := T.t = struct
      type 'a versioned = V0 of 'a [@@deriving yojson]
      let to_json t =
        versioned_to_yojson T.to_yojson (V0 t)
      let serialize t =
        to_json t |> Yojson.Safe.pretty_to_string ~std:true
      let of_json_exn json : T.t =
        match versioned_of_yojson T.of_yojson json with
        | Ppx_deriving_yojson_runtime.Result.Ok (V0 t) -> t
        | Ppx_deriving_yojson_runtime.Result.Error str ->
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

  let day = 60. *. 60. *. 24.

  let to_filename f =
    let open Unix in
    let tm = gmtime f in
    fmt "%04d-%02d-%02d-%02dh%02dm%02ds%03dms-UTC"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      (tm.tm_mday)
      (tm.tm_hour)
      (tm.tm_min)
      (tm.tm_sec)
      ((f -. (floor f)) *. 1000. |> int_of_float)

  let to_string_hum f =
    let open Unix in
    let tm = localtime f in
    fmt "%04d-%02d-%02d-%02dh%02dm%02ds%03dms"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      (tm.tm_mday)
      (tm.tm_hour)
      (tm.tm_min)
      (tm.tm_sec)
      ((f -. (floor f)) *. 1000. |> int_of_float)

  let log f = Log.s (to_filename f)

  let show f = to_filename f
  let pp formatter f = Format.fprintf formatter "%s" (show f)
end


(** Provide pseudo-unique identifiers. *)
module Unique_id = struct

  type t = string [@@deriving yojson]

  (** Create a fresh filename-compliant identifier. *)
  let create () =
    fmt "ketrew_%s_%09d"
      Time.(now () |> to_filename) (Random.int 1_000_000_000)
end


module Display_markup = struct

  module AST = struct
    type t =
      | Date of float
      | Time_span of float
      | Text of string
      | Path of string
      | Command of string
      | Code_block of string
      | Uri of string
      | Concat of (t option) * (t list)
      | Description of string * t
      | Itemize of t list
    [@@deriving yojson]
  end
  include Json.Versioned.Of_v0(AST)
  include AST

  let date d = Date d
  let path p = Path p
  let command p = Command p
  let code_block b = Code_block b
  let description name v = Description (name, v)
  let itemize l = Itemize l

  let uri i = Uri i

  let concat ?sep l = Concat (sep, l)
  let flat_list l ~f =
    concat (List.map l ~f:(fun item -> concat [f item]))

  let time_span s = Time_span s

  let text t = Text t
  let textf fmt = Printf.ksprintf text fmt

  let description_list l =
    Itemize (List.map l ~f:(fun (n, t) -> Description (n, t)))

  let text_of_stringable f c = Text (f c)
  let text_of_loggable f c = Text (f c |> Log.to_long_string)

  let option o ~f =
    Option.value_map ~f ~default:(Text "None") o

  let date_now () = date (Time.now ())

  let of_pp pp a =
    ignore (Format.flush_str_formatter ());
    (pp Format.str_formatter a);
    Format.flush_str_formatter () |> code_block

  let big_string ?(max_display = 50) s =
    match String.sub s ~index:0 ~length:max_display with
    | None -> textf "%S" s
    | Some substr -> textf "%S... %d Bytes" substr (String.length s)

  let big_itemize ?(max_display = 10) ~render l =
    match List.length l with
    | small when small <= max_display ->
      itemize (List.map l ~f:render)
    | big ->
      textf "%d items" big

  let rec log =
    function
    | Date f -> Time.log f
    | Time_span span -> Log.(f span % nbsp % s "s.") 
    | Text s -> Log.verbatim s
    | Code_block s -> Log.verbatim s
    | Uri c | Path c | Command c -> Log.quote c
    | Concat (None, l) -> Log.concat (List.map l ~f:log)
    | Concat (Some sep, l) -> Log.separate (log sep) (List.map l ~f:log)
    | Description (name, content) ->
      Log.(s name % s ":" %
           begin match content with
           | Itemize _  -> n % indent (log content)
           | _ -> sp % log content
           end)
    | Itemize l ->
      Log.(separate n (List.map l ~f:(fun content -> s "*" %nbsp % log content)))

end

module Typed_log = struct
  module Item = struct
    type t = {
      date : Time.t;
      content : Display_markup.t;
    } [@@deriving yojson]
    let show_content c =
      Display_markup.log c |> Log.to_long_string
    let show {date; content} =
      fmt "[%s]\n%s" (Time.show date) (show_content content)

    module Constants = struct
      let word_error = "Error"
      let word_info = "Info"
      let word_module = "Module"
    end

    module Construct = struct
      let now content = {date = Time.now (); content }
      let info s = now (Display_markup.text s)
      let error e s =
        let open Display_markup in
        description_list [
          Constants.word_error, text e;
          Constants.word_info, text s;
        ]
        |> now
    end

    module Condition = struct
      type t = [
        | `Field_equals of string * string
        | `Has_field of string
        | `True
        | `False
        | `And of t list
        | `Or of t list
        | `Ignore_case of t
      ]
      let rec eval ?(ignore_case = false) t =
        let continue c = eval ~ignore_case t c in
        let string_eq a b =
          if ignore_case then
            String.legacy_lowercase a = String.legacy_lowercase b
          else
            a = b in
        function
        | `True -> true
        | `False -> false
        | `And al -> List.for_all al ~f:continue
        | `Or ol -> List.exists ol ~f:continue
        | `Ignore_case c -> eval ~ignore_case:true t c
        | `Has_field f ->
          let open Display_markup in
          begin match t.content with
          | Itemize l ->
            List.exists l ~f:(function
              | Description (field_v, _) -> string_eq field_v f
              |  _ -> false)
          | _ -> false
          end
        | `Field_equals (field, value) ->
          let open Display_markup in
          begin match t.content with
          | Itemize l ->
            List.exists l ~f:(function
              | Description (field_v, Text value_v) ->
                string_eq field_v field && string_eq value_v value 
              |  _ -> false)
          | _ -> false
          end
    end
  end
end
