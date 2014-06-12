(** [Pervasives] module for the library. *)

include Nonstd
module Result = Pvem.Result
include  Pvem_lwt_unix
include  Pvem_lwt_unix.Deferred_result
module String = struct
  include Sosa.Native_string
end

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

(** Application of the functor [Docout.Make_logger] to write to [stderr]
    without buffering. *)
module Log = 
  Docout.Make_logger (struct
    type ('a, 'b) result = 'a
    let debug_level () = !global_debug_level
    let with_color () = !global_with_color
    let line_width = 72
    let indent = 4
    let print_string = Printf.eprintf "%s%!"
    let do_nothing () = ()
    let name = "ketrew"
  end)

(** Function that have a documented, easy to check contract, can raise
    [Invalid_argument _] (their name should end in [_exn]). *)
let invalid_argument_exn ?(where="pervasives") what =
  raise (Invalid_argument (fmt "[%S]: %s" where what))

(** Handle timestamps. *)
module Time = struct
  include Ketrew_gen_base_v0_t
  type t = time

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
end


(** Provide pseudo-unique identifiers. *)
module Unique_id = struct

  include Ketrew_gen_base_v0_t
  type t = unique_id
  (** [string] seems to be the best-suited primitive *)

  (** Create a fresh filename-compliant identifier. *)
  let create () =
    fmt "ketrew_%s_%09d"
      Time.(now () |> to_filename) (Random.int 1_000_000_000)
end

(** Deal with error values common accros the library. *)
module Error = struct

  let to_string = function
  | `Wrong_command_line sl ->
    fmt "Wrong command line: %s" 
      (String.concat ~sep:", " (List.map sl (fmt "%S")))
  | `IO _ as io -> IO.error_to_string io
  | `System _ as s -> System.error_to_string s
  | `Database (`Load, path) -> fmt "DB-load: %S" path
  | `Host (`Execution (one, two, three, four)) ->
    fmt "Host-exec(%s, %s, %s, %s)" one two three four
  | `Persistent_state (`Deserilization s) ->
    fmt "Persistent_state-Deserilization: %S" s
  | `Target (`Deserilization s) -> fmt "target-deserialization: %s" s
  | `Database_unavailable s -> fmt "DB %s" s
  | `Not_implemented s -> fmt "Not-impl %S" s
  | `Missing_data p -> fmt "missing data at id: %s" p
  | `Failed_to_kill msg -> fmt "Failed to kill target: %S" msg
  | `Long_running_failed_to_start (id, msg) ->
    fmt "Long running %s failed to start: %s" id msg
  | `Failure msg -> fmt "Failure: %S" msg

end
