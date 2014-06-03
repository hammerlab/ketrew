
include Nonstd
module Result = Pvem.Result
include  Pvem_lwt_unix
include  Pvem_lwt_unix.Deferred_result
module String = struct
  include Sosa.Native_string
end
let printf = `No
let sprintf = `No
let fmt = Printf.sprintf


let global_debug_level = ref 2
let global_with_color = ref true
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

let failwithf fmt =
  ksprintf (fun str ->
      Log.(s "Failing: " % s str @ error);
      failwith str
    ) fmt

let invalid_argument_exn ?(where="pervasives") what =
  raise (Invalid_argument (fmt "[%S]: %s" where what))

module Time = struct
  type t = float
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


module Unique_id = struct
  (** Provide pseudo-unique identifiers. *)

  type t = string
  (** [string] seems to be the best-suited primitive *)

  (** Create a fresh filename-compliant identifier. *)
  let create () =
    fmt "ketrew_%s_%09d"
      Time.(now () |> to_filename) (Random.int 1_000_000_000)
end

module Error = struct

  let to_string = function
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
  | `Long_running_failed_to_start (id, msg) ->
    fmt "Long running %s failed to start: %s" id msg

end
