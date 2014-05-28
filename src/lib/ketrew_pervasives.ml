
include Nonstd
module Result = Pvem.Result
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


