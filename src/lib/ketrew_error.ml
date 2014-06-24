(** Deal with error values common accros the library. *)

open Ketrew_pervasives

let to_string = function
| `Wrong_command_line sl ->
  fmt "Wrong command line: %s" 
    (String.concat ~sep:", " (List.map sl (fmt "%S")))
| `IO _ as io -> IO.error_to_string io
| `System _ as s -> System.error_to_string s
| `Configuration (`Parsing e) ->
  fmt "Parsing error in config-file: %S" e
| `Database _ as dberr ->
  Ketrew_database.log_error dberr |> Log.to_long_string
| `Host e ->
  fmt "Host: %s" (Ketrew_host.Error.log e |> Log.to_long_string)
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
| `Process _ as pe -> Ketrew_unix_process.error_to_string pe
| `Shell _ as se -> System.error_to_string se
