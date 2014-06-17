
open Ketrew_pervasives

type t = Ketrew_gen_base_v0_t.program

let rec to_shell_commands = function
| `Shell_command s -> [s]
| `Exec sl -> 
  [fmt "exec %s" (List.map sl ~f:Filename.quote |> String.concat ~sep:" ")]
| `And l -> List.concat_map l ~f:to_shell_commands

let to_single_shell_command t = 
  String.concat ~sep:" && " (to_shell_commands t)

let rec log = function
| `Shell_command str -> Log.(s "Sh:" % brakets (s str))
| `Exec sl -> Log.(s "Exec:" % OCaml.list (sf "%S") sl)
| `And l -> Log.(separate (s " && ") (List.map ~f:log l))

let to_string_hum p = Log.to_long_string (log p)



