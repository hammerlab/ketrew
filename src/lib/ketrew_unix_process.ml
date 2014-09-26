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

(** Manage external processes. *)

open Ketrew_pervasives

module Exit_code = struct
  type t = [
    | `Exited of int
    | `Signaled of int
    | `Stopped of int
  ]
  let to_string = function
  | `Exited n ->   fmt "exited:%d" n
  | `Signaled n -> fmt "signaled:%d" n
  | `Stopped n ->  fmt "stopped:%d" n
  let to_log exit_status = Log.s (to_string exit_status)

end

let exec ?(bin="") argl =
  let command = (bin, Array.of_list argl) in
  let process = Lwt_process.open_process_full command in
  wrap_deferred ~on_exn:(fun e ->
      Log.(s "Tarminating process: " % parens (
          quote bin % s ", " % OCaml.list quote argl) @ verbose);
      process#terminate; 
      Lwt.ignore_result process#close; 
      `Process (`Exec (bin, argl), `Exn e))
    Lwt.(fun () ->
        Lwt_list.map_p Lwt_io.read
          [process#stdout; process#stderr; ]
        >>= fun output_2list ->
        process#close >>= fun status ->
        return (status, output_2list))
  >>= fun (ret, output_2list) ->
  let code =
    match ret with
    | Lwt_unix.WEXITED n ->   (`Exited n)
    | Lwt_unix.WSIGNALED n -> (`Signaled n)
    | Lwt_unix.WSTOPPED n ->  (`Stopped n)
  in
  begin match output_2list with
  | [out; err] -> return (out, err, code)
  | _ -> assert false
  end

let succeed ?(bin="") argl =
  exec ~bin argl
  >>= fun (out, err, status) ->
  let failure fmt =
    ksprintf (fun s -> fail (`Process (`Exec (bin, argl), `Non_zero s)))
      fmt in
  begin match status with
  | `Exited 0 -> return  (out, err)
  | code -> failure "%s" (Exit_code.to_string code)
  end

let error_to_string = function
| `Process (`Exec (bin, cmd), how) ->
  fmt "Executing %S[%s]: %s"
    bin (String.concat ~sep:", " (List.map cmd ~f:(fmt "%S")))
    (match how with
     | `Exn e -> fmt "Exn %s" @@ Printexc.to_string e
     | `Non_zero s -> fmt "Non-zero: %s" s)
