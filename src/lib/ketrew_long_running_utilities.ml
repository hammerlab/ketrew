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

open Ketrew_pervasives
open Ketrew_unix_io

module Path = Ketrew_path
module Program = Ketrew_program
module Error = Ketrew_error

let fail_fatal msg =
  fail (`Fatal msg)
let out_file_path ~playground =
  Path.(concat playground (relative_file_exn "out"))
let err_file_path ~playground =
  Path.(concat playground (relative_file_exn "err"))
let script_path ~playground =
  Path.(concat playground (relative_file_exn "monitored_script"))


let classify_and_transform_errors :
  ('a, _) Result.t ->
  ('a, [`Fatal of string | `Recoverable of string ]) Deferred_result.t =
  function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Fatal _ as e -> fail e
    | `Host he as e ->
      begin match Ketrew_host_io.Error.classify he with
      | `Ssh | `Unix -> fail (`Recoverable (Error.to_string e))
      | `Execution -> fail_fatal (Error.to_string e)
      end
    | `Timeout _ -> fail (`Recoverable "timeout")
    | `IO _ | `System _ as e ->
      fail_fatal (Error.to_string e)
    end

let fresh_playground_or_fail host =
  begin match Ketrew_host_io.get_fresh_playground host with
  | None ->
    fail_fatal (fmt  "Host %s: Missing playground" (Ketrew_host.to_string_hum host))
  | Some playground -> return playground
  end

let get_log_of_monitored_script ~host ~script =
  let log_file = Ketrew_monitored_script.log_file script in
  begin Ketrew_host_io.get_file host ~path:log_file
    >>< function
    | `Ok c -> return (Some c)
    | `Error (`Cannot_read_file _) -> return None
    | `Error (`Timeout _ as e) -> fail e
  end
  >>= fun log_content ->
  let log = Option.map ~f:Ketrew_monitored_script.parse_log log_content in
  return log

let get_pid_of_monitored_script ~host ~script =
  let pid_file = Ketrew_monitored_script.pid_file script in
  begin Ketrew_host_io.get_file host ~path:pid_file
    >>< function
    | `Ok c ->
      Log.(s "get_pid_of_monitored_script, got: " % quote c @ verbose);
      return (Int.of_string (String.strip ~on:`Both c))
    | `Error (`Cannot_read_file (s1, s2)) ->
      Log.(s "get_pid_of_monitored_script: cannot-read-file: "
             % quote s1 % s ", " % quote s2 @ verbose);
      return None
    | `Error (`Timeout _ as e) -> fail e
  end


let shell_command_output_or_log ~host cmd =
  begin Ketrew_host_io.get_shell_command_output host cmd
    >>< function
    | `Ok (o, _) -> return o
    | `Error e ->
      fail Log.(s "Command " % quote cmd % s " on " % Ketrew_host.log host
                % s " failed: " % s (Ketrew_error.to_string e))
  end
