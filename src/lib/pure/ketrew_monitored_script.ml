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
module Path = Ketrew_path

type t = Ketrew_gen_base_v0.Monitored_script.t =
  {playground: Path.t; program: Ketrew_program.t}

let create ~playground program = {playground; program}

let log_file t =
  Path.(concat t.playground (relative_file_exn "log"))

let pid_file t =
  Path.(concat t.playground (relative_file_exn "pid"))

let to_string ?(write_pid=true) t  =
  let cmds = Ketrew_program.to_shell_commands t.program in
  let log = log_file t |> Path.to_string in
  let date = "date -u +'%F %T'" in
  let backquoted s = fmt "`%s`" s in
  let to_log format =
    Printf.ksprintf (fun s -> fmt "echo \"%s\" >> %s" s log) format in
  let tagged_log tag fmt =
    Printf.ksprintf (to_log "%s\t%s\t%s" tag (backquoted date)) fmt in
  let return_variable index = fmt "return_of_%04d" index in
  let escape_for_echo cmd =
    let b = Buffer.create 42 in
    String.iter cmd (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
      | ' ' | '_' | '@' | '/' | '-' as c ->
        Buffer.add_char b c
      | other ->
        Buffer.add_string b "_");
    Buffer.contents b in
  let monitor index cmd =
    let ret_v = return_variable index in
    [ tagged_log "before-cmd" "CMD%04d\t%s" index (escape_for_echo cmd);
      fmt "%s" cmd;
      fmt "%s=$?" ret_v;
      tagged_log "after-cmd" "CMD%04d\treturned $%s" index ret_v;
      fmt "if [ $%s -ne 0 ] ; then" ret_v;
      tagged_log "failure" "CMD%04d\treturned $%s" index ret_v;
      fmt "exit $%s" ret_v;
      "fi";
    ] in
  let script =
    [ fmt "mkdir -p %s" (Path.to_string t.playground); ]
    @ (if write_pid then
         [fmt "echo \"$$\" > %s" (pid_file t |> Path.to_string)]
       else
         [])
    @ [ tagged_log "start" "" ]
    @ (List.mapi cmds monitor |> List.concat)
    @ [tagged_log "success" ""]
  in
  script |> String.concat ~sep:"\n"

let parse_log log =
  let lines = String.split ~on:(`Character '\n') log in
  let table =
    List.filter_map lines ~f:(fun line ->
        let cells =
          String.split ~on:(`Character '\t') line
          |> List.map ~f:(String.strip)
          |> List.filter ~f:((<>) "")
        in
        match cells with
        | [] | [""] -> None
        | more ->
          let translated =
            match more with
            | "start" :: date :: [] -> `Start date
            | "before-cmd" :: date :: label :: cmd :: [] ->
              `Before (date, label, cmd)
            | "after-cmd" :: date :: label :: ret :: [] ->
              `After (date, label, ret)
            | "success" :: date :: [] -> `Success date
            | "failure" :: date :: label :: ret :: [] ->
              `Failure (date, label, ret)
            | other -> `Error other
          in
          Some translated)
  in
  table
