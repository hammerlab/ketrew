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

module Host = Ketrew_host

module Volume = struct

  type structure = Ketrew_gen_base_v0.Volume_structure.t
  type t = Ketrew_gen_base_v0.Volume.t = {
    host: Host.t;
    root: Path.t;
    structure: structure;
  }
  let create ~host ~root structure = {host; root; structure}
  let file s = `File s
  let dir name contents = `Directory (name, contents)

  let rec all_structure_paths = fun s ->
    match s with
    | `File s -> [Path.relative_file_exn s ]
    | `Directory (name, children) ->
      let children_paths = 
        List.concat_map ~f:all_structure_paths children in
      let this_one = Path.relative_directory_exn name in
      this_one :: List.map ~f:(Path.concat this_one) children_paths

  let all_paths t: Path.t list =
    List.map ~f:(Path.concat t.root) (all_structure_paths t.structure)

  let exists t =
    let paths = all_paths t in
    Host.do_files_exist t.host paths

  let log_structure structure = 
    let all_paths = all_structure_paths structure |> List.map ~f:Path.to_string in
    let open Log in
    match all_paths with
    | [] -> s "EMPTY"
    | one :: [] -> s "Single path: " % quote one
    | more -> i (List.length more) % sp % s "paths"

  let log {host; root; structure} =
    Log.(braces (
        parens (Host.log host) % sp
        % parens (s "Root: " % s (Path.to_string root)) % sp
        % parens (s "Tree: " % log_structure structure)
      ))

  let to_string_hum v =
    Log.to_long_string (log v)

  let get_size t =
    let paths = all_paths t in
    (* let cmds = List.map paths ~f:Path.size_shell_command in *)
    begin
      exists t
      >>= function
      | true ->
        Deferred_list.while_sequential paths (fun path ->
            let cmd = Path.size_shell_command path in
            Log.(s "while_sequential : " % quote cmd @ warning);
            Host.get_shell_command_output t.host cmd
            >>= fun (str, _) ->
            match  String.strip str |> Int.of_string with
            | None -> 
              let msg =
                Log.(s "Command " % s cmd % s " did not return an int but "
                     % quote str) in
              fail (`Volume (`No_size msg))
            | Some i -> return i
          )
        >>| List.fold ~init:0 ~f:(+)
      | false -> return 0
    end

end

