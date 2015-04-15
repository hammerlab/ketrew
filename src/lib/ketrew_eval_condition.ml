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

module Host = Ketrew_host

module Program = Ketrew_program

type id = Unique_id.t

module Command = struct

  open Ketrew_target.Command

  let get_output {host; action} =
    let cmd = Program.to_single_shell_command action in
    Ketrew_host_io.get_shell_command_output host cmd

  let get_return_value {host; action} =
    let cmd = Program.to_single_shell_command action in
    Ketrew_host_io.get_shell_command_return_value host cmd

  let run t =
    get_output t (* TODO optimize to not record the output *)
    >>= fun (_, _) ->
    return ()


end


module Volume = struct

  open Ketrew_target.Volume

  let exists t =
    let paths = all_paths t in
    Ketrew_host_io.do_files_exist t.host paths

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
            Ketrew_host_io.get_shell_command_output t.host cmd
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




open Ketrew_target.Condition

let rec eval = 
  function
  | `Satisfied -> return true
  | `Never -> return false
  | `Volume_exists v -> Volume.exists v
  | `Volume_size_bigger_than (v, sz) ->
    Volume.get_size v
    >>= fun size ->
    return (size >= sz)
  | `Command_returns (c, ret) ->
    Command.get_return_value c  
    >>= fun return_value ->
    return (ret = return_value)
  | `And list_of_conditions -> 
    (* Should start at the first that returns `false` *)
    let rec go = function
    | [] -> return true
    | cond :: rest ->
      eval cond
      >>= function
      | true -> go rest
      | false -> return false
    in
    go list_of_conditions

let bool = eval
  

(*

let should_start t =
  match Ketrew_target.condition t with
  | Some c -> Condition.eval c >>| not
  | None -> return true

let did_ensure_condition t =
  match Ketrew_target.condition t with
  | Some c -> Condition.eval c
  | None -> return true


*)
