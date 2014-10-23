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
module Program = Ketrew_program
module Host = Ketrew_host
module Error = Ketrew_error

open Ketrew_long_running_utilities

open Ketrew_gen_pbs_v0.Run_parameters
open Ketrew_gen_pbs_v0.Running
open Ketrew_gen_pbs_v0.Created

type run_parameters = Ketrew_gen_pbs_v0.Run_parameters.t

include Json.Make_versioned_serialization
    (Ketrew_gen_pbs_v0.Run_parameters)
    (Ketrew_gen_versioned.Pbs_run_parameters)

let name = "PBS"
let create
    ?(host=Host.tmp_on_localhost)
    ?queue ?name ?(wall_limit=`Hours 24.) ?(processors=1) ?(email_user=`Never) 
    ?(shell="/usr/bin/env bash")
    program =
  `Long_running (
    "PBS",
    `Created {host; program; queue; name; 
              email_user; shell; wall_limit; processors}
    |> serialize)

let log =
  let open Log in
  function
  | `Created c -> [
      "Status", s "Created";
      "Host", Host.log c.host;
      "Program", Program.log c.program;
    ]
| `Running rp -> [
      "Status", s "Running";
      "Host", Host.log rp.created.host;
      "Program", Program.log rp.created.program;
      "PSB-ID", s rp.pbs_job_id;
      "Playground", s (Path.to_string rp.playground);
  ]

let start: run_parameters -> (_, _) Deferred_result.t = function
| `Running _ ->
  fail_fatal "Wrong state: already running"
| `Created created ->
  begin
    fresh_playground_or_fail created.host
    >>= fun playground ->
    let script = Ketrew_monitored_script.create ~playground created.program in
    let monitored_script_path = script_path ~playground in
    Host.ensure_directory created.host playground
    >>= fun () ->
    let out = out_file_path ~playground in
    let err = err_file_path ~playground in
    let opt o ~f = Option.value_map ~default:[] o ~f:(fun s -> [f s]) in
    let resource_list =
      match created.wall_limit with
      | `Hours h ->
        let hr = floor (abs_float h) in
        let min = floor ((abs_float h -. hr) *. 60.) in
        fmt "nodes=1:ppn=%d,walltime=%02d:%02d:00"
          created.processors (int_of_float hr) (int_of_float min)
    in
    let content =
      String.concat ~sep:"\n" (List.concat [
          [fmt "#! %s" created.shell];
          begin match created.email_user with
           | `Never -> []
           | `Always email -> [
               fmt "#PBS -m abe";
               fmt "#PBS -M %s" email;
             ]
          end;
          [fmt "#PBS -e %s" (Path.to_string err)];
          [fmt "#PBS -o %s" (Path.to_string out)];
          opt created.name ~f:(fmt "#PBS -N %s");
          opt created.queue ~f:(fmt "#PBS -q %s");
          [fmt "#PBS -l %s" resource_list];
          [Ketrew_monitored_script.to_string script];
        ]) in
    Host.put_file ~content created.host ~path:monitored_script_path
    >>= fun () ->
    let cmd = fmt "qsub %s" (Path.to_string_quoted monitored_script_path) in
    Host.get_shell_command_output created.host cmd
    >>= fun (stdout, stderr) ->
    Log.(s "Cmd: " % s cmd %n % s "Out: " % s stdout %n
         % s "Err: " % s stderr @ verbose);
    let pbs_job_id = String.strip stdout in
    return (`Running { pbs_job_id; playground; script; created})
  end
  >>< classify_and_transform_errors

let additional_queries = function
| `Created _ -> []
| `Running _ ->
  [
    "stdout", Log.(s "PBS output file");
    "stderr", Log.(s "PBS error file");
    "log", Log.(s "Monitored-script `log` file");
    "script", Log.(s "Monitored-script used");
    "qstat", Log.(s "Call `qstat -f1 <ID>`");
  ]

let query run_parameters item =
  match run_parameters with
  | `Created _ -> fail Log.(s "not running")
  | `Running rp ->
    begin match item with
    | "log" ->
      let log_file = Ketrew_monitored_script.log_file rp.script in
      Host.grab_file_or_log rp.created.host log_file
    | "stdout" ->
      let out_file = out_file_path ~playground:rp.playground in
      Host.grab_file_or_log rp.created.host out_file
    | "stderr" ->
      let err_file = err_file_path ~playground:rp.playground in
      Host.grab_file_or_log rp.created.host err_file
    | "script" ->
      let monitored_script_path = script_path ~playground:rp.playground in
      Host.grab_file_or_log rp.created.host monitored_script_path
    | "qstat" ->
      begin Host.get_shell_command_output rp.created.host
          (fmt "qstat -f1 %s" rp.pbs_job_id)
        >>< function
        | `Ok (o, _) -> return o
        | `Error e ->
          fail Log.(s "Command `qstat -f1 <ID>` failed: " % s (Error.to_string e))
      end
    | other -> fail Log.(s "Unknown query: " % sf "%S" other)
    end

let update = function
| `Created _ -> fail_fatal "not running"
| `Running run as run_parameters ->
  begin
    get_log_of_monitored_script ~host:run.created.host ~script:run.script
    >>= fun log_opt ->
    begin match Option.bind log_opt  List.last with
    | Some (`Success date) ->
      return (`Succeeded run_parameters)
    | Some (`Failure (date, label, ret)) ->
      return (`Failed (run_parameters, fmt "%s returned %s" label ret))
    | None | Some _->
      Host.execute run.created.host ["qstat"; "-f1"; run.pbs_job_id]
      >>= fun return_obj ->
      begin match return_obj#exited with
      | 0 ->
        let job_state =
          String.split ~on:(`Character '\n') return_obj#stdout
          |> List.find_map ~f:(fun line ->
              String.split line ~on:(`Character '=') 
              |> List.map ~f:String.strip
              |> (function
                | ["job_state"; state] ->
                  begin match state with
                  | "Q" (* queued *)
                  | "E" (* exiting *)
                  | "H" (* held *)
                  | "T" (* moved *)
                  | "W" (* waiting *)
                  | "S" (* suspended *)
                  | "R" -> Some (state, `Running)
                  | "C" -> Some (state, `Completed)
                  | other -> 
                    Log.(s "Can't understand job_state: " % s other @ warning);
                    None
                  end
                | other -> None)
            )
        in
        begin match job_state with
        | Some (state, `Running) ->
          return (`Still_running run_parameters)
        | Some (state, `Completed) ->
          return (`Failed (run_parameters, fmt "PBS status: %S" state))
        | None ->
          return (`Failed (run_parameters, fmt "PBS status: None"))
        end
      | other ->
        return (`Failed (run_parameters, 
                         fmt "log says not finished; qstat returned %d" other))
      end
    end
  end
  >>< classify_and_transform_errors

let kill run_parameters =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run as run_parameters ->
    begin
      let cmd = fmt "qdel %s" run.pbs_job_id in
      Host.get_shell_command_output run.created.host cmd
      >>= fun (_, _) ->
      return (`Killed run_parameters)
    end
  end
  >>< classify_and_transform_errors
