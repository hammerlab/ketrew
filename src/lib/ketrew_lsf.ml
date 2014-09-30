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

open Ketrew_gen_lsf_v0.Run_parameters
open Ketrew_gen_lsf_v0.Running
open Ketrew_gen_lsf_v0.Created

type run_parameters = Ketrew_gen_lsf_v0.Run_parameters.t

module Serialize_versioned = Json.Make_serialization(Ketrew_gen_versioned.Lsf_run_parameters)

let serialize t =
  Serialize_versioned.serialize (`V0 t)

let deserialize_exn s =
  begin match Serialize_versioned.deserialize_exn s with
  | `V0 v0 -> v0
  end

let name = "LSF"
let create
    ?(host=Host.tmp_on_localhost)
    ?queue ?name ?wall_limit ?processors
    program =
  `Long_running ("LSF",
                 `Created {host; program; queue; name; wall_limit; processors}
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
      "LSF-ID", i rp.lsf_id;
      "Playground", s (Path.to_string rp.playground);
  ]

let out_file_path ~playground =
  Path.(concat playground (relative_file_exn "out"))
let err_file_path ~playground =
  Path.(concat playground (relative_file_exn "err"))
let script_path ~playground =
  Path.(concat playground (relative_file_exn "monitored_script"))

let parse_bsub_output s =
  (* Output looks like
          Job <1386656> is submitted to queue <queuename>.
  According to
     http://www.vub.ac.be/BFUCC/LSF/bsub.1.html
  the job id is a positive number.  *)
  let splitted =
    String.split s ~on:(`Character '<')
    |> List.map ~f:(String.split ~on:(`Character '>'))
    |> List.concat in
  match splitted with
  | _ :: jobid :: _ ->
    Int.of_string jobid
  | _ -> None

let fail_fatal msg = fail (`Fatal msg)

let additional_queries = function
| `Created _ -> []
| `Running _ ->
  [
    "stdout", Log.(s "LSF output file");
    "stderr", Log.(s "LSF error file");
    "log", Log.(s "Monitored-script `log` file");
    "bpeek", Log.(s "Call `bpeek`");
    "script", Log.(s "Monitored-script used");
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
    | "bpeek" ->
      begin Host.get_shell_command_output rp.created.host
          (fmt "bpeek %d" rp.lsf_id)
        >>< function
        | `Ok (o, _) -> return o
        | `Error e ->
          fail Log.(s "Command `bpeek` failed: " % s (Error.to_string e))
      end
    | other -> fail Log.(s "Unknown query: " % sf "%S" other)
    end

let start: run_parameters -> (_, _) Deferred_result.t = function
| `Running _ ->
  fail_fatal "Wrong state: already running"
| `Created created ->
  begin match Host.get_fresh_playground created.host with
  | None ->
    fail_fatal (fmt  "Host %s: Missing playground"
                  (Host.to_string_hum created.host))
  | Some playground ->
    let script = Ketrew_monitored_script.create ~playground created.program in
    let monitored_script_path = script_path ~playground in
    Host.ensure_directory created.host playground
    >>= fun () ->
    let content = Ketrew_monitored_script.to_string script in
    Host.put_file ~content created.host ~path:monitored_script_path
    >>= fun () ->
    let out = out_file_path ~playground in
    let err = err_file_path ~playground in
    let cmd =
      let option o ~f = Option.value_map o ~f ~default:"" in
      String.concat ~sep:" " [
        "bsub";
        fmt "-o %s" (Path.to_string out);
        fmt "-e %s" (Path.to_string err);
        (option created.queue (fmt "-q %s"));
          (option created.name (fmt "-J %s"));
        (option created.wall_limit (fmt "-W %s"));
        (option created.processors (function
           | `Min m -> fmt "-n %d" m
           | `Min_max (mi, ma) -> fmt "-n %d,%d" mi ma));
        fmt "< %s"
          (Path.to_string_quoted monitored_script_path)
      ]
    in
    Log.(s "Cmd: " % s cmd %n  @ verbose);
    Host.get_shell_command_output created.host cmd
    >>= fun (stdout, stderr) ->
    Log.(s "Cmd: " % s cmd %n % s "Out: " % s stdout %n
         % s "Err: " % s stderr @ verbose);
    begin match parse_bsub_output stdout with
    | Some lsf_id ->
      return (`Running {lsf_id; playground; script; created})
    | None ->
      fail_fatal (fmt "bsub did not give a JOB ID: %S %S" stdout stderr)
    end
  end
  >>< begin function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Fatal _ as e -> fail e
    | `Host he as e ->
      begin match Host.Error.classify he with
      | `Ssh | `Unix -> fail (`Recoverable (Error.to_string e))
      | `Execution -> fail_fatal (Error.to_string e)
      end
    | `IO _ | `System _ as e ->
      fail_fatal (Error.to_string e)
    end
  end

let get_lsf_job_status host lsf_id =
  let cmd = fmt "bjobs -l %d" lsf_id in
  Host.get_shell_command_output host cmd
  >>= fun (stdout, stderr) ->
  Log.(s "Cmd: " % s cmd %n % s "Out: " % s stdout %n
       % s "Err: " % s stderr @ verbose);
  let status =
    let sanitized =
      String.split ~on:(`Character '\n') stdout
      |> List.map ~f:(String.strip ~on:`Left)
      |> String.concat ~sep:"" in
    let re = Re_posix.compile_pat "Status <([A-Z]+)>" in
    let subs = Re.(exec re sanitized |> get_all) in
    try Some (Array.get subs 1) with _ -> None
  in
  let ketrew_status =
    match status with
    | Some s ->
      begin match s with 
      | "PEND" | "UNKWN" | "RUN" -> `Running
      | "DONE" -> `Done
      | "USUSP" | "PSUSP" | "SSUSP" | "EXIT" | "ZOMBI" -> `Failed
      | other ->
        Log.(s "LSF: unrocognized status string: " % sf "%S" other
             @ error);
        `Failed
      end
    | None ->
      Log.(s "LSF: cannot parse status: " % quote stdout @ error);
      `Failed
  in
  return ketrew_status

let update = function
| `Created _ -> fail_fatal "not running"
| `Running run as run_parameters ->
  begin
    let log_file = Ketrew_monitored_script.log_file run.script in
    begin Host.get_file run.created.host ~path:log_file
      >>< function
      | `Ok c -> return (Some c)
      | `Error (`Cannot_read_file _) -> return None
      | `Error (`Timeout _ as e) -> fail e
    end
    >>= fun log_content ->
    let log = Option.map ~f:Ketrew_monitored_script.parse_log log_content in
    get_lsf_job_status run.created.host run.lsf_id
    >>= fun status ->
    begin match Option.bind log List.last with
    | None when status = `Failed || status = `Done ->
      (* yes, Done without log ⇒ failed *)
      return (`Failed (run_parameters, fmt "LSF status"))
    | None -> (* when status = `Running -*)
      return (`Still_running run_parameters)
    | Some (`Failure (date, label, ret)) ->
      return (`Failed (run_parameters, fmt "%s returned %s" label ret))
    | Some (`Success  date) ->
      return (`Succeeded run_parameters)
    | Some other when status = `Running ->
      return (`Still_running run_parameters)
    | Some other ->
      return (`Failed (run_parameters, fmt "LSF status"))
    end
  end
  >>< begin function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Host he as e ->
      begin match Host.Error.classify he with
      | `Ssh | `Unix -> fail (`Recoverable (Error.to_string e))
      | `Execution -> fail_fatal (Error.to_string e)
      end
    | `Timeout _ -> fail (`Recoverable "timeout")
    | `IO _ | `System _ as e -> fail_fatal (Error.to_string e)
    end
  end

let kill run_parameters =
  begin
    match run_parameters with
    | `Created _ -> fail_fatal "not running"
    | `Running run as run_parameters ->
      begin
        let cmd = fmt "bkill %d" run.lsf_id in
        Host.get_shell_command_output run.created.host cmd
        >>= fun (_, _) ->
        return (`Killed run_parameters)
      end
  end
  >>< begin function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Fatal _ as e -> fail e
    | `Host he as e ->
      begin match Host.Error.classify he with
      | `Ssh | `Unix -> fail (`Recoverable (Error.to_string e))
      | `Execution -> fail_fatal (Error.to_string e)
      end
    | `IO _ | `System _ as e -> fail_fatal (Error.to_string e)
    end
  end
