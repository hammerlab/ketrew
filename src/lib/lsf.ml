(**************************************************************************)
(*    Copyright 2014, 2015:                                               *)
(*          Sebastien Mondet <seb@mondet.org>,                            *)
(*          Leonid Rozenberg <leonidr@gmail.com>,                         *)
(*          Arun Ahuja <aahuja11@gmail.com>,                              *)
(*          Jeff Hammerbacher <jeff.hammerbacher@gmail.com>               *)
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

open Ketrew_pure
open Internal_pervasives
open Unix_io

open Long_running_utilities

open Logging.Global
include Make_module_error_and_info(struct
    let module_name = "Lsf"
  end)

module Run_parameters = struct
  type created = {
    host: Host.t;
    program: Program.t;
    queue: string option;
    name: string option;
    wall_limit: string option;
    project: string option;
    processors: [
        | `Min of int
        | `Min_max of (int * int)
      ] option;
    request_memory: ([ `GB of int | `MB of int ] option [@default None]);
    raw_options: (string list [@default []]);
  } [@@deriving yojson]
  type running = {
    lsf_id: int;
    playground: Path.t;
    script: Monitored_script.t;
    created: created;
  } [@@deriving yojson]
  type t = [
    | `Created of created
    | `Running of running
  ] [@@deriving yojson]
end
type run_parameters = Run_parameters.t
include Json.Versioned.Of_v0(Run_parameters)
open Run_parameters


let name = "LSF"
let create
    ?(host=Host.tmp_on_localhost)
    ?queue ?name ?wall_limit ?processors ?project
    ?request_memory ?(raw_options = [])
    program =
  `Long_running ("LSF",
                 `Created {host; program; queue; name;
                           wall_limit; project; processors;
                           request_memory; raw_options}
                 |> serialize)

let markup =
  let open Display_markup in
  let created {host; program; queue; name;
               wall_limit; project; processors;
               request_memory; raw_options} = [
    "Host", Host.markup host;
    "Program", Program.markup program;
    "Queue", option ~f:text queue;
    "Name", option ~f:text name;
    "Wall-Limit", option ~f:text wall_limit;
    "Project", option ~f:text project;
    "Processors",
    begin match processors with
    | None -> text "Default"
    | Some (`Min min) -> textf "≥ %d" min
    | Some (`Min_max (min, max)) ->
      textf "∈ [%d, %d]" min max
    end;
    "request-memory", 
    begin match request_memory with
    | None -> text "Default"
    | Some (`GB i) -> textf "%d GB" i
    | Some (`MB i) -> textf "%d GB" i
    end;
    "raw-options", itemize (List.map raw_options ~f:command);
  ] in
  function
  | `Created c ->
    description_list @@ ("Status", text "Created") :: created c
  | `Running rp ->
    description_list [
      "Status", text "Activated";
      "Created as", description_list (created rp.created);
      "LSF-ID", textf "%d" rp.lsf_id;
      "Playground", path (Path.to_string rp.playground);
    ]

let log rp = ["LSF", Display_markup.log (markup rp)]

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

let additional_queries = function
| `Created _ -> [
    "ketrew-markup/status", Log.(s "Get the status as Markup");
  ]
| `Running _ ->
  [
    "ketrew-markup/status", Log.(s "Get the status as Markup");
    "stdout", Log.(s "LSF output file");
    "stderr", Log.(s "LSF error file");
    "log", Log.(s "Monitored-script `log` file");
    "bjobs", Log.(s "Call `bjobs -l`");
    "bpeek", Log.(s "Call `bpeek`");
    "script", Log.(s "Monitored-script used");
  ]

let query run_parameters ~host_io item =
  match run_parameters with
  | `Created _ ->
    begin match item with
    | "ketrew-markup/status" ->
      return (markup run_parameters |> Display_markup.serialize)
    | other -> fail Log.(s "not running")
    end
  | `Running rp ->
    begin match item with
    | "ketrew-markup/status" ->
      return (markup run_parameters |> Display_markup.serialize)
    | "log" ->
      let log_file = Monitored_script.log_file rp.script in
      Host_io.grab_file_or_log host_io ~host:rp.created.host log_file
    | "stdout" ->
      let out_file = out_file_path ~playground:rp.playground in
      Host_io.grab_file_or_log host_io ~host:rp.created.host out_file
    | "stderr" ->
      let err_file = err_file_path ~playground:rp.playground in
      Host_io.grab_file_or_log host_io ~host:rp.created.host err_file
    | "script" ->
      let monitored_script_path = script_path ~playground:rp.playground in
      Host_io.grab_file_or_log host_io ~host:rp.created.host monitored_script_path
    | "bjobs" ->
      begin Host_io.get_shell_command_output host_io ~host:rp.created.host
          (fmt "bjobs -l %d" rp.lsf_id)
        >>< function
        | `Ok (o, _) -> return o
        | `Error e ->
          fail Log.(s "Command `bjobs -l <ID>` failed: " % s (Error.to_string e))
      end
    | "bpeek" ->
      begin Host_io.get_shell_command_output host_io ~host:rp.created.host
          (fmt "bpeek %d" rp.lsf_id)
        >>< function
        | `Ok (o, _) -> return o
        | `Error e ->
          fail Log.(s "Command `bpeek` failed: " % s (Error.to_string e))
      end
    | other -> fail Log.(s "Unknown query: " % sf "%S" other)
    end

let start rp ~host_io =
  match rp with
  | `Running _ ->
    fail_fatal "Wrong state: already running"
  | `Created created ->
    let markup_to_log = ref Display_markup.[
        "function", text "start";
        "start-date", date Time.(now ());
      ] in
    let add_to_final_log l = markup_to_log := !markup_to_log @ l in
    begin
      fresh_playground_or_fail ~host_io created.host
      >>= fun playground ->
      let script = Monitored_script.create ~playground created.program in
      let monitored_script_path = script_path ~playground in
      add_to_final_log Display_markup.[
          "playground",  Path.markup playground;
          "monitored_script_path", Path.markup monitored_script_path;
        ];
      Host_io.ensure_directory host_io ~host:created.host ~path:playground
      >>= fun () ->
      let content = Monitored_script.to_string script in
      Host_io.put_file ~content host_io ~host:created.host
        ~path:monitored_script_path
      >>= fun () ->
      let out = out_file_path ~playground in
      let err = err_file_path ~playground in
      let cmd =
        let option o ~f = Option.value_map o ~f ~default:"" in
        String.concat ~sep:" " [
          "bsub";
          fmt "-o %s" (Path.to_string out);
          fmt "-e %s" (Path.to_string err);
          (option created.queue (fmt "-q '%s'"));
          (option created.name (fmt "-J '%s'"));
          (option created.wall_limit (fmt "-W '%s'"));
          (option created.project (fmt "-P '%s'"));
          (option created.processors (function
             | `Min m -> fmt "-n %d -R 'span[hosts=1]'" m
             | `Min_max (mi, ma) -> fmt "-n %d,%d -R 'span[hosts=1]'" mi ma));
          option created.request_memory (fun m ->
              fmt "-R rusage[mem=%d]"
                (match m with `MB i -> i | `GB g -> 1024 * g));
          String.concat ~sep:" " created.raw_options;
          fmt "< %s"
            (Path.to_string_quoted monitored_script_path)
        ]
      in
      add_to_final_log Display_markup.[
          "out", Path.markup out;
          "err", Path.markup err;
          "command", command cmd;
        ];
      Log.(s "Cmd: " % s cmd %n  @ verbose);
      Host_io.get_shell_command_output host_io ~host:created.host cmd
      >>= fun (stdout, stderr) ->
      Log.(s "Cmd: " % s cmd %n % s "Out: " % s stdout %n
           % s "Err: " % s stderr @ verbose);
      add_to_final_log Display_markup.[
          "bsub-out", path stdout;
          "bsub-err", path stderr;
        ];
      begin match parse_bsub_output stdout with
      | Some lsf_id ->
        return (`Running {lsf_id; playground; script; created})
      | None ->
        fail_fatal (fmt "bsub did not give a JOB ID: %S %S" stdout stderr)
      end
    end
    >>< begin fun res ->
      add_to_final_log Display_markup.[
        "return-date", date (Time.now ());
      ];
      log_markup !markup_to_log;
      classify_and_transform_errors res
    end

let get_lsf_job_status ~host_io host lsf_id =
  let cmd = fmt "bjobs -l %d" lsf_id in
  Host_io.get_shell_command_output host_io ~host cmd
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

let update rp ~host_io =
  match rp with
  | `Created _ -> fail_fatal "not running"
  | `Running run as run_parameters ->
    begin
      get_log_of_monitored_script ~host_io ~host:run.created.host
        ~script:run.script
      >>= fun log_opt ->
      begin match Option.bind log_opt  List.last with
      | Some (`Success date) ->
        return (`Succeeded run_parameters)
      | Some (`Failure (date, label, ret)) ->
        return (`Failed (run_parameters, fmt "%s returned %s" label ret))
      | None | Some _->
        get_lsf_job_status ~host_io run.created.host run.lsf_id
        >>= fun status ->
        begin match status with
        | `Failed ->
          return (`Failed (run_parameters, fmt "LSF status"))
        | `Running ->
          return (`Still_running run_parameters)
        | `Done ->
          (* To be sure we need to get again the log file, because there could
             have been a race condition. *) 
          get_log_of_monitored_script ~host_io ~host:run.created.host
            ~script:run.script
          >>= fun log_opt ->
          begin match Option.bind log_opt List.last with
          | None -> (* no log at all *)
            return (`Failed (run_parameters, "no log file"))
          | Some (`Success  date) ->
            return (`Succeeded run_parameters)
          | Some other ->
            return (`Failed (run_parameters, "failure in log"))
          end
        end
      end
    end
    >>< classify_and_transform_errors

let kill run_parameters ~host_io =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run as run_parameters ->
    begin
      let cmd = fmt "bkill %d" run.lsf_id in
      Host_io.get_shell_command_output host_io ~host:run.created.host cmd
      >>= fun (_, _) ->
      return (`Killed run_parameters)
    end
  end
  >>< classify_and_transform_errors
