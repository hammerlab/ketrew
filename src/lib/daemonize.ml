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

module Run_parameters = struct
  type created = {
    host: Host.t;
    program: Program.t;
    using: [ `Nohup_setsid | `Python_daemon ];
    starting_timeout: Time.t;
    shell_command: string list;
    no_log_is_ok: bool [@default false];
  } [@@deriving yojson]

  type running = {
    pid: int option;
    playground: Path.t;
    script: Monitored_script.t;
    created: created;
    start_time: Time.t;
  } [@@deriving yojson]

  type t = [
    | `Created of created
    | `Running of running
  ] [@@deriving yojson]
end

type run_parameters = Run_parameters.t [@@deriving yojson]
include Json.Versioned.Of_v0(Run_parameters)
open Run_parameters


let running =
  function `Running r -> r
         | _ -> invalid_argument_exn ~where:"daemonize" "running"
let created =
  function `Created c -> c
         | _ -> invalid_argument_exn ~where:"daemonize" "created"

let name = "daemonize"

let default_shell = "bash"
let script_placeholder = "<script>"

let create
  ?(starting_timeout=5.) ?(call_script=fun s -> [default_shell; s])
  ?(using=`Nohup_setsid) ?(host=Host.tmp_on_localhost)
  ?(no_log_is_ok=false)
  program =
  let shell_command = call_script script_placeholder in
  let c =
    {host; program; using; starting_timeout; shell_command; no_log_is_ok } in
  `Long_running (name, `Created c |> serialize)

let using_to_string = function
| `Nohup_setsid -> "Nohup+Setsid"
| `Python_daemon -> "Python-script"

let rec markup : t -> Display_markup.t =
  let open Display_markup in
  function
  | `Created c -> description_list [
      "Using", textf "%s" (using_to_string c.using);
      "Host", Host.markup c.host;
      "Program", Program.markup c.program;
      "Starting-timeout", time_span c.starting_timeout;
      "Call-script", command (String.concat ~sep:" " c.shell_command);
      "No-log-is-OK", text_of_loggable Log.OCaml.bool c.no_log_is_ok;
    ]
  | `Running rp -> description_list [
      "Created-as", markup (`Created rp.created);
      "PID", option ~f:(textf "%d") rp.pid;
      "Playground", path (Path.to_string rp.playground);
      "Start-time", date rp.start_time;
    ]

let log rp = ["Daemonize", Display_markup.log (markup rp)]

let python_using_path ~playground =
  Path.(concat playground (relative_file_exn "daemonizator.py"))

let get_pid run =
  match run.pid with
  | Some p -> return (Some p)
  | None ->
    get_pid_of_monitored_script ~host:run.created.host ~script:run.script


let additional_queries = function
| `Created _ -> [
    "ketrew-markup/status", Log.(s "Get the status as Markup");
  ]
| `Running _ ->
  [
    "stdout", Log.(s "Stardard output");
    "stderr", Log.(s "Stardard error");
    "log", Log.(s "Monitored-script `log` file");
    "script", Log.(s "Monitored-script used");
    "check-process", Log.(s "Check the process-group with `ps`");
    "ketrew-markup/status", Log.(s "Get the status as Markup");
  ]

let query run_parameters item =
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
      Host_io.grab_file_or_log rp.created.host log_file
    | "stdout" ->
      let out_file = out_file_path ~playground:rp.playground in
      Host_io.grab_file_or_log rp.created.host out_file
    | "stderr" ->
      let err_file = err_file_path ~playground:rp.playground in
      Host_io.grab_file_or_log rp.created.host err_file
    | "script" ->
      let monitored_script_path = script_path ~playground:rp.playground in
      Host_io.grab_file_or_log rp.created.host monitored_script_path
    | "check-process" ->
      begin
        get_pid rp
        >>= begin function
        | Some pid ->
          Host_io.get_shell_command_output rp.created.host
            (fmt "ps -g %d" pid)
        | None ->
          fail `No_pid
        end >>< function
        | `Ok (o, _) -> return o
        | `Error (`Timeout t) ->
          fail Log.(s "Getting PID failed: time-out " % sf "%f" t)
        | `Error `No_pid ->
          fail Log.(s "Cannot get the processes status, PID not known (yet)")
        | `Error (`Host _ as e) ->
          fail Log.(s "Command `ps -g PID` failed: " % s (Error.to_string e))
      end
    | other -> fail Log.(s "Unknown query: " % sf "%S" other)
    end

let make_python_script ~out ~err ~pid_file ~call_script monitored_script_path =
  fmt {python|
import os               # Miscellaneous OS interfaces.
import sys              # System-specific parameters and functions.
import subprocess
if __name__ == '__main__':
    try:
        pid = os.fork()
        if pid > 0:
            # exit first parent
            sys.exit(0)
    except OSError as e:
        sys.stderr.write('fork #1 failed: %%d (%%s)' %% (e.errno, e.strerror))
        sys.exit(1)
    # decouple from parent environment
    os.chdir('/')
    os.setsid()
    os.umask(0)
    pid_file = open('%s', 'w')
    pid_file.write('%%d' %% os.getpid())
    pid_file.close()
    # do second fork
    try:
        pid = os.fork()
        if pid > 0:
            # exit from second parent, print eventual PID before
            print('Daemon PID %%d' %% pid)
            sys.exit(0)
    except OSError as e:
        sys.stderr.write('fork #2 failed: %%d (%%s)' %% (e.errno, e.strerror))
        sys.exit(1)
    p = subprocess.Popen([%s],
            cwd='/',
            stdout=open('%s', 'w'),
            stderr=open('%s', 'w'))
|python}
    (Path.to_string pid_file)
    (call_script
       (Path.to_string monitored_script_path)
     |> List.map ~f:(fmt "'%s'")
     |> String.concat ~sep:", ")
    (Path.to_string out)
    (Path.to_string err)

let start rp =
  let created = created rp in
  begin match Host_io.get_fresh_playground created.host with
  | None ->
    fail_fatal (fmt  "Host %s: Missing playground"
                  (Host.to_string_hum created.host))
  | Some playground ->
    let monitored_script =
      Monitored_script.create ~playground created.program in
    let monitored_script_path = script_path ~playground in
    Host_io.ensure_directory created.host playground
    >>= fun () ->
    let content =
      let write_pid =          (* the python-script creates the process group *)
        match created.using with     (* so, it will write the PID itself *)
        | `Nohup_setsid -> true | `Python_daemon -> false in
      Monitored_script.to_string ~write_pid monitored_script in
    Host_io.put_file ~content created.host ~path:monitored_script_path
    >>= fun () ->
    let out = out_file_path ~playground in
    let err = err_file_path ~playground in
    let call_script s =
      List.map created.shell_command ~f:(function
        | tok when tok = script_placeholder -> s
        | other -> other) in
    begin match created.using with
    | `Nohup_setsid ->
      let cmd =
        fmt "nohup setsid %s > %s 2> %s &"
          (call_script
             (Path.to_string monitored_script_path)
           |> List.map ~f:Filename.quote
           |> String.concat ~sep:" ")
          (Path.to_string_quoted out) (Path.to_string_quoted err) in
      Host_io.run_shell_command created.host cmd
      >>= fun () ->
      Log.(s "daemonize: Ran " % s cmd @ very_verbose);
      return ()
    | `Python_daemon ->
      let pid_file = Monitored_script.pid_file monitored_script in
      let content =
        make_python_script ~out ~err ~pid_file ~call_script
          monitored_script_path in
      let path = python_using_path ~playground in
      Host_io.put_file ~content created.host ~path
      >>= fun () ->
      Host_io.run_shell_command created.host
        (fmt "python %s" (Path.to_string_quoted path))
    end
    >>= fun () ->
    return (`Running {pid = None; playground;  created;
                      script = monitored_script; start_time = Time.now ()})
  end
  >>< begin function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Fatal _ as e -> fail e
    | `Host he as e ->
      begin match Host_io.Error.classify he with
      | `Ssh | `Unix -> fail (`Recoverable (Error.to_string e))
      | `Execution -> fail_fatal (Error.to_string e)
      end
    | `IO _ | `System _ as e ->
      fail_fatal (Error.to_string e)
    end
  end

let update run_parameters =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run as run_parameters ->
    get_log_of_monitored_script ~host:run.created.host ~script:run.script
    >>= fun log_opt ->
    begin match Option.bind log_opt  List.last with
    | Some (`Success date) ->
      return (`Succeeded run_parameters)
    | Some (`Failure (date, label, ret)) ->
      return (`Failed (run_parameters, fmt "%s returned %s" label ret))
    | None | Some _->
      get_pid run
      >>= fun pid ->
      let elapsed = Time.(now ()) -. run.start_time in
      begin match pid with
      | None when  elapsed > run.created.starting_timeout ->
        (* no pid after timeout => fail! *)
        return (`Failed (run_parameters,
                         fmt "Can't the PID after %.2f seconds \
                              (> configured time-out: %.2f, \
                              file: %s)"
                           elapsed run.created.starting_timeout
                           (Monitored_script.pid_file run.script
                            |> Path.to_string)))
      | None ->
        (* we consider it didn't start yet *)
        return (`Still_running run_parameters)
      | Some p ->
        let new_run_parameters = `Running {run with pid = Some p} in
        let cmd = fmt "ps -g %d" p in
        Host_io.get_shell_command_return_value run.created.host cmd
        >>= fun ps_return ->
        begin match ps_return with
        | 0 -> (* most likely still running *)
          return (`Still_running new_run_parameters)
        | n -> (* not running, for “sure” *)
          (* we fetch the log file again, because the process could have
             finished between the last fetch and the call to `ps`. *)
          get_log_of_monitored_script ~host:run.created.host ~script:run.script
          >>= fun log_opt ->
          begin match Option.bind log_opt List.last with
          | None when elapsed <= run.created.starting_timeout ->
            (* no log at all *)
            return (`Still_running new_run_parameters)
          | None when run.created.no_log_is_ok ->
            (* The caller explicitly asked not to care about the log file. *)
            return (`Succeeded new_run_parameters)
          | None ->
            return (`Failed (new_run_parameters, "no log file"))
          | Some (`Success  date) ->
            return (`Succeeded new_run_parameters)
          | Some other ->
            return (`Failed (new_run_parameters, "failure in log"))
          end
        end
      end
    end
  end >>< classify_and_transform_errors

let kill run_parameters =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run as run_parameters ->
    get_pid run
    >>= fun pid ->
    begin match pid with
    | None ->
      (* either it didn't start yet, or it already crashed …
         should count the number of retries or compare dates and have a timeout
      *)
      fail_fatal "Pid file empty"
    | Some p ->
      let cmd = fmt "kill -- -%d" p in
      Log.(s "Killing group " % i p % s " with " % sf "%S" cmd @ very_verbose);
      Host_io.run_shell_command run.created.host cmd
      >>= fun () ->
      return (`Killed run_parameters)
    end
  end
  >>< classify_and_transform_errors
