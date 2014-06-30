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
module Error = Ketrew_error
module Program = Ketrew_program

include Ketrew_gen_daemonize_v0_t

let running =
  function `Running r -> r 
         | _ -> invalid_argument_exn ~where:"daemonize" "running"
let created = 
  function `Created c -> c
         | _ -> invalid_argument_exn ~where:"daemonize" "created"

let serialize t =
  Ketrew_gen_versioned_j.string_of_daemonize_run_parameters (`V0 t)
let deserialize_exn s = 
  begin match Ketrew_gen_versioned_j.daemonize_run_parameters_of_string s with
  | `V0 v0 -> v0
  end

let name = "daemonize"
let create 
  ?(starting_timeout=5.)
  ?(using=`Nohup_setsid) ?(host=Host.tmp_on_localhost) program =
  let c = {host; program; using_hack = using; starting_timeout } in
  `Long_running (name, `Created c |> serialize)

let using_hack = function
| `Created c -> c.using_hack
| `Running r -> r.created.using_hack

let hack_to_string = function
| `Nohup_setsid -> "Nohup+Setsid"
| `Python_daemon -> "Python-script"

let log = 
  let open Log in
  function
  | `Created c -> [
      "Status", s "Created" % sp % parens (s (hack_to_string c.using_hack));
      "Host", Host.log c.host;
      "Program", Program.log c.program;
      "Starting-timeout", f c.starting_timeout % s " sec."
    ]
  | `Running rp -> [
      "Status", s "Running" % sp 
                % parens (s (hack_to_string rp.created.using_hack));
      "Host", Host.log rp.created.host;
      "PID", OCaml.option i rp.pid;
      "Playground", s (Path.to_string rp.playground);
      "Start-time", Time.log rp.start_time;
    ]

let out_file_path ~playground =
  Path.(concat playground (relative_file_exn "out"))
let err_file_path ~playground =
  Path.(concat playground (relative_file_exn "err"))
let script_path ~playground =
  Path.(concat playground (relative_file_exn "monitored_script"))
let python_hack_path ~playground =
  Path.(concat playground (relative_file_exn "daemonizator.py"))

let fail_fatal msg = fail (`Fatal msg)

let additional_queries = function
| `Created _ -> []
| `Running _ ->
  [
    "stdout", Log.(s "Stardard output");
    "stderr", Log.(s "Stardard error");
    "log", Log.(s "Monitored-script `log` file");
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
    | other -> fail Log.(s "Unknown query: " % sf "%S" other)
    end

let make_python_script ~out ~err ~pid_file monitored_script_path =
  fmt "
import os               # Miscellaneous OS interfaces.
import sys              # System-specific parameters and functions.
import subprocess
if __name__ == '__main__':
    try: 
        pid = os.fork() 
        if pid > 0:
            # exit first parent
            sys.exit(0) 
    except OSError, e: 
        print >>sys.stderr, 'fork #1 failed: %%d (%%s)' %% (e.errno, e.strerror) 
        sys.exit(1)
    # decouple from parent environment
    os.chdir('/') 
    os.setsid() 
    os.umask(0) 
    pid_file = file('%s', 'w')
    pid_file.write('%%d\\n' %% os.getpid())
    pid_file.close()
    # do second fork
    try: 
        pid = os.fork() 
        if pid > 0:
            # exit from second parent, print eventual PID before
            print 'Daemon PID %%d' %% pid 
            sys.exit(0) 
    except OSError, e: 
        print >>sys.stderr, 'fork #2 failed: %%d (%%s)' %% (e.errno, e.strerror) 
        sys.exit(1) 
    p = subprocess.Popen(['bash', '%s'],
            cwd='/',
            stdout=file('%s', 'w'), 
            stderr=file('%s', 'w'))
"
    (Path.to_string pid_file)
    (Path.to_string monitored_script_path)
    (Path.to_string out)
    (Path.to_string err)


let start rp =
  let created = created rp in
  begin match Host.get_fresh_playground created.host with
  | None ->
    fail_fatal (fmt  "Host %s: Missing playground" 
                  (Host.to_string_hum created.host))
  | Some playground ->
    let monitored_script =
      Ketrew_monitored_script.create ~playground created.program in
    let monitored_script_path = script_path ~playground in
    Host.ensure_directory created.host playground
    >>= fun () ->
    let content =
      let write_pid =          (* the python-script creates the process group *)
        match created.using_hack with     (* so, it will write the PID itself *)
        | `Nohup_setsid -> true | `Python_daemon -> false in
      Ketrew_monitored_script.to_string ~write_pid monitored_script in
    Host.put_file ~content created.host ~path:monitored_script_path
    >>= fun () ->
    let out = out_file_path ~playground in
    let err = err_file_path ~playground in
    begin match created.using_hack with
    | `Nohup_setsid ->
      let cmd =
        fmt "nohup setsid bash %s > %s 2> %s &" 
          (Path.to_string_quoted monitored_script_path)
          (Path.to_string_quoted out) (Path.to_string_quoted err) in
      Host.run_shell_command created.host cmd
      >>= fun () ->
      Log.(s "daemonize: Ran " % s cmd @ very_verbose);
      return ()
    | `Python_daemon ->
      let pid_file = Ketrew_monitored_script.pid_file monitored_script in
      let content =
        make_python_script ~out ~err ~pid_file monitored_script_path in
      let path = python_hack_path ~playground in
      Host.put_file ~content created.host ~path
      >>= fun () ->
      Host.run_shell_command created.host 
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
      begin match Host.Error.classify he with
      | `Ssh | `Unix -> fail (`Recoverable (Error.to_string e))
      | `Execution -> fail_fatal (Error.to_string e)
      end
    | `IO _ | `System _ as e -> 
      fail_fatal (Error.to_string e)
    end
  end

let _pid_and_log run_parameters =
  let run = running run_parameters in
  let log_file = Ketrew_monitored_script.log_file run.script in
  let pid_file = Ketrew_monitored_script.pid_file run.script in
  begin Host.get_file run.created.host ~path:log_file
    >>< function
    | `Ok c -> return (Some c)
    | `Error (`Cannot_read_file _) -> return None
    | `Error (`IO _ as e) -> fail e
  end
  >>= fun log_content ->
  let log = Option.map ~f:Ketrew_monitored_script.parse_log log_content in
  begin Host.get_file run.created.host ~path:pid_file
    >>< function
    | `Ok c -> return (Int.of_string (String.strip ~on:`Both c))
    | `Error (`Cannot_read_file _) -> return None
    | `Error (`IO _ as e) -> fail e
  end
  >>= fun pid ->
  Log.(s "daemonize.update: got " % indent (OCaml.option s log_content)
       % s " log values and the Pid: " % OCaml.option i pid
       % sp % brakets (s "pid file: " % s (Path.to_string pid_file))
       @ very_verbose);
  return (`Pid pid, `Log log)

let _update run_parameters =
  _pid_and_log run_parameters
  >>= fun (`Pid pid, `Log log) ->
  let run = running run_parameters in
  let elapsed = Time.(now ()) -. run.start_time in
  begin match pid with
  | None when  elapsed > run.created.starting_timeout ->
    (* no pid after timeout => fail! *)
    return (`Failed (run_parameters,
                     fmt "starting timeouted: %.2f > %.2f"
                       elapsed run.created.starting_timeout))
  | None ->
    (* we consider it didn't start yet *)
    return (`Still_running run_parameters)
  | Some p ->
    let cmd = fmt "ps -g %d" p in
    Host.get_shell_command_return_value run.created.host cmd
    >>= fun ps_return ->
    begin match ps_return with
    | 0 -> (* most likely still running *)
      (* TOOD save pid + find other way of checking *)
      return (`Still_running run_parameters)
    | n -> (* not running, for “sure” *)
      begin match Option.bind log List.last with
      | None -> (* no log at all *)
        return (`Failed (run_parameters, "no log file"))
      | Some (`Success  date) ->
        return (`Succeeded run_parameters)
      | Some other ->
        return (`Failed (run_parameters, "failure in log"))
      end
    end
  end

let update run_parameters =
  _update run_parameters
  >>< function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Host he as e ->
      begin match Host.Error.classify he with
      | `Ssh | `Unix -> fail (`Recoverable (Error.to_string e))
      | `Execution -> fail_fatal (Error.to_string e)
      end
    | `IO _ | `System _ as e -> fail_fatal (Error.to_string e)
    end

let kill run_parameters =
  begin
    _pid_and_log run_parameters
    >>= fun (`Pid pid, `Log log) ->
    let run = running run_parameters in
    begin match pid with
    | None ->
      (* either it didn't start yet, or it already crashed …
         should count the number of retries or compare dates and have a timeout
      *)
      fail_fatal "Pid file empty"
    | Some p ->
      let cmd = fmt "kill -- -%d" p in
      Log.(s "Killing group " % i p % s " with " % sf "%S" cmd @ very_verbose);
      Host.run_shell_command run.created.host cmd
      >>= fun () ->
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


