(**************************************************************************)
(*  Copyright 2015, Sebastien Mondet <seb@mondet.org>                     *)
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

open Ketrew_long_running_utilities

open Ketrew_gen_yarn_v0.Distributed_shell_parameters
open Ketrew_gen_yarn_v0.Run_parameters
open Ketrew_gen_yarn_v0.Running
open Ketrew_gen_yarn_v0.Created

include Json.Make_versioned_serialization
    (Ketrew_gen_yarn_v0.Run_parameters)
    (Ketrew_gen_versioned.Yarn_run_parameters)

type run_parameters = Ketrew_gen_yarn_v0.Run_parameters.t

let name = "yarn-cluster"

let default_distributed_shell_jar =
  "/opt/cloudera/parcels/CDH/lib/hadoop-yarn/hadoop-yarn-applications-distributedshell.jar"

let distributed_shell_program
    ?(hadoop_bin="hadoop")
    ?(distributed_shell_shell_jar=default_distributed_shell_jar)
    ~container_memory
    ~timeout
    ~application_name
    program =
  `Distributed_shell (
    {hadoop_bin; distributed_shell_shell_jar;
     container_memory; timeout; application_name}, program)

let create
    ?(host=Ketrew_host.tmp_on_localhost)
    ?(daemonize_using=`Python_daemon)
    ?(daemon_start_timeout=3600.)
    program =
  let created = {host; program; daemonize_using; daemon_start_timeout} in
  `Long_running (name, `Created created |> serialize)


let hack_to_string = function
| `Nohup_setsid -> "Nohup+Setsid"
| `Python_daemon -> "Python-script"

let log =
  let open Log in
  let prog =
    function
    | `Yarn_application ya -> ["Yarn Program", Ketrew_program.log ya]
    | `Distributed_shell (params, ds) ->
      let {hadoop_bin; distributed_shell_shell_jar;
           container_memory; timeout; application_name} = params in
      [
        "Hadoop Binary", s hadoop_bin;
        "DistShell.jar", s distributed_shell_shell_jar;
        "Container Memory",
        (match container_memory with
         | `GB gb -> sf "%d GB" gb
         | `Raw raw -> sf "%S" raw);
        "Timeout",
        (match timeout with
         | `Raw raw -> sf "%S" raw
         | `Seconds secs -> sf "%d s." secs);
        "DistShell Program", Ketrew_program.log ds;
      ] in
  let created c =
    List.append
      [ "Method", s (hack_to_string c.daemonize_using);
        "Host", Ketrew_host.log c.host; ]
      (prog c.program)
  in
  function
  | `Created c -> ("Status", s "Created") :: created c
  | `Running rp ->
    List.concat [
      ("Status", s "Running") :: created rp.created;
      let open Ketrew_gen_daemonize_v0.Running in
      ["PID", OCaml.option i rp.daemonized_script.pid;
       "Playground", s (Ketrew_path.to_string rp.daemonized_script.playground);
       "Start-time", Time.log rp.daemonized_script.start_time;];
    ]


let additional_queries run_param =
  match run_param with
  | `Created _ -> []
  | `Running rp ->
    let daemonize_run_param = `Running rp.daemonized_script in
    begin match Ketrew_daemonize.additional_queries daemonize_run_param with
    | [] -> []
    | more ->
      ("status", Log.(s "Get the Yarn application status"))
      :: ("logs", Log.(s "Get the Yarn application logs"))
      :: more
    end

(*
Dirty way of finding the application ID: we parse the output to find the logging

See
https://svn.apache.org/repos/asf/hadoop/common/trunk/hadoop-yarn-project/hadoop-yarn/hadoop-yarn-client/src/main/java/org/apache/hadoop/yarn/client/api/impl/YarnClientImpl.java
line 251
(or 
http://www.codatlas.com/github.com/apache/hadoop/trunk/hadoop-yarn-project/hadoop-yarn/hadoop-yarn-client/src/main/java/org/apache/hadoop/yarn/client/api/impl/YarnClientImpl.java?keyword=impl.YarnClientImpl&line=251)
*)
let re_find_application_id =
  Re_posix.compile_pat
    ~opts:[`ICase; `Newline] "Submitted *application *([a-zA-Z0-9_-]+)"

let find_application_id stdout_stderr =
  begin try
    let subs = Re.exec re_find_application_id stdout_stderr |> Re.get_all in
    return subs.(1)
  with e ->
    fail Log.(s "Could not find application ID" % n
              % quote "stdout ^ stderr" % s ":" % n % indent (s stdout_stderr))
  end

let get_application_id daemonize_run_param =
  Ketrew_daemonize.query daemonize_run_param "stdout"
  >>= fun stdout ->
  Ketrew_daemonize.query daemonize_run_param "stderr"
  >>= fun stderr ->
  find_application_id (stdout ^ stderr)


let query run_param item =
  match run_param with
  | `Created _ -> fail Log.(s "not running")
  | `Running rp ->
    let host = rp.created.host in
    let daemonize_run_param = `Running rp.daemonized_script in
    match item with
    | "status"  ->
      get_application_id daemonize_run_param
      >>= fun app_id ->
      shell_command_output_or_log ~host (fmt "yarn application -status %s" app_id)
    | "logs" ->
      get_application_id daemonize_run_param
      >>= fun app_id ->
      let tmp_file = Filename.concat "/tmp" (Unique_id.create ()) in
      shell_command_output_or_log ~host
        (fmt "yarn logs -applicationId %s > %s" app_id tmp_file)
      >>= fun (_ : string) ->
      Ketrew_host.grab_file_or_log host (Ketrew_path.absolute_file_exn tmp_file)
    | other -> Ketrew_daemonize.query daemonize_run_param other


let hadoop_distshell_call
    ~distshell_jar ~hadoop_bin ~container_memory ~timeout ~application_name =
  [hadoop_bin; 
   "org.apache.hadoop.yarn.applications.distributedshell.Client";
   "-jar"; distshell_jar;
   "-num_containers"; "1";
   "-shell_script"; Ketrew_daemonize.script_placeholder;
   "-appname"; application_name;
   "-container_memory"; container_memory;
   "-timeout"; timeout]

let start = function
| `Created ({host; program; daemonize_using; daemon_start_timeout} as created) ->
  let shell_command, actual_program =
    match program with
    | `Distributed_shell (params, p) ->
      let {hadoop_bin; distributed_shell_shell_jar;
           container_memory; timeout; application_name} = params in
      let container_memory =
        match container_memory with
        | `GB i -> fmt "%d" (i * 1024)
        | `Raw s -> s
      in
      let timeout =
        match timeout with
        | `Raw s -> s
        | `Seconds secs -> fmt "%d" (secs * 1000)
      in
      (hadoop_distshell_call ~hadoop_bin
         ~distshell_jar:distributed_shell_shell_jar
         ~container_memory ~timeout ~application_name, p)
    | `Yarn_application p -> (Ketrew_daemonize.default_shell_command, p)
  in
  let daemonize_run_param =
    let open Ketrew_gen_daemonize_v0.Created in
    `Created {host; program = actual_program;
              using_hack = daemonize_using;
              starting_timeout = daemon_start_timeout;
              shell_command } in
  Ketrew_daemonize.start daemonize_run_param
  >>= fun daemonize_run_params ->
  begin match daemonize_run_params with
  | `Running daemonized_script -> return (`Running {created; daemonized_script})
  | `Created _ -> fail (`Fatal "Daemonized script did not start")
  end
| `Running _ -> fail (`Fatal "Already running")

let update run_parameters =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run ->
    let daemonize_run_params = `Running run.daemonized_script in
    Ketrew_daemonize.update daemonize_run_params
    >>= fun daemon_updated ->
    let make_new_rp old_one =
      match old_one with
      | `Created rp -> fail_fatal "wrong state from Ketrew_daemonize: Created"
      | `Running rp -> return (`Running {run with daemonized_script = rp})
    in
    begin match daemon_updated with
    | `Failed (rp, s) ->
      make_new_rp rp >>= fun new_rp ->
      return (`Failed (new_rp, s))
    | `Succeeded rp ->
      make_new_rp rp >>= fun new_rp ->
      return (`Succeeded new_rp)
    | `Still_running rp ->
      make_new_rp rp >>= fun new_rp ->
      return (`Still_running new_rp)
    end
  end

let kill run_parameters =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run ->
    let daemonize_run_params = `Running run.daemonized_script in
    let host = run.created.host in
    begin
      (* We try to kill with yarn but we just log any potential error
         without failing. *)
      get_application_id daemonize_run_params
      >>< function
      | `Ok app_id ->
        shell_command_output_or_log ~host
          (fmt "yarn application -kill %s" app_id)
        >>< begin function
        | `Ok output ->
          Log.(s "Killing: " % s app_id % s ": SUCCESS" %n
               % verbatim output @ verbose);
          return ()
        | `Error log ->
          Log.(s "Killing: " % s app_id % s ": FAILED" %n % log @ verbose);
          return ()
        end
      | `Error log ->
        Log.(s "Error while killing yarn-application: cannot get application-id"
             %n %s ":" % log @ error);
        return ()
    end
    >>= fun () ->
    Ketrew_daemonize.kill daemonize_run_params
    >>= fun (`Killed rp) ->
    begin match rp with
    | `Created rp -> fail_fatal "wrong state from Ketrew_daemonize: Created"
    | `Running rp -> return (`Killed (`Running {run with daemonized_script = rp}))
    end
  end
