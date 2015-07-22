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
  type distributed_shell_parameters = {
    hadoop_bin: string;
    distributed_shell_shell_jar: string;
    container_memory: [ `GB of int | `MB of int | `Raw of string ];
    timeout: [ `Seconds of int | `Raw of string ];
    application_name: string;
  } [@@deriving yojson]
  type created = {
    host: Host.t;
    program: [
      | `Distributed_shell of (distributed_shell_parameters * Program.t)
      | `Yarn_application of Program.t
    ];
    daemonize_using: [ `Nohup_setsid | `Python_daemon ];
    daemon_start_timeout: float;
  } [@@deriving yojson]
  type running = {
    created: created;
    daemonized_script: Daemonize.run_parameters;
  } [@@deriving yojson]
  type t = [
    | `Created of created
    | `Running of running
  ] [@@deriving yojson]
end
type run_parameters = Run_parameters.t
type distributed_shell_parameters = Run_parameters.distributed_shell_parameters
include Json.Versioned.Of_v0(Run_parameters)
open Run_parameters


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
    ?(host=Host.tmp_on_localhost)
    ?(daemonize_using=`Python_daemon)
    ?(daemon_start_timeout=20.)
    program =
  let created = {host; program; daemonize_using; daemon_start_timeout} in
  `Long_running (name, `Created created |> serialize)


let using_to_string = function
| `Nohup_setsid -> "Nohup+Setsid"
| `Python_daemon -> "Python-script"

let rec markup : t -> Display_markup.t =
  let open Display_markup in
  function
  | `Created c ->
    let kind, program =
      match c.program with
      | `Distributed_shell (dsp, p) ->
        let dspm =
          description_list [
            "Hadoop-binary", path dsp.hadoop_bin;
            "DS-Jar", path dsp.distributed_shell_shell_jar;
            "Container-memory",
            begin match dsp.container_memory with
            | `GB i -> textf "%d GB" i
            | `MB i -> textf "%d GB" i
            | `Raw s -> textf "Raw: %S" s
            end;
            "Timeout",
            begin match dsp.timeout with
            | `Seconds s -> time_span (float s)
            |  `Raw r -> textf "Raw: %S" r
            end;
            "Application-name", textf "%s" dsp.application_name;
          ] in
        (description "YARN-Distributed-Shell" dspm, p)
      | `Yarn_application p -> (textf "YARN-Application", p)
    in
    description_list [
      "Host", Host.markup c.host;
      "Kind", kind;
      "Program", Program.markup program;
      "Daemonize-starting-timeout", time_span c.daemon_start_timeout;
      "Daemonize-using",
      begin match c.daemonize_using with
      | `Nohup_setsid -> textf "Nohup-setsid"
      | `Python_daemon -> textf "Python-daemon"
      end;
    ]
  | `Running rp ->
    description_list [
      "Created-as", markup (`Created rp.created);
      "Daemonized-as", Daemonize.markup rp.daemonized_script;
    ]

let log rp = ["YARN", Display_markup.log (markup rp)]

let additional_queries run_param =
  let always_there = [
    "ketrew-markup/status", Log.(s "Get the status as Markup");
  ] in
  match run_param with
  | `Created _ -> always_there
  | `Running rp ->
    always_there
    @ [
      ("status", Log.(s "Get the Yarn application status"));
      ("logs", Log.(s "Get the Yarn application logs"));
    ]
    @ (Daemonize.additional_queries rp.daemonized_script
       |> List.filter ~f:(fun (n, _) -> n <> "ketrew-markup/status"))

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
  Daemonize.query daemonize_run_param "stdout"
  >>= fun stdout ->
  Daemonize.query daemonize_run_param "stderr"
  >>= fun stderr ->
  find_application_id (stdout ^ stderr)

let parse_status str =
  let lines = String.split ~on:(`Character '\n') str in
  let key_values =
    List.map lines ~f:(fun line ->
        String.split ~on:(`Character ':') line
        |> List.map ~f:String.strip)
  in
  match
    List.find key_values ~f:(function "Final-State" :: _ -> true | _ -> false)
  with
  | Some (_ :: "SUCCEEDED" :: _) -> `Succeeded
  | Some (_ :: "FAILED" :: _)
  | Some (_ :: "KILLED" :: _) -> `Failed
  | Some _ | None -> `Unknown

let query run_param item =
  match run_param with
  | `Created _ ->
    begin match item with
    | "ketrew-markup/status" ->
      return (markup run_param |> Display_markup.serialize)
    | other -> fail Log.(s "not running")
    end
  | `Running rp ->
    let host = rp.created.host in
    begin match item with
    | "status"  ->
      get_application_id rp.daemonized_script
      >>= fun app_id ->
      shell_command_output_or_log ~host (fmt "yarn application -status %s" app_id)
    | "logs" ->
      get_application_id rp.daemonized_script
      >>= fun app_id ->
      let tmp_file = Filename.concat "/tmp" (Unique_id.create ()) in
      shell_command_output_or_log ~host
        (fmt "yarn logs -applicationId %s > %s" app_id tmp_file)
      >>= fun (_ : string) ->
      Host_io.grab_file_or_log host (Path.absolute_file_exn tmp_file)
    | "ketrew-markup/status" ->
      return (markup run_param |> Display_markup.serialize)
    | other -> Daemonize.query rp.daemonized_script other
    end

let hadoop_distshell_call
    ~distshell_jar ~hadoop_bin ~container_memory ~timeout ~application_name
    script =
  [hadoop_bin; 
   "org.apache.hadoop.yarn.applications.distributedshell.Client";
   "-jar"; distshell_jar;
   "-num_containers"; "1";
   "-shell_script"; script;
   "-appname"; application_name;
   "-container_memory"; container_memory;
   "-timeout"; timeout]

let start = function
| `Created ({host; program; daemonize_using; daemon_start_timeout} as created) ->
  let call_script, actual_program =
    match program with
    | `Distributed_shell (params, p) ->
      let {hadoop_bin; distributed_shell_shell_jar;
           container_memory; timeout; application_name} = params in
      let container_memory =
        match container_memory with
        | `GB i -> fmt "%d" (i * 1024)
        | `MB i -> fmt "%d" i
        | `Raw s -> s
      in
      let timeout =
        match timeout with
        | `Raw s -> s
        | `Seconds secs -> fmt "%d" (secs * 1000)
      in
      (Some (
          hadoop_distshell_call ~hadoop_bin
            ~distshell_jar:distributed_shell_shell_jar
            ~container_memory ~timeout ~application_name),
       p)
    | `Yarn_application p -> (None, p)
  in
  let `Long_running (_, daemonize_run_param) =
    Daemonize.create
      ~starting_timeout:daemon_start_timeout
      ~host actual_program ~using:daemonize_using
      ?call_script ~no_log_is_ok:true in
  Daemonize.(start (deserialize_exn daemonize_run_param))
  >>= fun daemonized_script ->
  return (`Running {created; daemonized_script})
| `Running _ -> fail (`Fatal "Already running")

let update run_parameters =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run ->
    Daemonize.update run.daemonized_script
    >>= fun daemon_updated ->
    let make_new_rp old_one =
      return (`Running {run with daemonized_script = old_one}) in
    begin match daemon_updated with
    | `Failed (rp, s) ->
      make_new_rp rp >>= fun new_rp ->
      return (`Failed (new_rp, s))
    | `Succeeded rp ->
      make_new_rp rp >>= fun new_rp ->
      (* Since we use `~no_log_is_ok:true` it is pretty easy for a
         daemonized process to succeed while the yarn application
         failed, hence we need to get the status from yarn. *)
      begin
        begin
          let host = run.created.host in
          get_application_id run.daemonized_script
          >>= fun app_id ->
          shell_command_output_or_log ~host (fmt "yarn application -status %s" app_id)
          >>= fun application_status_string ->
          begin match parse_status application_status_string with
          | `Succeeded -> return (`Succeeded new_rp)
          | `Failed -> return (`Failed (new_rp, "Yarn-status: FAILED"))
          | `Unknown -> return (`Still_running new_rp)
          end
        end >>< function
        | `Ok o -> return o
        | `Error log -> fail (`Fatal (Log.to_long_string log))
      end
    | `Still_running rp ->
      make_new_rp rp >>= fun new_rp ->
      return (`Still_running new_rp)
    end
  end

let kill run_parameters =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run ->
    let host = run.created.host in
    begin
      (* We try to kill with yarn but we just log any potential error
         without failing. *)
      get_application_id run.daemonized_script
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
    Daemonize.kill run.daemonized_script
    >>= fun (`Killed rp) ->
    return (`Killed (`Running {run with daemonized_script = rp}))
  end
