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
    container_vcores: (int [@default 1]);
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
    mutable application_id: (string option [@default None]);
    mutable rm_url: (Uri.t option [@default None]);
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
    ?(container_vcores = 1)
    ~container_memory
    ~timeout
    ~application_name
    program =
  `Distributed_shell (
    {hadoop_bin; distributed_shell_shell_jar; container_vcores;
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
            "Container cores", textf "%d" dsp.container_vcores;
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
      "Application-ID", 
      Option.value_map ~default:(text "Unknown") rp.application_id
        ~f:(textf "%S");
      "RM-URL", 
      Option.value_map ~default:(text "Unknown") rp.rm_url
        ~f:(fun u -> uri (Uri.to_string u));
      "Daemonized-as", Daemonize.markup rp.daemonized_script;
    ]

let log rp = ["YARN", Display_markup.log (markup rp)]

let additional_queries run_param =
  let always_there = [
    "ketrew-markup/status", Log.(s "Get the state contents of the job");
  ] in
  match run_param with
  | `Created _ -> always_there
  | `Running rp ->
    always_there
    @ [
      ("status", Log.(s "Get the YARN application status"));
      ("logs", Log.(s "Get the YARN application logs"));
      ("api-app-raw", Log.(s "Call the YARN API and retrieve the JSON blob"));
      ("ketrew-markup/api-app",
       Log.(s "Call the YARN API and format the result"));
    ] @ (
      Daemonize.additional_queries rp.daemonized_script
      |> List.filter_map ~f:(function
        | "ketrew-markup/status", _ ->
          None (* it's already included in `always_there` *)
        | "log", _ -> Some ("log", Log.(s "Step-by-step log of the Program.t"))
        | ("stdout", _ | "stderr", _) as std->
          Some (fst std,
                Log.(sf "`%s`" (fst std) %
                     s " of the YARN client script, not your Program.t"))
        | "check-process", _ ->
          Some ("check-process",
                Log.(s "Check the process-group of the YARN client with `ps`"))
        | other -> Some other
        )
    )

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

let re_find_rm_url =
  Re_posix.compile_pat
    ~opts:[`ICase; `Newline] "appTrackingUrl=([-a-zA-Z0-9:/_\\.]+)"

let find_resource_manager_url stdout_stderr =
  begin try
    let subs = Re.exec re_find_rm_url  stdout_stderr |> Re.get_all in
    let url = Uri.of_string subs.(1) in
    let scheme = Uri.scheme url in
    let host = Uri.host url in
    let port = Uri.port url in
    let userinfo = Uri.userinfo url in
    return (Uri.make ?scheme ?host ?port ?userinfo ())
  with e ->
    fail Log.(s "Could not find application ID" % n
              % quote "stdout ^ stderr" % s ":" % n % indent (s stdout_stderr))
  end


let get_application_id ~host_io run_parameters =
  match run_parameters.application_id with
  | Some id -> return id
  | None ->
    Daemonize.query run_parameters.daemonized_script ~host_io "stdout"
    >>= fun stdout ->
    Daemonize.query run_parameters.daemonized_script ~host_io "stderr"
    >>= fun stderr ->
    find_application_id (stdout ^ stderr)
    >>= fun app_id ->
    run_parameters.application_id <- Some app_id;
    return app_id


let get_application_id_and_rm_url ~host_io run_parameters =
  get_application_id ~host_io run_parameters
  >>= fun appid ->
  begin match run_parameters.rm_url with
  | Some u -> return (`Id appid, `Url u)
  | None ->
    Daemonize.query run_parameters.daemonized_script ~host_io "stdout"
    >>= fun stdout ->
    Daemonize.query run_parameters.daemonized_script ~host_io "stderr"
    >>= fun stderr ->
    let both = stdout ^ stderr in
    find_resource_manager_url both
    >>= fun url ->
    run_parameters.rm_url <- Some url;
    return (`Id appid, `Url url)
  end

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

let yarn_api_get_app_raw ~host_io rp =
  get_application_id_and_rm_url rp ~host_io
  >>= fun (`Id appid, `Url rm_url) ->
  Log.(s "Got app id: " % quote appid
       % s " and url: " % uri rm_url @ verbose);
  let to_call = Uri.with_path rm_url ("/ws/v1/cluster/apps/" ^ appid) in
  wrap_deferred 
    ~on_exn:(fun e -> Log.(s "Cohttp_lwt_unix.Client.get: " % exn e))
    Lwt.(fun () ->
        Cohttp_lwt_unix.Client.get to_call
        >>= fun (resp, body) ->
        Cohttp_lwt_body.to_string body)

let parse_yarn_api_app_json s =
  begin try
    let json = Yojson.Basic.from_string s in
    return json
  with e ->
    fail Log.(s "Parsing of the JSON form YARN's API failed: " % exn e)
  end

  
let yarn_api_get_app_markup ~host_io rp =
  yarn_api_get_app_raw ~host_io rp
  >>= fun json_string ->
  parse_yarn_api_app_json json_string
  >>= fun json ->
  let rec json_to_markup : Yojson.Basic.json -> Display_markup.t  =
    let open Display_markup in
    function
    | `Assoc l ->
      description_list 
        (List.map l ~f:(function
           | "trackingUrl", `String url ->
             "Trarcking-URL", uri url
           | "amContainerLogs", `String url ->
             "AM-Container-Logs-URL", uri url
           | "startedTime", `Int t ->
             "Started-time", (date (float t /. 1000.))
           | "finishedTime", `Int t ->
             "Finished-time", (date (float t /. 1000.))
           | "elapsedTime", `Int t ->
             "Elapsed-time", (time_span (float t /. 1000.))
           | "app", json ->
             "YARN-Application",
             description_list [
               "Documentation",
               uri "http://hadoop.apache.org/docs/r2.7.0/hadoop-yarn/\
                    hadoop-yarn-site/ResourceManagerRest.html\
                    #Elements_of_the_app_Application_object";
               "Content", json_to_markup json
             ]
           | (name, content) ->
             name, json_to_markup content))
    | `Bool b -> textf "%b" b
    | `Float f -> textf "%g" f
    | `Int d -> textf "%d" d
    | `List l ->
      concat ~sep:(text ", ") (List.map l ~f:json_to_markup)
    | `Null -> text "null"
    | `String s -> text s
  in
  return (json_to_markup json |> Display_markup.serialize)

let query run_param ~host_io item =
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
      get_application_id rp ~host_io
      >>= fun app_id ->
      shell_command_output_or_log ~host_io ~host
        (fmt "yarn application -status %s" app_id)
    | "logs" ->
      get_application_id rp ~host_io
      >>= fun app_id ->
      let tmp_file = 
        let tmp_dir = 
          match rp.daemonized_script |> Daemonize.get_playground with
          | Some p -> Path.to_string p
          | None -> "/tmp" (* this case should never happen; the daemon is
                              running hence answers `Some _` *) in
        Filename.concat tmp_dir ("yarn-logs-" ^ Unique_id.create ()) in
      shell_command_output_or_log ~host_io ~host
        (fmt "yarn logs -applicationId %s > %s" app_id tmp_file)
      >>= fun (_ : string) ->
      Host_io.grab_file_or_log host_io ~host (Path.absolute_file_exn tmp_file)
    | "api-app-raw" ->
      yarn_api_get_app_raw ~host_io rp
    | "ketrew-markup/api-app" ->
      yarn_api_get_app_markup ~host_io rp
    | "ketrew-markup/status" ->
      return (markup run_param |> Display_markup.serialize)
    | other -> Daemonize.query rp.daemonized_script ~host_io other
    end

let hadoop_distshell_call
    ~distshell_jar ~hadoop_bin ~container_memory ~container_vcores
    ~timeout ~application_name
    script =
  [hadoop_bin; 
   "org.apache.hadoop.yarn.applications.distributedshell.Client";
   "-jar"; distshell_jar;
   "-num_containers"; "1";
   "-shell_script"; script;
   "-appname"; application_name;
   "-container_memory"; container_memory;
   "-container_vcores"; Int.to_string container_vcores;
   "-timeout"; timeout]

let start rp ~host_io =
  begin match rp with
  | `Created created ->
    let {host; program; daemonize_using; daemon_start_timeout} = created in
    let call_script, actual_program =
      match program with
      | `Distributed_shell (params, p) ->
        let {hadoop_bin; distributed_shell_shell_jar;
             container_memory; container_vcores;
             timeout; application_name} = params in
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
            hadoop_distshell_call ~hadoop_bin ~container_vcores
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
    Daemonize.(start ~host_io (deserialize_exn daemonize_run_param))
    >>= fun daemonized_script ->
    return (`Running {created; daemonized_script; 
                      application_id = None; rm_url = None})
  | `Running _ -> fail (`Fatal "Already running")
  end

let update run_parameters ~host_io =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run ->
    Daemonize.update ~host_io run.daemonized_script
    >>= fun daemon_updated ->
    let make_new_rp rp ~daemonized:old_one =
      return (`Running {rp with daemonized_script = old_one}) in
    (* Calling this here will update the cached values; which will make queries
       faster. *)
    begin
      get_application_id_and_rm_url ~host_io run
      >>< fun _ -> return ()
    end
    >>= fun () ->
    begin match daemon_updated with
    | `Failed (rp, s) ->
      make_new_rp run ~daemonized:rp >>= fun new_rp ->
      return (`Failed (new_rp, s))
    | `Succeeded rp ->
      (* Since we use `~no_log_is_ok:true` it is pretty easy for a
         daemonized process to succeed while the yarn application
         failed, hence we need to get the status from yarn. *)
      begin
        begin
          let host = run.created.host in
          begin
            get_application_id_and_rm_url ~host_io run
            >>< function
            | `Ok (`Id app_id, `Url _) -> return app_id
            | `Error log ->
              fail (`Fatal (fmt "Cannot get application-id: %s"
                              (Log.to_long_string log)))
          end
          >>= fun app_id ->
          begin
            Host_io.get_shell_command_output host_io ~host
              (fmt "yarn application -status %s" app_id)
            >>< function
            | `Ok (stdout, stderr) -> return stdout
            | `Error (`Host (`Non_zero (s, i))) ->
              fail (`Fatal (fmt "The command `yarn application -status %s` \
                                 returned %d: %s" app_id i s))
            | `Error other ->
              fail (`Recoverable
                      (fmt "The command `yarn application -status %s` \
                            on Host %s failed (non-fatal): %s"
                         app_id (Host.to_string_hum host)
                         (Error.to_string other)))
          end
          >>= fun application_status_string ->
          make_new_rp run ~daemonized:rp >>= fun new_rp ->
          begin match parse_status application_status_string with
          | `Succeeded -> return (`Succeeded new_rp)
          | `Failed -> return (`Failed (new_rp, "Yarn-status: FAILED"))
          | `Unknown -> return (`Still_running new_rp)
          end
        end
      end
    | `Still_running rp ->
      make_new_rp run ~daemonized:rp >>= fun new_rp ->
      return (`Still_running new_rp)
    end
  end

let kill run_parameters ~host_io =
  begin match run_parameters with
  | `Created _ -> fail_fatal "not running"
  | `Running run ->
    let host = run.created.host in
    begin
      (* We try to kill with yarn but we just log any potential error
         without failing. *)
      get_application_id ~host_io run
      >>< function
      | `Ok app_id ->
        shell_command_output_or_log ~host_io ~host
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
    Daemonize.kill ~host_io run.daemonized_script
    >>= fun (`Killed rp) ->
    return (`Killed (`Running {run with daemonized_script = rp}))
  end
