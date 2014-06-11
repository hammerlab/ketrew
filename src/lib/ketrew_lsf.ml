open Ketrew_pervasives

module Path = Ketrew_path

module Host = Ketrew_host

include Ketrew_gen_lsf_v0_t

let serialize t =
  Ketrew_gen_versioned_j.string_of_lsf_run_parameters (`V0 t)

let deserialize_exn s =
  begin match Ketrew_gen_versioned_j.lsf_run_parameters_of_string s with
  | `V0 v0 -> v0
  end

let name = "LSF"
let create
    ?(host=Host.localhost)
    ?queue ?name ?wall_limit ?processors
    commands =
  `Long_running ("LSF",
                 `Created {host; commands; queue; name; wall_limit; processors}
                 |> serialize)

let out_file_path ~playground =
  Path.(concat playground (relative_file_exn "out"))
let err_file_path ~playground =
  Path.(concat playground (relative_file_exn "err"))

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

let start: run_parameters -> (_, _) t = function
| `Running _ ->
  fail (`Failed_to_start "Wrong state: already running")
| `Created created ->
  begin match Host.get_fresh_playground created.host with
  | None -> fail (`Failed_to_start "Missing playground")
  | Some playground ->
    let script = Ketrew_monitored_script.create ~playground created.commands in
    let monitored_script_path =
      Path.(concat playground (relative_file_exn "monitored_script")) in
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
      fail (`Failed_to_start 
              (fmt "bsub did not give a JOB ID: %S %S" stdout stderr))
    end
  end
  >>< begin function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Failed_to_start _ as e -> fail e
    | `Host _ | `IO _ | `System _ as e -> 
      fail (`Failed_to_start (Error.to_string e))
    end
  end

let update = function
| `Created _ -> fail (`Failed_to_update "not running")
| `Running run as run_parameters ->
  begin
    let log_file = Ketrew_monitored_script.log_file run.script in
    begin Host.get_file run.created.host ~path:log_file
      >>< function
      | `Ok c -> return (Some c)
      | `Error (`Cannot_read_file _) -> return None
      | `Error (`IO _ as e) -> fail e
    end
    >>= fun log_content ->
    let log = Option.map ~f:Ketrew_monitored_script.parse_log log_content in
    (* TODO Also get bstatus output *)
    begin match Option.bind log List.last with
    | None ->
      return (`Still_running run_parameters)
    | Some (`Failure (date, label, ret)) ->
      return (`Failed (run_parameters, fmt "%s returned %s" label ret))
    | Some (`Success  date) ->
      return (`Succeeded run_parameters)
    | Some other ->
      return (`Still_running run_parameters)
    end
  end
  >>< begin function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Failed_to_update _ as e -> fail e
    | `Host _ | `IO _ | `System _ as e -> 
      fail (`Failed_to_update (Error.to_string e))
    end
  end
  
let kill _ = fail (`Failed_to_kill "kill: not implemented")
