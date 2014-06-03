open Ketrew_pervasives

module Path = Ketrew_path

module Host = Ketrew_host

type runnning = {
  pid: int option;
  playground: Path.absolute_directory;
  script: Ketrew_monitored_script.t;
  host: Host.t;
}
type run_parameters = [
  | `Created of Host.t * string list
  | `Running of runnning
]
let running =
  function `Running r -> r 
         | _ -> invalid_argument_exn ~where:"Nohup_setsid" "running"
let created = 
  function `Created c -> c
         | _ -> invalid_argument_exn ~where:"Nohup_setsid" "created"

let serialize t = Marshal.to_string t []
let deserialize_exn s = (Marshal.from_string s 0 : run_parameters)

let name = "nohup-setsid"
let create ?(host=Host.localhost) cmds =
  `Long_running (name, `Created (host, cmds) |> serialize)

let out_file_path ~playground =
  Path.(concat playground (relative_file_exn "out"))
let err_file_path ~playground =
  Path.(concat playground (relative_file_exn "err"))

let start rp =
  (* let script = Command.monitored_script cmd in *)
  let (host, cmds) = created rp in
  begin match Host.get_fresh_playground host with
  | None -> fail (`Failed_to_start "Missing playground")
  | Some playground ->
    let monitored_script = Ketrew_monitored_script.create ~playground cmds in
    let monitored_script_path =
      Path.(concat playground (relative_file_exn "monitored_script")) in
    Host.ensure_directory host playground
    >>= fun () ->
    let content = Ketrew_monitored_script.to_string monitored_script in
    Host.put_file ~content host ~path:monitored_script_path
    >>= fun () ->
    let out = out_file_path ~playground in
    let err = err_file_path ~playground in
    let cmd =
      (* TODO find a macosx-compliant version (?) harness tmux/screen? *)
      fmt "nohup setsid bash %s > %s 2> %s &" 
        (Path.to_string_quoted monitored_script_path)
        (Path.to_string_quoted out) (Path.to_string_quoted err) in
    Host.run_shell_command host cmd
    >>= fun () ->
    Log.(s "Nohup_setsid: Ran " % s cmd @ very_verbose);
    return (`Running {pid = None; playground; 
                      script = monitored_script; host})
  end
  >>< function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Failed_to_start _ as e -> fail e
    | `Host _ | `IO _ | `System _ as e -> 
      fail (`Failed_to_start (Error.to_string e))
    end

let _pid_and_log run_parameters =
  let run = running run_parameters in
  let log_file = Ketrew_monitored_script.log_file run.script in
  let pid_file = Ketrew_monitored_script.pid_file run.script in
  begin Host.get_file run.host ~path:log_file
    >>< function
    | `Ok c -> return (Some c)
    | `Error (`Cannot_read_file _) -> return None
    | `Error (`IO _ as e) -> fail e
  end
  >>= fun log_content ->
  let log = Option.map ~f:Ketrew_monitored_script.parse_log log_content in
  begin Host.get_file run.host ~path:pid_file
    >>< function
    | `Ok c -> return (Int.of_string (String.strip ~on:`Both c))
    | `Error (`Cannot_read_file _) -> return None
    | `Error (`IO _ as e) -> fail e
  end
  >>= fun pid ->
  Log.(s "Nohup_setsid.update: got " % indent (OCaml.option s log_content)
       % s " log values and the Pid: " % OCaml.option i pid
       % sp % brakets (s "pid file: " % s (Path.to_string pid_file))
       @ very_verbose);
  return (`Pid pid, `Log log)

let _update run_parameters =
  _pid_and_log run_parameters
  >>= fun (`Pid pid, `Log log) ->
  let run = running run_parameters in
  begin match pid with
  | None ->
    (* either it didn't start yet, or it already crashed …
       should count the number of retries or compare dates and have a timeout
    *)
    (* fail (`Failed_to_update "Pid file empty") *)
    return (`Still_running run_parameters)
  | Some p ->
    let cmd = fmt "ps -p %d" p in
    Host.get_shell_command_return_value run.host cmd
    >>= fun ps_return ->
    begin match ps_return with
    | 0 -> (* most likely still running *)
      (* TOOD save pid + find other way of checking *)
      return (`Still_running run_parameters)
    | n -> (* not running, for “sure” *)
      begin match Option.bind log List.last with
      | None -> (* no log at all *)
        return (`Failed (run_parameters, "no log file"))
      | Some (`Failure (date, label, ret)) ->
        return (`Failed (run_parameters, fmt "%s returned %s" label ret))
      | Some (`Success  date) ->
        return (`Succeeded run_parameters)
      | Some other ->
        return (`Still_running run_parameters)
      end
    end
  end

let update run_parameters =
  _update run_parameters
  >>< function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Failed_to_update _ as e -> fail e
    | `Host _ | `IO _ | `System _ as e -> 
      fail (`Failed_to_update (Error.to_string e))
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
      fail (`Failed_to_kill "Pid file empty")
    | Some p ->
      let cmd = fmt "kill -- -%d" p in
      Host.run_shell_command run.host cmd
      >>= fun () ->
      return (`Killed run_parameters)
    end
  end
  >>< function
  | `Ok o -> return o
  | `Error (`Failed_to_kill _ as e) -> fail e
  | `Error (`Host _ | `IO _ | `System _ as e) ->
    fail (`Failed_to_kill (Error.to_string e))


