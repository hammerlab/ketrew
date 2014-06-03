
open Ketrew_pervasives
module Path = Ketrew_path

module Host = Ketrew_host

module Artifact = Ketrew_artifact


module Target = struct


  module Command = struct

    type t = {
      host: Host.t;
      action: [ `Shell of string ];
    }
    let shell ?(host=Host.localhost) s = { host; action = `Shell s}

    let get_host t = t.host

    let to_string_hum {host; action = `Shell cmd} =
      fmt "Shell[%S] on %s" cmd (Host.to_string host)

    let get_output {host; action} =
      match action with
      | `Shell cmd ->
        Host.get_shell_command_output host cmd
    let run t =
      get_output t (* TODO optimize to not record the output *)
      >>= fun (_, _) ->
      return ()


  end

  type build_process = [
    | `Artifact of Artifact.t
    | `Get_output of Command.t
    | `Direct_command of Command.t
    | `Long_running of string * string
  ]
  let nop : build_process = `Artifact (`Value `Unit)

  type submitted_state = [`Created of Time.t]
  type activated_state =
    [`Activated of Time.t * submitted_state * [ `User | `Dependency ] ]
  type run_bookkeeping = 
    { plugin_name: string; run_parameters: string; run_history: string list}
  type running_state = [ `Running of run_bookkeeping * activated_state ]
  type death_reason = [`Killed of string | `Failed of string]
  type finished_state = [ 
    | `Dead of Time.t * [activated_state | running_state] * death_reason
    | `Successful of Time.t * [activated_state | running_state ] * Artifact.t
  ]
  type workflow_state = [ submitted_state | activated_state | running_state | finished_state]
  type id = Unique_id.t
  type t = {
    id: id;
    name: string;
    persistance: [ `Input_data | `Recomputable of float | `Result ];
    metadata: Artifact.value;
    dependencies: id list;
    make: build_process;
    result_type: Artifact.Type.t;
    history: workflow_state;
  }
  let create
      ?name ?(persistance=`Input_data) ?(metadata=Artifact.unit)
      ?(dependencies=[]) ?(make=nop)
      result_type = 
    let history = `Created Time.(now ()) in
    let id = Unique_id.create () in
    { id; name = Option.value name ~default:id; persistance; metadata;
      dependencies; make; result_type; history }

  (** Create a new  target but activated from a created one; 
    raises [Invalid_argument _] if current status is not [`Created _]. *)
  let activate_exn t ~by = 
    match t.history with 
    | `Created _ as c ->
      { t with history = `Activated (Time.now (), c, by) }
    | _ -> raise (Invalid_argument "activate_exn")

  let make_succeed_exn t artifact =
    match t.history with
    | `Activated _ | `Running _ as state -> 
      { t with history = `Successful (Time.now (), state, artifact) }
    | _ -> raise (Invalid_argument "make_succeed_exn")

  let kill_exn ?(msg="") t =
    match t.history with
    | `Activated _ | `Running _ as state -> 
      { t with history = `Dead (Time.now (), state, `Killed msg) }
    | _ -> raise (Invalid_argument "kill_exn")

  let make_fail_exn ?(msg="") t =
    match t.history with
    | `Activated _ | `Running _ as state -> 
      { t with history = `Dead (Time.now (), state, `Failed msg) }
    | _ -> raise (Invalid_argument "kill_exn")

  let set_running_exn t ~plugin_name ~run_parameters =
    match t.history with
    | `Activated _ as state -> 
      { t with
        history =
          `Running ({plugin_name; run_parameters; run_history = []}, state)}
    | _ -> invalid_argument_exn ~where:"Target" (fmt "set_running_exn")

  let update_running_exn t ~run_parameters =
    match t.history with
    | `Running (bookkeeping, activation)  ->
      { t with
        history =
          `Running ({bookkeeping with 
                     run_parameters;
                     run_history = 
                       bookkeeping.run_parameters :: bookkeeping.run_history},
                    activation)}
    | _ -> invalid_argument_exn ~where:"Target" (fmt "update_running_exn")


  let active 
      ?name ?persistance ?metadata
      ?dependencies ?make
      artifact = 
    activate_exn ~by:`User (create ?name ?persistance ?metadata
                    ?dependencies ?make artifact)

  let id t : Unique_id.t = t.id
  let serialize t = Marshal.to_string t []
  let deserialize s : (t, _) Result.t =
    let open Result in
    try return (Marshal.from_string s 0)
    with e -> fail (`Target (`Deserilization (Printexc.to_string e)))

  let log t = Log.(brakets (sf "Target: %s (%s)" t.name t.id))

end

module Database = struct

  type stupid_db = (string * string) list
  type action =
    | Set of string * string
    | Sequence of action list
    | Check of string * string option
  let set ~key value = Set (key, value)
  let seq l = Sequence l
  let contains ~key v = Check (key, Some v) 
  let is_not_set key = Check (key, None)

  type t = {
    mutable db: stupid_db;
    (* mutable history: (action * stupid_db) list; *)
    parameters: string;
  }
  let create parameters = {db = []; parameters} 

  let load parameters =
    IO.read_file parameters
    >>= fun content ->
    begin try return (Marshal.from_string content 0 : t) with
    | e -> fail (`Database (`Load, parameters))
    end
  let save t =
    let content = Marshal.to_string t [] in
    IO.write_file t.parameters ~content

  let get t ~key =
    List.find_map t.db ~f:(fun (k, v) -> if k = key then Some v else None)
    |> return
  let act t ~action =
    let current = ref t.db in
    let rec go = 
      function
      | Set (key, value) ->
        current := (key, value) :: !current;
        true
      | Check (key, value) ->
        begin match List.find !current (fun (k, v) -> key = k) with
        | Some (k, v) when Some v = value -> true
        | None when value = None -> true
        | _ -> false
        end
      | Sequence actions ->
        List.for_all actions go
    in
    if go action 
    then begin
      t.db <- !current;
      save t >>= fun () ->
      return `Done
    end
    else return `Not_done

end

module Persistent_state = struct
  type t = {
    current_targets: Target.id list;
    (* keep db id of a list of all "archived" targets *)
  }
  let create () = {current_targets = [];}

  let serialize t = Marshal.to_string t []
  let unserialize s =
    try return (Marshal.from_string s 0 : t)
    with e -> fail (`Persistent_state (`Deserilization (Printexc.to_string e)))

  let add t target = { current_targets = Target.id target :: t.current_targets }

  let current_targets t = t.current_targets
end

module Configuration = struct
  type t = {
    database_parameters: string;
    persistent_state_key: string;
  }
  let default_persistent_state_key = "ketrew_persistent_state"
  let create 
      ?(persistent_state_key=default_persistent_state_key) ~database_parameters () =
    { database_parameters; persistent_state_key }
end

module Error = struct

  let to_string = function
  | `IO _ as io -> IO.error_to_string io
  | `System _ as s -> System.error_to_string s
  | `Database (`Load, path) -> fmt "DB-load: %S" path
  | `Host (`Execution (one, two, three, four)) ->
    fmt "Host-exec(%s, %s, %s, %s)" one two three four
  | `Persistent_state (`Deserilization s) ->
    fmt "Persistent_state-Deserilization: %S" s
  | `Target (`Deserilization s) -> fmt "target-deserialization: %s" s
  | `Database_unavailable s -> fmt "DB %s" s
  | `Not_implemented s -> fmt "Not-impl %S" s
  | `Missing_data p -> fmt "missing data at id: %s" p
  | `Long_running_failed_to_start (id, msg) ->
    fmt "Long running %s failed to start: %s" id msg

end
module type LONG_RUNNING = sig

  type run_parameters

  val name: string

  val serialize: run_parameters -> string
  val deserialize_exn: string -> run_parameters

  val start: run_parameters ->
    (run_parameters, [>  `Failed_to_start of string]) Deferred_result.t
  val update: run_parameters ->
    ([`Succeeded of run_parameters
     | `Failed of run_parameters * string
     | `Still_running of run_parameters],
     [> `Failed_to_update of string]) Deferred_result.t

  val kill: run_parameters ->
    ([`Killed of run_parameters],
     [> `Failed_to_kill of string]) Deferred_result.t

end
module Nohup_setsid: sig
  (* type t = Command.t *)
  include LONG_RUNNING
  val create: ?host:Host.t -> string list -> [> `Long_running of string * string ]
end = struct

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

end


(** The “application” state *)
module State = struct
  type t = {
    mutable database_handle: Database.t option;
    configuration: Configuration.t;
    long_running_plugins: (string * (module LONG_RUNNING)) list;
  }
  let default_plugins = [
    Nohup_setsid.name, (module Nohup_setsid: LONG_RUNNING);
  ]
  let create ?(plugins=default_plugins) configuration =
    return {database_handle = None; configuration;
            long_running_plugins = plugins}

  let not_implemented msg = 
    Log.(s "Going through not implemented stuff: " % s msg @ verbose);
    fail (`Not_implemented msg)

  let database t =
    match t.database_handle with
    | Some db -> return db
    | None -> 
      let path = t.configuration.Configuration.database_parameters in
      begin System.file_info ~follow_symlink:true path
        >>= function
        | `Regular_file _ ->
          Log.(s "Loading database at " % s path @ very_verbose);
          Database.load path
          >>= fun db ->
          t.database_handle <- Some db;
          return db
        | _ -> 
          Log.(s "Creating new database at " % s path @ very_verbose);
          let db = Database.create path in
          Database.save db (* should create + save be in `Database`? *)
          >>= fun () ->
          t.database_handle <- Some db;
          return db
      end

  let get_persistent t =
    database t >>= fun db ->
    begin Database.get db ~key:t.configuration.Configuration.persistent_state_key
      >>= function
      | Some persistent_serialized ->
        Persistent_state.unserialize persistent_serialized
      | None ->
        let e = Persistent_state.create () in
        return e
    end

  let save_persistent t persistent =
    database t >>= fun db ->
    let key = t.configuration.Configuration.persistent_state_key in
    let action = Database.(set ~key (Persistent_state.serialize persistent)) in
    begin Database.act db ~action
      >>= function
      | `Done -> return ()
      | `Not_done -> fail (`Database_unavailable key)
    end

  let add_or_update_target t target =
    database t
    >>= fun db ->
    begin Database.(act db (set target.Target.id Target.(serialize target)))
      >>= function
      | `Done -> return ()
      | `Not_done ->
        (* TODO: try again a few times instead of error *)
        fail (`Database_unavailable target.Target.id)
    end

  let add_target t target =
    add_or_update_target t target
    >>= fun () ->
    get_persistent t
    >>= fun persistent ->
    let new_persistent = Persistent_state.add persistent target in
    save_persistent t new_persistent
      (* TODO: remove target if this fails, or put in same transaction *)

  let get_target db id =
    Database.get db id
    >>= function
    | Some t -> of_result (Target.deserialize t)
    | None -> fail (`Missing_data id)

  let current_targets t =
    database t >>= fun db ->
    get_persistent t >>= fun persistent ->
    let target_ids = Persistent_state.current_targets persistent in
    Deferred_list.for_concurrent target_ids ~f:(get_target db)
    >>= fun (targets, errors) ->
    begin match errors with
    | [] -> return targets
    | some :: more -> fail some (* TODO do not forget other errors *)
    end

  let _check_and_activate_dependencies ~t ids =
    database t >>= fun db ->
    let what_happened = ref [] in
    let happened h = what_happened := h :: !what_happened in
    Deferred_list.while_sequential ids ~f:(fun dep ->
        get_target db dep >>= fun dependency ->
        match dependency.Target.history with
        | `Created _  ->
          let newdep =
            Target.(activate_exn dependency ~by:`Dependency) in
          add_or_update_target t newdep
          >>= fun () ->
          `Target_activated (Target.id dependency, `Dependency) |> happened;
          return `Wait
        | `Activated _ | `Running _ -> return `Wait
        | `Dead _ -> return (`Die dep)
        | `Successful _ -> return `Go)
    >>= fun statuses ->
    let happenings = List.rev !what_happened in 
    begin match statuses with
    | some_list when List.for_all some_list ~f:((=) `Go) ->
      return (`Go_now, happenings)
    | some_dependency_died
      when List.exists some_dependency_died
          ~f:(function `Die _ -> true | _ -> false) ->
      return (`Some_dependencies_died
                (List.filter_map some_dependency_died
                   ~f:(function `Die d -> Some d | _ -> None)),
              happenings)
    | no_death_but_not_all_go -> return (`Wait, happenings)
    end

  let with_plugin_or_kill_target t ~target ~plugin_name f =
    begin match 
      List.find t.long_running_plugins (fun (n, _) -> n = plugin_name)
    with
    | Some (_, m) ->
      f m
    | None -> 
      add_or_update_target t Target.(
          make_fail_exn target  
            ~msg:(fmt "Plugin not found: %s" plugin_name))
      >>= fun () ->
      return [`Target_died (Target.id target, `Plugin_not_found plugin_name)]
    end

  let _start_running_target t target =
    begin match target.Target.make with
    | `Artifact a ->
      add_or_update_target t Target.(make_succeed_exn target a)
      >>= fun () ->
      return [`Target_succeeded (Target.id target, `Artifact_literal)]
    | `Get_output cmd ->
      begin Target.Command.get_output cmd
        >>< function
        | `Ok (out, _) ->
          Log.(s "Cmd output: " % s out @ very_verbose);
          let new_target =
            Target.make_succeed_exn target (`Value (`String out)) in
          add_or_update_target t new_target
          >>= fun () ->
          return [`Target_succeeded (Target.id target, `Process_success)]
        | `Error (`Host (`Execution (where, out, err, msg))) ->
          Log.(s "Cmd error: " % s err @ very_verbose);
          add_or_update_target t Target.(
              make_fail_exn target  
                ~msg:(fmt "On %S, out: %S, err: %S, msg: %S" where out err msg))
          >>= fun () ->
          return [`Target_died (Target.id target, `Process_failure)]
      end
    | `Direct_command cmd ->
      begin Target.Command.run cmd
        >>< function
        | `Ok () -> 
          begin Artifact.is_ready target.Target.result_type
            >>= function
            | false ->
              add_or_update_target t Target.(
                  make_fail_exn target  
                    ~msg:(fmt "command %S did not create %S" 
                            (Command.to_string_hum cmd)
                            (Artifact.Type.to_string target.result_type)))
              >>= fun () ->
              return [`Target_died (Target.id target, `Process_failure)]
            | true ->
              (* result_type must be a Volume: *)
              let v = Artifact.of_type target.Target.result_type in
              add_or_update_target t Target.(make_succeed_exn target v)
              >>= fun () ->
              return [`Target_succeeded (Target.id target, `Process_success)]
          end
        | `Error (`Host (`Execution (where, out, err, msg))) ->
          Log.(s "Cmd error: " % s err @ very_verbose);
          add_or_update_target t Target.(
              make_fail_exn target  
                ~msg:(fmt "On %S, out: %S, err: %S, msg: %S" 
                        where out err msg))
          >>= fun () ->
          return [`Target_died (Target.id target, `Process_failure)]
      end
    | `Long_running (plugin_name, created_run_paramters) ->
      with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
          let module Long_running = (val m : LONG_RUNNING) in
          let c = Long_running.deserialize_exn created_run_paramters in
          begin Long_running.start c
            >>< function
            | `Ok run_parameters ->
              let run_parameters = Long_running.serialize run_parameters in
              add_or_update_target t Target.(
                  set_running_exn target ~plugin_name ~run_parameters)
              >>= fun () ->
              return [`Target_started (Target.id target, plugin_name)]
            | `Error (`Failed_to_start s) ->
              add_or_update_target t Target.(
                  make_fail_exn target  
                    ~msg:(fmt "[%s] %s" plugin_name s))
              >>= fun () ->
              return [`Target_died (Target.id target,
                                    `Failed_to_start (plugin_name, s))]
          end)
    end

  let _update_status t ~target ~bookkeeping =
    let plugin_name = bookkeeping.Target.plugin_name in
    with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
        let module Long_running = (val m : LONG_RUNNING) in
        let run_parameters =
          Long_running.deserialize_exn bookkeeping.Target.run_parameters in
        begin Long_running.update run_parameters
            >>< function
            | `Ok (`Still_running run_parameters) ->
              let run_parameters = Long_running.serialize run_parameters in
              add_or_update_target t Target.(
                  update_running_exn target ~run_parameters)
              >>= fun () ->
              return []
            | `Ok (`Succeeded run_parameters) ->
              let run_parameters = Long_running.serialize run_parameters in
              (* result_type must be a Volume: *)
              let v = Artifact.of_type target.Target.result_type in
              add_or_update_target t Target.(
                  update_running_exn target ~run_parameters
                  |> fun trgt ->  make_succeed_exn trgt v
                )
              >>= fun () ->
              return [`Target_succeeded (Target.id target, `Process_success)]
            | `Ok (`Failed (run_parameters, msg)) ->
              let run_parameters = Long_running.serialize run_parameters in
              (* result_type must be a Volume: *)
              add_or_update_target t Target.(
                  update_running_exn target ~run_parameters
                  |> fun trgt ->  make_fail_exn trgt ~msg
                )
              >>= fun () ->
              return [`Target_died (Target.id target, `Process_failure)]
            | `Error (`Failed_to_update s) ->
              add_or_update_target t Target.(
                  make_fail_exn target  
                    ~msg:(fmt "[%s] %s" plugin_name s))
              >>= fun () ->
              return [`Target_died (Target.id target,
                                    `Failed_to_update (plugin_name, s))]
        end)

  let log_what_happened =
    let open Log in
    function
    | `Target_activated (id, `Dependency) ->
      s "Target " % s id % s " activated: " % s "Dependency"
    | `Target_succeeded (id, how) ->
      s "Target " % s id % s " succeeded: " 
      % (match how with
        | `Artifact_ready -> s "Artifact_ready"
        | `Artifact_literal -> s "Artifact_literal"
        | `Process_success -> s "Process success")
    | `Target_started (id, plugin_name) ->
      s "Target " % s id % s " started " % parens (s plugin_name)
    | `Target_died (id, how) ->
      s "Target " % s id % s " died: " 
      % (match how with
        | `Dependencies_died -> s "Dependencies_died"
        | `Failed_to_start (plugin_name, msg) -> 
          sf "[%s] failed to start: %s" plugin_name msg
        | `Failed_to_update (plugin_name, msg) ->
          sf "[%s] failed to update: %s" plugin_name msg
        | `Plugin_not_found p -> sf "Plugin %S not found" p
        | `Process_failure -> s "Process_failure")

  let what_happened_to_string w =
    Log.to_string ~indent:0 ~line_width:max_int (log_what_happened w)

  let step t =
    begin
      current_targets t >>= fun targets ->
      database t >>= fun db ->
      Deferred_list.while_sequential targets ~f:(fun target ->
          match target.Target.history with
          | `Created _ -> (* nothing to do *) return []
          | `Activated _ ->
            begin Artifact.is_ready target.Target.result_type
              >>= function
              | false ->
                _check_and_activate_dependencies ~t target.Target.dependencies
                >>= fun (what_now, happenings) ->
                begin match what_now with
                | `Go_now ->
                  _start_running_target t target
                | `Some_dependencies_died l ->
                  let msg = String.concat ~sep:", " l |>
                            fmt "Dependencies died: %s" in
                  (* TODO also cancel other running dependencies? *)
                  add_or_update_target t Target.(make_fail_exn target ~msg)
                  >>= fun () ->
                  return (`Target_died (Target.id target, `Dependencies_died) :: happenings)
                | `Wait -> return happenings
                end
              | true ->
                (* result_type must be a Volume: *)
                let v = Artifact.of_type target.Target.result_type in
                add_or_update_target t Target.(make_succeed_exn target v)
                >>= fun () ->
                return [`Target_succeeded (Target.id target, `Artifact_ready)]
            end
          (* start or run *)
          | `Running (bookkeeping, _)  ->
            _update_status t ~target ~bookkeeping
          | `Dead _ | `Successful _ -> return [])
      >>| List.concat
      >>= fun what_happened ->
      Log.(s "Step: " % OCaml.list log_what_happened what_happened 
           @ very_verbose);
      return what_happened
    end 

  let get_status t id =
    database t >>= fun db ->
    get_target db id >>= fun target ->
    return target.Target.history 

end

