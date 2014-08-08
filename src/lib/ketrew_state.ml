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
open Ketrew_long_running

module Path = Ketrew_path
module Host = Ketrew_host
module Artifact = Ketrew_artifact
module Target = Ketrew_target
module Database = Ketrew_database

module Configuration = Ketrew_configuration


module Daemonize = Ketrew_daemonize

module Persistent_state = struct
  type t = Ketrew_gen_base_v0_t.persistent_state = {
    current_targets: Target.id list;
    archived_targets: Target.id list;
    pointers: (Target.id * Target.id) list;
  }
  let create () = {current_targets = []; archived_targets = []; pointers = []}

  let serialize t = Ketrew_gen_versioned_j.string_of_persistent_state (`V0 t)

  let deserialize s = 
    try return (
        match Ketrew_gen_versioned_j.persistent_state_of_string s with
        | `V0 v0 -> v0
      )
    with e -> fail (`Persistent_state (`Deserilization (Printexc.to_string e)))

  let add t target = { t with current_targets = Target.id target :: t.current_targets }

  let archive t target =
    { t with
      current_targets = List.filter t.current_targets (fun i -> i <> target);
      archived_targets = target :: t.archived_targets }

  let current_targets t = t.current_targets
  let archived_targets t = t.archived_targets

  let add_pointer t ~permanent ~newcomer =
    let pointers = (newcomer, permanent) :: t.pointers in
    { t with pointers }

  let rec follow_pointers t ~id =
    match List.find t.pointers ~f:(fun (left, _) -> left = id) with
    | Some (_, right) -> follow_pointers t ~id:right
    | None -> id

end

type t = {
  mutable database_handle: Database.t option;
  configuration: Configuration.t;
  long_running_plugins: (string * (module LONG_RUNNING)) list;
}
let default_plugins = [
  Daemonize.name, (module Daemonize: LONG_RUNNING);
  Ketrew_lsf.name, (module Ketrew_lsf: LONG_RUNNING);
]
let create ?(plugins=default_plugins) configuration =
  return {database_handle = None; configuration;
          long_running_plugins = plugins}

let release t =
  match t.database_handle with
  | Some s -> Database.close s
  | None -> return ()

let with_state ?plugins ~configuration f =
  create ?plugins configuration
  >>= fun state ->
  begin try f ~state with
  | e -> 
    release state
    >>= fun () ->
    fail (`Failure (fmt "with_state: client function threw exception: %s" 
                      (Printexc.to_string e)))
  end
  >>< begin function
  | `Ok () ->
    release state
  | `Error e ->
    release state >>< fun _ ->
    fail e
  end


let not_implemented msg = 
  Log.(s "Going through not implemented stuff: " % s msg @ verbose);
  fail (`Not_implemented msg)

let database t =
  match t.database_handle with
  | Some db -> return db
  | None -> 
    let path = Configuration.database_parameters t.configuration in
    Database.load path
    >>= fun db ->
    t.database_handle <- Some db;
    return db

let get_persistent t =
  database t >>= fun db ->
  let key = Configuration.persistent_state_key t.configuration in
  begin Database.get db ~key >>= function
    | Some persistent_serialized ->
      Persistent_state.deserialize persistent_serialized
    | None ->
      let e = Persistent_state.create () in
      return e
  end

let save_persistent t persistent =
  database t >>= fun db ->
  let key = Configuration.persistent_state_key t.configuration in
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


(** This internal function gets a target value from the database {b without
    following pointers}.
*)
let _get_target_from_db db id =
  Database.get db id
  >>= function
  | Some t -> of_result (Target.deserialize t)
  | None -> fail (`Missing_data id)

let get_target t id =
  database t >>= fun db ->
  get_persistent t >>= fun persistent ->
  let actual_id = Persistent_state.follow_pointers persistent id in
  _get_target_from_db db actual_id (** After following pointers we can say we
                                       get the target from the DB. *)

let archive_target t target_id =
  get_persistent t
  >>= fun persistent ->
  let new_persistent = Persistent_state.archive persistent target_id in
  save_persistent t new_persistent

let current_targets t =
  database t >>= fun db ->
  get_persistent t >>= fun persistent ->
  let target_ids = Persistent_state.current_targets persistent in
  (** The [current_targets] are the ones we care about; not pointers which
      would create a lot useless duplicates. *)
  Deferred_list.for_concurrent target_ids ~f:(_get_target_from_db db)
  >>= fun (targets, errors) ->
  begin match errors with
  | [] -> return targets
  | some :: more -> fail some (* TODO do not forget other errors *)
  end

module Target_graph = struct
  type state = t
  type t = {
    vertices: Target.t list;
    edges: (Target.t * Target.t) list;
  }

  let get_current ~state =
    current_targets state >>= fun vertices ->
    get_persistent state >>= fun persistent ->
    let archived_but_there = ref [] in
    Deferred_list.while_sequential vertices ~f:(fun trgt ->
        Deferred_list.while_sequential trgt.Target.dependencies (fun id ->
            let actual_id = Persistent_state.follow_pointers persistent id in
            match List.find vertices ~f:(fun t -> Target.id t = actual_id) with
            | Some t -> return (trgt, t)
            | None -> 
              get_target state actual_id
              >>= fun t ->
              archived_but_there := t :: !archived_but_there;
              return (trgt, t)
          ))
    >>| List.concat
    >>= fun edges ->
    return {vertices = vertices @ !archived_but_there; edges}

  let log g =
    Log.(
      OCaml.list (fun (t1, t2) ->
          s "* " % Target.log t1 % s " → " % Target.log t2) g.edges
    )

  let vertices g = g.vertices

  let transitive_predecessors g ~target = 
    let pred g t =
      List.filter_map g.edges ~f:(fun (t1, t2) ->
          if Target.id t2 = Target.id t then Some t1 else None) in
    let rec trans_pred g t =
      let preds = pred g t in
      List.dedup (preds @ List.concat_map preds (fun v -> trans_pred g v))
    in
    trans_pred g target

end 

let add_target t target =
  add_or_update_target t target
  >>= fun () ->
  get_persistent t
  >>= fun persistent ->
  begin
    current_targets t
    >>| List.filter 
      ~f:(fun t ->
          Target.Is.(created t || activated t || running t)
          && Target.is_equivalent target t)
    >>= fun targets ->
    Log.(s "Target " % Target.log target % s " is "
         % (match targets with
           | [] -> s "pretty fresh"
           | more -> s " equivalent to " % OCaml.list Target.log targets)
         @ very_verbose);
    begin match targets with
    | [] ->
      let new_persistent = Persistent_state.add persistent target in
      save_persistent t new_persistent
    | at_least_one :: _ ->
      let new_persistent =
        Persistent_state.add_pointer persistent 
          ~permanent:at_least_one.Target.id
          ~newcomer:target.Target.id in
      save_persistent t new_persistent
    end
  end

let archived_targets t =
  database t >>= fun db ->
  get_persistent t >>= fun persistent ->
  let target_ids = Persistent_state.archived_targets persistent in
  (** Here also we don't want duplicates, [Persistent_state.archived_targets]
      are “real” targets. *)
  Deferred_list.for_concurrent target_ids ~f:(_get_target_from_db db)
  >>= fun (targets, errors) ->
  begin match errors with
  | [] -> return targets
  | some :: more -> fail some (* TODO do not forget other errors *)
  end

let is_archived t tid =
  archived_targets t
  >>= fun arch ->
  return (List.exists arch ~f:(fun x -> Target.id x = tid))

let _check_and_activate_dependencies ~t ids =
  (* database t >>= fun db -> *)
  let what_happened = ref [] in
  let happened h = what_happened := h :: !what_happened in
  Deferred_list.while_sequential ids ~f:(fun dep ->
      get_target t dep >>< function
      | `Ok dependency ->
        begin match dependency.Target.history with
        | `Created _  ->
          let newdep = Target.(activate_exn dependency ~by:`Dependency) in
          add_or_update_target t newdep
          >>= fun () ->
          `Target_activated (Target.id dependency, `Dependency) |> happened;
          return `Wait
        | `Activated _ | `Running _ -> return `Wait
        | `Dead _ -> return (`Die dep)
        | `Successful _ -> return `Go
        end
      | `Error (`Database _ as e)
      | `Error (`Missing_data _ as e) ->
        (* Dependency not-found => should get out of the way *)
        Log.(s "Error while activating dependencies: " %
             s (Ketrew_error.to_string e) @ error);
        return (`Die dep)
      | `Error (`Persistent_state _ as e)
      | `Error (`Target _ as e) -> fail e
    )
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

let host_error_to_potential_target_failure t ~target ~error =
  let should_kill = Configuration.is_unix_ssh_failure_fatal t.configuration in
  match Host.Error.classify error with
  | `Ssh | `Unix when not should_kill ->
    let e = Host.Error.log error in
    Log.(s "SSH failed, but not killing " % s (Target.id target)
         % sp % e @ warning);
    return []
  | _ ->
    add_or_update_target t Target.(
        make_fail_exn target  
          ~msg:(fmt "Host error: %s" 
                  (Host.Error.log error |> Log.to_long_string)))
    >>= fun () ->
    return [`Target_died (Target.id target, `Process_failure)]

let long_running_error_to_potential_target_failure t
    ~target ~make_error ~plugin_name e =
  let should_kill = Configuration.is_unix_ssh_failure_fatal t.configuration in
  match e, should_kill with
  | `Recoverable str, true
  | `Fatal str, _ ->
    add_or_update_target t Target.(
        make_fail_exn target  ~msg:(fmt "[%s] Error %s" plugin_name str))
    >>= fun () ->
    return [`Target_died (Target.id target,
                          make_error plugin_name str)]
  | `Recoverable str, false ->
    Log.(s "Recoverable error: " % s str @ warning);
    return []

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
      | `Error (`Host error) ->
        host_error_to_potential_target_failure ~target ~error t
    end
  | `Direct_command cmd ->
    begin Target.Command.run cmd
      >>< function
      | `Ok () -> 
        begin Target.did_ensure_condition target
          >>= function
          | false ->
            add_or_update_target t Target.(
                make_fail_exn target  
                  ~msg:(fmt "command %S did not ensure %S" 
                          (Command.to_string_hum cmd)
                          (Condition.to_string_hum target.condition)))
            >>= fun () ->
            return [`Target_died (Target.id target, `Process_failure)]
          | true ->
            add_or_update_target t Target.(make_succeed_exn target (`Value `Unit))
            >>= fun () ->
            return [`Target_succeeded (Target.id target, `Process_success)]
        end
      | `Error (`Host error) ->
        host_error_to_potential_target_failure ~target ~error t
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
          | `Error (`Fatal s) ->
            add_or_update_target t Target.(
                make_fail_exn target  
                  ~msg:(fmt "[%s] Fatal error %s" plugin_name s))
            >>= fun () ->
            return [`Target_died (Target.id target,
                                  `Long_running_unrecoverable (plugin_name, s))]
          | `Error e ->
            long_running_error_to_potential_target_failure t e ~target
              ~plugin_name
              ~make_error:(fun plugin_name s ->
                  `Long_running_unrecoverable (plugin_name, s))
        end)
  end

let _update_status t ~target ~bookkeeping =
  let plugin_name = bookkeeping.Target.plugin_name in
  with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
      let log_prefix =
        Log.(brakets (s plugin_name) % sp % Target.log target % s ": ") in
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
          begin Target.did_ensure_condition target >>= function
            | false ->
              Log.(log_prefix 
                   % s "succeeded by itself but did not ensure condition"
                   @ very_verbose);
              add_or_update_target t Target.(
                  make_fail_exn target  
                    ~msg:(fmt "the target did not ensure %s" 
                            (Condition.to_string_hum target.condition)))
              >>= fun () ->
              return [`Target_died (Target.id target, `Process_failure)]
            | true ->
              let run_parameters = Long_running.serialize run_parameters in
              (* result_type must be a Volume: *)
              add_or_update_target t Target.(
                  update_running_exn target ~run_parameters
                  |> fun trgt ->  make_succeed_exn trgt (`Value `Unit))
                >>= fun () ->
                return [`Target_succeeded (Target.id target, `Process_success)]
          end
        | `Ok (`Failed (run_parameters, msg)) ->
          let run_parameters = Long_running.serialize run_parameters in
          Log.(log_prefix % s " failed: " % s msg @ very_verbose);
          (* result_type must be a Volume: *)
          add_or_update_target t Target.(
              update_running_exn target ~run_parameters
              |> fun trgt ->  make_fail_exn trgt ~msg
            )
          >>= fun () ->
          return [`Target_died (Target.id target, `Process_failure)]
        | `Error e ->
          long_running_error_to_potential_target_failure t e ~target
            ~plugin_name
            ~make_error:(fun plugin_name s ->
                `Long_running_unrecoverable (plugin_name, s))
      end)

type happening =
  [ `Target_activated of Ketrew_target.id * [ `Dependency ]
  | `Target_died of
      Ketrew_target.id  *
      [ `Dependencies_died
      | `Plugin_not_found of string
      | `Wrong_type
      | `Killed
      | `Long_running_unrecoverable of string * string
      | `Process_failure ]
  | `Target_started of Ketrew_target.id * string
  | `Target_succeeded of
      Ketrew_target.id *
      [ `Artifact_literal | `Artifact_ready | `Process_success ] ]

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
      | `Plugin_not_found p -> sf "Plugin %S not found" p
      | `Killed -> s "Killed"
      | `Long_running_unrecoverable (plug, str) ->
        brakets (s plug) % s ": Unrecoverable error: " % s str
      | `Wrong_type ->  s "Wrong typing"
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
          begin Target.should_start target
            >>= function
            | true ->
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
            | false ->
              add_or_update_target t
                Target.(make_succeed_exn target (`Value `Unit))
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
  (* database t >>= fun db -> *)
  get_target t id >>= fun target ->
  return target.Target.history 

let kill t ~id =
  get_target t id >>= fun target ->
  begin match target.Target.history with
  | `Created c ->
    add_or_update_target t Target.(
        kill_exn (activate_exn target ~by:`Dependency)
          ~msg:(fmt "Manual killing"))
   (* we use `Dependency` because if a target is there and just "created"
      it is most likely a dependency. *)
    >>= fun () ->
    return [`Target_died (Target.id target, `Killed)]
  | `Activated _ ->
    add_or_update_target t Target.(
        kill_exn target ~msg:(fmt "Manual killing"))
    >>= fun () ->
    return [`Target_died (Target.id target, `Killed)]
  | `Running (bookkeeping, activation) ->
    let plugin_name = bookkeeping.Target.plugin_name in
    with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
        let module Long_running = (val m : LONG_RUNNING) in
        let run_parameters =
          Long_running.deserialize_exn bookkeeping.Target.run_parameters in
        Long_running.kill run_parameters
        >>< function
        | `Ok (`Killed rp) ->
          add_or_update_target t Target.(
              kill_exn target ~msg:(fmt "Manual killing (%s)" plugin_name))
          >>= fun () ->
          return [`Target_died (Target.id target, `Killed)]
        | `Error e ->
          long_running_error_to_potential_target_failure t e ~target
            ~plugin_name
            ~make_error:(fun plugin_name s ->
                `Long_running_unrecoverable (plugin_name, s))
      )
  | `Dead _ | `Successful _ ->
    return []
  end

let find_plugin ~state plugin_name =
  List.find state.long_running_plugins (fun (n, _) -> n = plugin_name)
  |> Option.map ~f:(fun (_, m) -> m)

let long_running_log ~state plugin_name content =
  begin match find_plugin ~state plugin_name with
  | Some m ->
    let module Long_running = (val m : LONG_RUNNING) in
    begin try
      let c = Long_running.deserialize_exn content in
      Long_running.log c
    with e -> 
      let log = Log.(s "Serialization exception: " % exn e) in
      Log.(log @ error);
      ["Error", log]
    end
  | None -> 
    let log = Log.(s "Plugin not found: " % sf "%S" plugin_name) in
    Log.(log @ error);
    ["Error", log]
  end

let additional_queries ~state target =
  match target.Target.make with
  | `Long_running (plugin, _) ->
    begin match Target.latest_run_parameters target with
    | Some rp ->
      begin match find_plugin ~state plugin with
      | Some m ->
        let module Long_running = (val m : LONG_RUNNING) in
        begin try
          let c = Long_running.deserialize_exn rp in
          Long_running.additional_queries c
        with e -> 
          let log = Log.(s "Serialization exception: " % exn e) in
          Log.(log @ error);
          []
        end
      | None ->
        let log = Log.(s "Plugin not found: " % sf "%S" plugin) in
        Log.(log @ error);
        []
      end
    | None ->
      Log.(s "Target has no run-parameters: " % Target.log target @ error);
      []
    end
  | other -> []

let call_query ~state ~target query =
  match target.Target.make with
  | `Long_running (plugin, _) ->
    begin match Target.latest_run_parameters target with
    | Some rp ->
      begin match find_plugin ~state plugin with
      | Some m ->
        let module Long_running = (val m : LONG_RUNNING) in
        begin try
          let c = Long_running.deserialize_exn rp in
          Long_running.query c query
        with e ->
          fail Log.(s "Run-parameters deserialization" % exn e)
        end
      | None ->
        let log = Log.(s "Plugin not found: " % sf "%S" plugin) in
        fail log
      end
    | None -> fail Log.(s "Target has no run-parameters: " % Target.log target)
    end
  | other -> fail Log.(s "Target has no queries: " % Target.log target)

let restart_target ~state target =
  current_targets state
  >>= fun targets ->
  let id_translation = ref [] in
  let new_target trgt  =
    let with_name = "Re:" ^ Target.name trgt in
    let re = Target.reactivate ~with_name trgt in
    id_translation := (Target.id trgt, Target.id re) :: !id_translation;
    re
  in
  let get_reverse_dependencies trgt =
    List.filter targets ~f:(fun t ->
        List.exists t.Target.dependencies ~f:(fun dep -> Target.id trgt = dep))
  in
  let rec explore_upper_dag trgt =
    let reverse_dependencies = get_reverse_dependencies trgt in
    Log.(s "reverse_dependencies of " % Target.log trgt % s ": "
         % OCaml.list Target.log reverse_dependencies @ very_verbose);
    trgt :: reverse_dependencies
    @ List.concat_map ~f:explore_upper_dag reverse_dependencies
  in
  let this_new_target = new_target target in
  let upper_dag =
    let its_reverse_deps = get_reverse_dependencies target in
    List.dedup ~compare:(fun ta tb -> Target.(String.compare (id ta) (id tb)))
      (List.concat_map ~f:explore_upper_dag its_reverse_deps)
    |> List.map ~f:new_target
    |> List.map ~f:(fun t ->
        let open Target in
        { t with dependencies = 
                   List.map t.dependencies ~f:(fun dep ->
                       match
                         List.find !id_translation (fun (a, _) -> a = dep)
                       with
                       | Some (_, new_id) -> new_id
                       | None -> dep) })
  in
  Deferred_list.while_sequential (this_new_target :: upper_dag) (fun trgt ->
      add_target state trgt)
  >>= fun (_ : unit list) ->
  return (this_new_target, upper_dag)
    
