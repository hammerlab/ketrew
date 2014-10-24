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
  type t = Ketrew_gen_base_v0.Persistent_state.t = {
    current_targets: Target.id list;
    archived_targets: Target.id list;
    pointers: (Target.id * Target.id) list;
  }
  let create () = {current_targets = []; archived_targets = []; pointers = []}

  include
    Json.Make_versioned_serialization
      (Ketrew_gen_base_v0.Persistent_state)
      (Ketrew_gen_versioned.Persistent_state)

  let deserialize s = 
    try return (deserialize_exn s)
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

module Measurement_collection = struct
  type item = Ketrew_gen_base_v0.Measurement_item.t
  type t = Ketrew_gen_base_v0.Measurement_collection.t ref
  let item content = 
    let open Ketrew_gen_base_v0.Measurement_item in
    { time = Time.(now ()); content}
  let create () = ref [item `Creation]
  let add collection log = 
    collection := item log :: !collection

  include Json.Make_versioned_serialization
      (struct 
        type t = Ketrew_gen_base_v0.Measurement_collection.t
      end)
      (Ketrew_gen_versioned.Measurement_collection)

  let flush collection db =
    let action =
      let key = Unique_id.create () in
      let value = serialize !collection in
      Ketrew_database.(set ~collection:"measurements" ~key value)
    in
    begin Ketrew_database.act db ~action
      >>= function
      | `Done ->
        collection :=  [item `Creation]; 
        return ()
      | `Not_done -> fail (`Database_unavailable "measurements")
    end

  let load_all db =
    Ketrew_database.get_all db ~collection:"measurements"
    >>= fun all_strings ->
    Deferred_list.while_sequential all_strings (fun s ->
        try return (deserialize_exn s)
        with e -> fail (`Deserialization (e, s)))
    >>| List.concat
    >>= fun collection ->
    return collection

  let make_http_request connection_id request =
    let meth = Cohttp.Request.meth request in
    let uri = Cohttp.Request.uri request |> Uri.to_string in
    {Ketrew_gen_base_v0.Http_request. connection_id;  meth; uri}

end


type t = {
  mutable database_handle: Database.t option;
  configuration: Configuration.engine;
  measurements: Measurement_collection.t;
}
let create configuration =
  return {
    database_handle = None; configuration;
    measurements = Measurement_collection.create ();
  }

let unload t =
  match t.database_handle with
  | Some s ->
    Measurement_collection.flush t.measurements s
    >>= fun () ->
    Database.close s
  | None -> return ()

let load ~configuration =
  create configuration

let with_engine ~configuration f =
  create configuration
  >>= fun engine ->
  begin try f ~engine with
  | e -> 
    unload engine
    >>= fun () ->
    fail (`Failure (fmt "with_state: client function threw exception: %s" 
                      (Printexc.to_string e)))
  end
  >>< begin function
  | `Ok () ->
    unload engine
  | `Error e ->
    unload engine >>< fun _ ->
    fail e
  end

let configuration t = t.configuration


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
    | `Done -> 
      return ()
    | `Not_done -> fail (`Database_unavailable key)
  end

let set_target_db_action target =
  Database.(set ~collection:"targets"
              ~key:target.Target.id Target.(serialize target))

let add_or_update_target t target =
  database t
  >>= fun db ->
  begin Database.(act db (set_target_db_action target))
    >>= function
    | `Done -> 
      return ()
    | `Not_done ->
      (* TODO: try again a few times instead of error *)
      fail (`Database_unavailable target.Target.id)
  end


(** This internal function gets a target value from the database {b without
    following pointers}.
*)
let _get_target_no_pointers t id =
  database t >>= fun db ->
  Database.get db ~collection:"targets" ~key:id
  >>= function
  | Some s -> 
    of_result (Target.deserialize s)
  | None -> fail (`Missing_data id)

let get_target t id =
  database t >>= fun db ->
  get_persistent t >>= fun persistent ->
  let actual_id = Persistent_state.follow_pointers persistent id in
  _get_target_no_pointers t actual_id (** After following pointers we can say we
                                          get the target from the DB. *)

let current_targets t =
  database t >>= fun db ->
  get_persistent t >>= fun persistent ->
  let target_ids = Persistent_state.current_targets persistent in
  (** The [current_targets] are the ones we care about; not pointers which
      would create a lot useless duplicates. *)
  Deferred_list.for_concurrent target_ids ~f:(_get_target_no_pointers t)
  >>= fun (targets, errors) ->
  begin match errors with
  | [] -> return targets
  | some :: more -> fail some (* TODO do not forget other errors *)
  end

let archive_target t target_id =
  get_target t target_id (* Assert this targets "exists" *)
  >>= fun actual_target ->
  get_persistent t
  >>= fun persistent ->
  let target_ids = Persistent_state.current_targets persistent in
  begin match List.exists target_ids ~f:((=) (Target.id actual_target)) with
  | true ->
    let new_persistent = Persistent_state.archive persistent target_id in
    save_persistent t new_persistent
    >>= fun () ->
    return [`Target_archived target_id]
  | false -> return [] (* already archived no-op *)
  end


module Target_graph = struct
  type engine = t
  type arrow = Target.t * Target.t
  type edge = [
    | `Dependency of arrow
    | `Fallback of arrow
    | `Success_triggers of arrow
  ]
  module Target_set = struct
    include Set.Make(struct
        type t = Target.t
        let compare t1 t2 = String.compare t1.Target.id t2.Target.id
      end)
    let mem set v = mem v set
    let add set v = add v set
    let remove set v = remove v set
    let find t v = try Some (find v t) with _ -> None
    let exists t ~f = exists f t
    let fold t ~f ~init = 
      fold (fun elt a -> f a elt) t init

  end
  type t = {
    vertices: Target_set.t;
    edges: edge list;
  }

  let get_current ~engine =
    current_targets engine >>= fun targets ->
    get_persistent engine >>= fun persistent ->
    let archived_but_there = ref [] in
    let build_edges ~from_list ~edgify =
      Deferred_list.while_sequential from_list (fun id ->
          let actual_id = Persistent_state.follow_pointers persistent id in
          match List.find targets ~f:(fun t -> Target.id t = actual_id) with
          | Some t -> return (edgify t)
          | None -> 
            get_target engine actual_id
            >>= fun t ->
            archived_but_there := t :: !archived_but_there;
            return (edgify t)
        )
    in
    Deferred_list.while_sequential targets ~f:(fun trgt ->
        build_edges ~from_list:trgt.Target.dependencies
          ~edgify:(fun dep -> `Dependency (trgt, dep))
        >>= fun dep_edges ->
        build_edges ~from_list:trgt.Target.if_fails_activate
          ~edgify:(fun dep -> `Fallback (trgt, dep))
        >>= fun fb_edges ->
        build_edges ~from_list:trgt.Target.success_triggers
          ~edgify:(fun dep -> `Success_triggers (trgt, dep))
        >>= fun st_edges ->
        return (dep_edges @ fb_edges @ st_edges))
    >>| List.concat
    >>= fun edges ->
    let vertices = 
      List.fold ~init:Target_set.empty (targets @ !archived_but_there) 
        ~f:Target_set.add in
    return {vertices; edges}

  let log_arrow verb (t1, t2) =
    let open Log in
    Target.log t1 % sp % s verb % sp % Target.log t2

  let log_edge = function
  | `Dependency a -> log_arrow "depends on" a
  | `Fallback a -> log_arrow "fallsback with" a
  | `Success_triggers a -> log_arrow "triggers" a

  let log g =
    Log.(
      separate n (List.map g.edges ~f:(fun e -> s "* " % log_edge e))
    )

  let vertices g = g.vertices

  let transitive_sub_graph g ~target = 
    let connections t ~available =
      List.filter_map g.edges ~f:(function
        | `Dependency (t1, t2)
        | `Fallback (t1, t2)
        | `Success_triggers (t1, t2) ->
          if Target.id t1 = Target.id t && Target_set.mem available t2
          then Some t2
          else if Target.id t2 = Target.id t && Target_set.mem available t1 
          then Some t1
          else None
        ) in
    let rec trans_connections t available acc =
      match connections t available with
      | [] ->  acc
      | conns -> 
        let new_available = 
          List.fold conns ~init:available ~f:Target_set.remove in
        List.map conns (fun conn ->
            Target_set.add 
              (trans_connections conn new_available acc)
              conn
          )
        |> List.reduce ~f:Target_set.union
        |> Option.value ~default:Target_set.empty
    in
    trans_connections target (Target_set.remove g.vertices target) 
      Target_set.empty

  let targets_to_clean_up graph how_much =
    let vertices = vertices graph in
    Log.(s "Graph: " % log graph @ normal);
    let to_kill =
      (* a target should be killed if it is "created" and:

         - no other target that is activated transitively dependends on it
         - no other target that is activated or running can
         trigger it because it is a fallback or success-trigger,
         or a dependency of one of these, and so on transitively …

         It's so complicated that for now we take a simpler but
         conservative approach:

         - grab the whole transitive sub-graph, 
         - check that no-one is activated or running in there.

         This is not “exact” (some “unreachable” targets may remain).
      *)
      Target_set.fold vertices ~init:[] ~f:(fun pred target ->
          if Ketrew_target.Is.created target
          then
            let sub_graph = transitive_sub_graph graph ~target in
            Log.(s "Subgraph of " % Target.log target % n
                 % (Target_set.fold ~init:[] sub_graph ~f:(fun l e -> e :: l)
                    |> OCaml.list Target.log)
                 @ very_verbose);
            if Target_set.for_all 
                (fun trgt ->
                   not (Ketrew_target.Is.activated trgt)
                   && not (Ketrew_target.Is.running trgt))
                sub_graph
            then (Ketrew_target.id target :: pred)
            else pred
          else
            pred)
    in
    let to_archive =
      (* A target that is just-killed, or finished depending on `how_much` *)
      Target_set.fold vertices ~init:[] ~f:(fun pred trgt ->
          match how_much with
          | `Soft when Ketrew_target.Is.successful trgt ->
            (Ketrew_target.id trgt :: pred)
          | `Hard when Ketrew_target.Is.finished trgt ->
            (Ketrew_target.id trgt :: pred)
          | other -> pred)
    in
    (`To_kill to_kill, `To_archive to_archive)


end 


let add_targets t tlist =
  get_persistent t
  >>= fun persistent ->
  current_targets t
  >>= fun current_targets ->
  let stuff_to_do =
    List.fold ~init:([], persistent) tlist ~f:(fun (targets, persistent) target ->
        let equivalences =
          List.filter (current_targets @ targets)
            ~f:(fun t ->
                Target.Is.(created t || activated t || running t)
                && Target.is_equivalent target t) in
        Log.(Target.log target % s " is "
             % (match equivalences with
               | [] -> s "pretty fresh"
               | more ->
                 s " equivalent to " % OCaml.list Target.log equivalences)
             @ very_verbose);
        match equivalences with
        | [] -> 
          (target :: targets, Persistent_state.add persistent target)
        | at_least_one :: _ -> 
          (targets,
           Persistent_state.add_pointer persistent 
             ~permanent:at_least_one.Target.id
             ~newcomer:target.Target.id))
  in
  let targets_to_add, new_persistent = stuff_to_do in
  let transaction = 
    let open Database in
    let persistent_action =
      let key = Configuration.persistent_state_key t.configuration in
      (set ~key (Persistent_state.serialize new_persistent)) in
    Database.(seq (
        persistent_action
        :: List.map targets_to_add ~f:set_target_db_action
      ))
  in
  database t
  >>= fun db ->
  Log.(s "Going to perform: " % Database.log_action transaction @ verbose);
  begin
    Database.(act db transaction)
    >>= function
    | `Done -> return ()
    | `Not_done ->
      (* TODO: try again a few times instead of error *)
      fail (`Database_unavailable "transaction failed")
  end

let archived_targets t =
  database t >>= fun db ->
  get_persistent t >>= fun persistent ->
  let target_ids = Persistent_state.archived_targets persistent in
  (** Here also we don't want duplicates, [Persistent_state.archived_targets]
      are “real” targets. *)
  Deferred_list.for_concurrent target_ids ~f:(_get_target_no_pointers t)
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
        let errlog =
          match e with
          | `Database _ as e -> Ketrew_database.log_error e
          | `Missing_data id -> Log.(s "Missing target: " % quote id) in
        Log.(s "Error while activating dependencies: " % errlog @ error);
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

let make_target_die ?explanation t ~target ~reason =
  let msg =
    begin match reason with
    | `Dependencies_died -> "Dependencies died"
    | `Plugin_not_found p -> fmt "Plugin not found: %S" p
    | `Process_failure -> "Process Failure"
    | `Long_running_unrecoverable (plugin_name, s) ->
      fmt "[%s] Unrecoverable: %s" plugin_name s
    | `Killed -> "Killed"
    end
    ^ Option.value_map ~default:"" explanation ~f:(fmt ": %s")
  in
  let new_target =
    match reason with
    | `Killed -> Target.kill_exn target ~msg
    | other -> Target.make_fail_exn target ~msg in
  add_or_update_target t new_target
  >>= fun () ->
  Deferred_list.while_sequential target.Target.if_fails_activate ~f:(fun tid ->
      get_target t tid
      >>= fun trgt ->
      (* Here two targets with the same fallback could activate the same target
         concurrently, we don't care, target activation just means
         “change the state in the DB”. *)
      begin match trgt.Target.history with
      | `Created _ ->
        let newdep = Target.(activate_exn trgt ~by:`Fallback) in
        add_or_update_target t newdep
        >>= fun () ->
        return (Some (`Target_activated (tid, `Fallback)))
      | other -> return None
      end)
  >>| List.filter_opt
  >>= fun happens ->
  return (`Target_died (Target.id target, reason) :: happens)

let make_target_succeed t target ~why ~artifact =
  add_or_update_target t Target.(make_succeed_exn target artifact)
  >>= fun () ->
  Deferred_list.while_sequential target.Target.success_triggers ~f:(fun tid ->
      get_target t tid
      >>= fun trgt ->
      begin match trgt.Target.history with
      | `Created _ ->
        let newdep = Target.(activate_exn trgt ~by:`Success_trigger) in
        add_or_update_target t newdep
        >>= fun () ->
        return (Some (`Target_activated (tid, `Success_trigger)))
      | other -> return None
      end)
  >>| List.filter_opt
  >>= fun happens ->
  return (`Target_succeeded (Target.id target, why) :: happens)

let with_plugin_or_kill_target t ~target ~plugin_name f =
  match Ketrew_plugin.find_plugin plugin_name with
  | Some m -> f m
  | None -> 
    make_target_die t ~target ~reason:(`Plugin_not_found plugin_name)

let host_error_to_potential_target_failure t ~target ~error =
  let should_kill = Configuration.is_unix_ssh_failure_fatal t.configuration in
  match Host.Error.classify error with
  | `Ssh | `Unix when not should_kill ->
    let e = Host.Error.log error in
    Log.(s "SSH failed, but not killing " % s (Target.id target)
         % sp % e @ warning);
    add_or_update_target t Target.({
        target  with log = (Time.now (), 
                            fmt "Non fatal error: %s" (Log.to_long_string e)) 
                           :: target.log })
    >>= fun () ->
    return []
  | _ ->
    make_target_die t ~target ~reason:`Process_failure
      ~explanation:(fmt "Host error: %s" 
                      (Host.Error.log error |> Log.to_long_string))

let long_running_error_to_potential_target_failure t
    ~target ~make_error ~plugin_name e =
  let should_kill = Configuration.is_unix_ssh_failure_fatal t.configuration in
  match e, should_kill with
  | `Recoverable str, true
  | `Fatal str, _ ->
    make_target_die t ~target ~reason:(make_error plugin_name str)
  | `Recoverable str, false ->
    add_or_update_target t Target.({
        target  with log = (Time.now (),
                            fmt "Non fatal long-running error: %s" str)
                           :: target.log })
    >>= fun () ->
    Log.(s "Recoverable error: " % s str @ warning);
    return []

let _start_running_target t target =
  begin match target.Target.make with
  | `Artifact a ->
    begin Target.did_ensure_condition target
      >>= function
      | false ->
        make_target_die t ~target ~reason:(`Process_failure)
          ~explanation:
            (fmt "artifact-literal %S did not ensure %S" 
               (Ketrew_artifact.log a |> Log.to_long_string)
               (Option.value_map ~default:"None-condition" 
                  ~f:Target.Condition.to_string_hum target.Target.condition))
      | true ->
        make_target_succeed t target ~why:`Artifact_literal ~artifact:a
    end
  | `Direct_command cmd ->
    begin Target.Command.run cmd
      >>< function
      | `Ok () -> 
        begin Target.did_ensure_condition target
          >>= function
          | false ->
            make_target_die t ~target ~reason:(`Process_failure)
              ~explanation:Target.(
                  fmt "command %S did not ensure %S" 
                    (Command.to_string_hum cmd)
                    (Option.value_map ~default:"None-condition" 
                       ~f:Condition.to_string_hum target.condition))
          | true ->
            make_target_succeed t target ~artifact:(`Value `Unit)
              ~why:`Process_success
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
            make_target_die t ~target
              ~reason:(`Long_running_unrecoverable (plugin_name, s))
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
              make_target_die t ~target ~reason:(`Process_failure)
                ~explanation:Target.(
                    fmt "the target did not ensure %s" 
                      (Option.value_map ~default:"None-condition" 
                         ~f:Condition.to_string_hum target.condition))
            | true ->
              let run_parameters = Long_running.serialize run_parameters in
              (* result_type must be a Volume: *)
              make_target_succeed t 
                (Target.update_running_exn target ~run_parameters)
                ~artifact:(`Value `Unit)
                ~why:`Process_success
          end
        | `Ok (`Failed (run_parameters, msg)) ->
          let run_parameters = Long_running.serialize run_parameters in
          Log.(log_prefix % s " failed: " % s msg @ very_verbose);
          (* result_type must be a Volume: *)
          make_target_die t  ~reason:(`Process_failure)
            ~target:Target.(update_running_exn target ~run_parameters)
        | `Error e ->
          long_running_error_to_potential_target_failure t e ~target
            ~plugin_name
            ~make_error:(fun plugin_name s ->
                `Long_running_unrecoverable (plugin_name, s))
      end)

type happening = Ketrew_gen_base_v0.Happening.t

let log_what_happened =
  let open Log in
  function
  | `Error e -> s "Error " % s e
  | `Target_activated (id, by) ->
    s "Target " % s id % s " activated: " %
    (match by with
     | `Dependency -> s "Dependency"
     | `Fallback -> s "Fallback"
     | `Success_trigger -> s "Success-trigger")
  | `Target_succeeded (id, how) ->
    s "Target " % s id % s " succeeded: " 
    % (match how with
      | `Artifact_ready -> s "Artifact_ready"
      | `Artifact_literal -> s "Artifact_literal"
      | `Process_success -> s "Process success")
  | `Target_started (id, plugin_name) ->
    s "Target " % s id % s " started " % parens (s plugin_name)
  | `Target_archived id ->
    s "Target " % s id % s " was archived "
  | `Target_created id ->
    s "Target " % s id % s " was created "
  | `Target_died (id, how) ->
    s "Target " % s id % s " died: " 
    % (match how with
      | `Dependencies_died -> s "Dependencies_died"
      | `Plugin_not_found p -> sf "Plugin %S not found" p
      | `Killed -> s "Killed"
      | `Long_running_unrecoverable (plug, str) ->
        brakets (s plug) % s ": Unrecoverable error: " % s str
      | `Process_failure -> s "Process_failure")

let what_happened_to_string w =
  Log.to_string ~indent:0 ~line_width:max_int (log_what_happened w)

let step t: (happening list, _) Deferred_result.t =
  begin
    current_targets t >>= fun targets ->
    database t >>= fun db ->
    Deferred_list.for_concurrent targets ~f:(fun target ->
        (* Log.(s "Engine.step dealing with " % Target.log target @ verbose); *)
        match target.Target.history with
        | `Created _ -> (* nothing to do *) return []
        | `Activated _ ->
          begin Target.should_start target
            >>< function
            | `Ok true ->
              _check_and_activate_dependencies ~t target.Target.dependencies
              >>= fun (what_now, happenings) ->
              begin match what_now with
              | `Go_now ->
                _start_running_target t target
              | `Some_dependencies_died l ->
                let explanation = String.concat ~sep:", " l ^ " died" in
                make_target_die t ~target ~reason:(`Dependencies_died)
                  ~explanation
                >>= fun happened ->
                return (happened @ happenings)
              | `Wait -> return happenings
              end
            | `Ok false ->
              make_target_succeed t  target
                ~artifact:(`Value `Unit)
                ~why:`Artifact_ready
            | `Error (`Volume (`No_size log)) ->
              make_target_die t ~target ~reason:(`Process_failure)
                ~explanation:Log.(to_long_string
                                    (s "No-size for volume, " % log))
              >>= fun happened ->
              return happened
            | `Error (`Host error) ->
              host_error_to_potential_target_failure ~target ~error t
          end
        (* start or run *)
        | `Running (bookkeeping, _)  ->
          _update_status t ~target ~bookkeeping
        | `Dead _ | `Successful _ -> return [])
    >>= fun (what_happened, errors) ->
    let what_happened = 
      List.map errors ~f:(fun e -> `Error (Ketrew_error.to_string e))
      @ List.concat  what_happened
      |> List.dedup
    in
    Log.(s "Step: " % OCaml.list log_what_happened what_happened 
         @ very_verbose);
    return what_happened
  end 


let fix_point state =
  let rec fix_point ~count history =
    step state
    >>= fun what_happened ->
    let count = count + 1 in
    begin match history with
    | _ when what_happened = [] ->
      return (count, what_happened :: history)
    | previous :: _ when previous = what_happened ->
      return (count, what_happened :: history)
    | _ -> fix_point ~count (what_happened :: history)
    end
  in
  fix_point ~count:0 []
  >>= fun (count, happened) ->
  return (`Steps count, happened)

let get_status t id =
  (* database t >>= fun db -> *)
  get_target t id >>= fun target ->
  return target.Target.history 

let kill t ~id =
  get_target t id >>= fun target ->
  begin match target.Target.history with
  | `Created c ->
    make_target_die t ~reason:(`Killed)
      ~target:(Target.activate_exn target ~by:`Dependency)
   (* we use `Dependency` because if a target is there and just "created"
      it is most likely a dependency. *)
  | `Activated _ ->
    make_target_die t ~target ~reason:(`Killed)
  | `Running (bookkeeping, activation) ->
    let plugin_name = bookkeeping.Target.plugin_name in
    with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
        let module Long_running = (val m : LONG_RUNNING) in
        let run_parameters =
          Long_running.deserialize_exn bookkeeping.Target.run_parameters in
        Long_running.kill run_parameters
        >>< function
        | `Ok (`Killed rp) ->
          make_target_die t ~target ~reason:`Killed
        | `Error e ->
          long_running_error_to_potential_target_failure t e ~target
            ~plugin_name
            ~make_error:(fun plugin_name s ->
                `Long_running_unrecoverable (plugin_name, s))
      )
  | `Dead _ | `Successful _ ->
    return []
  end

let restart_target engine target_id =
  current_targets engine
  >>= fun targets ->
  get_target engine target_id
  >>= fun target ->
  let new_target trgt  =
    let with_name = "Re:" ^ Target.name trgt in
    let re = Target.reactivate ~with_name trgt in
    re
  in
  let this_new_target = new_target target in
  add_targets engine [this_new_target]
  >>= fun () ->
  let id = Target.id this_new_target in
  return ([`Target_created id; `Target_activated (id, `Dependency)]: happening list)
    
module Measure = struct
  open Measurement_collection
  let incomming_request t ~connection_id ~request =
    add t.measurements 
      (`Incoming_request (make_http_request connection_id request))
  let end_of_request t ~connection_id ~request =
    add t.measurements
      (`End_of_request (make_http_request connection_id request))
  let tag t s =
    add t.measurements (`Tag s)
end
module Measurements = struct

  let flush t =
    database t
    >>= fun db ->
    Measurement_collection.flush t.measurements db

  let get_all t =
    database t
    >>= fun db ->
    Measurement_collection.load_all db
end
