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
module Database = Trakeva_sqlite
module Database_action = Trakeva.Action
module Database_error = Trakeva.Error

module Configuration = Ketrew_configuration


module Daemonize = Ketrew_daemonize

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
      Trakeva.Action.(set ~collection:"measurements" ~key value)
    in
    begin Database.act db ~action
      >>= function
      | `Done ->
        collection :=  [item `Creation]; 
        return ()
      | `Not_done -> fail (`Database_unavailable "measurements")
    end

  let load_all db =
    Database.get_all db ~collection:"measurements"
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

  let make_reponse_log response body_length =
    {Ketrew_gen_base_v0.Response_log. response; body_length}

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


let targets_collection = "targets"
let targets_to_kill_collection = "targets-to-kill"
  
let set_target_db_action target =
  let key = Target.id target in
  Database_action.(set ~collection:targets_collection ~key
                     Target.Stored_target.(of_target target |> serialize))

let run_database_action ?(msg="NO INFO") t action =
  database t
  >>= fun db ->
  Log.(s "Going to to run DB action: " % s msg @ very_verbose);
  begin Database.(act db  action)
    >>= function
    | `Done -> 
      return ()
    | `Not_done ->
      (* TODO: try again a few times instead of error *)
      fail (`Database_unavailable (fmt "running DB action: %s" msg))
  end

let add_or_update_targets t target_list =
  run_database_action t
    Database_action.(seq (List.map target_list ~f:set_target_db_action))
    ~msg:(fmt "add_or_update_targets [%s]"
            (List.map target_list ~f:Target.id |> String.concat ~sep:", "))

let add_stored_targets t st_list =
  let action =
    let open Database_action in
    List.map st_list ~f:(fun st ->
        let key = Target.Stored_target.id st in
        set ~collection:targets_collection ~key
          (Target.Stored_target.serialize st))
    |> seq in
  run_database_action t action
    ~msg:(fmt "add_or_update_targets [%s]"
            (List.map st_list ~f:Target.Stored_target.id
             |> String.concat ~sep:", "))

let add_target_ids_to_kill_list t id_list =
  let action =
    let open Database_action in
    List.map id_list ~f:(fun id ->
        set ~collection:targets_to_kill_collection ~key:id id)
    |> seq in
  run_database_action t action
    ~msg:(fmt "add_target_ids_to_kill_list [%s]"
            (String.concat ~sep:", " id_list))

let get_all_targets_to_kill t : (Target.id list, _) Deferred_result.t =
  database t
  >>= fun db ->
  Database.get_all db ~collection:targets_to_kill_collection

let remove_from_kill_list_action id =
  Database_action.(unset ~collection:targets_to_kill_collection id)


let get_target t id =
  database t >>= fun db ->
  let rec get_following_pointers ~key ~count =
    Database.get db ~collection:targets_collection ~key
    >>= begin function
    | Some serialized_stored ->
    of_result (Target.Stored_target.deserialize serialized_stored)
    >>= fun stored ->
    begin match Target.Stored_target.get_target stored with
    | `Pointer _ when count >= 30 ->
      fail (`Missing_data (fmt "there must be a loop or something (from %s)" id))
    | `Pointer key ->
      get_following_pointers ~count:(count + 1) ~key
    | `Target t -> return t
    end
    | None ->
      fail (`Missing_data (fmt "get_target %S" id))
    end
  in
  get_following_pointers ~key:id ~count:0



let fold_targets t ~init ~f =
  database t
  >>= fun db ->
  let target_stream = Database.iterator db ~collection:"targets" in
  let rec iter_stream previous =
    target_stream ()
    >>= fun stored_target ->
    begin match stored_target with
    | Some stored ->
      of_result Target.Stored_target.(deserialize stored)
      >>| Target.Stored_target.get_target
      >>= fun topt ->
      begin match topt with
      | `Pointer _ -> (* it's a pointer, keep going *) iter_stream previous
      | `Target target ->
        f previous ~target
        >>= fun next ->
        iter_stream next
      end
    | None -> return previous (* done with the stream *)
    end
  in
  iter_stream init


let alive_targets t =
  fold_targets t ~init:[] ~f:begin fun previous ~target ->
    match Target.State.simplify (Target.state target) with
    | `Failed
    | `Successful -> return previous
    | `In_progress
    | `Activable -> return (target :: previous)
  end

let all_targets t =
  fold_targets t ~init:[] ~f:begin fun previous ~target ->
    return (target :: previous)
  end

let current_targets = all_targets

let archive_target t target_id =
  assert false

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
        let compare t1 t2 = String.compare Target.(id t1) Target.(id t2)
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
    assert false
      (*
    all_targets engine >>= fun targets ->
    let archived_but_there = ref [] in
    let build_edges ~from_list ~edgify =
      Deferred_list.while_sequential from_list (fun id ->
          let actual_id = Persistent_state.follow_pointers persistent ~id in
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
        build_edges ~from_list:(Target.dependencies trgt)
          ~edgify:(fun dep -> `Dependency (trgt, dep))
        >>= fun dep_edges ->
        build_edges ~from_list:(Target.fallbacks trgt)
          ~edgify:(fun dep -> `Fallback (trgt, dep))
        >>= fun fb_edges ->
        build_edges ~from_list:(Target.success_triggers trgt)
          ~edgify:(fun dep -> `Success_triggers (trgt, dep))
        >>= fun st_edges ->
        return (dep_edges @ fb_edges @ st_edges))
    >>| List.concat
    >>= fun edges ->
    let vertices = 
      List.fold ~init:Target_set.empty (targets @ !archived_but_there) 
        ~f:Target_set.add in
    return {vertices; edges}
*)

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
    assert false
      (*
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
  *)

end 


let add_targets t tlist =
  alive_targets t
  >>= fun current_targets ->
  (* current targets are alive, so activable or in_progress *)
  let stuff_to_actually_add =
    List.fold ~init:[] tlist ~f:begin fun to_store_targets target ->
      let equivalences =
        let we_kept_so_far =
          List.filter_map to_store_targets
            ~f:(fun st ->
                match Target.Stored_target.get_target st with
                | `Target t -> Some t
                | `Pointer _ -> None) in
        List.filter (current_targets @ we_kept_so_far)
          ~f:(fun t -> Target.is_equivalent target t) in
      Log.(Target.log target % s " is "
           % (match equivalences with
             | [] -> s "pretty fresh"
             | more ->
               s " equivalent to " % OCaml.list Target.log equivalences)
           @ very_verbose);
      match equivalences with
      | [] -> 
        (Target.Stored_target.of_target target :: to_store_targets)
      | at_least_one :: _ -> 
        (Target.Stored_target.make_pointer
           ~from:target ~pointing_to:at_least_one :: to_store_targets)
    end
  in
  Log.(s "Adding new " % i (List.length stuff_to_actually_add)
       % s " things to the DB" @ verbose);
  add_stored_targets t stuff_to_actually_add



module Run_automaton = struct
  
  let _long_running_action_error t ~error ~bookkeeping =
    let should_kill = Configuration.is_unix_ssh_failure_fatal t.configuration in
    match error, should_kill with
    | `Recoverable str, true
    | `Fatal str, _ ->
      `Fatal, str, bookkeeping
    | `Recoverable str, false ->
      `Try_again, str, bookkeeping

  let _start_running_target t bookkeeping =
    let {Target.Automaton. plugin_name; run_parameters} = bookkeeping in
    begin match Ketrew_plugin.find_plugin plugin_name with
    | Some m ->
      let module Long_running = (val m : LONG_RUNNING) in
      let run_parameters =
        Long_running.deserialize_exn run_parameters in
      begin Long_running.start run_parameters
        >>< function
        | `Ok rp ->
          let run_parameters = Long_running.serialize rp in
          return { Target.Automaton. plugin_name; run_parameters}
        | `Error e ->
          fail (_long_running_action_error t ~error:e ~bookkeeping)
      end
    | None ->
      let error = `Recoverable (fmt "Missing plugin %S" plugin_name) in
      fail (_long_running_action_error t ~error ~bookkeeping)
    end

  let _check_and_activate_dependencies t ~dependency_of ~ids =
    Deferred_list.for_concurrent ids ~f:(fun dep ->
        get_target t dep >>< function
        | `Ok dependency ->
          begin match Target.state dependency |> Target.State.simplify with
          | `Activable ->
            let newdep =
              Target.(activate_exn dependency ~reason:(`Dependency dependency_of))
            in
            add_or_update_targets t [newdep]
            >>= fun () ->
            return (`In_progress)
          | `In_progress
          | `Successful
          | `Failed as c ->
            return c
          end
        | `Error (`Database _ as e)
        | `Error (`Missing_data _ as e) ->
          (* Dependency not-found => should get out of the way *)
          let errlog =
            match e with
            | `Database e -> Log.s (Database_error.to_string e)
            | `Missing_data id -> Log.(s "Missing target: " % quote id) in
          Log.(s "Error while activating dependencies: " % errlog @ error);
          return (`Failed)
        | `Error (`Persistent_state _ as e)
        | `Error (`Target _ as e) -> fail e)
    >>= begin function
    | (oks, []) when List.for_all oks ~f:((=) `Successful) ->
      return `All_succeeded
    | (oks, []) when List.exists oks ~f:((=) `Failed) ->
      return (`At_least_one_failed [])
    | (oks, []) (* equivalent to: when List.exists oks ~f:((=) `In_progress) *) ->
      return `Still_processing
    | (_, errors) ->
      Log.(s "Some errors while activating dependencies: " %n
           % separate n
             (List.map ~f:(fun x -> s (Ketrew_error.to_string x)) errors)
           @ error);
      return (`At_least_one_failed [])
    end


  (*
  let with_plugin t ~plugin_name ~f =
    begin match Ketrew_plugin.find_plugin plugin_name with
    | Some m ->
      (* let module Long_running = (val m : LONG_RUNNING) in *)
      begin f m
        >>< function
        | `Ok o -> return o
        | `Error e ->
          fail (_long_running_action_error t ~error:e ~bookkeeping)
      end
    | None ->
      let error = `Recoverable (fmt "Missing plugin %S" plugin_name) in
      fail (_long_running_action_error t ~error ~bookkeeping)
    end
  *)

  let _attempt_to_kill t ~target ~bookkeeping =
    let {Target.Automaton. plugin_name; run_parameters} = bookkeeping in
    begin match Ketrew_plugin.find_plugin plugin_name with
    | Some m ->
      let module Long_running = (val m : LONG_RUNNING) in
      let run_parameters = Long_running.deserialize_exn run_parameters in
      begin Long_running.kill run_parameters
        >>< function
        | `Ok (`Killed rp) ->
          let run_parameters = Long_running.serialize rp in
          return { Target.Automaton. plugin_name; run_parameters}
        | `Error e ->
          fail (_long_running_action_error t ~error:e ~bookkeeping)
      end
    | None ->
      let error = `Recoverable (fmt "Missing plugin %S" plugin_name) in
      fail (_long_running_action_error t ~error ~bookkeeping)
    end

  let _check_process t ~target ~bookkeeping =
    let {Target.Automaton. plugin_name; run_parameters} = bookkeeping in
    begin match Ketrew_plugin.find_plugin plugin_name with
    | Some m ->
      let module Long_running = (val m : LONG_RUNNING) in
      let run_parameters = Long_running.deserialize_exn run_parameters in
      begin Long_running.update run_parameters
        >>< function
        | `Ok (`Still_running run_parameters) ->
          let run_parameters = Long_running.serialize run_parameters in
          return (`Still_running
                    { bookkeeping with
                      Target.Automaton.run_parameters = run_parameters })
        | `Ok (`Succeeded run_parameters) ->
          let run_parameters = Long_running.serialize run_parameters in
          return (`Successful
                    { bookkeeping with
                      Target.Automaton.run_parameters = run_parameters })
        | `Ok (`Failed (run_parameters, msg)) ->
          let run_parameters = Long_running.serialize run_parameters in
          Log.(s (Target.id target) % s " failed: " % s msg @ very_verbose);
          fail (`Fatal, msg,
                { bookkeeping with
                  Target.Automaton.run_parameters = run_parameters })
        | `Error e ->
          fail (_long_running_action_error t ~error:e ~bookkeeping)
      end
    | None ->
      let error = `Recoverable (fmt "Missing plugin %S" plugin_name) in
      fail (_long_running_action_error t ~error ~bookkeeping)
    end

  let _process_automaton_transition t target =
    begin match Target.Automaton.transition target with
    | `Do_nothing make_new_target ->
      return (make_new_target ())
    | `Kill (bookkeeping, make_new_target) ->
      _attempt_to_kill t ~target ~bookkeeping
      >>< fun murder_attempt_result ->
      let new_target =
        make_new_target ~log:"Attempted to kill" murder_attempt_result in
      return new_target
    | `Check_and_activate_dependencies make_new_target ->
      let ids = Target.dependencies target in
      let log =
        fmt "Check-and-Activation of [%s]" (String.concat ~sep:", " ids)
      in
      _check_and_activate_dependencies t
        ~dependency_of:(Target.id target) ~ids
      >>| (make_new_target ~log)
    | `Start_running (bookkeeping, make_new_target) ->
      _start_running_target t bookkeeping
      >>< fun starting_attemp ->
      return (make_new_target ~log:("Attempt to start") starting_attemp)
    | `Eval_condition (condition, make_new_target) ->
      Target.Condition.eval condition
      >>| make_new_target ?log:None
    | `Activate (ids, make_new_target) ->
      _check_and_activate_dependencies t
        ~dependency_of:(Target.id target) ~ids
      >>= fun (_ : Target.Automaton.dependencies_status) ->
      return (make_new_target ())
    | `Check_process (bookkeeping, make_new_target) ->
      _check_process t ~target ~bookkeeping
      >>< fun result ->
      return (make_new_target result)
    end

  let step t: (bool, _) Deferred_result.t =
  (*
Call Target.Automaton.step, do the action requested, call the target function, save-it
Process Murders to-do list (set as `Killing, remove from list)
Process Archival to-do list (?)
Process to-add list
*)
    fold_targets t ~init:[] ~f:begin fun previous_happenings ~target ->
      _process_automaton_transition t target
      >>= fun (new_target, progress) ->
      add_or_update_targets t [new_target]
      >>= fun () ->
      Log.(s "Transition for target: "
           % Target.log target
           % s "Done: " % n
           % Target.(State.log ~depth:2 (state new_target))
           @ verbose);
      return (progress :: previous_happenings)
    end
    >>| List.exists ~f:((=) `Changed_state)
    >>= fun has_progressed ->
    get_all_targets_to_kill t
    >>= fun to_kill_list ->
    Log.(s "Going to actuall kill: "
         % OCaml.list (sf "{%S}") to_kill_list @ verbose);
    List.fold to_kill_list ~init:(return []) ~f:(fun prev id ->
        prev >>= fun prev_list ->
        get_target t id
        >>= fun target ->
        begin match Target.kill target with
        | Some t ->
          return [
            remove_from_kill_list_action id;
            set_target_db_action t;
          ]
        | None ->
          return [remove_from_kill_list_action id;]
        end
        >>= fun actions ->
        Log.(s "Going to add: "
             % OCaml.list (fun act -> s (Database_action.to_string act))
               actions @ verbose);
        return (prev_list @ actions)
      )
    >>= fun actions ->
    run_database_action t Database_action.(seq actions)
      ~msg:(fmt "killing %d targets" (List.length to_kill_list))
    >>= fun () ->
    return (has_progressed || List.length to_kill_list > 0)

  let fix_point state =
    let rec fix_point ~count =
      step state
      >>= fun progressed ->
      let count = count + 1 in
      begin match progressed with
      | true -> fix_point ~count
      | false -> return count
      end
    in
    fix_point ~count:0
    >>= fun (count) ->
    return (`Steps count)
end

    (*
  begin
    current_targets t >>= fun targets ->
    database t >>= fun db ->
    Deferred_list.for_sequential targets ~f:(fun target ->
        (* Log.(s "Engine.step dealing with " % Target.log target @ verbose); *)
        match (Target.history target) with
        | `Created _ -> (* nothing to do *) return []
        | `Activated _ ->
          begin Target.should_start target
            >>< function
            | `Ok true ->
              _check_and_activate_dependencies ~t (Target.dependencies target)
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
*)



let get_status t id =
  (* database t >>= fun db -> *)
  get_target t id >>= fun target ->
  return (Target.state target) 

let kill t ~id =
  add_target_ids_to_kill_list t [id]

let restart_target engine target_id =
  assert false
    (*
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
*)
    
module Measure = struct
  open Measurement_collection
  let incomming_request t ~connection_id ~request =
    add t.measurements 
      (`Incoming_request (make_http_request connection_id request))
  let end_of_request t ~connection_id ~request ~response_log ~body_length =
    add t.measurements
      (`End_of_request (make_http_request connection_id request,
                        make_reponse_log response_log body_length))
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
