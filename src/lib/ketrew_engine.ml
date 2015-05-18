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

open Ketrew_pervasives
open Ketrew_unix_io
open Ketrew_long_running

module Path = Ketrew_path
module Host = Ketrew_host
module Target = Ketrew_target
module Database = Trakeva_cache.Add(Trakeva_sqlite)
module Database_action = Trakeva.Action
module Database_error = Trakeva.Error

module Configuration = Ketrew_configuration


module Daemonize = Ketrew_daemonize



type t = {
  mutable database_handle: Database.t option;
  configuration: Configuration.engine;
  measurements: Ketrew_measurement.Collection.t;
}
let create configuration =
  return {
    database_handle = None; configuration;
    measurements = Ketrew_measurement.Collection.create ();
  }

let database t =
  match t.database_handle with
  | Some db -> return db
  | None ->
    let path = Configuration.database_parameters t.configuration in
    Database.load path
    >>= fun db ->
    t.database_handle <- Some db;
    return db

module Measurements = struct
  let flush t =
    database t
    >>= fun db ->
    let action =
      let key = Unique_id.create () in
      let value = Ketrew_measurement.Collection.serialize t.measurements in
      Trakeva.Action.(set ~collection:"measurements" ~key value)
    in
    begin Database.act db ~action
      >>= function
      | `Done ->
        Ketrew_measurement.Collection.clear t.measurements;
        return ()
      | `Not_done -> fail (`Database_unavailable "measurements")
    end
  let get_all t =
    let collection = "measurements" in
    database t
    >>= fun db ->
    Database.get_all db ~collection
    >>= fun all_keys  ->
    Deferred_list.while_sequential all_keys (fun key ->
        Database.get db ~collection ~key
        >>= function
        | Some s ->
          begin try
            return (Ketrew_measurement.Collection.deserialize_exn s)
          with e -> fail (`Deserialization (e, s))
          end
        | None -> fail (`Missing_data (fmt "Missing measurement (from %s)" key)))
    >>| Ketrew_measurement.Collection.concat
    >>= fun collection ->
    return collection
end

module Measure = struct
  open Ketrew_measurement
  let incomming_request t ~connection_id ~request =
    Collection.add t.measurements
      (Item.incoming_request (Item.make_http_request connection_id request))
  let end_of_request t ~connection_id ~request ~response_log ~body_length =
    Collection.add t.measurements
      (Item.end_of_request
         (Item.make_http_request connection_id request)
         (Item.make_reponse_log response_log body_length))
  let tag t s =
    Collection.add t.measurements (Item.tag s)
end

let unload t =
  match t.database_handle with
  | Some s ->
    Measurements.flush t
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


(* These are the names of the collections used for storing targets
   with Trakeva: *)
let passive_targets_collection = "passive-targets"
let active_targets_collection = "active-targets"
let finished_targets_collection = "finished-targets"

let set_target_db_action target =
  let key = Target.id target in
  Database_action.(set ~collection:active_targets_collection ~key
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

let move_target t ~target ~src ~dest =
  (* Caller will assume `target` is the new value; it may have changed *)
  let key = Target.id target in
  run_database_action ~msg:(fmt "move-%s-from-%s-to-%s" key src dest) t
    Database_action.(
      seq [
        unset ~collection:src key;
        set ~collection:dest ~key
          Target.Stored_target.(of_target target |> serialize)
      ])

let move_target_to_finished_collection t ~target =
  move_target t ~target
    ~src:active_targets_collection
    ~dest:finished_targets_collection

let activate_target t ~target ~reason =
  let newone = Target.(activate_exn target ~reason) in
  move_target t ~target:newone ~src:passive_targets_collection
    ~dest:active_targets_collection

let add_or_update_targets t target_list =
  run_database_action t
    Database_action.(seq (List.map target_list ~f:set_target_db_action))
    ~msg:(fmt "add_or_update_targets [%s]"
            (List.map target_list ~f:Target.id |> String.concat ~sep:", "))

let get_stored_target t key =
  database t >>= fun db ->
  Database.get db ~collection:active_targets_collection ~key
  >>= begin function
  | Some serialized_stored ->
    of_result (Target.Stored_target.deserialize serialized_stored)
  | None ->
    Database.get db ~collection:finished_targets_collection ~key
    >>= begin function
    | Some serialized_stored ->
      of_result (Target.Stored_target.deserialize serialized_stored)
    | None ->
      Database.get db ~collection:passive_targets_collection ~key
      >>= begin function
      | Some serialized_stored ->
        of_result (Target.Stored_target.deserialize serialized_stored)
      | None ->
        fail (`Missing_data (fmt "get_stored_target %S" key))
      end
    end
  end

let get_target t id =
  let rec get_following_pointers ~key ~count =
    get_stored_target t key
    >>= fun stored ->
    begin match Target.Stored_target.get_target stored with
    | `Pointer _ when count >= 30 ->
      fail (`Missing_data (fmt "there must be a loop or something (from %s)" id))
    | `Pointer key ->
      get_following_pointers ~count:(count + 1) ~key
    | `Target t -> return t
    end
  in
  get_following_pointers ~key:id ~count:0

let fold_active_targets t ~init ~f =
  database t
  >>= fun db ->
  let target_stream =
    Database.iterator db ~collection:active_targets_collection in
  let rec iter_stream previous =
    target_stream ()
    >>= begin function
    | Some key ->
      get_stored_target t key
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

let get_collections_of_targets t ~from =
  database t
  >>= fun db ->
  Deferred_list.while_sequential from ~f:(fun collection ->
      Database.get_all db ~collection
      >>= fun ids ->
      return (collection, ids))
  >>= fun col_ids ->
  Log.(s "Getting : "
       % separate (s " + ")
         (List.map col_ids ~f:(fun (col, ids) ->
              i (List.length ids) % sp % parens (quote col)
            ))
       % s " targets" @verbose);
  Deferred_list.while_sequential col_ids ~f:(fun (_, ids) ->
      Deferred_list.while_sequential ids ~f:(fun key ->
          get_stored_target t key
          >>| Target.Stored_target.get_target
          >>= fun topt ->
          begin match topt with
          | `Pointer _ ->
            return None
          | `Target target -> return (Some target)
          end)
      >>| List.filter_opt)
  >>| List.concat

(* Alive should mean In-progess or activable *)
let alive_targets t =
  get_collections_of_targets t ~from:[passive_targets_collection; active_targets_collection]
  >>= fun targets ->
  let filtered =
    List.filter_map targets ~f:(fun target ->
        match Target.State.simplify (Target.state target) with
        | `Failed
        | `Successful -> None
        | `In_progress
        | `Activable -> Some target)
  in
  return filtered

let all_targets t =
  get_collections_of_targets t ~from:[
    passive_targets_collection;
    active_targets_collection;
    finished_targets_collection;
  ]

module Killing_targets = struct
  let targets_to_kill_collection = "targets-to-kill"
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
    >>= fun all_keys ->
    (* Keys are equal to values, so we can take short cut *)
    return all_keys

  let remove_from_kill_list_action id =
    Database_action.(unset ~collection:targets_to_kill_collection id)

  let proceed_to_mass_killing t =
    get_all_targets_to_kill t
    >>= fun to_kill_list ->
    Log.(s "Going to actually kill: "
         % OCaml.list (sf "{%S}") to_kill_list @ verbose);
    List.fold to_kill_list ~init:(return []) ~f:(fun prev id ->
        prev >>= fun prev_list ->
        get_target t id
        >>= fun target ->
        let pull_out_from_passiveness =
          if Target.state target |> Target.State.Is.passive then
            [Database_action.unset ~collection:passive_targets_collection id]
          else [] in
        begin match Target.kill target with
        | Some t ->
          return (pull_out_from_passiveness @ [
            remove_from_kill_list_action id;
            set_target_db_action t;
          ])
        | None ->
          return (pull_out_from_passiveness @ [remove_from_kill_list_action id])
        end
        >>= fun actions ->
        return (prev_list @ actions)
      )
    >>= begin function
    | [] -> return false
    | actions ->
      run_database_action t Database_action.(seq actions)
        ~msg:(fmt "killing %d targets" (List.length to_kill_list))
      >>= fun () ->
      return true
    end

end



module Adding_targets = struct
  let targets_to_add_collection = "targets-to-add"
  let store_targets_to_add t t_list =
    let action =
      let open Database_action in
      List.map t_list ~f:(fun trgt ->
          let st = Target.Stored_target.of_target trgt in
          let key = Target.Stored_target.id st in
          set ~collection:targets_to_add_collection ~key
            (Target.Stored_target.serialize st))
      |> seq in
    Log.(s "Storing " % i (List.length t_list) % s " to be added at next step"
         @ verbose);
    run_database_action t action
      ~msg:(fmt "store_targets_to_add [%s]"
              (List.map t_list ~f:Target.id
               |> String.concat ~sep:", "))

  let get_all_targets_to_add t =
    database t
    >>= fun db ->
    Database.get_all db ~collection:targets_to_add_collection
    >>= fun keys ->
    Deferred_list.while_sequential keys ~f:(fun key ->
        Database.get db ~collection:targets_to_add_collection ~key
        >>= begin function
        | Some blob ->
          of_result (Target.Stored_target.deserialize blob)
        | None ->
          fail (`Missing_data (fmt "target to add: %s" key))
        end
        >>= fun st ->
        begin match Target.Stored_target.get_target st with
        | `Target t -> return t
        | `Pointer p ->
          get_target t p (* We don't use this case in practice maybe
                            it would be better to fail there *)
        end)

  let check_and_really_add_targets t =
    get_all_targets_to_add t
    >>= fun tlist ->
    begin match tlist with
    | [] -> return false
    | _ :: _ ->
      alive_targets t
      >>= fun current_living_targets ->
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
            List.filter (current_living_targets @ we_kept_so_far)
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
      let action =
        let open Database_action in
        List.map tlist ~f:(fun trgt ->
            let key = Target.id trgt in
            unset ~collection:targets_to_add_collection key)
        @ List.map stuff_to_actually_add ~f:(fun st ->
            let key = Target.Stored_target.id st in
            let collection =
              match Target.Stored_target.get_target st with
              | `Pointer _ -> finished_targets_collection
              | `Target t when Target.state t |> Target.State.Is.passive ->
                passive_targets_collection
              | `Target t -> active_targets_collection
            in
            set ~collection ~key (Target.Stored_target.serialize st))
        |> seq in
      run_database_action t action
        ~msg:(fmt "check_and_really_add_targets [%s]"
                (List.map tlist ~f:Target.id |> String.concat ~sep:", "))
      >>= fun () ->
      return true
    end


end
let add_targets = Adding_targets.store_targets_to_add




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
      begin
        begin
          try return (Long_running.deserialize_exn run_parameters)
          with e ->
            fail (_long_running_action_error t
                    ~error:(`Fatal (fmt "Deserialize-long-running: %s"
                                      (Printexc.to_string e)))
                    ~bookkeeping)
        end
        >>= fun run_parameters ->
        Long_running.start run_parameters
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
            activate_target t ~target:dependency
              ~reason:(`Dependency dependency_of)
            >>= fun () ->
            return (dep, `In_progress)
          | `In_progress
          | `Successful
          | `Failed as c ->
            return (dep, c)
          end
        | `Error (`Database _ as e)
        | `Error (`Missing_data _ as e) ->
          (* Dependency not-found => should get out of the way *)
          let errlog =
            match e with
            | `Database e -> Log.s (Database_error.to_string e)
            | `Missing_data id -> Log.(s "Missing target: " % quote id) in
          Log.(s "Error while activating dependencies: " % errlog @ error);
          Log.(s "return (dep, `Failed)" @ verbose);
          return (dep, `Failed)
        | `Error (`Target _ as e) -> fail e)
    >>= begin
      let is a b =
        match b with
        | (_, s) when s = a -> true
        | _ -> false in
      let all_successful = List.for_all ~f:(is `Successful) in
      let one_failed = List.exists ~f:(is `Failed) in
      function
      | (oks, []) when all_successful oks -> return `All_succeeded
      | (oks, []) when one_failed oks ->
        let failed_ones = List.filter oks ~f:(is `Failed) |> List.map ~f:fst in
        Log.(s "Targets " % OCaml.list s failed_ones % s " considered failed"
             @ verbose);
        Log.(s "return (`At_least_one_failed failed_ones)" @ verbose);
        return (`At_least_one_failed failed_ones)
      | (oks, []) (* equivalent to: when List.exists oks ~f:((=) `In_progress) *) ->
        return `Still_processing
      | (_, errors) ->
        Log.(s "Some errors while activating dependencies: " %n
             % separate n
               (List.map ~f:(fun x -> s (Ketrew_error.to_string x)) errors)
             @ error);
        return (`At_least_one_failed [])
    end


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
      let ids = Target.depends_on target in
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
      begin
        Ketrew_eval_condition.bool condition
        >>< function
        | `Ok answer ->
          return (make_new_target ?log:None (`Ok answer))
        | `Error e ->
          let log = Ketrew_error.to_string e in
          let severity =
            match e with
            | `Volume _  -> `Fatal
            | `Host _ -> `Try_again
          in
          return (make_new_target ?log:None (`Error (severity, log)))
      end
    | `Activate (ids, make_new_target) ->
      _check_and_activate_dependencies t
        ~dependency_of:(Target.id target) ~ids
      >>< fun (_ : (_, [`Empty]) Result.t) ->
      return (make_new_target ())
    | `Check_process (bookkeeping, make_new_target) ->
      _check_process t ~target ~bookkeeping
      >>< fun result ->
      return (make_new_target result)
    end

  let step t: (bool, _) Deferred_result.t =
  (*
    - Call Target.Automaton.step, do the action requested, call the
      target function, save-it
    - Process Murders to-do list (set as `Killing, remove from list)
    - Process Archival to-do list (?)
    - Process to-add list
  *)
    fold_active_targets t ~init:[] ~f:begin fun previous_happenings ~target ->
      begin match Target.state target with
      | s when Target.State.Is.finished s ->
        move_target_to_finished_collection t ~target
        >>= fun () ->
        return [] (* moving to the finsihed-set is not a worthy “change” *)
      | other ->
        _process_automaton_transition t target
        >>< function
        | `Ok (new_target, progress) ->
          add_or_update_targets t [new_target]
          >>= fun () ->
          Log.(s "Transition for target: "
               % Target.log target
               % s "Done: " % n
               % Target.(State.log ~depth:2 (state new_target))
               @ very_verbose);
          return (progress :: previous_happenings)
        | `Error `Empty_should_not_exist ->
          return []
      end
    end
    >>| List.exists ~f:((=) `Changed_state)
    >>= fun has_progressed ->
    Killing_targets.proceed_to_mass_killing t
    >>= fun killing_did_something ->
    Adding_targets.check_and_really_add_targets t
    >>= fun adding_did_something ->
    return (has_progressed || adding_did_something || killing_did_something)

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


let get_status t id =
  get_target t id >>= fun target ->
  return (Target.state target)

let get_list_of_target_ids t query =
  all_targets t
  >>= fun targets ->
  let list_of_ids =
    match query with
    | `All -> List.map targets ~f:Ketrew_target.id
    | `Not_finished_before time ->
      Log.(s "Getting targets not-finished-before: " % Time.log time @ verbose);
      List.filter_map targets ~f:(fun t ->
          let st = Ketrew_target.state t in
          match Ketrew_target.State.finished_time st with
          | Some t when t < time -> None
          | _ -> Some (Ketrew_target.id t))
    | `Created_after time ->
      Log.(s "Getting targets created after: " % Time.log time @ verbose);
      List.filter_map targets ~f:(fun t ->
          let pt = Ketrew_target.(state t |> State.passive_time) in
          match pt < time with
          | true -> None
          | false -> Some (Ketrew_target.id t))
  in
  return list_of_ids

let kill t ~id =
  Killing_targets.add_target_ids_to_kill_list t [id]

let restart_target engine target_id =
  get_target engine target_id
  >>= fun target ->
  let new_target trgt  =
    let with_name = "Re:" ^ Target.name trgt in
    let log = fmt "Reactivation of %s" (Target.id trgt) in
    let re = Target.reactivate ~with_name ~log trgt in
    re
  in
  let this_new_target = new_target target in
  add_targets engine [this_new_target]
  >>= fun () ->
  let id = Target.id this_new_target in
  return id
