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
open Long_running

type t = {
  data: Persistent_data.t;
  configuration: Configuration.engine;
  host_io: Host_io.t;
}

open Logging.Global
include Make_module_error_and_info(struct
    let module_name = "Engine"
  end)

let create configuration =
  Persistent_data.create
    (Configuration.database_parameters configuration)
    (Configuration.archival_age_threshold configuration)
  >>= fun data ->
  return {data; configuration; host_io = Host_io.create ()}

let host_io t = t.host_io

let unload t =
  Persistent_data.unload t.data

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

let next_changes t = Persistent_data.next_changes t.data

module Run_automaton = struct

  let _long_running_action_error t ~error ~bookkeeping ~previous_attempts =
    let should_kill = Configuration.is_unix_ssh_failure_fatal t.configuration in
    match error, should_kill with
    | `Recoverable str, true
    | `Fatal str, _ -> `Fatal, str, bookkeeping
    | `Recoverable str, false when
        previous_attempts >=
        Configuration.maximum_successive_attempts t.configuration ->
      `Fatal, str, bookkeeping
    | `Recoverable str, false -> `Try_again, str, bookkeeping

  let _start_running_target t ~target ~bookkeeping =
    let {Target.Automaton. plugin_name; run_parameters} = bookkeeping in
    let previous_attempts =
      Target.(state target |> State.Count.consecutive_recent_attempts) in
    begin match Plugin.find_plugin plugin_name with
    | Some m ->
      let module Long_running = (val m : LONG_RUNNING) in
      begin
        begin
          try return (Long_running.deserialize_exn run_parameters)
          with e ->
            fail (_long_running_action_error t
                    ~error:(`Fatal (fmt "Deserialize-long-running: %s"
                                      (Printexc.to_string e)))
                    ~bookkeeping ~previous_attempts)
        end
        >>= fun run_parameters ->
        let host_io = t.host_io in
        Long_running.start ~host_io run_parameters
        >>< function
        | `Ok rp ->
          let run_parameters = Long_running.serialize rp in
          return { Target.Automaton. plugin_name; run_parameters}
        | `Error e ->
          fail (_long_running_action_error t ~error:e ~bookkeeping
          ~previous_attempts)
      end
    | None ->
      let error = `Recoverable (fmt "Missing plugin %S" plugin_name) in
      fail (_long_running_action_error t ~error ~bookkeeping ~previous_attempts)
    end

  let _check_and_activate_dependencies t ~dependency_of ~ids =
    Deferred_list.for_concurrent ids ~f:(fun dep ->
        Persistent_data.get_target t.data dep >>< function
        | `Ok dependency ->
          begin match Target.state dependency |> Target.State.simplify with
          | `Activable ->
            Persistent_data.activate_target t.data ~target:dependency
              ~reason:(`Dependency dependency_of)
            >>= fun () ->
            (** Even in a case where the target is not activable anymore inside
                the activate_target mutex, the function “ensures” the next
                simple-state is [`In_progress]. *)
            return (dep, `In_progress)
          | `In_progress
          | `Successful
          | `Failed as c ->
            return (dep, c)
          end
        | `Error (`Database _ as e)
        | `Error (`Database_unavailable _ as e)
        | `Error (`Fetching_node _ as e) ->
          log_error e
            Log.(s "Error while activating dependencies of " %
                 quote dependency_of % s " → "
                 % OCaml.list quote ids);
          fail e
        | `Error (`Target _ as e) -> fail e)
    >>= fun (oks, errors) ->
    let is a b =
      match b with
      | (_, s) when s = a -> true
      | _ -> false in
    let all_successful = List.for_all ~f:(is `Successful) in
    let one_failed = List.exists ~f:(is `Failed) in
    begin match (oks, errors) with
    | (oks, []) when all_successful oks -> return `All_succeeded
    | (oks, []) when one_failed oks ->
      let failed_ones = List.filter oks ~f:(is `Failed) |> List.map ~f:fst in
      log_info
        Log.(s "Targets " % OCaml.list s failed_ones
             % s " considered failed");
      return (`At_least_one_failed failed_ones)
    | (oks, []) (* equivalent to: when List.exists oks ~f:((=) `In_progress) *) ->
      return `Still_processing
    | (_, [one_error]) -> fail one_error
    | (_, errors) -> fail (`List errors)
    end


  let _attempt_to_kill t ~target ~bookkeeping =
    let {Target.Automaton. plugin_name; run_parameters} = bookkeeping in
    let previous_attempts =
      Target.(state target |> State.Count.consecutive_recent_attempts) in
    begin match Plugin.find_plugin plugin_name with
    | Some m ->
      let module Long_running = (val m : LONG_RUNNING) in
      let run_parameters = Long_running.deserialize_exn run_parameters in
      let host_io = t.host_io in
      begin Long_running.kill ~host_io run_parameters
        >>< function
        | `Ok (`Killed rp) ->
          let run_parameters = Long_running.serialize rp in
          return { Target.Automaton. plugin_name; run_parameters}
        | `Error e ->
          fail (_long_running_action_error t
                  ~error:e ~bookkeeping ~previous_attempts)
      end
    | None ->
      let error = `Recoverable (fmt "Missing plugin %S" plugin_name) in
      fail (_long_running_action_error t
              ~error ~bookkeeping ~previous_attempts)
    end

  let _check_process t ~target ~bookkeeping =
    let {Target.Automaton. plugin_name; run_parameters} = bookkeeping in
    let host_io = t.host_io in
    begin match Plugin.find_plugin plugin_name with
    | Some m ->
      let module Long_running = (val m : LONG_RUNNING) in
      let run_parameters = Long_running.deserialize_exn run_parameters in
      begin Long_running.update ~host_io run_parameters
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
          log_info Log.(s (Target.id target) % s " failed: " % s msg);
          fail (`Fatal, msg,
                { bookkeeping with
                  Target.Automaton.run_parameters = run_parameters })
        | `Error e ->
          let previous_attempts =
            Target.(state target |> State.Count.consecutive_recent_attempts) in
          fail (_long_running_action_error t
                  ~error:e ~bookkeeping ~previous_attempts)
      end
    | None ->
      let error = `Recoverable (fmt "Missing plugin %S" plugin_name) in
      let previous_attempts =
        Target.(state target |> State.Count.consecutive_recent_attempts) in
      fail (_long_running_action_error t ~error ~bookkeeping ~previous_attempts)
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
      _start_running_target t ~target ~bookkeeping
      >>< fun starting_attemp ->
      return (make_new_target ~log:("Attempt to start") starting_attemp)
    | `Eval_condition (condition, make_new_target) ->
      begin
        let host_io = t.host_io in
        Eval_condition.bool ~host_io condition
        >>< function
        | `Ok answer ->
          return (make_new_target ?log:None (`Ok answer))
        | `Error e ->
          let attempts =
            Target.(state target |> State.Count.consecutive_recent_attempts) in
          let log = Error.to_string e in
          let severity =
            match e with
            | `Volume _  -> `Fatal
            | `Host _ ->
              if attempts >=
                 Configuration.maximum_successive_attempts t.configuration
              then `Fatal else `Try_again
          in
          return (make_new_target ?log:None (`Error (severity, log)))
      end
    | `Activate (ids, make_new_target) ->
      _check_and_activate_dependencies t
        ~dependency_of:(Target.id target) ~ids
      >>= fun _ ->
      return (make_new_target ())
    | `Check_process (bookkeeping, make_new_target) ->
      _check_process t ~target ~bookkeeping
      >>< fun result ->
      return (make_new_target result)
    end

  type step_allowed_errors = [
    | `Database of Trakeva.Error.t
    | `Database_unavailable of string
    | `Fetching_node of Persistent_data.Error.fetching_node
    | `Target of [ `Deserilization of string ]
    | `List of step_allowed_errors list
  ]

  let rec try_to_fix_step_error t ~info (e: step_allowed_errors) =
    match e with
    | `Database _
    | `Database_unavailable _
    | `Target _ -> fail (`Not_fixable e)
    | `List l ->
      Deferred_list.while_sequential l ~f:(fun e ->
          try_to_fix_step_error t ~info e)
      >>= fun _ ->
      return ()
    | `Fetching_node (how, `Id id) ->
      let name =
        fmt "Placeholder-node created to fix the Engine; always fails. \
             Error: %s, Info: %s" (Error.to_string e) info in
      let new_node =
        Ketrew_pure.Target.create ()
          ~id ~name
          ~condition:`Never
          ~equivalence:`None
      in
      Printf.eprintf "Adding  %s (%s) for E: %s\n%!"
        id name (Error.to_string e);
      Persistent_data.Adding_targets.force_add_passive_target t.data new_node



  let step t: (bool, step_allowed_errors) Deferred_result.t =
  (*
    - Call Target.Automaton.step, do the action requested, call the
      target function, save-it
    - find orphans, kill them all
    - Process Murders to-do list (set as `Killing, remove from list)
    - Process to-add list
  *)
    let concurrency_number =
      Configuration.concurrent_automaton_steps t.configuration in
    let step_log = ref Display_markup.[
        Typed_log.Item.Constants.word_module, text "Engine";
        "function", text "step";
        "start-time", date_now ();
        "concurrency_number", textf "%d" concurrency_number;
      ] in
    let transitions_log = ref [] in
    let step_target target :
      (* This type annotation is for safety, we want to know if a
         new kind of error appears here: *)
      (Ketrew_pure.Target.Automaton.progress list,
       [> `Database of [> `Act of Trakeva.Action.t | `Load of string ] * string
       | `Database_unavailable of string ]) Deferred_result.t =
      begin match Target.state target with
      | s when Target.State.Is.finished s ->
        return []
      | other ->
        _process_automaton_transition t target
        >>= fun  (new_target, progress) ->
        Persistent_data.update_target t.data new_target
        >>= fun () ->
        transitions_log := !transitions_log @ Display_markup.[
            concat [
              text_of_loggable Target.log target;
              text " -> ";
              textf "%s" (Target.State.name @@ Target.state new_target);
            ]
          ];
        return [progress]
      end in
    let concurrent_step targets =
      Deferred_list.for_concurrent targets ~f:(fun target ->
          step_target target)
      >>= fun (happens, errors) ->
      begin match errors with
      | [] -> return (List.concat happens)
      | [one] -> fail one
      | more -> fail (`List more)
      end
    in
    Persistent_data.fold_active_targets t.data
      ~init:(`Happenings [], `Targets [], `Count 0)
      ~f:begin fun (`Happenings previous_happenings, `Targets targets, `Count count) ~target ->
        if List.length targets < concurrency_number - 1 then
          return (`Happenings previous_happenings, `Targets (target :: targets), `Count (count + 1))
        else (
          concurrent_step (target :: targets)
          >>= fun happens ->
          return (`Happenings (happens @ previous_happenings), `Targets [], `Count (count + 1))
        )
      end
    >>= fun (`Happenings hap, `Targets remmaining, `Count before_remaining) ->
    concurrent_step remmaining
    >>= fun more_hap ->
    step_log := !step_log @ [
        "after-automaton-steps", Display_markup.date_now ()
      ];
    let has_progressed = List.exists ~f:((=) `Changed_state) (more_hap @ hap) in
    Persistent_data.find_all_orphans t.data
    >>= fun orphans ->
    step_log := !step_log @ [
        "after-find_all_orphans", Display_markup.date_now ()
      ];
    Persistent_data.Killing_targets.add_target_ids_to_kill_list t.data
      (List.map orphans ~f:(fun tr -> Target.id tr))
    >>= fun () ->
    Persistent_data.Killing_targets.proceed_to_mass_killing t.data
    >>= fun killing_did_something ->
    Persistent_data.Adding_targets.check_and_really_add_targets t.data
    >>= fun adding_did_something ->
    step_log := 
      begin
        let open Display_markup in
        let bool b = textf "%b" b in
        !step_log @ [
          "transitions", (
            match List.length !transitions_log with
            | 0 -> text "None"
            | a_few when a_few < 10 ->
              itemize !transitions_log
            | a_lot -> textf "%d" a_lot
          );
          "has_progressed", bool has_progressed;
          "adding_did_something", bool adding_did_something;
          "killing_did_something", bool killing_did_something;
          "orphans-killed", (
            let ornb = List.length orphans in
            match ornb with
            | 0 -> text "None"
            | some when some < 20 -> 
              description_list
                (List.map orphans
                   ~f:(fun tr -> Target.id tr, text (Target.name tr)))
            | a_lot -> textf "%d" a_lot
          );
          "end-time", date_now ();
        ]
      end;
    Logger.(
      description_list !step_log |> log
    );
    return (has_progressed || adding_did_something || killing_did_something)

  let sleep_time =
    try
      if Sys.getenv "KETREW_DEBUG_SLOW_ENGINE" = "true"
      then 3.0
      else 0.1
    with _ -> 0.1

  let fix_point state =
    let rec fix_point ~count =
      step state
      >>= fun progressed ->
      (System.sleep sleep_time >>< fun _ -> return ())
      >>= fun () ->
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
  Persistent_data.get_target t.data id >>= fun target ->
  return (Target.state target)

let get_list_of_target_ids t query =
  let start_time = Time.now () in
  Persistent_data.all_visible_targets t.data
  >>= fun targets ->
  let all_targets_time = Time.now () in
  let list_of_ids =
    let open Protocol.Up_message in
    List.filter_map targets ~f:(fun target ->
        let wins () = Some (Target.id target) in
        let open Option in
        begin match query.time_constraint with
        | `All -> wins ()
        | `Not_finished_before time ->
          begin
            let st = Target.state target in
            match Target.State.finished_time st with
            | Some t when t < time -> None
            | _ -> wins ()
          end
        | `Created_after time ->
          begin
            let pt = Target.(state target |> State.passive_time) in
            match pt < time with
            | true -> None
            | false -> wins ()
          end
        | `Status_changed_since time ->
          let (`Time t, _, _) = Target.(state target |> State.summary) in
          begin match time <= t with
          | true -> wins ()
          | false -> None
          end
        end
        >>= fun _ ->
        let string_predicate ~p string =
          match p with
          | `Equals s -> String.compare s string = 0
          | `Matches rex_str ->
            begin match Re_posix.compile_pat rex_str with
            | rex -> Re.execp rex string
            | exception _ -> false
            end
        in
        let rec apply_filter =
          function
          | `True -> true
          | `False -> false
          | `And l -> List.for_all l ~f:apply_filter
          | `Or l -> List.exists l ~f:apply_filter
          | `Not f -> not (apply_filter f)
          | `Status status ->
            let state = Target.state target in
            let open Target.State in
            begin match status with
            | `Simple s -> simplify state = s
            | `Really_running ->
              Is.started_running state || Is.still_running state
              || Is.ran_successfully state
            | `Killable -> Is.killable state
            | `Dead_because_of_dependencies ->
              Is.dependency_dead state
            | `Activated_by_user ->
              Is.activated_by_user state
            end
          | `Has_tag pred ->
            let tags = Target.tags target in
            List.exists tags ~f:(string_predicate ~p:pred)
          | `Name p ->
            Target.name target |> string_predicate ~p
          | `Id p ->
            Target.id target |> string_predicate ~p
        in
        if apply_filter query.filter then wins () else None
      )
  in
  let list_of_ids_time = Time.now () in
  log_markup Display_markup.([
      "function", text "get_list_of_target_ids";
      "timing", description_list [
        "start", date start_time;
        "all-targets", time_span (all_targets_time -. start_time);
        "list-of-ids", time_span (list_of_ids_time -. all_targets_time);
        "total", time_span (list_of_ids_time -. start_time);
      ];
      "list_of_ids", textf "length: %d" (List.length list_of_ids);
    ]);
  return list_of_ids

let kill t ~id =
  Persistent_data.Killing_targets.add_target_ids_to_kill_list t.data [id]
  >>= fun () ->
  Logging.User_level_events.workflow_node_killed ~id;
  return ()

let restart_target engine target_id =
  Persistent_data.get_target engine.data target_id
  >>= fun target ->
  let new_target trgt  =
    let with_name = "Re:" ^ Target.name trgt in
    let log = fmt "Reactivation of %s" (Target.id trgt) in
    let re = Target.reactivate ~with_name ~log trgt in
    re
  in
  let this_new_target = new_target target in
  Persistent_data.Adding_targets.register_targets_to_add
    engine.data [this_new_target]
  >>= fun () ->
  let id = Target.id this_new_target in
  Logging.User_level_events.workflow_node_restarted
    ~old_id:target_id ~new_id:id ~name:(Target.name this_new_target);
  return id

let all_visible_targets t =
  Persistent_data.all_visible_targets t.data
let get_target t id =
  Persistent_data.get_target t.data id
let add_targets t l =
  let names, count =
    List.fold l ~init:([], 0) ~f:(fun (l, count) t ->
        if Target.State.Is.activated_by_user (Target.state t)
        then (Target.name t :: l, count + 1)
        else (l, count + 1)) in
  Persistent_data.Adding_targets.register_targets_to_add t.data l
  >>= fun () ->
  Logging.User_level_events.workflow_received ~names ~count;
  return ()
