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
}
let create configuration =
  Persistent_data.create (Configuration.database_parameters configuration)
  >>= fun data ->
  return {data; configuration;}


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


let not_implemented msg =
  Log.(s "Going through not implemented stuff: " % s msg @ verbose);
  fail (`Not_implemented msg)




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
        Long_running.start run_parameters
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
            | `Database e -> Log.s (Trakeva.Error.to_string e)
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
               (List.map ~f:(fun x -> s (Error.to_string x)) errors)
             @ error);
        return (`At_least_one_failed [])
    end


  let _attempt_to_kill t ~target ~bookkeeping =
    let {Target.Automaton. plugin_name; run_parameters} = bookkeeping in
    let previous_attempts =
      Target.(state target |> State.Count.consecutive_recent_attempts) in
    begin match Plugin.find_plugin plugin_name with
    | Some m ->
      let module Long_running = (val m : LONG_RUNNING) in
      let run_parameters = Long_running.deserialize_exn run_parameters in
      begin Long_running.kill run_parameters
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
    begin match Plugin.find_plugin plugin_name with
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
        Eval_condition.bool condition
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
    Persistent_data.fold_active_targets t.data ~init:[]
      ~f:begin fun previous_happenings ~target ->
        begin match Target.state target with
        | s when Target.State.Is.finished s ->
          Persistent_data.move_target_to_finished_collection t.data ~target
          >>= fun () ->
          return [] (* moving to the finsihed-set is not a worthy “change” *)
        | other ->
          _process_automaton_transition t target
          >>< function
          | `Ok (new_target, progress) ->
            Persistent_data.update_target t.data new_target
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
    Persistent_data.Killing_targets.proceed_to_mass_killing t.data
    >>= fun killing_did_something ->
    Persistent_data.Adding_targets.check_and_really_add_targets t.data
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
  Persistent_data.get_target t.data id >>= fun target ->
  return (Target.state target)

let get_list_of_target_ids t query =
  Persistent_data.all_targets t.data
  >>= fun targets ->
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
              Is.finished_because_dependencies_died state
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
  return list_of_ids

let kill t ~id =
  Persistent_data.Killing_targets.add_target_ids_to_kill_list t.data [id]

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
  return id

let all_targets t =
  Persistent_data.all_targets t.data
let get_target t id =
  Persistent_data.get_target t.data id
let add_targets t l =
  Persistent_data.Adding_targets.register_targets_to_add t.data l

  
