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

module Test = struct
  exception Tests_failed

  let max_failures = 
    try Sys.getenv "MAX_FAILURES" |> int_of_string with _ -> 2_000_000

  let failed_tests = ref []
  let fail s =
    failed_tests := s :: !failed_tests;
    if List.length !failed_tests > max_failures then (
      Log.(s "Some tests failed: " %n % OCaml.list s (List.rev !failed_tests)
           @ error);
      raise Tests_failed 
    ) else ()

  let over_verbose =
    try Sys.getenv "KETREW_TEST_VERBOSE" = "true" with _ -> false

  let checkf b fmt =
    ksprintf (function
      | name when not b -> fail name
      | name when over_verbose ->
        Log.(s "Testing " % quote name % s ": SUCCESS" @ warning);
      | _ -> ()) fmt

  let test_ssh_host =
    let open Ketrew in
    try
      Host.ssh ~playground:Path.(absolute_directory_exn "/tmp")
        (Unix.getenv "ketrew_test_ssh")
    with Not_found -> Host.ssh "localhost"

  let wrong_ssh_host =
    Ketrew.EDSL.Host.parse "ssh://SomelongNameThatIHopeDoesNotExist:42/tmp/bouh"

  let test_targets ?wait_between_steps ~engine ~name targets checks =
    assert false
      (*
    let open Ketrew in
    Ketrew_engine.add_targets engine targets
    >>= fun () ->
    Deferred_list.while_sequential checks ~f:(fun check ->
        Option.value_map ~default:(return ()) ~f:System.sleep wait_between_steps
        >>< fun _ ->
        match check with
        | `Dont_care -> Ketrew_engine.Run_automaton.step engine >>= fun _ -> return ()
        | `Happens check ->
          Ketrew_engine.Run_automaton.step engine >>= fun happening ->
          begin match check happening with
          | true -> return ()
          | false ->
            fail (fmt "T: %S: wrong happening: %s" name
                    (List.map ~f:Ketrew_engine.what_happened_to_string happening
                     |> String.concat ~sep:",\n  "));
            return ()
          end
        | `Status (id, check) ->
          Ketrew_engine.Run_automaton.step engine >>= fun _ ->
          begin Ketrew_engine.get_status engine id
            >>= function
            | s when check s -> return ()
            | other -> fail (fmt "T: %S: wrong status" name); return ()
          end
        | `Kill_and (id, check) ->
          Ketrew_engine.kill engine id
          >>= fun happening ->
          begin match check happening with
          | true -> return ()
          | false ->
            fail (fmt "T: %S: wrong kill happening: %s" name
                    (List.map ~f:Ketrew_engine.what_happened_to_string happening
                     |> String.concat ~sep:",\n  "));
            return ()
          end)
    >>= fun (_: unit list) ->
    return ()
  *)

  (*
    let test_target_one_step ?condition ~engine ~make name check =
      let open Ketrew in
      let target = Target.active ~name ?condition  ~make () in
      test_targets ~name ~engine [target] [check ~id:(Target.id target)]
    let target_succeeds ~id =
      `Status (id, function `Successful _ -> true | _ -> false)
    let target_fails ~id =
      `Status (id, function `Dead _ -> true | _ -> false)
    let check_one_step ~id check = `Happens (check ~id)
    let nothing_happens = `Happens (function [] -> true | _ -> false)
  *)

    let new_db_file () =
      let db_file = Filename.concat (Sys.getcwd ()) "_kdb_test"  in
      begin System.Shell.do_or_fail (fmt "rm -rf %s" db_file)
        >>< fun _ -> return ()
      end
      >>= fun () ->
      return db_file

    module Target = struct
      let check_history t ~matches fmt =
        ksprintf (fun msg ->
            let state = Ketrew_target.(state t) in
            let b = state |> matches in
            if not b && over_verbose then
              Log.(s "Test: " % s msg % s ": unexpected history: "
                   % n % indent (Ketrew_target.State.log state) @ warning);
            checkf b "%s" msg
          ) fmt
    end

end

let test_0 () =
  Lwt_main.run begin
    Test.new_db_file ()
    >>= fun db_file ->
    let configuration = 
      Ketrew_configuration.engine ~database_parameters:db_file () in
    Ketrew_engine.with_engine ~configuration (fun ~engine ->
        Ketrew_engine.Run_automaton.step engine
        >>= fun v ->
        Test.checkf (not v) "1st step, nothing to do";
        let test_steps ~checks msg =
          List.foldi checks ~init:(return ()) ~f:begin fun idx prev check_step ->
            prev >>= fun () ->
            Ketrew_engine.Run_automaton.step engine
            >>= fun v ->
            let rec do_checks = 
              function
              | `None -> return ()
              | `Step_returns ret ->
                Test.checkf (ret = v) "step %d of %s step-returned: %b" idx msg v;
                return ()
              | `Target_is (id, matches) ->
                Ketrew_engine.get_target engine id
                >>= fun re_re_target_01 ->
                Test.Target.check_history re_re_target_01 ~matches
                  "step %d of %s target-matching %s" idx msg id;
                return ()
              | `And l ->
                Deferred_list.while_sequential l ~f:do_checks
                >>= fun (_ : unit list) ->
                return ()
            in
            do_checks check_step
          end 
        in
        let target_01 = Ketrew_edsl.target "01" in
        target_01#activate;
        Ketrew_engine.add_targets engine [target_01#render]
        >>= fun () ->
        let open Ketrew_target.State.Is in
        test_steps "almost empty target" ~checks:[
          `Step_returns true;
          `And [
            `Step_returns true;
            `Target_is (target_01#id, building);
          ];
          `Target_is (target_01#id, starting);
          `Target_is (target_01#id, successfully_did_nothing);
          `Target_is (target_01#id, verified_success);
          `Target_is (target_01#id, finished);
          `Step_returns false;
          `None;
        ]
        >>= fun () ->
        return ())
    (*
    Ketrew_engine.with_engine ~configuration begin fun ~engine ->
      Ketrew_engine.add_targets engine
        [Target.(create ~name:"First target" ())]
      >>= fun () ->
      begin Ketrew_engine.current_targets engine
        >>= function
        | [one] when one.Target.name = "First target" -> return ()
        | other ->
          Test.fail (fmt "too many targets: %d" (List.length other)); return ()
      end
      >>= fun () ->


      Test.test_target_one_step ~engine "ls /" Test.target_succeeds
        ~make:(`Direct_command Target.Command.(shell "ls /"))
      >>= fun () ->

      Test.test_target_one_step ~engine "ls /crazypath" Test.target_fails
        ~make:(`Direct_command Target.Command.(shell "ls /crazypath"))
      >>= fun () ->

      let host = Test.test_ssh_host in
      Test.test_target_one_step ~engine "ls / over ssh" Test.target_succeeds
        ~make:(`Direct_command Target.Command.(shell ~host "ls /"))
      >>= fun () ->

      let root = Path.absolute_directory_exn "/tmp" in
      Test.test_target_one_step ~engine "ls / > <file> over ssh" Test.target_succeeds
        ~make:(`Direct_command
                 Target.Command.(shell ~host "ls / > /tmp/ketrew_test"))
        ~condition:(`Volume_exists
                      Artifact.Volume.(create ~host ~root (file "ketrew_test")))
      >>= fun () ->

      (* A target that creates a more complex file structure *)
      let cmd =
        Target.Command.(
          shell ~host
            "mkdir -p /tmp/ketrew_test2/somedir && \
             mkdir -p /tmp/ketrew_test2/somedir_empty && \
             ls / > /tmp/ketrew_test2/ls && \
             ps aux > /tmp/ketrew_test2/somedir/psaux ")
      in
      let vol =
        Artifact.Volume.(create ~host ~root
                           (dir "ketrew_test2" [file "ls"; dir "somedir_empty" [];
                                                dir "somedir" [file "psaux"]]))
      in
      Test.test_target_one_step ~engine "2 files, 2 dirs over ssh" Test.target_succeeds
        ~make:(`Direct_command cmd) ~condition:(`Volume_exists vol)
      >>= fun () ->

      (* doing it again should succeed for a good reason *)
      Test.test_target_one_step ~engine "2 files, 2 dirs over ssh, AGAIN"
        ~make:(`Direct_command cmd) ~condition:(`Volume_exists vol)
        (Test.check_one_step (fun ~id -> function
           | [`Target_succeeded (i, `Artifact_ready)] when i = id -> true
           | _ -> false))
      >>= fun () ->

      (* A target that fails to create a more complex file structure; typo on file  *)
      let vol =
        Artifact.Volume.(create ~host ~root
                           (dir "ketrew_test2" [file "ls"; dir "somedir_empty" [];
                                                dir "somedir_typo" [file "psaux"]]))
      in
      Test.test_target_one_step ~engine "2 files, 2 dirs over ssh + file typo" Test.target_fails
        ~make:(`Direct_command cmd) ~condition:(`Volume_exists vol)
      >>= fun () ->

      (* A target that fails to create a more complex file structure; typo on dir  *)
      let vol =
        Artifact.Volume.(create ~host ~root
                           (dir "ketrew_test2" [file "ls"; dir "somedir_empty_2" [];
                                                dir "somedir" [file "psaux"]]))
      in
      Test.test_target_one_step ~engine "2 files, 2 dirs over ssh + dir typo" Test.target_fails
        ~make:(`Direct_command cmd) ~condition:(`Volume_exists vol)
      >>= fun () ->

    (*

      This tests 3 cases, for target1 depending on target2:

      - both may succeed consecutively
      - target 1 may fail and make target2 fail because of dependency
      - target 1 may succeed and target2 fail by itself

    *)
      let two_targets (succeed1, succeed2) check =
        let tmpfile = fmt "ketrew_test_%s" (Unique_id.create ()) in
        let target1 =
          let shell_command =
            if succeed1 then (fmt "ls -l / > /tmp/%s" tmpfile) else "echo bouh"
          in
          Target.create ~name:"ls / > some tmp"
            ~make:(`Direct_command Target.Command.(shell ~host shell_command))
            ~condition:(`Volume_exists Artifact.Volume.(create ~host ~root (file tmpfile)))
            ()
        in
        let target2 =
          let shell_command =
            if succeed2 then (fmt "wc -l /tmp/%s" tmpfile) else "exit 42" in
          Target.active ~name:"count lines of dependency"
            ~dependencies:[ Target.id target1 ]
            ~make:(`Direct_command Target.Command.(shell ~host shell_command))
            ()
        in
        let id1 = Target.id target1 in
        let id2 = Target.id target2 in
        let expected_status b = if b then "succeeds" else "fails" in
        let name =
          fmt "%s (%s) depends on %s (%s)"
            id1 (expected_status succeed1)
            id2 (expected_status succeed2)
        in
        Test.test_targets ~engine ~name [target1; target2] (check ~id1 ~id2)
      in
      two_targets (true, true) (fun ~id1 ~id2 ->
          [
            `Happens (function
              | [`Target_activated (i, `Dependency)] when i = id1 -> true
              | _ -> false);
            `Happens (function
              | [`Target_succeeded (i, `Process_success)] when i = id1 -> true
              | _ -> false);
            `Happens (function
              | [`Target_succeeded (i, `Process_success)] when i = id2 -> true
              | _ -> false);
            Test.nothing_happens
          ])
      >>= fun () ->
      two_targets (false, true) (fun ~id1 ~id2 ->
          [
            `Happens (function
              | [`Target_activated (i, `Dependency)] when i = id1 -> true
              | _ -> false);
            `Happens (function
              | [`Target_died (i, `Process_failure)] when i = id1 -> true
              | _ -> false);
            `Happens (function
              | [`Target_died (i, `Dependencies_died)] when i = id2 -> true
              | _ -> false);
            Test.nothing_happens
          ])
      >>= fun () ->
      two_targets (true, false) (fun ~id1 ~id2 ->
          [
            `Happens (function
              | [`Target_activated (i, `Dependency)] when i = id1 -> true
              | _ -> false);
            `Happens (function
              | [`Target_succeeded (i, `Process_success)] when i = id1 -> true
              | _ -> false);
            `Happens (function
              | [`Target_died (i, `Process_failure)] when i = id2 -> true
              | _ -> false);
            Test.nothing_happens
          ])
      >>= fun () ->

      let target_with_missing_dep =
        Target.active ~name:"target_with_missing_dep"
          ~dependencies:[ "hope this one is not in the database" ]
          ~make:(`Direct_command Target.Command.(shell "ls"))
          ()
      in
      Test.test_targets ~engine ~name:"target_with_missing_dep" [target_with_missing_dep] [
        `Happens Happenings.(
            look () ~like:[
              death ~how:`Dependencies_died;
            ]);
        `Dont_care;
      ]
      >>= fun () ->

    
      (* A target with 2 fallback targets: *)
      let fallback1 =
        Target.create ~name:"Fallback 1" ()
          ~make:(`Direct_command Target.Command.(shell "ls")) in
      let fallback2 =
        Target.create ~name:"Fallback 2" ()
          ~make:(`Direct_command Target.Command.(shell "ls")) in
      let target_with_fallbacks =
        Target.active ~name:"target_with_fallbacks"
          ~if_fails_activate:[Target.id fallback1; Target.id fallback2 ]
          ~make:(`Direct_command Target.Command.(shell "ls /somelong_STUPID-path"))
          ()
      in
      Test.test_targets ~engine ~name:"target_with_fallbacks"
        [target_with_fallbacks; fallback1; fallback2] [
        `Happens Happenings.(
            look () ~like:[
              death ~how:`Process_failure;
              activation ~how:`Fallback ~what:(Target.id fallback1);
              activation ~how:`Fallback ~what:(Target.id fallback2);
            ]);
        `Dont_care;
        `Dont_care;
        `Dont_care;
      ]
      >>= fun () ->

      (* Two targets with the same fallback *)
      let fallback =
        Target.create ~name:"Fallback" ()
          ~make:(`Direct_command Target.Command.(shell "ls")) in
      let target_with_fallback_1 =
        Target.active ~name:"target_with_fallback_1"
          ~if_fails_activate:[Target.id fallback]
          ~make:(`Direct_command Target.Command.(shell "ls /somelong_STUPID-path"))
          ()
      in
      let target_with_fallback_2 =
        Target.active ~name:"target_with_fallback_2"
          ~if_fails_activate:[Target.id fallback]
          ~make:(`Direct_command Target.Command.(shell "ls /somelong_STUPID-path"))
          ()
      in
      Test.test_targets ~engine ~name:"targets_with_same_fallback"
        [target_with_fallback_1; target_with_fallback_2; fallback] [
        `Happens Happenings.(
            look () ~like:[
              death ~how:`Process_failure;
              death ~how:`Process_failure;
              activation ~how:`Fallback;
            ]);
        `Dont_care;
        `Dont_care;
        `Dont_care;
      ]
      >>= fun () ->

      let triggered =
        Target.create ~name:"Triggered" ()
          ~make:(`Direct_command Target.Command.(shell "ls /"))
      in
      let target_that_triggers =
        Target.active ~name:"target-with-trigger" ()
          ~make:(`Direct_command Target.Command.(shell "ls /"))
          ~success_triggers:[Target.id triggered]
      in
      Test.test_targets ~engine ~name:"target-with-trigger"
        [target_that_triggers; triggered] [
        `Happens Happenings.(look () ~like:[
            success ~what:(Target.id target_that_triggers); 
            activation ~how:`Success_trigger;
          ]);
        `Happens Happenings.(look () ~like:[
            success ~what:(Target.id triggered); 
          ]);
        `Dont_care;
        `Dont_care;
      ]
    end


*)
    >>= fun () ->
    return ()
  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_0: Ketrew ERROR:  " %s (Ketrew.Error.to_string e) @ error);
    Test.fail "test_0 ends with error"

let test_ssh_failure_vs_target_failure () =
  assert false
    (*
  let open Ketrew in
  Lwt_main.run begin
    let test_wrong_host turn_unix_ssh_failure_into_target_failure expect =
      Test.new_db_file ()
      >>= fun database_parameters ->
      let configuration =
        Configuration.engine
          ~turn_unix_ssh_failure_into_target_failure ~database_parameters () in
      Ketrew_engine.with_engine ~configuration (fun ~engine ->
          let target_with_wrong_host =
            let host = Test.wrong_ssh_host in
            Target.active ~name:"target_with_missing_dep"
              ~make:(`Direct_command Target.Command.(shell ~host "ls")) ()
          in
          Test.test_targets
            ~engine ~name:"target_with_wrong_host" [target_with_wrong_host]
            expect)
    in
    (* `false` is the default:
       The target won't be killed (so Ketrew will try to run it again at each
       `step`)
    *)
    test_wrong_host false [
      `Happens (function (* Nothing happens. *)
        | []  -> true
        | _ -> false);
      `Happens (function
        | []  -> true
        | _ -> false);
      `Dont_care;
    ]
    >>= fun () ->
    (* The target will be kill at the first attempt: *)
    test_wrong_host true [
      `Happens (function (* Nothing happens. *)
        | [`Target_died _]  -> true
        | _ -> false);
      `Happens (function
        | []  -> true
        | _ -> false);
      `Dont_care;
    ]

  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_ssh_failure_vs_target_failure: Ketrew ERROR:  " %s (Ketrew.Error.to_string e) @ error);
    Test.fail "test_ssh_failure_vs_target_failure ends with error"
*)



let test_long_running_nohup () =
  assert false
    (*
  let open Ketrew in
  Lwt_main.run begin
    Test.new_db_file ()
    >>= fun db_file ->
    let configuration = Configuration.engine ~database_parameters:db_file () in
    Ketrew_engine.with_engine ~configuration (fun ~engine ->

        Test.test_targets  ~engine ~name:("one bad plugin")
          [Target.active ~name:"one"
             ~make:(`Long_running ("bad_plugin", "useless string")) ()
          ]
          [`Happens (function
             | [`Target_died (_, `Plugin_not_found "bad_plugin")] -> true
             | _ -> false);
           `Dont_care;
           Test.nothing_happens;
          ]
        >>= fun () ->

        let root = Path.absolute_directory_exn "/tmp" in
        Deferred_list.while_sequential [Test.test_ssh_host] ~f:(fun host ->
            let name n = fmt "%s on %s" n Host.(to_string_hum host) in
            let new_name = Unique_id.create () in
            Test.test_targets  ~engine ~name:(name "good ls")
              ~wait_between_steps:1.
              [Target.active ~name:"one" ()
                 ~make:(Daemonize.create ~host
                          (`Shell_command (fmt "ls > /tmp/%s" new_name)))
                 ~condition:(`Volume_exists
                               Artifact.Volume.(create ~host ~root (file new_name)))
              ]
              [`Happens (function
                 | [`Target_started (_, _)] -> true
                 | _ -> false);
               `Happens (function (* We hope that 1 second intervals are enough
                                     but there is no good synchronization *)
                 | [`Target_succeeded _] -> true
                 | _ -> false);
               Test.nothing_happens;
              ]
            >>= fun () ->

            (* Test the `kill` function: *)
            let id = Unique_id.create () in
            Test.test_targets  ~engine ~name:(name "sleep 42")
              ~wait_between_steps:1.
              [Target.active ~name:"one" ~id
                 ~make:(Daemonize.create ~host
                          (`Shell_command (fmt "echo %S && sleep 42" String.(make 60 '='))))
                 ()
              ]
              [`Happens (function
                 | [`Target_started (_, _)] -> true
                 | _ -> false);
               `Happens (function (* We hope that 1 second intervals are enough *)
                 | [] -> true
                 | _ -> false);
               `Dont_care;
               `Dont_care;
               `Dont_care;
               `Kill_and (id, (function
                 | [`Target_died (_, `Killed)] -> true
                 | _ -> false));
               Test.nothing_happens;
              ]
            >>= fun () ->

            (* Test that a target fails when the process succeeds but does not
               create the right target: *)
            let t =
              Ketrew.EDSL.(
                target "some name"
                  ~make:(daemonize ~host Program.(sh "ls > /tmp/some_temp_file"))
                  ~done_when:(file ~host "/tmp/some_temp_file_with_error")#exists
              )
            in
            t#activate;
            Test.test_targets ~engine ~name:(name "wrong 'returns'")
              ~wait_between_steps:0.3 [t#render] [
              `Happens (function
                | [`Target_started (_, _)] -> true
                | _ -> false);
              `Happens (function
                | [`Target_died (_, `Process_failure)] -> true
                | _ -> false);
              `Dont_care;
            ]
            >>= fun () ->

            return ())
        >>= fun (_ : unit list) ->

        return ())
  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_long_running_nohup: Ketrew ERROR:  "
         %s (Error.to_string e) @ error);
    Test.fail "test_long_running_nohup ends with error"
*)

type state = { name : string }
type action = string
type tree = [
  | `Leaf of state
  | `Node of state * action * (string * tree) list
]
let rec tree_to_log =
  let open Log in
  function
  | `Leaf {name} -> braces (s name)
  | `Node ({name}, action, trees) ->
    parens (braces (s name) % s " → " % s action 
            % OCaml.list (fun (response, tree) ->
                s response % s ": " % tree_to_log tree)
              trees)

let rec tree_has_action t action =
  match t with
  | `Leaf _ -> false
  | `Node (_, a, _) when a = action -> true
  | `Node (_, a, l) -> List.exists l ~f:(fun (_, t) -> tree_has_action t action)

let tree_to_dot ?(style=`Action_boxes) t =
  let open Log in
  let arrow = s " -> " in
  let semicolon = s ";" in
  let transition state1 action response state2 =
    match style with
    (* | `Arrow_labels -> *)
    (*   (s state1 % arrow % s state2 *)
    (*    % sp % brakets (s "label=" % sf "%S" action) *)
    (*    % s ";" % n) *)
    | `Action_boxes ->
      let action_name, action_attributes =
        match state2 with
        | "Killing" ->
          state1 ^ state2,
          sf "fontname=\"monospace\",shape=doubleoctagon, label=%S" "MURDER"
        | "Active" ->
    
      state1 ^ action ^ state2,
          sf "fontname=\"monospace\",shape=doubleoctagon, label=%S" "ACTIVATION"
        | _ ->
          state1 ^ action, sf "fontname=\"monospace\",shape=box label=\"%s\"" action in
      let response_name, response_attributes =
        "response" ^ state1 ^ action ^ state2,
        sf "shape=diamond,fontsize=9,fontname=\"monospace\",label=%S" response
      in
      (* s state1 % sp % brakets (s "shape=oval") % s ";" % n *)
      (* % s state2 % sp % brakets (s "shape=oval") % s ";" % n *)
      (* % s action_name % sp % brakets action_attributes % s ";" % n *)
      (* % s state1 % arrow % s action_name % arrow % s state2 % semicolon % n *)
      let state_attributes =
        s "fontname=\"monospace\",fontsize=18,shape=oval" in
      let state_action_arrow =
        s "arrowhead=\"open\"" in
      let action_respoonse_arrow =
        s "dir=\"none\"" in
      [
        s state1 % sp % brakets state_attributes;
        s state2 % sp % brakets state_attributes;
        s action_name % sp % brakets action_attributes;
        s response_name % sp % brakets response_attributes;
        s state1 % arrow % s action_name % brakets state_action_arrow;
        s action_name % arrow % s response_name % brakets action_respoonse_arrow;
        s response_name % arrow % s state2;
      ]
  in
  s "digraph target_graph"
  % braces (
    let rec go =
      let continue = fun (_, t) -> go t in
      function
      | `Leaf (state) -> []
      | `Node (state, "NONE", trees) ->
        List.concat_map ~f:continue trees
      | `Node (state, action, trees) ->
        let arrows =
          List.map trees (function
            | response, `Leaf (statei)
            | response, `Node (statei, _, _) ->
              transition state.name action response statei.name)
        in
        List.append arrows (List.map ~f:continue trees) |> List.concat
    in
    (go t |> List.dedup |> separate (semicolon % n))
  )

(* "{" ^ action ^ " -> " ^ state ^ "\n" *)
(* ^ String.concat ~sep:" --- " (List.map ~f:tree_to_string trees) ^ "}" *)

let make_automaton_graph () =
  let module T = Ketrew_target in
  let state_name t = {name = Ketrew_target.(State.name (state t)) } in
  let rec loop ?(stop_afterwards=false) target =
    (* Log.(s "status: " % Ketrew_target.(State.log ~depth:1 (state target)) @ normal); *)
    let node action l : tree =
      `Node (state_name target, action, l) in
    let additional =
      match T.state target |> T.State.Is.killable with
      | true ->
        ["OK", (T.kill target |> Option.value_exn ~msg:"Killing TOKILL",
                    `Changed_state)]
      | false -> [] in
    let node_map action l : tree =
      node action (List.map (additional @ l) ~f:(function
        | response, (t, `No_change) when stop_afterwards ->
          response, `Leaf (state_name t)
        | response, (t, `No_change) -> response, loop ~stop_afterwards:true t
        | response, (t, `Changed_state) -> response, loop t)) in
    match Ketrew_target.Automaton.transition target with
    | `Kill (b, mkt) ->
      node_map "kill"
        ["OK", mkt (`Ok b);
         "Try-Again", mkt (`Error (`Try_again,  "", b));
         "Fatal", mkt (`Error (`Fatal,  "", b));]
    | `Do_nothing mkt ->
      let action = "do_nothing"  in
      node_map action ["OK", mkt ()]
    | `Check_and_activate_dependencies mkt ->
      node_map "check_and_activate_dependencies" [
        "All-succeeded", mkt `All_succeeded;
        "At-least-one-failed", mkt (`At_least_one_failed []);
        "Still-processing", mkt `Still_processing;
      ]
    | `Start_running (book, mkt) ->
      node_map "start_running" [
        "OK", mkt (`Ok book);
        "Try-again", mkt (`Error (`Try_again,  "", book));
        "Fatal", mkt (`Error (`Fatal,  "", book));
      ]
    | `Activate (_, mkt) ->
      node_map "activate_targets" ["OK", mkt ()]
    | `Eval_condition (condition, mkt) ->
      node_map "eval_condition" [
        "true", mkt (`Ok true);
        "false", mkt (`Ok false);
        "Try-again", mkt (`Error (`Try_again,  ""));
        "Fatal", mkt (`Error (`Fatal,  ""));
      ]
    | `Check_process (book, mkt) ->
      node_map "check_process" [
        "Success", mkt (`Ok (`Successful book));
        "Still-running", mkt (`Ok (`Still_running book));
        "Try-again", mkt (`Error (`Try_again,  "", book));
        "Fatal", mkt (`Error (`Fatal,  "", book));
      ]
  in
  let activate_and_render t = t#activate; t#render in
  let dep =(Ketrew.EDSL.target "01") in
  let targets = [
    dep #render;
    (Ketrew.EDSL.target "02" |> activate_and_render);
    (Ketrew.EDSL.target "03" ~dependencies:[dep] |> activate_and_render);
    (Ketrew.EDSL.target "03" ~make:(`Long_running ("", "")) |> activate_and_render);
    (Ketrew.EDSL.target "03" ~make:(`Long_running ("", ""))
       ~done_when:`False |> activate_and_render);
    (Ketrew.EDSL.target "04" ~done_when:(`True) |> activate_and_render);
    (Ketrew.EDSL.target "04" |> activate_and_render
     |> Ketrew_target.kill ?log:None |> Option.value_exn ~msg:"not killable?");
    (Ketrew.EDSL.target "TOKILL" ~make:(`Long_running ("", ""))
     |> activate_and_render);
  ] in
  let result_tree =
    (`Node ({name= "ROOT"}, "NONE",
            ("OK", `Node ({name = "Passive"}, "Activation", ["OK", `Leaf {name = "Active"}]))
            :: List.map targets ~f:(fun t ->
                ("", `Node (state_name t, "NONE", ["", loop t]))))) in
  let all_actions = [
    "kill"; "do_nothing"; "check_and_activate_dependencies";
    "start_running"; "activate_targets"; "eval_condition"; "check_process"
  ] in
  Test.checkf (List.for_all all_actions (tree_has_action result_tree))
    "tree having all actions: missing: %s"
    (List.filter all_actions ~f:(fun a -> tree_has_action result_tree a |> not)
     |> String.concat ~sep:", ") ;
  let o = open_out "target_graph.dot" in
  output_string o (Log.to_long_string
                     (tree_to_dot result_tree));
  close_out o;
  if Test.over_verbose then
    Log.(s "Trrreeees: " % n % tree_to_log result_tree @ normal);
  ()

let () =
  let argl = Sys.argv |> Array.to_list in
  global_with_color := not (List.mem ~set:argl "-no-color");
  global_debug_level := 3;
  let all = List.mem ~set:argl "ALL" in
  if List.mem ~set:argl "basic-test" || all then test_0 ();
  if List.mem ~set:argl "nohup-test" || all then test_long_running_nohup ();
  if List.mem ~set:argl "ssh-failure" || all then test_ssh_failure_vs_target_failure ();
  if List.mem ~set:argl "automaton-graph" || all then make_automaton_graph ();
  begin match !Test.failed_tests with
  | [] ->
    Log.(s "No tests failed \\o/ (arg-list: "
         % OCaml.list (sf "%S") argl % s ")" @ normal);
    exit 0
  | some ->
    Log.(s "Some tests failed: " %n % OCaml.list string some @ error);
    exit 3
  end
