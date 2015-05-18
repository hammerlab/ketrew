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
    Printf.ksprintf (function
      | name when not b -> fail name
      | name when over_verbose ->
        Log.(s "Testing " % quote name % s ": SUCCESS" @ warning);
      | _ -> ()) fmt


  let new_db_file () =
    let db_file = Filename.concat (Sys.getcwd ()) "_kdb_test"  in
    begin System.Shell.do_or_fail (fmt "rm -rf %s" db_file)
      >>< fun _ -> return ()
    end
    >>= fun () ->
    return db_file

  module Target = struct
    let check_history t ~matches fmt =
      Printf.ksprintf (fun msg ->
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
    >>= fun () ->
    return ()
  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_0: Ketrew ERROR:  " %s (Ketrew.Error.to_string e) @ error);
    Test.fail "test_0 ends with error"


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
          List.map trees ~f:(function
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
  let rec loop ~depth ?(stop_afterwards=false) target =
    (* Log.(s "status: " % Ketrew_target.(State.log ~depth:1 (state target)) @ normal); *)
    let node action l : tree =
      `Node (state_name target, action, l) in
    let additional =
      match T.state target |> T.State.Is.killable with
      | true ->
        ["OK", (T.kill target |> Option.value_exn ~msg:"Killing TOKILL",
                `Changed_state)]
      | false -> [] in
    let protect_rec_call log f =
      (* If a change in the state-machine makes this happen, it
         usually means that the author forgot to pass the `No-change`
         information in `Target.Automaton.transition`. *)
      try f ()
      with Stack_overflow ->
        Printf.eprintf "Test error! Stack_overflow: %d, %s\n%!" depth log;
        raise Stack_overflow
    in
    let node_map action l : tree =
      node action (List.map (additional @ l) ~f:(function
        | response, (t, `No_change) when stop_afterwards ->
          response, `Leaf (state_name t)
        | response, (t, `No_change) ->
          response,
          protect_rec_call "no-change" (fun () ->
              loop ~depth:(depth + 1) ~stop_afterwards:true t)
        | response, (t, `Changed_state) ->
          response,
          protect_rec_call "changed-state" (fun () ->
              loop ~depth:(depth + 1) t))) in
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
    (Ketrew.EDSL.target "03" ~depends_on:[dep] |> activate_and_render);
    (Ketrew.EDSL.target "03" ~make:(`Long_running ("", "")) |> activate_and_render);
    (Ketrew.EDSL.target "03" ~make:(`Long_running ("", ""))
       ~done_when:`Never |> activate_and_render);
    (Ketrew.EDSL.target "04" ~done_when:(`Satisfied) |> activate_and_render);
    (Ketrew.EDSL.target "04" |> activate_and_render
     |> Ketrew_target.kill ?log:None |> Option.value_exn ~msg:"not killable?");
    (Ketrew.EDSL.target "TOKILL" ~make:(`Long_running ("", ""))
     |> activate_and_render);
  ] in
  let result_tree =
    (`Node ({name= "ROOT"}, "NONE",
            ("OK", `Node ({name = "Passive"}, "Activation", ["OK", `Leaf {name = "Active"}]))
            :: List.map targets ~f:(fun t ->
                ("", `Node (state_name t, "NONE", ["", loop ~depth:0 t]))))) in
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


let run_all_tests () =
  Log.(s "Starting ALL Tests" @ normal);
  Log.(s "Basic test:" @ normal);
  test_0 ();
  Log.(s "Automaton graph test:" @ normal);
  make_automaton_graph ();
  Log.(s "End of ALL Tests" @ normal);
  ()

let () =
  let argl = Sys.argv |> Array.to_list in
  global_with_color := not (List.mem ~set:argl "-no-color");
  global_debug_level := 3;
  let all = List.mem ~set:argl "ALL" in
  begin if all
    then run_all_tests ()
    else begin
      if List.mem ~set:argl "basic-test" then test_0 ();
      if List.mem ~set:argl "automaton-graph" then make_automaton_graph ();
    end
  end;
  begin match !Test.failed_tests with
  | [] ->
    Log.(s "No tests failed \\o/ (arg-list: "
         % OCaml.list (sf "%S") argl % s ")" @ normal);
    exit 0
  | some ->
    Log.(s "Some tests failed: " %n % OCaml.list string some @ error);
    exit 3
  end
