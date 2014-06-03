open Ketrew_pervasives

module Test = struct
  let failed_tests = ref []
  let fail s = failed_tests := s :: !failed_tests

  let test_ssh_host =
    let open Ketrew in
    try
      Host.ssh ~playground:Path.(absolute_directory_exn "/tmp")
        (Unix.getenv "ketrew_test_ssh")
    with Not_found -> Host.ssh "localhost"

  let test_targets ?wait_between_steps ~state ~name targets checks =
    let open Ketrew in
    Deferred_list.while_sequential targets (State.add_target state)
    >>= fun (_ : unit list) ->
    Deferred_list.while_sequential checks ~f:(fun check ->
        Option.value_map ~default:(return ()) ~f:System.sleep wait_between_steps
        >>< fun _ ->
        match check with
        | `Dont_care -> State.step state >>= fun _ -> return ()
        | `Happens check ->
          State.step state >>= fun happening ->
          begin match check happening with
          | true -> return ()
          | false -> 
            fail (fmt "T: %S: wrong happening: %s" name
                    (List.map ~f:State.what_happened_to_string happening
                     |> String.concat ~sep:",\n  "));
            return ()
          end
        | `Status (id, check) ->
          State.step state >>= fun _ ->
          begin State.get_status state id
            >>= function
            | s when check s -> return ()
            | other -> fail (fmt "T: %S: wrong status" name); return ()
          end)
    >>= fun (_: unit list) ->
    return ()

    let test_target_one_step ~state ~make ~spec name check =
      let open Ketrew in
      let target = Target.(active ~name ~make spec) in
      test_targets ~name ~state [target] [check ~id:(Target.id target)]
    let target_succeeds ~id =
      `Status (id, function `Successful _ -> true | _ -> false)
    let target_fails ~id = 
      `Status (id, function `Dead _ -> true | _ -> false)
    let check_one_step ~id check = `Happens (check ~id)
    let nothing_happens = `Happens (function [] -> true | _ -> false)
end

let mini_db_test () =
  Lwt_main.run begin
    let module DB = Ketrew_database in
    let db_file = "/tmp/ketrew_db_test"  in
    begin System.remove db_file >>< fun _ -> return () end
    >>= fun () ->
    DB.load db_file
    >>= fun db ->
    DB.get db ~key:"k"
    >>= fun res ->
    begin match res with
    | None -> return ()
    | Some v -> Test.fail (fmt "key k got %S" v); return ()
    end
    >>= fun () ->
    begin DB.act db DB.(seq [ is_not_set "k"; set ~key:"k" "V" ])
      >>= function
      | `Done -> return ()
      | `Not_done -> Test.fail "seq 1 not done"; return ()
    end
    >>= fun () ->
    let check_k current_db =
      begin DB.get current_db ~key:"k" >>= function
        | Some v when v = "V" -> return ()
        | None -> Test.fail (fmt "get k got None"); return ()
        | Some v -> Test.fail (fmt "get k got %S" v); return ()
      end
    in
    check_k db >>= fun () ->
    DB.load db_file
    >>= fun db2 ->
    check_k db2 >>= fun () ->
    begin DB.act db DB.(seq [contains ~key:"k" "V"])
      >>= function
      | `Done -> return ()
      | `Not_done -> Test.fail "seq 2 not done"; return ()
    end
    >>= fun () ->
    begin DB.act db DB.(seq [contains ~key:"k" "v"])
      >>= function
      | `Not_done -> return ()
      | `Done -> Test.fail "seq 3 done"; return ()
    end
    >>= fun () ->
    return ()
  end
  |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "mini_db_test: ERROR: TODO " @ error);
    Test.fail "mini_db_test ends with error"

let test_0 () =
  let open Ketrew in
  let db_file = "/tmp/ketrew_test_database"  in
  Lwt_main.run begin
    begin System.remove db_file >>< fun _ -> return () end
    >>= fun () ->
    let configuration = State.Configuration.create db_file () in
    State.create configuration
    >>= fun state ->
    State.add_target state 
      Target.(create ~name:"First target" Artifact.Type.string_value)
    >>= fun () ->
    begin State.current_targets state
      >>= function
      | [one] when one.Target.name = "First target" -> return ()
      | other ->
        Test.fail (fmt "too many targets: %d" (List.length other)); return ()
    end
    >>= fun () ->


    Test.test_target_one_step ~state "ls /" Test.target_succeeds
      ~make:(`Get_output Target.Command.(shell "ls /"))
      ~spec:Artifact.Type.string_value
    >>= fun () ->

    Test.test_target_one_step ~state "ls /crazypath" Test.target_fails
      ~make:(`Get_output Target.Command.(shell "ls /crazypath"))
      ~spec:Artifact.Type.string_value
    >>= fun () ->

    let host = Test.test_ssh_host in
    Test.test_target_one_step ~state "ls / over ssh" Test.target_succeeds
      ~make:(`Get_output Target.Command.(shell ~host "ls /"))
      ~spec:Artifact.Type.string_value
    >>= fun () ->

    let root = Path.absolute_directory_exn "/tmp" in
    Test.test_target_one_step ~state "ls / > <file> over ssh" Test.target_succeeds
      ~make:(`Direct_command 
               Target.Command.(shell ~host "ls / > /tmp/ketrew_test"))
      ~spec:(Artifact.Type.volume 
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
    Test.test_target_one_step ~state "2 files, 2 dirs over ssh" Test.target_succeeds
      ~make:(`Direct_command cmd) ~spec:(Artifact.Type.volume vol)
    >>= fun () ->

    (* doing it again should succeed for a good reason *)
    Test.test_target_one_step ~state "2 files, 2 dirs over ssh, AGAIN"
      ~make:(`Direct_command cmd) ~spec:(Artifact.Type.volume vol)
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
    Test.test_target_one_step ~state "2 files, 2 dirs over ssh + file typo" Test.target_fails
      ~make:(`Direct_command cmd) ~spec:(Artifact.Type.volume vol)
    >>= fun () ->

    (* A target that fails to create a more complex file structure; typo on dir  *)
    let vol =
      Artifact.Volume.(create ~host ~root
                (dir "ketrew_test2" [file "ls"; dir "somedir_empty_2" [];
                                     dir "somedir" [file "psaux"]]))
    in
    Test.test_target_one_step ~state "2 files, 2 dirs over ssh + dir typo" Test.target_fails
      ~make:(`Direct_command cmd) ~spec:(Artifact.Type.volume vol)
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
          (Artifact.Type.volume Artifact.Volume.(create ~host ~root (file tmpfile)))
      in
      let target2 = 
        let shell_command =
          if succeed2 then (fmt "wc -l /tmp/%s" tmpfile) else "exit 42" in
        Target.active ~name:"count lines of dependency"
          ~dependencies:[ Target.id target1 ]
          ~make:(`Get_output Target.Command.(shell ~host shell_command))
          (Artifact.Type.string_value)
      in
      let id1 = Target.id target1 in
      let id2 = Target.id target2 in
      let expected_status b = if b then "succeeds" else "fails" in
      let name =
        fmt "%s (%s) depends on %s (%s)"
          id1 (expected_status succeed1)
          id2 (expected_status succeed2)
      in
      Test.test_targets ~state ~name [target1; target2] (check ~id1 ~id2)
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


    System.remove db_file
    >>= fun () ->
    return ()
  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_0: Ketrew ERROR:  " %s (Error.to_string e) @ error);
    Test.fail "test_0 ends with error"

let test_long_running_nohup () =
  let open Ketrew in
  let db_file = "/tmp/ketrew_test_database"  in
  Lwt_main.run begin
    begin System.remove db_file >>< fun _ -> return () end
    >>= fun () ->
    let configuration = State.Configuration.create db_file () in
    State.create configuration >>= fun state ->

    Test.test_targets  ~state ~name:("one bad plugin")
      [Target.active ~name:"one"
         ~make:(`Long_running ("bad_plugin", "useless string"))
         Artifact.Type.string_value
      ]
      [`Happens (function
         | [`Target_died (_, `Plugin_not_found "bad_plugin")] -> true
         | _ -> false);
       `Dont_care;
       Test.nothing_happens;
      ]
    >>= fun () ->

    let root = Path.absolute_directory_exn "/tmp" in
    (* Deferred_list.while_sequential [Host.localhost; Test.test_ssh_host] ~f:(fun host -> *)
    Deferred_list.while_sequential [Test.test_ssh_host] ~f:(fun host ->
        let name n = fmt "%s on %s" n Host.(to_string host) in
        let new_name = Unique_id.create () in
        Test.test_targets  ~state ~name:(name "good ls")
          ~wait_between_steps:1.
          [Target.active ~name:"one"
             ~make:(Nohup_setsid.create ~host [fmt "ls > /tmp/%s" new_name])
             (Artifact.Type.volume
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

        return ())
    >>= fun (_ : unit list) ->

    System.remove db_file
    >>= fun () ->
    return ()
  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_long_running_nohup: Ketrew ERROR:  "
         %s (Error.to_string e) @ error);
    Test.fail "test_long_running_nohup ends with error"


let () =
  let argl = Sys.argv |> Array.to_list in
  global_with_color := not (List.mem ~set:argl "-no-color");
  global_debug_level := 3;
  let all = List.mem ~set:argl "ALL" in
  if List.mem ~set:argl "db-test" || all then mini_db_test ();
  if List.mem ~set:argl "basic-test" || all then test_0 ();
  if List.mem ~set:argl "nohup-test" || all then test_long_running_nohup ();
  begin match !Test.failed_tests with
  | [] ->
    Log.(s "No tests failed \\o/ (arg-list: "
         % OCaml.list (sf "%S") argl % s ")" @ normal);
    exit 0
  | some ->
    Log.(s "Some tests failed: " %n % OCaml.list s some @ error);
    exit 3
  end
