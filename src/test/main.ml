open Ketrew_pervasives

module Test = struct
  let failed_tests = ref []
  let fail s = failed_tests := s :: !failed_tests

  let test_ssh_host =
    let open Ketrew in
    try
      Host.ssh (Unix.getenv "ketrew_test_ssh")
    with Not_found -> Host.ssh "localhost"
end

let mini_db_test () =
  Lwt_main.run begin
    let module DB = Ketrew.Database in
    let db_file = "/tmp/ketrew_db_test"  in
    let new_db = DB.create db_file in
    DB.save new_db
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
    let test_get_persistent () =
      let configuration = Configuration.create db_file () in
      State.create configuration
      >>= fun state ->
      State.get_persistent state
      >>= fun persistent ->
      return (state, persistent)
    in
    test_get_persistent () >>= fun (state, persistent) ->
    State.save_persistent  state persistent >>= fun () ->
    test_get_persistent ()
    >>= fun (state, persistent) ->
    State.add_target state 
      Target.(create ~name:"First target" Artifact_type.string_value)
    >>= fun () ->
    begin State.current_targets state
      >>= function
      | [one] when one.Target.name = "First target" -> return ()
      | other ->
        Test.fail (fmt "too many targets: %d" (List.length other)); return ()
    end
    >>= fun () ->

    let test_target_one_step ~state ~make ~spec name check =
      let target = Target.(active ~name ~make spec) in
      State.add_target state target
      >>= fun () ->
      begin State.get_status state (Target.pointer target)
        >>= function
        | `Activated _ -> return ()
        | other -> Test.fail (fmt "T: %S: not activated" name); return ()
      end
      >>= fun () ->
      State.step state >>= fun what_happened ->
      begin State.get_status state (Target.pointer target)
        >>= function
        | s when check s -> return ()
        | other -> Test.fail (fmt "T: %S: wrong status" name); return ()
      end
    in
    let target_succeeds = function `Successful _ -> true | _ -> false in
    let target_fails = function `Dead _ -> true | _ -> false in

    test_target_one_step ~state "ls /" target_succeeds
      ~make:Process.(`Get_output Command.(shell "ls /"))
      ~spec:Artifact_type.string_value
    >>= fun () ->

    test_target_one_step ~state "ls /crazypath" target_fails
      ~make:Process.(`Get_output Command.(shell "ls /crazypath"))
      ~spec:Artifact_type.string_value
    >>= fun () ->

    let host = Test.test_ssh_host in
    test_target_one_step ~state "ls / over ssh" target_succeeds
      ~make:Process.(`Get_output Command.(shell ~host "ls /"))
      ~spec:Artifact_type.string_value
    >>= fun () ->

    test_target_one_step ~state "ls / > <file> over ssh" target_succeeds
      ~make:Process.(`Direct_command 
                       Command.(shell ~host "ls / > /tmp/ketrew_test"))
      ~spec:(Artifact_type.volume 
               Volume.(create ~host ~root:"/tmp" (file "ketrew_test")))
    >>= fun () ->

    (* A target that creates a more complex file structure *)
    let cmd = 
      Command.(
        shell ~host 
          "mkdir -p /tmp/ketrew_test2/somedir && \
           ls / > /tmp/ketrew_test2/ls && \
           ps aux > /tmp/ketrew_test2/somedir/psaux ")
    in
    let vol =
      Volume.(create ~host ~root:"/tmp" 
                (dir "ketrew_test2" [file "ls"; dir "somedir" [file "psaux"]]))
    in
    test_target_one_step ~state "2 files, 2 dirs over ssh" target_succeeds
      ~make:Process.(`Direct_command cmd) ~spec:(Artifact_type.volume vol)
    >>= fun () ->

    (* A target that fails to create a more complex file structure *)
    let vol =
      Volume.(create ~host ~root:"/tmp" 
                (dir "ketrew_test2" [file "ls";
                                     dir "somedir_typo" [file "psaux"]]))
    in
    test_target_one_step ~state "2 files, 2 dirs over ssh + typo" target_fails
      ~make:Process.(`Direct_command cmd) ~spec:(Artifact_type.volume vol)
    >>= fun () ->

    System.remove db_file
    >>= fun () ->
    return ()
  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_0: Ketrew ERROR:  " %s (Error.to_string e) @ error);
    Test.fail "test_0 ends with error"


let () =
  let argl = Sys.argv |> Array.to_list in
  global_with_color := not (List.mem ~set:argl "-no-color");
  global_debug_level := 3;
  mini_db_test ();
  test_0 ();
  begin match !Test.failed_tests with
  | [] ->
    Log.(s "No tests failed \\o/ " @ normal);
    exit 0
  | some ->
    Log.(s "Some tests failed: " %n % OCaml.list s some @ error);
    exit 3
  end
