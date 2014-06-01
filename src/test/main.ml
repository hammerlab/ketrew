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
    let second_target =
      Target.(active ~name:"Second target, active"
                ~make:Process.(`Get_output Command.(shell "ls /"))
                Artifact_type.string_value) in
    State.add_target state  second_target >>= fun () ->
    begin State.current_targets state
      >>= function
      | [one; two] -> return ()
      | other ->
        Test.fail (fmt "too many targets: %d" (List.length other)); return ()
    end
    >>= fun () ->
    State.step state >>= fun () ->
    let count_targets state =
      State.current_targets state >>= fun targets ->
      let count ~s ~i ~r ~d =
        (`Successful s, `Inactive i, `Running r, `Dead d) in
      let init = count 0 0 0 0 in
      List.fold targets ~init ~f:(fun prev trgt ->
          let `Successful s, `Inactive i, `Running r, `Dead d = prev in
          match trgt.Target.history with
          | `Created _ -> count ~s ~d ~r ~i:(i + 1)
          | `Successful _ -> count ~s:(s + 1) ~d ~r ~i
          | `Running _ -> count ~r:(r + 1) ~d ~s ~i
          | `Dead _ -> count ~d:(d + 1) ~s ~r ~i
          | `Activated _ -> prev)
      |> return
    in
    begin count_targets state >>= function
      | `Successful 1, `Inactive 1, _, _ -> return ()
      | `Successful s, `Inactive i, _, _ ->
        Test.fail (fmt "wrong counts 1: %d %d" s i); return ()
    end
    >>= fun () ->

    State.add_target state 
      Target.(active ~name:"3rd target, active, failing"
                ~make:Process.(`Get_output Command.(shell "ls /crazypath"))
                Artifact_type.string_value)
    >>= fun () ->
    State.step state >>= fun () ->
    begin count_targets state >>= function
      | `Successful 1, `Inactive 1, _, `Dead 1 -> return ()
      | `Successful s, `Inactive i, _, `Dead d ->
        Test.fail (fmt "wrong counts 3rd: %d %d %d" s i d); return ()
    end
    >>= fun () ->

    State.add_target state 
      Target.(active ~name:"4th target, active, over SSH"
                ~make:Process.(
                    `Get_output Command.(shell ~host:Test.test_ssh_host "ls /"))
                Artifact_type.string_value)
    >>= fun () ->
    State.step state >>= fun () ->
    begin count_targets state >>= function
      | `Successful 2, `Inactive 1, _, `Dead 1 -> return ()
      | `Successful s, `Inactive i, _, `Dead d ->
        Test.fail (fmt "wrong counts 4rth: %d %d %d" s i d); return ()
    end
    >>= fun () ->

    let host = Test.test_ssh_host in
    State.add_target state 
      Target.(active ~name:"5th target, active, file creation over SSH"
                ~make:Process.(`Direct_command 
                                 Command.(shell ~host "ls / > /tmp/ketrew_test"))
                (Artifact_type.volume 
                   Volume.(create ~host ~root:"/tmp" (file "ketrew_test"))))
    >>= fun () ->
    State.step state >>= fun () ->
    begin count_targets state >>= function
      | `Successful 3, `Inactive 1, _, `Dead 1 -> return ()
      | `Successful s, `Inactive i, _, `Dead d ->
        Test.fail (fmt "wrong counts 4rth: %d %d %d" s i d); return ()
    end
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
    State.add_target state 
      Target.(active ~name:"6th target, active, tree creation over SSH"
                ~make:Process.(`Direct_command cmd)
                (Artifact_type.volume vol))
    >>= fun () ->
    State.step state >>= fun () ->
    begin count_targets state >>= function
      | `Successful 4, `Inactive 1, _, `Dead 1 -> return ()
      | `Successful s, `Inactive i, _, `Dead d ->
        Test.fail (fmt "wrong counts 4rth: %d %d %d" s i d); return ()
    end
    >>= fun () ->

    (* A target that fails to create a more complex file structure *)
    let cmd = 
      Command.(
        shell ~host 
          "mkdir -p /tmp/ketrew_test2/somedir && \
           ls / > /tmp/ketrew_test2/ls && \
           ps aux > /tmp/ketrew_test2/somedir/psaux ")
    in
    let vol =
      Volume.(create ~host ~root:"/tmp" 
                (dir "ketrew_test2" [file "ls"; dir "somedir_typo" [file "psaux"]]))
    in
    State.add_target state 
      Target.(active ~name:"6th target, active, tree creation over SSH"
                ~make:Process.(`Direct_command cmd)
                (Artifact_type.volume vol))
    >>= fun () ->
    State.step state >>= fun () ->
    begin count_targets state >>= function
      | `Successful 4, `Inactive 1, _, `Dead 2 -> return ()
      | `Successful s, `Inactive i, _, `Dead d ->
        Test.fail (fmt "wrong counts 4rth: %d %d %d" s i d); return ()
    end
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
