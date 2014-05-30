open Ketrew_pervasives

module Test = struct
  let failed_tests = ref []
  let fail s = failed_tests := s :: !failed_tests
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
      Target.(create ~name:"First target" Artefact.string_value)
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
                Artefact.string_value) in
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
                Artefact.string_value)
    >>= fun () ->
    State.step state >>= fun () ->
    begin count_targets state >>= function
      | `Successful 1, `Inactive 1, _, `Dead 1 -> return ()
      | `Successful s, `Inactive i, _, `Dead d ->
        Test.fail (fmt "wrong counts 2: %d %d %d" s i d); return ()
    end
    >>= fun () ->

    System.remove db_file
    >>= fun () ->
    return ()
  end |> function
  | `Ok () -> ()
  | `Error e ->
    Log.(s "test_0: ERROR: TODO " @ error);
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
