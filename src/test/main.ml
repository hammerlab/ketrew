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
    let test_get_engine () =
      let configuration = Configuration.create db_file () in
      State.create configuration
      >>= fun state ->
      State.get_engine state
      >>= fun engine ->
      return (state, engine)
    in
    test_get_engine () >>= fun (state, engine) ->
    State.save_engine  state engine >>= fun () ->
    test_get_engine ()
    >>= fun (state, engine) ->
    State.add_task state 
      Task.(create ~name:"First task"
              ~target:Target.string_value)
    >>= fun () ->
    begin State.current_tasks state
      >>= function
      | [one] when one.Task.name = "First task" -> return ()
      | other -> Test.fail (fmt "too many tasks: %d" (List.length other)); return ()
    end
    >>= fun () ->
    State.add_task state 
      Task.(create ~name:"Second task"
              ~target:Target.string_value)
    >>= fun () ->
    begin State.current_tasks state
      >>= function
      | [one; two] -> return ()
      | other -> Test.fail (fmt "too many tasks: %d" (List.length other)); return ()
    end
    >>= fun () ->

    Pvem_lwt_unix.System.remove db_file
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
    Log.(s "Some tests failed: " %n % s "" @ error);
    exit 3
  end
