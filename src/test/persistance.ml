
open Ketrew_pure
open Ketrew
open Internal_pervasives
open Unix_io

module P = Persistent_data

(*
   Cf. https://hub.docker.com/_/postgres/

   docker run --name ketrew-postgres -p 5432:5432 -e POSTGRES_PASSWORD=ketrewpostrespw -d postgres

   docker kill ketrew-postgres
   docker rm ketrew-postgres

   docker run -it --rm --link ketrew-postgres:postgres postgres psql -h postgres -U postgres

   Display results as records:

       \x on

   Display BYTEA as strings:

       SET bytea_output = 'escape';


   ./ketrew-persistance-test "postgresql://127.0.0.1/?user=postgres&password=ketrewpostrespw"
   

   rm -fr /tmp/kbackup
   ./ketrew synchr --from "postgresql://127.0.0.1/?user=postgres&password=ketrewpostrespw" --to backup:///tmp/kbackup
   find /tmp/kbackup/hecto_000000/ -type f | wc -l
   find /tmp/kbackup -type f | wc -l

   ./ketrew synchr --to "postgresql://127.0.0.1/?user=postgres&password=ketrewpostrespw" --from backup:///tmp/kbackup
*)

let pr fmt = Printf.ksprintf (Printf.printf "test-persist: %s\n%!") fmt

module Test = struct

  let failures = ref []
  let fail msg = failures := msg :: !failures; return ()
  let failf fmt = Printf.ksprintf fail fmt

  let assertf c fmt =
    Printf.ksprintf (fun s -> if c then return () else fail s) fmt


  let display () =
    List.iter !failures ~f:(fun m ->
        Printf.printf "Test failure: %s\n%!" m;
      );
    if List.length !failures = 0
    then (Printf.printf "All tests OK\n%!"; 0)
    else 1

end

let main () =
  P.create ~database_parameters:Sys.argv.(1)
  >>= fun persist ->
  begin
    P.get_target persist "some id does not exist"
    >>< function
    | `Ok t ->
      Test.failf "First get_target got Ok"
    | `Error (`Database _) -> return ()
    | `Error e ->
      Test.failf "First get_target wrong error: %s" (Error.to_string e)
  end
  >>= fun () ->
  let node1 = Target.create ~name:"first one" () in
  P.Adding_targets.force_add_passive_target persist node1
  >>= fun () ->
  P.get_target persist (Target.id node1)
  >>= fun t ->
  Test.assertf Target.(name t = name node1) "Node1 get didn't work!"
  >>= fun () ->
  P.activate_target persist ~target:node1 ~reason:`User
  >>= fun () ->
  P.get_target persist (Target.id node1)
  >>= fun t ->
  Test.assertf Target.(
      name t = name node1
      && state t |> State.Is.activated_by_user
    ) "Node1 activation didn't work!"
  >>= fun () ->
  P.fold_active_targets persist ~init:[] ~f:(fun l ~target ->
      pr "Active: %s" (Target.id target);
      Test.assertf Target.(
          name t = name node1
          && state t |> State.simplify = `In_progress
        ) "Fold active targets"
      >>= fun () ->
      return ((Target.id target) :: l)
    )
  >>= fun all_active ->
  let to_kill = Target.id node1 :: List.take all_active 2 in
  pr "To-kill: %s" (String.concat ~sep:"," to_kill);
  P.Killing_targets.add_target_ids_to_kill_list persist to_kill
  >>= fun () ->
  P.Killing_targets.proceed_to_mass_killing persist
  >>= fun ch ->
  let nodes =
    let m n = Target.create ~name:n () in
    [m "one"; m "two"; m "three"]
  in
  P.Adding_targets.register_targets_to_add persist nodes
  >>= fun () ->
  (* The `node1` was at least alive so this should return `true`: *)
  Test.assertf ch "Mass killing didn't change anything?"
  >>= fun () ->
  P.unload persist
  >>= fun () ->
  return ()

let () =
  let of_error = function
  | e ->
    Log.(s "Error: " % s (Error.to_string e) @ error);
    2
  in
  let transform_error = function
  | `Ok () -> return ()
  | `Error e -> fail (of_error e)
  in
  match Lwt_main.run (main () >>< transform_error) with
  | `Ok () -> exit (Test.display ())
  | `Error n -> exit n
