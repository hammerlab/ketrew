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

(** Display errors, and return an exit code (integer). *)
module Return_code = struct
  let cmdliner_error = 3
  let ketrew_error = 5

  let of_error = function
  | e ->
    Log.(s "Error: " % s (Error.to_string e) @ error);
    ketrew_error

  let transform_error = function
  | `Ok () -> return ()
  | `Error e -> fail (of_error e)

end

let get_status ~client =
  Client.all_targets client
  >>= fun targets ->
  let in_progress =
    List.fold targets ~init:0 ~f:(fun prev t ->
        let open Target in
        match state t  |> State.simplify with
        | `In_progress -> prev + 1
        | `Failed
        | `Activable
        | `Successful -> prev)
  in
  return (`In_progress in_progress)

(** The function behind the [ketrew status] sub-command (and the equivalent
    command in [ketrew interactive]). *)
let rec display_status ~client ~loop  =
  let display () =
    get_status ~client
    >>= fun (`In_progress inp) ->
    Log.(s "Current targets “in-progress”: " % i inp @ normal);
    return (inp = 0)
  in
  begin match loop with
  | None -> display () >>= fun _ -> return ()
  | Some seconds ->
    let keep_going = ref true in
    let rec loop () = 
      display ()
      >>= fun nothing_left_to_do ->
      (System.sleep seconds >>< fun _ -> return ())
      >>= fun () ->
      (if !keep_going && not nothing_left_to_do then loop () else return ())
    in
    Interaction.run_with_quit_key
      (object
        method start = loop ()
        method stop = keep_going := false
      end)
  end

(** The function behind the [ketrew run <how>] sub-command (and the equivalent
    command in [ketrew interactive]). *)
let run_state ~client ~max_sleep ~how =
  begin match Client.get_local_engine client with
  | None ->
    fail (`Failure "This client is not Standalone, it can not run stuff.")
  | Some state ->
    begin match how with
    | ["step"] ->
      Engine.Run_automaton.step state
      >>= fun (_ : bool) ->
      return ()
    | ["fix"] ->
      Engine.Run_automaton.fix_point state
      >>= fun (`Steps step_count) ->
      Log.(i step_count % s " steps ran" @ normal);
      return ()
    | "loop" :: [] ->
      let block, should_keep_going, stop_it =
        let keep_going = ref true in
        let traffic_light = Light.create () in
        (fun () -> Light.try_to_pass traffic_light),
        (fun () -> !keep_going),
        (fun () ->
           keep_going := false;
           Light.green traffic_light ) in
      let rec loop previous_sleep =
        Engine.Run_automaton.fix_point state
        >>= fun (`Steps step_count) ->
        Log.(s "Getting new status" @ verbose);
        get_status ~client
        >>= begin function
        | `In_progress 0 ->
          Log.(s "Nothing left to do" @ verbose);
          stop_it ();
          return ()
        | _ ->
          let seconds =
            match step_count with
            | 1 -> 2.
            | _ -> min (previous_sleep *. 2.) max_sleep
          in
          Log.(s "Sleeping " % f seconds % s " s" @ very_verbose);
          begin Deferred_list.pick_and_cancel [
              System.sleep seconds;
              block ();
            ] >>< function
            | `Ok () when should_keep_going () ->
              loop seconds
            | `Ok () -> return ()
            | `Error e ->
              Log.(s "System.Sleep Error!!"  @ error);
              fail (`Failure "System.sleep")
          end
        end
      in
      Interaction.run_with_quit_key
        (object method start = loop 2. method stop = stop_it () end)
    | sl ->
      Log.(s "Unknown client-running command: " % OCaml.list (sf "%S") sl
           @ error);
      fail (`Wrong_command_line sl)
    end
  end

(** Kill targets (command line, ["--interactive"], or within
    [ketrew interactive]. *)
let kill ~client ~interactive ids =
  begin
    begin if interactive then
        Interaction.build_sublist_of_targets ~client ~list_name:"Kill list"
          ~all_log:Log.(s "Kill'em All") ~go_verb:Log.(s "kill")
          ~filter:(fun t -> Target.(state t |> State.Is.killable))
      else
        return (`Go [])
    end
    >>= function
    | `Go additional_ids ->
      let to_kill = additional_ids @ ids in
      Log.(
        (match List.length to_kill with
         | 0 -> s "There is nothing to kill."
         | _ -> s "Killing " % OCaml.list s to_kill)
        @ warning);
      Client.kill client to_kill
    | `Cancel ->
      Log.(s "Cancelling murder plans." @ normal);
      return ()
  end


(** The function behind [ketrew interact]. *)
let interact ~client =
  let can_run_stuff =
    Client.get_local_engine client <> None
  in
  let rec main_loop () =
    Interaction.(
      menu ~sentence:Log.(s "Main menu")
        ~always_there:[ menu_item ~char:'q' ~log:Log.(s "Quit") `Quit;
                        menu_item ~char:'v' ~log:Log.(s "Toggle verbose") `Verbose]
        (
          [ menu_item ~char:'s' ~log:Log.(s "Display current status")
              (`Status None); ]
          @ (if can_run_stuff then [
              menu_item ~char:'r' ~log:Log.(s "Run fix-point") (`Run ["fix"]);
              menu_item ~char:'l' ~log:Log.(s "Run loop") (`Run ["loop"]);
            ] else [
               menu_item ~char:'l' ~log:Log.(s "Loop displaying the status")
                 (`Status (Some 2.));
             ])
          @ [
            menu_item ~char:'k' ~log:Log.(s "Kill targets") `Kill;
            menu_item ~char:'e' ~log:Log.(s "The Target Explorer™") `Explore;
          ]
        )
    )
    >>= function
    | `Quit -> return ()
    | `Verbose ->
        Interaction.toggle_verbose ();
        main_loop ()
    | `Status loop ->
      display_status ~client ~loop
      >>= fun () ->
      main_loop ()
    | `Kill ->
      kill ~interactive:true ~client []
      >>= fun () ->
      main_loop ()
    | `Run how ->
      run_state ~client  ~max_sleep:120. ~how
      >>= fun () ->
      main_loop ()
    | `Explore ->
      Explorer.(create ~client () |> explore)
      >>= fun () ->
      main_loop ()
  in
  main_loop ()

let daemonize_if_applicable config =
  let exit_parent () =
    (* in case some library add at-exit hooks:
       cf. https://github.com/ocsigen/lwt/blob/master/src/unix/lwt_daemon.ml#L60
    *)
    Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
    exit 0
  in
  match config with
  | `Do_not_daemonize -> ()
  | `Daemonize_with log_path_opt ->
    (* Cf. http://stackoverflow.com/questions/3095566/linux-daemonize *)
    begin match Unix.fork () with
    | pid when pid <> 0 ->
      Log.(s "Session leader goes to the background as " % i pid @ verbose);
      exit_parent ();
    | _ ->
      let retsetsid = Unix.setsid () in
      Log.(s "Daemonizing: setsid → " % i retsetsid @ verbose);
      begin match Unix.fork () with
      | pid when pid <> 0 ->
        Log.(s "Child of session-leader goes to the background as " % i pid @ verbose);
        exit_parent ();
      | _ ->
        begin match log_path_opt with
        | None -> ()
        | Some file_name ->
          let out =
            UnixLabels.(
              openfile ~perm:0o600 file_name ~mode:[O_APPEND; O_CREAT; O_WRONLY]
              |> out_channel_of_descr)
          in
          global_with_color := false;
          Log.(s "Daemonizing: Logging to " % quote file_name @ verbose);
          let pid = Unix.getpid () in
          global_log_print_string := begin fun s ->
            Printf.fprintf out "####### %s (PID: %d)\n%s%!"
              Time.(now () |> to_filename) pid s
          end;
        end;
        Unix.chdir "/";
        ignore (Unix.umask 0);
        Unix.close Unix.stdin;
        Unix.close Unix.stdout;
        Unix.close Unix.stderr;
        let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o666 in
        Unix.dup2 dev_null Unix.stdin;
        Unix.dup2 dev_null Unix.stdout;
        Unix.dup2 dev_null Unix.stderr;
      end;
    end

(** One {!Cmdliner} hack found in Opam codebase to create command aliases. *)
let make_command_alias cmd ?(options="") name =
  let open Cmdliner in
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s%s)." orig options in
  let man = [
    `S "DESCRIPTION";
    `P (Printf.sprintf "$(b,$(mname) %s) is an alias for $(b,$(mname) %s%s)."
          name orig options);
    `P (Printf.sprintf "See $(b,$(mname) %s --help) for details."
          orig);
  ] in
  (term, Term.info name ~docs:"SOME COMMAND ALIASES" ~doc ~man)

(** The configuration of the command line, using the [Cmdliner] library. *)
let cmdliner_main ?override_configuration ?argv ?(additional_commands=[]) () =
  let open Cmdliner in
  let version = Lazy.force Metadata.version in
  let common_options_section = "COMMON OPTIONS" in
  let sub_command ~info ~term = (term, info) in
  let config_file_argument =
    Term.(
      pure (fun profile path_opt ->
          let how =
            match path_opt, override_configuration with
            | Some path, _ -> `From_path path
            | None, Some conf -> `Override conf
            | None, None -> `Guess
          in
          Configuration.load_exn ?profile how)
      $ Arg.(value & opt (some string) None
             & info ["P"; "configuration-profile"]
               ~docs:common_options_section ~docv:"NAME"
               ~doc:"Use the profile $(docv) within the configuration file \
                     (can be overriden also with `$KETREW_PROFILE`).")
      $ Arg.(value & opt (some string) None
             & info ["C"; "configuration-file"]
               ~docs:common_options_section ~docv:"FILE"
               ~doc:"Use $(docv) as configuration file (can be overriden also \
                     with `$KETREW_CONFIGURATION`)." )
    )
  in
  let init_cmd =
    sub_command
      ~info:(Term.info "initialize" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
               ~doc:"Initialize the client (create a config-file)")
      ~term:Term.(
          pure (fun db_path debug_level config_path ->
              System.ensure_directory_path (Filename.dirname config_path)
              >>= fun () ->
              let open Configuration in
              let config = [
                profile "default"
                  (create ~debug_level
                     (standalone ()
                        ~engine:(engine ~database_parameters:db_path ())));
              ] in
              IO.write_file ~content:(config |> to_json) config_path)
          $ Arg.(value & opt string Configuration.default_database_path
                 & info ["database"] ~docv:"FILE"
                   ~doc:"Use $(docv) as database.")
          $ Arg.(value & opt int 1
                 & info ["debug-level"] ~docv:"INT"
                   ~doc:"Set the debug-level $(docv).")
          $ Arg.(required & pos 0 (some string) None
                 & info [] ~docv:"PATH" ~doc:"Path of the generated config file")
        ) in
  let start_gui =
    sub_command
      ~info:Term.(info "gui" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
                    ~doc:"Get info about this instance.")
      ~term: Term.(
          pure (fun configuration open_cmd  ->
              match Configuration.mode configuration  with
              | `Server _
              | `Standalone _ -> fail (`Failure "This not a client")
              | `Client c ->
                let url = Configuration.connection c in
                let token = Configuration.token c in
                System.Shell.do_or_fail
                  (fmt "%s %s/gui?token=%s" open_cmd url token)
            )
          $ config_file_argument
          $ Arg.(value @@ opt string "open"
                 @@ info ["O"; "open-command"]
                   ~doc:"Command to use as browser (default: `open`).")
        ) in
  let status_cmd =
    sub_command
      ~info:(Term.info "status" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
               ~doc:"Get info about this instance.")
      ~term: Term.(
          pure (fun configuration loop  ->
              match Configuration.mode configuration  with
              | `Client _ | `Standalone _ ->
                let loop = if loop then Some 2. else None in
                Client.as_client ~configuration ~f:(display_status ~loop)
              | `Server s ->
                Server.status ~configuration:s
                >>= fun stat ->
                begin match stat with
                | `Running ->
                  Log.(s "The server appears to be doing well." @ normal);
                  return ()
                | `Wrong_response response ->
                  Log.(s "There is a server on that port but its response was: "
                       % sexp Cohttp.Response.sexp_of_t response @ warning);
                  return ()
                | `Not_responding why ->
                  Log.(s "The server does not seem to be running"
                       % sp % parens (s why) % s "." @ normal);
                  return ()
                end)
          $ config_file_argument
          $ Arg.(value @@ flag
                 @@ info ["L"; "loop"]
                   ~doc:"(As client) loop until there is nothing left to do.")
        ) in
  let run_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration max_sleep how ->
            Client.as_client ~configuration
              ~f:(run_state ~max_sleep ~how))
        $ config_file_argument
        $ Arg.(value & opt float 60.
               & info ["max-sleep"] ~docv:"SECONDS"
                 ~doc:"Maximal sleep time between 2 steps (applies to `loop`)")
        $ Arg.(non_empty @@ pos_all string [] @@
               info [] ~docv:"HOW"
                 ~doc:"Tell Ketrew to run in a given mode (see below)")
      )
      ~info:(
        let man = [
          `S "THE HOW ARGUMENT";
          `P "The following $(i,“run”) methods are available:";
          `I ("`step`", "run one single step"); `Noblank;
          `I ("`fix`", "run  steps until nothing new happens"); `Noblank;
          `I ("`loop`", "loop `fix` until pressing 'q' (there is a \
                         timed-wait starting at 2 seconds until `--max-sleep`)")
        ] in
        info "run-engine" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Run steps of the engine."  ~man)
  in
  let interactive_flag doc =
    Arg.(value & flag & info ["i"; "interactive"] ~doc) in
  let kill_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration interactive ids ->
            Client.as_client ~configuration
              ~f:(kill ~interactive ids))
        $ config_file_argument
        $ interactive_flag "Go through running targets and kill them with 'y' \
                            or 'n'."
        $ Arg.(value @@ pos_all string [] @@
               info [] ~docv:"Target-Id" ~doc:"Kill target $(docv)"))
      ~info:(
        info "kill" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Kill a target."
          ~man:[])
  in
  let print_conf_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration ->
            Log.(Configuration.log configuration
                 @ normal); return ())
        $ config_file_argument)
      ~info:(info "print-configuration" ~version
               ~doc:"Display current configuration." ~man:[])
  in
  let interact_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration ->
            Client.as_client ~configuration ~f:interact)
        $ config_file_argument)
      ~info:(
        info "interact" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Run the interactive menu." ~man:[])
  in
  let explore_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration ->
            Client.as_client ~configuration
              ~f:Explorer.(fun ~client -> create ~client () |> explore)
          )
        $ config_file_argument)
      ~info:(
        info "explore" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Run the interactive Target Explorer." ~man:[])
  in
  let start_server_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration ->
            match Configuration.mode configuration with
            | `Server srv ->
              daemonize_if_applicable
                Configuration.(
                  match daemon srv, log_path srv  with
                  | true, popt -> `Daemonize_with popt
                  | false, _ -> `Do_not_daemonize
                );
              Server.start srv
            | other -> fail (`Failure "not a server")
          )
        $ config_file_argument)
      ~info:(
        info "start-server" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Start the server." ~man:[])
  in
  let stop_server_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration ->
            begin match Configuration.mode configuration with
            | `Server srv -> Server.stop srv
            | other -> fail (`Failure "not a server")
            end
            >>= function
            | `Done -> Log.(s "Server killed."  @ normal); return ()
            | `Timeout ->
              Log.(s "Write-operation timeout; the server must not be \
                      running, try sub-command `status`" @ warning);
              return ())
        $ config_file_argument)
      ~info:(
        info "stop-server" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Stop the server." ~man:[])
  in
  let default_cmd =
    let doc = "A Workflow Engine for Complex Experimental Workflows" in
    let man = [
      `S "AUTHORS";
      `P "Sebastien Mondet <seb@mondet.org>"; `Noblank;
      `S "BUGS";
      `P "Browse and report new issues at"; `Noblank;
      `P "<https://github.com/hammerlab/ketrew>.";
    ] in
    sub_command
      ~term:Term.(ret (pure (`Help (`Plain, None))))
      ~info:(Term.info "ketrew" ~version ~doc ~man) in
  let cmds =
    List.map additional_commands
      ~f:(fun (t, i) ->
          let open Term in
          pure (fun res ->
              res >>< function
              | `Ok o -> return o
              | `Error s -> fail (`Failure s)) $ t, i)
    @ [
      init_cmd; status_cmd; start_gui; run_cmd; kill_cmd;
      interact_cmd;
      explore_cmd;
      start_server_cmd; stop_server_cmd;
      print_conf_cmd; make_command_alias print_conf_cmd "pc";
    ] in
  match Term.eval_choice ?argv default_cmd cmds with
  | `Ok f -> f
  | `Error _ -> exit Return_code.cmdliner_error
  | `Version | `Help -> exit 0


let run_main ?argv ?override_configuration ?additional_commands () =
  Log.(s "Set preemptive bounds: 10, 52" @ verbose);
  Lwt_preemptive.init 10 52 (fun str ->
      Log.(s" Lwt_preemptive error: " % s str @ error);
      );
  let main_lwt_thread =
    cmdliner_main ?argv ?override_configuration ?additional_commands ()
  in
  (* Log.(s "Calling Lwt_main.run" @ very_verbose); *)
  match Lwt_main.run (main_lwt_thread >>< Return_code.transform_error) with
  | `Ok () -> exit 0
  | `Error n -> exit n



