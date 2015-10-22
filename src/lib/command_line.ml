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

let submit ?add_tags ~configuration ~wet_run what =
  let workflow =
    match what with
    | `Daemonize (host_str, cmd) ->
      let name =
        fmt "%S on %s"
          (match String.sub cmd ~index:0 ~length:20 with
          | Some s -> s ^ "..."
          | None -> cmd)
          host_str
      in
      let make =
        EDSL.daemonize ~using:`Python_daemon ~host:(EDSL.Host.parse host_str)
          EDSL.Program.(sh cmd)
      in
      EDSL.target name ~make
  in
  begin match wet_run with
  | true ->
    Client.submit ~override_configuration:configuration ?add_tags workflow
  | false ->
    Log.(s "Dry-run: not submitting the workflow: " % n
         % s (EDSL.to_display_string workflow)
         @ normal);
  end;
  return ()

let generate_token () =
  let () = Random.self_init () in
  List.init 64 ~f:(fun _ -> (Random.bits ()) land 255)
  |> List.map ~f:char_of_int
  |> String.of_character_list
  |> B64.encode ~alphabet:B64.uri_safe_alphabet 

let initialize_configuration
    ?use_database ~tokens ~tls ~port ~debug_level config_path =
  let tokens = if tokens = [] then [generate_token ()] else tokens in
  System.ensure_directory_path config_path
  >>= fun () ->
  begin match tls with
  | `Use (cert, key) -> return (fmt {o|`Tls (%S, %S, %d)|o} cert key port)
  | `Create_self_signed ->
    let cert = config_path // "certificate.pem" in
    let key = config_path // "privkey-nopass.pem" in
    System.Shell.do_or_fail
      (fmt "openssl req -x509 -newkey rsa:2048 \
            -keyout %s -out %s \
            -days 10 -nodes -subj \"/CN=test_ketrew\" 2> /dev/null" key cert)
    >>= fun () ->
    return (fmt {o|`Tls (%S, %S, %d)|o} cert key port)
  | `Don't -> return (fmt "`Tcp %d" port)
  end
  >>= fun server_listen ->
  let auth_tokens_file = config_path // "authorized_tokens" in
  System.Shell.do_or_fail (fmt "touch %s" (Filename.quote auth_tokens_file))
  >>= fun () ->
  System.Shell.do_or_fail (fmt "echo 'PKG ketrew' > %s"
                             ((config_path // ".merlin") |> Filename.quote))
  >>= fun () ->
  let ocaml_config_file_header =
    fmt {ocaml|
let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ();;
#use "topfind"
#thread
#require "ketrew"
open Ketrew.Configuration
let debug_level = %d
|ocaml} debug_level in
  let db_params =
    match use_database with
    | None  -> config_path // "database"
    | Some s -> s in
  let engine =
    fmt {ocaml|
let engine =
  engine ~database_parameters:%S ()
|ocaml}
      db_params
  in
  let server =
    fmt {ocaml|
let server ~daemon =
  server ~daemon ~engine
    ~authorized_tokens:[
       %s
       authorized_tokens_path %S
     ]
    ~return_error_messages:true
    ~log_path:%S
    ~command_pipe:%S
    (%s)
|ocaml}
      (List.map tokens ~f:(fun tok ->
           fmt "authorized_token ~name:\"%s\" %S;" tok tok)
       |> String.concat ~sep:"\n")
      auth_tokens_file
      (config_path // "server-log")
      (config_path // "command.pipe")
      server_listen
  in
  let client =
    fmt {ocaml|
let client =
  client ~token:%S "http%s://127.0.0.1:%d"
|ocaml}
      (match tokens with one :: _ -> one | [] -> "TODO:set-tokens")
      (match tls with `Don't -> "" | _ -> "s")
      port
  in
  let config =
    ocaml_config_file_header
    ^ engine
    ^ server
    ^ client
    ^ {ocaml|
let () =
  output [
    profile "standalone"
      (create ~debug_level (standalone () ~engine));
    profile "server" (create ~debug_level (server ~daemon:false));
    profile "daemon" (create ~debug_level (server ~daemon:true));
    profile "default" (create ~debug_level client);
    profile "client" (create ~debug_level client);
  ]
|ocaml}
  in
  IO.write_file ~content:config (config_path // "configuration.ml")

let show_server_logs ~max_number ?(condition = `True) server_config =
  let ask_for_server_memory_logs format =
    begin match Configuration.command_pipe server_config with
    | Some pipe ->
      let path =
        Filename.temp_file "ketrew-logs"
          (match format with `Json -> ".json" | _ -> ".txt") in
      System.file_info ~follow_symlink:true pipe
      >>= begin function
      | `Fifo ->
        let command =
          fmt "get-log:%s:%s" path
            (match format with `Json -> "json" | _ -> "txt") in
        Log.(s "Sending " % quote command
             % s " through the command pipe: " % quote pipe @ verbose);
        begin 
          System.with_timeout 2. (fun () ->
              IO.with_out_channel (`Append_to_file pipe) ~buffer_size:16 ~f:(fun oc ->
                  IO.write oc command
                  >>= fun () ->
                  IO.write oc "\n"
                )
            )
          >>< function
          | `Ok () -> return ()
          | `Error (`Timeout _) ->
            Log.(s "Writing to the command-pipe timeouted, \
                    is the server running?" @ warning);
            return ()
          | `Error (`IO _ as e) -> fail e
          | `Error (`System _) ->
            fail (`Stop_server_error "System.timeout failed!")
        end
        >>= fun () ->
        (System.sleep 1. >>< fun _ -> return ())
        >>= fun () ->
        return (Some path)
      | other ->
        fail (`Failure (fmt "File %S is not a FIFO!" pipe))
      end
    | None -> return None
    end in
  begin match Configuration.log_path server_config with
  | None ->
    ask_for_server_memory_logs `Txt
    >>= begin function
    | Some path ->
      IO.with_in_channel (`File path) ?buffer_size:None ~f:(fun i ->
          IO.with_out_channel `Stdout ~f:(fun o ->
              Log.(s "read" @ verbose);
              let rec loop () =
                IO.read i
                >>= function
                | "" -> return ()
                | more ->
                  IO.write o more
                  >>= fun () ->
                  loop ()
              in
              loop ()
            )
        )
    | None ->
      Log.(s "Server has no log-path and no command-pipe, cannot get the logs"
           @ error);
      return ()
    end
  | Some p ->
    ask_for_server_memory_logs `Json
    >>= fun from_memory ->
    let `Stream dir = System.list_directory p in
    let rec get_all acc =
      dir ()  >>= function
      | Some s when Filename.check_suffix s ".json" -> get_all (s :: acc)
      | Some s -> get_all acc
      | None -> return acc
    in
    get_all []
    >>| List.sort ~cmp:(fun a b -> String.compare b a)
    >>| List.map ~f:(fun f -> p // f)
    >>= fun rev_sorted_dir ->
    let rec display count =
      function
      | [] -> return ()
      | _ when count > max_number -> return ()
      | file :: files ->
        IO.read_file file
        >>= fun blob ->
        let json =
          match Yojson.Safe.from_string blob with
          |  j -> j
          | exception err ->
            Log.(s "Error parsing JSON in " % quote file % s " → " % exn err
                 @ warning);
            `List []
        in
        begin match json with
        | `List l ->
          let rec go_through_list count =  function
          | [] -> return count
          | _ when count > max_number -> return count
          | item :: more ->
            let output typed_item =
              Printf.printf "%s[%d]\n%s\n"
                (String.make 72 '=') count
                (Typed_log.Item.show typed_item);
              go_through_list (count + 1) more
            in
            begin match Typed_log.Item.of_yojson item  with
            | `Error err ->
              Log.(s "Error parsing JSON in " % quote file % s " → " % s err
                   @ warning);
              go_through_list count more
            | `Ok typed_item
              when Typed_log.Item.Condition.eval typed_item condition ->
              output typed_item
            | `Ok typed_item -> go_through_list count more
            end
          in
          (* Logging.Log_store writes in order, so we reverse: *)
          go_through_list count (List.rev l)
        | _ ->
          fail (`Failure "JSON logs not a list")
        end
        >>= fun count ->
        display count files
    in
    display 1
      (match from_memory with
      | None -> rev_sorted_dir
      | Some s -> s :: rev_sorted_dir)
  end

  
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
        | Some path ->
          Sys.command (fmt "mkdir -p %s" (Filename.quote path)) |> ignore;
          let file_name = path // "debug.txt" in
          let out =
            UnixLabels.(
              openfile ~perm:0o700 file_name ~mode:[O_APPEND; O_CREAT; O_WRONLY]
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
  let configuration_file_arg =
    Arg.(value & opt (some string) None
         & info ["C"; "configuration-file"]
           ~docs:common_options_section ~docv:"FILE"
           ~doc:"Use $(docv) as configuration file (can be overriden also \
                 with `$KETREW_CONFIGURATION`)." )
  in
  let make_profile_arg args =
    let at, names = match args with
      | [] -> Arg.(pos 0 (some string) None), []
      | p  -> Arg.(opt (some string) None), p
    in
    Arg.(value & at & info names
           ~docs:common_options_section ~docv:"NAME"
           ~doc:"Use the profile $(docv) within the configuration file \
                 (can be overriden also with `$KETREW_PROFILE`).")
  in
  let make_configuration_arg profile_names =
    Term.(
      pure (fun profile path_opt ->
          let how =
            match path_opt, override_configuration with
            | Some path, _ -> `From_path path
            | None, Some conf -> `Override conf
            | None, None -> `Guess
          in
          Configuration.load_exn ?profile how)
      $ make_profile_arg profile_names
      $ configuration_file_arg)
  in
  let configuration_arg = make_configuration_arg ["P"; "configuration-profile"] in
  let init_cmd =
    sub_command
      ~info:(Term.info "initialize" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
               ~doc:"Initialize the application (create a config-directory)")
      ~term:Term.(
          pure (fun cert_key self_tls debug_level tokens port
                 config_path use_database ->
                 let tls =
                   match cert_key with
                   | Some (cert, key) -> `Use (cert, key)
                   | None when self_tls -> `Create_self_signed
                   | _ -> `Don't
                 in
                 initialize_configuration
                   ?use_database ~tls ~port ~debug_level ~tokens config_path)
          $ Arg.(info ["tls"] ~docv:"CERT,KEY"
                   ~doc:"Configure the server to listen on HTTPS"
                 |> opt (pair string string |> some) None
                 |> value) 
          $ Arg.(info ["self-signed-tls"]
                   ~doc:"Configure the server to listen on HTTPS by \
                         generating a self-signed certificate/private-key pair"
                 |> flag |> value)
          $ Arg.(info ["debug-level"] ~docv:"INT"
                   ~doc:"Set the debug-level $(docv)."
                 |> opt int 0 |> value)
          $ Arg.(info ["with-token"]
                   ~docv:"STRING"
                   ~doc:"Add $(docv) to the list of authentication token."
                 |> opt_all string [] |> value)
          $ Arg.(value & opt int 8756
                 & info ["port"] ~docv:"INT"
                   ~doc:"Set the server port to $(docv).")
          $ Arg.(value
                 & opt string Configuration.default_configuration_directory_path
                 & info ["configuration-path"] ~docv:"DIR"
                   ~doc:"Create the configuration in $(docv).")
          $ Arg.(
              let doc =
                fmt "Use the given URI for the database configuration \
                     (the default being a Sqlite DB in the configuration \
                     directory, available backends: %s)."
                  (String.concat ~sep:", " Trakeva_of_uri.available_backends) in
              info ["use-database"] ~docv:"URI" ~doc
              |> opt (some string) None |> value) 
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
          $ configuration_arg
          $ Arg.(value @@ opt string "open"
                 @@ info ["O"; "open-command"]
                   ~doc:"Command to use as browser.")
        ) in
  let logs_cmd = 
    sub_command
      ~info:(Term.info "logs" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
               ~doc:"See the logs.")
      ~term: Term.(
          pure (fun configuration field_equals has_fields max_number ->
              let condition =
                `Ignore_case (
                  `And [
                    `And (List.map has_fields ~f:(fun f -> `Has_field f));
                    `And (List.map field_equals ~f:(fun p -> `Field_equals p));
                  ]
                ) in
              match Configuration.mode configuration  with
              | `Client _ | `Standalone _ ->
                fail (`Failure "This is not a configured Ketrew server")
              | `Server s ->
                show_server_logs ~max_number ~condition s)
          $ configuration_arg
          $ Arg.(
              info ["E"; "field-equals"]
                ~docv:"FIELD,VALUE"
                ~doc:"Filter the output for log-items that have the FIELD equal \
                      to the VALUE"
              |> opt_all (pair string string) []
              |> value)
          $ Arg.(
              info ["F"; "has-field"]
                ~docv:"FIELD"
                ~doc:"Filter the output for log-items that have the FIELD \
                      present"
              |> opt_all string []
              |> value)
          $ Arg.(value @@ opt int 4000
                 @@ info ["M"; "max-items"]
                   ~doc:"Set a maximum number of log items to display.")
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
          $ configuration_arg
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
        $ configuration_arg
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
        $ configuration_arg
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
        pure (fun configuration  ->
            Log.(Configuration.log configuration
                 @ normal); return ())
        $ make_configuration_arg [])
      ~info:(info "print-configuration" ~version
               ~doc:"Display current configuration." ~man:[])
  in
  let submit_cmd =
    let open Term in
    sub_command
      ~term:(
        pure begin fun configuration daemonize wet_run add_tags ->
          begin match daemonize with
          | Some (host, cmd) ->
            submit ~configuration ~add_tags
              ~wet_run (`Daemonize (host, cmd))
          | None -> 
            Log.(s "Nothing to do." @ normal);
            return ()
          end
        end
        $ configuration_arg
        $ Arg.(info ["daemonize"] ~docv:"HOST,CMD"
                 ~doc:"Submit a daemonized command on a given HOST."
               |> opt (pair string string |> some) None
               |> value)
        $ Arg.(info ["w"; "wet-run"]
                 ~doc:"Really submit the workflow (the default is to dry-run"
               |> flag |> value)
        $ Arg.(info ["tag"]
                 ~docv:"STRING"
                 ~doc:"Tag the workflow with $(docv)."
               |> opt_all string [] |> value)
      )
      ~info:(
        info "submit" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Submit (simple) workflows." ~man:[])
  in
  let interact_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun configuration ->
            Client.as_client ~configuration ~f:interact)
        $ configuration_arg)
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
        $ configuration_arg)
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
        $ configuration_arg)
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
        $ configuration_arg)
      ~info:(
        info "stop-server" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Stop the server." ~man:[])
  in
  let sync_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun src dst ->
            Persistent_data.Synchronize.copy src dst
          )
        $ Arg.(info ["from"] ~docv:"SRC" ~doc:"The source to copy from"
               |> opt (some string) None
               |> required)
        $ Arg.(info ["to"] ~docv:"DST" ~doc:"The destination to copy to"
               |> opt (some string) None
               |> required)
      )
      ~info:(
        info "synchronize" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Synchronize/copy a ketrew database."
          ~man:[
            `P "The strings SRC and DST are URIs that the Trakeva library \
                can accept (i.e. like in the configuration file) *or* \
                a URI starting with `backup://` and indicating a path \
                to a directory.";
          ]
      )
  in
  let internal_ssh_command =
    let open Term in
    sub_command
      ~term:(
        pure (fun command pipe_in pipe_out log_to control_path session_id_file
               uri ->
               Process_holder.Ssh_connection.setsid_ssh
                 ?session_id_file ?log_to ?command ?pipe_in ?pipe_out
                 ?control_path uri
             )
        $ Arg.(info ["command"; "c"] ~docv:"COMMAND" ~doc:"The command to run"
               |> opt (some string) None
               |> value)
        $ Arg.(info ["pipe-in"; "fifo-in"] ~docv:"PATH" ~doc:"Use PATH to communicate"
               |> opt (some string) None
               |> value)
        $ Arg.(info ["pipe-out"; "fifo-out"] ~docv:"PATH" ~doc:"Use PATH to communicate"
               |> opt (some string) None
               |> value)
        $ Arg.(info ["log-to"] ~docv:"PATH" ~doc:"Log JSON blobs to PATH"
               |> opt (some string) None
               |> value)
        $ Arg.(info ["control-path"] ~docv:"PATH" ~doc:"Set SSH ControlPath"
               |> opt (some string) None
               |> value)
        $ Arg.(info ["write-session-id"] ~docv:"PATH"
                 ~doc:"Write the session ID of the daemon to PATH"
               |> opt (some string) None
               |> value)
        $ Arg.(info ["to"] ~docv:"URI" ~doc:"The host in “URI form”"
               |> opt (some string) None
               |> required)
      )
      ~info:(
        info "internal-ssh" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Call ssh in the background and talk to it with a named-pipe \
                (for internal use)"
      )
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
      submit_cmd;
      explore_cmd;
      logs_cmd;
      start_server_cmd; stop_server_cmd;
      print_conf_cmd; make_command_alias print_conf_cmd "pc";
      sync_cmd;
      internal_ssh_command;
    ] in
  match Term.eval_choice ?argv default_cmd cmds with
  | `Ok f -> f
  | `Error _ -> exit Return_code.cmdliner_error
  | `Version | `Help -> exit 0


let run_main ?argv ?override_configuration ?additional_commands () =
  let main_lwt_thread =
    cmdliner_main ?argv ?override_configuration ?additional_commands ()
  in
  match Lwt_main.run (main_lwt_thread >>< Return_code.transform_error) with
  | `Ok () -> exit 0
  | `Error n -> exit n



