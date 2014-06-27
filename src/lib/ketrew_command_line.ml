open Ketrew_pervasives
module Target = Ketrew_target
module Artifact = Ketrew_artifact
module Error = Ketrew_error
module User_command = Ketrew_user_command
module Configuration = Ketrew_configuration

module Return_code = struct
  let user_todo_failure = 2
  let not_implemented = 3
  let cmdliner_error = 4
  let ketew_error = 5
  let wrong_command = 6

  let of_error = function
  | e -> 
    Log.(s "Error: " % s (Error.to_string e) @ error);
    ketew_error

  let transform_error = function
  | `Ok () -> return ()
  | `Error e -> fail (of_error e)

end

let default_item_format = 
  "- $id:$n  Name: $name → $status\n\
  \  Deps: $dependencies$n"

let format_target ~item_format t =
  let open Ketrew_target in
  let buf = Buffer.create 42 in
  Buffer.add_substitute buf (function
    | "n" -> "\n"
    | "id" -> id t
    | "name" -> t.name
    | "dependencies" -> String.concat t.dependencies ~sep:", "
    | "status" -> 
      begin match t.history with
      | `Created _ -> "Created"
      | `Activated _ -> "Activated"
      | `Running _ -> "Running"
      | `Dead (date, previous_state, `Killed reason) -> fmt "Killed: %s" reason
      | `Dead (date, previous_state, `Failed reason) -> fmt "Failed: %s" reason
      | `Successful _ -> "Successful"
      end
    | some_unknown -> fmt "$%s" some_unknown) item_format;
  Buffer.contents buf

module Document = struct

  let build_process ~state ?(with_details=false)  =
    let open Log in
    let open Target in
    let add_details details =
      if with_details then s ": " % n % details else empty in
    let command_details c = add_details (Target.Command.log c) in
    function
    | `Artifact a ->
      s "Artifact" % add_details (Artifact.log a)
    | `Direct_command c ->
      s "Direct command" % command_details c
    | `Get_output c -> 
      s "Get output of command" % command_details c
    | `Long_running (name, content) ->
      s "Long-running " % parens (s name)
      % if with_details 
      then 
        s ":" % n %
        indent (concat (
            List.map
              (Ketrew_state.long_running_log ~state name content)
              ~f:(fun (title, descr) -> s title % s ": " % descr % n)
        ))
      else empty
        

  let target ~state ?build_process_details t =
    let open Log in
    let open Target in
    let itemize l =
      indent (concat (List.map l ~f:(fun (name, log) -> 
          s "* " % s name % s ": " % log %n))) in
    s "Target " % s t.name % n
    % itemize [
      "ID", s t.id;
      "Persistance",
      (match t.persistance with
       | `Recomputable bility -> s "Recomputable " % f bility
       | `Result -> s "Result"
       | `Input_data -> s "Input-data");
      "Dependencies", OCaml.list s t.dependencies;
      "Metadata", Artifact.Value.log t.metadata;
      "Build-process", 
      build_process ~state ?with_details:build_process_details t.make;
      "Status",
      (match t.history with
       | `Dead _       -> s "Dead"
       | `Successful _ -> s "Successful"
       | `Activated _  -> s "Activated"
       | `Created _    -> s "Created"
       | `Running (rb, _) ->
         s "Running " % parens (s rb.plugin_name));
    ]
end

module Interaction = struct

  (** [with_cbreak f] calls with the terminal in “get key” mode. 
         It comes from
         http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html
  *)
  let with_cbreak (f: unit -> (_, _) t) =
    let open Lwt_unix in
    Lwt.(tcgetattr stdin
         >>= fun term_init ->
         let term_cbreak = { term_init with c_icanon = false; c_echo = false } in
         tcsetattr stdin TCSANOW term_cbreak
         >>= fun () ->
         catch f (fun exn -> return (`Error (`Failure "with_cbreak")))
         >>= fun res ->
         tcsetattr stdin TCSADRAIN term_init
         >>= fun () ->
         return res)

  let get_key () =
    with_cbreak (fun () ->
        wrap_deferred (fun () -> Lwt_io.read_char Lwt_io.stdin)
          ~on_exn:(fun e -> (`Failure "get_key")))

  let interaction_chars =
    List.init 10 (fun i -> Char.chr (48 + i))
    @ List.init 26 (fun i -> Char.chr (97 + i))
    
  let menu_item ?char ?(log=Log.empty) v =
    (char, log, v)

  let menu ?(max_per_page=10) ?(always_there=[]) ~sentence items =
    (* let item_number = List.length items in *)
    let items_splitted =
      let split_number = max_per_page - (List.length always_there) in
      let rec split acc l =
        let left, right = List.split_n l split_number in
        match right with
        | [] -> List.rev (left :: acc)
        | more -> split (left :: acc) more
      in
      split [] items
    in
    let number_of_menus = List.length items_splitted in
    let rec menu_loop nth =
      let items =
        always_there @ (List.nth items_splitted nth |> Option.value ~default:[])
      in
      let available_chars =
        List.filter interaction_chars (fun c ->
            List.for_all items (fun (k, _, _) -> k <> Some c)) in
      let filled_items =
        let n = ref 0 in
        List.map items ~f:(function
          | None, l, v -> (incr n; List.nth available_chars !n, l, v)
          | Some k, l, v -> Some k, l, v)
      in
      Log.(sentence % sp 
           % (if nth = 0 && number_of_menus = 1
              then empty
              else brakets (i (nth + 1) % s "/" % i number_of_menus)) % n
           % s "Press a key: " % n
           % concat 
             (List.map filled_items ~f:(function
                | (Some k, l, v) -> sf "* [%c]: " k % l % n
                | None, _, _ -> s "ERROR, wrong usage of `menu`"))
           % (
             let previous = sf "* [<]: previous screen" % n in
             let next = sf "* [>]: next screen" % n in
             match number_of_menus, nth with
             | 1, 0 -> empty
             | _, 0 -> next
             | n, t when t = n - 1 -> previous
             | _, _ -> previous % next
           )
           @ normal);
      get_key () 
      >>= fun key ->
      begin match key with
      | '<' -> menu_loop (nth - 1)
      | '>' -> menu_loop (nth + 1)
      | other ->
        begin match List.find filled_items (fun (k, _, _) -> k = Some key) with
        | Some (_, _, v) -> return v
        | None ->
          Log.(sf "Cannot understand: %c, try again please." key @ normal);
          menu_loop nth
        end
      end
    in 
    menu_loop 0

  let build_sublist_of_targets ~state ~list_name ~filter =
    Ketrew_state.current_targets state
    >>= fun all_targets ->
    let to_process = ref [] in
    let target_menu () =
      List.filter_map all_targets ~f:(fun target ->
          match not (filter target) 
                || List.exists !to_process ~f:(fun id -> id = Target.id target)
          with
          | true -> None
          | false ->
            let item_format = "$name ($status, $id)" in
            Some (
              menu_item 
                ~log:Log.(s "Add: "
                          % bold_yellow (s (format_target ~item_format target)))
                (`Add (Target.id target))))
        in
        let rec loop () =
          menu ~sentence:Log.(s "Add targets to “"
                              % s list_name % s "”")
            ~always_there:[
              menu_item ~char:'G' ~log:Log.(s "Go; proceed") `Done;
              menu_item ~char:'q' ~log:Log.(s "Cancel") `Cancel;
            ]
            (target_menu ())
          >>= function
          | `Add id -> to_process := id :: !to_process; loop ()
          | `Cancel -> return `Cancel
          | `Done -> return (`Go !to_process)
        in
        loop ()

  let make_target_menu ~targets ?(filter_target=fun _ -> true) () =
    let item_format = "$name ($id)" in
    List.filter_map targets ~f:(fun target ->
        match filter_target target with
        | false -> None
        | true ->
          Some (
            menu_item 
              ~log:Log.(bold_yellow (s (format_target ~item_format target)))
              (`Go (Target.id target))))

end
let display_status ~state ~all ~item_format =
  begin
    Log.(s "display_info !" @ verbose);
    Ketrew_state.current_targets state
    >>= fun targets ->
    List.iter targets (fun t -> 
        format_target ~item_format t |> print_string;
      );
    flush stdout;
    return ()
  end

let log_list ~empty l =
  let empty_log = empty in (* renaming because og Log.empty *)
  let open Log in
  let if_empty = sp % empty_log in
  match l with 
  | [] -> if_empty
  | more ->
    n % indent (separate n (List.map more ~f:(fun item -> s "- " % item)))

let run_state ~state ~max_sleep ~how =
  let log_happening ~what_happened =
    let open Log in
    let step_count = List.length what_happened in
    let happening_list =
      List.mapi what_happened ~f:(fun step_index happening_list ->
          List.map happening_list ~f:(fun happening ->
              brakets (s "step " % i (step_index + 1)) % sp % 
              s (Ketrew_state.what_happened_to_string happening)))
      |> List.concat
    in
    let step_sentence =
      match step_count with
      | 0 -> s "No step was executed"
      | 1 -> s "One step was executed" 
      | more -> i more % s " steps were executed"
    in
    Log.(step_sentence % s ":"  %
         log_list ~empty:(s "Nothing happened") happening_list
         @ normal); 
    return ()
  in
  let rec fix_point ~count history =
    Ketrew_state.step state
    >>= fun what_happened ->
    let count = count + 1 in
    begin match history with
    | _ when what_happened = [] ->
      return (count, what_happened :: history)
    | previous :: _ when previous = what_happened ->
      return (count, what_happened :: history)
    | _ -> fix_point ~count (what_happened :: history)
    end
  in
  Log.(s "Running " % OCaml.list s how % s " with max-spleep: " % f max_sleep 
       @ warning);
  begin match how with
  | ["step"] ->
    Ketrew_state.step state
    >>= fun what_happened ->
    log_happening ~what_happened:[what_happened]
  | ["fix"] ->
    fix_point 0 []
    >>= fun (step_count, what_happened) ->
    log_happening ~what_happened:List.(rev what_happened)
  | ["loop"] ->
    let keep_going = ref true in
    let traffic_light = Light.create () in
    let rec loop previous_sleep happenings =
      fix_point 0 []
      >>= fun (step_count, what_happened) ->
      let new_happenings = what_happened @ happenings in
      let seconds =
        match what_happened with
        | [] | [[]] -> 
          min (previous_sleep *. 2.) max_sleep
        | something -> 2.
      in
      Log.(s "Sleeping " % f seconds % s " s" @ very_verbose);
      log_happening ~what_happened:what_happened
      >>= fun () ->
      begin Deferred_list.pick_and_cancel [
          System.sleep seconds;
          Light.try_to_pass traffic_light;
        ] >>< function
        | `Ok () when !keep_going -> loop seconds new_happenings
        | `Ok () ->
          log_happening ~what_happened:(List.rev new_happenings)
        | `Error e ->
          Log.(s "System.Sleep Error!!"  @ error);
          fail (`Failure "System.sleep")
      end
    in
    Deferred_list.for_concurrent ~f:(fun x -> x) [
      begin loop 2. [] end;
      begin
        let rec kbd_loop () =
          Log.(s "Press the 'q' key to stop loopping." @ normal);
          Interaction.get_key ()
          >>= function
          | 'q' | 'Q' ->
            keep_going := false;
            Light.green traffic_light;
            return ()
          | _ -> kbd_loop ()
        in
        kbd_loop ()
      end;
    ]
    >>= fun ((_ : unit list), errors) ->
    begin match errors with
    | [] -> return ()
    | some -> 
      Log.(s "Errors: "
           % OCaml.list (fun e -> s (Error.to_string e)) some @ error);
      fail (`Failure "run")
    end
  | sl -> 
    Log.(s "Unknown state-running command: " % OCaml.list (sf "%S") sl
         @ error);
    fail (`Wrong_command_line sl)
  end

let kill ~state ~interactive ids =
  begin 
    begin if interactive then
        Interaction.build_sublist_of_targets ~state ~list_name:"Kill list"
          ~filter:Target.Is.killable
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
      Deferred_list.while_sequential to_kill (fun id ->
          Ketrew_state.kill state ~id
          >>= function
          | [`Target_died (_, `Killed)] -> 
            Log.(s "Target " % s id % s " killed" @ normal); 
            return ()
          | [`Target_died (_, `Plugin_not_found p)] -> 
            Log.(s "Target " % s id % s ": plugin not found: " % sf "%S" p
                 @ error); 
            return ()
          | [] ->
            Log.(s "Target " % s id % s " was already finished" @ warning); 
            return ()
          | more ->
            Log.(s "Target " % s id % s ": too much happened :" %
                 OCaml.list Ketrew_state.log_what_happened more @ error); 
            return ()
        )
      >>= fun (_ : unit list) ->
      return ()
    | `Cancel -> 
      Log.(s "Cancelling murder plans." @ normal);
      return ()
  end

let archive ~state ~interactive ids =
  begin 
    begin if interactive then
        Interaction.build_sublist_of_targets ~state ~list_name:"Archival list"
          ~filter:Target.Is.finished
      else
        return (`Go [])
    end
    >>= function
    | `Go additional_ids ->
      let to_archive = additional_ids @ ids in
      if List.length to_archive = 0 then 
        Log.(s "There is nothing to archive." @ warning);
      Deferred_list.while_sequential to_archive (fun id ->
          Ketrew_state.archive_target state id)
      >>= fun (_ : unit list) ->
      return ()
    | `Cancel ->
      Log.(s "Cancelling archival" @ normal);
      return ()
  end

module Explorer = struct
  type exploration_state = {
    build_process_details: bool;
    show_archived: bool;
    target_filter: (Target.t -> bool) * Log.t;
    current_target: Target.id option;
  }
  let create_state () =
    {build_process_details = false;
     show_archived = false;
     target_filter = (fun _ -> true), Log.(s "No-filter, see them all");
     current_target = None}


  let cancel_menu_items =
    Interaction.([
        menu_item ~char:'Q' ~log:Log.(s "Quit explorer") `Quit;
        menu_item ~char:'q' ~log:Log.(s "Cancel/Go-back") `Cancel;
      ])

  let filter ~log ~char f =
    (char, log, `Set (f, log))

  let filters = [
    filter (fun _ -> true)      ~char:'n' ~log:Log.(s "No-filter, see them all");
    filter Target.Is.created    ~char:'c' ~log:Log.(s "Just created");
    filter Target.Is.activated  ~char:'a' ~log:Log.(s "Activated");
    filter Target.Is.running    ~char:'r' ~log:Log.(s "Running");
    filter Target.Is.failed     ~char:'t' ~log:Log.(s "Terminated, success or failure");
    filter Target.Is.finished   ~char:'f' ~log:Log.(s "Failed");
    filter Target.Is.successful ~char:'s' ~log:Log.(s "Successful");
    filter Target.Is.activated_by_user ~char:'u'
      ~log:Log.(s "Activated by user (i.e. not as dependency)");
  ]

  let get_filter () =
    Interaction.(
      menu ~sentence:Log.(s "Pick a filter")
        (List.map filters (fun (char, log, tag) ->
             menu_item ~char ~log tag)))
    >>= function
    | `Set f  -> return f

  let pick_a_target ~state (es : exploration_state) =
    Ketrew_state.current_targets state >>= fun current_targets ->
    begin match es.show_archived with
    | true ->
      Ketrew_state.archived_targets state >>= fun l ->
          return (current_targets @ l)
    | false -> return current_targets
    end
    >>= fun targets ->
    Interaction.(
      menu ~sentence:Log.(s "Pick a target")
        ~always_there:(
          cancel_menu_items
          @ [
            menu_item ~char:'f' 
              ~log:Log.(s "Change filter "
                        % parens (s "current: " % snd es.target_filter))
              `Filter;
            menu_item ~char:'a'  (`Set_with_archived (not es.show_archived))
              ~log:Log.(s (if es.show_archived then "Hide" else "Show")
                        % s " archived targets");
          ])
        (make_target_menu ~targets ~filter_target:(fst es.target_filter) ()))

  let explore_single_target ~state (es: exploration_state) target =
    let sentence =
      let build_process_details = es.build_process_details in
      Log.(s "Exploring " 
           % Document.target ~build_process_details ~state target) in
    Ketrew_state.is_archived state (Target.id target)
    >>= fun is_archived ->
    Interaction.(
      let kill_item =
        if Target.Is.killable target
        then [menu_item ~char:'k' ~log:Log.(s "Kill") `Kill]
        else [] in
      let archive_item =
        if not is_archived && Target.Is.finished target
        then [menu_item ~char:'a' ~log:Log.(s "Archive") `Archive]
        else [] in
      let build_process_details_item =
        match es.build_process_details with
        | true ->
          [menu_item ~char:'b'
             ~log:Log.(s "Hide build process details") `Hide_make]
        | false ->
          [menu_item ~char:'b'
             ~log:Log.(s "Show build process details") `Show_make]
      in
      let follow_deps_item =
        match target.Target.dependencies with
        | [] -> []
        | some -> 
          [menu_item ~char:'d'
             ~log:Log.(s "Follow a dependency") `Follow_dependencies]
      in
      menu ~sentence ~always_there:cancel_menu_items (
        [menu_item ~char:'s' ~log:Log.(s "Show status") `Status]
        @ build_process_details_item
        @ follow_deps_item
        @ kill_item
        @ archive_item
        @ [menu_item ~char:'O' ~log:Log.(s "See JSON in $EDITOR") `View_json]
      ))

  let view_in_dollar_editor ?(extension="txt") ~state content =
    let tmp =
      Filename.(concat temp_dir_name 
                  (fmt "%s.%s" (Unique_id.create ()) extension))
    in
    IO.write_file ~content tmp
    >>= fun () ->
    let editor =
      try Sys.getenv "EDITOR" 
      with _ -> 
        Log.(s "Using `vi` since $EDITOR is not defined" @ warning);
        "vi" in
    let command = fmt "%s %s" editor tmp in
    Log.(s "Running " % s command @ verbose);
    (* We actually want (for now) to bloc the whole process and wait for
       the editor to end. *)
    ignore (Sys.command command);
    return ()

  let view_json ~state target =
    let content = Target.serialize target in 
    view_in_dollar_editor ~extension:"json" ~state content

  let pick_a_dependency ~state target =
    let target_ids = target.Target.dependencies in
    Deferred_list.while_sequential target_ids
      (Ketrew_state.get_target state)
    >>= fun targets ->
    Interaction.(
      menu ~sentence:Log.(s "Pick a target")
        ~always_there:cancel_menu_items
        (make_target_menu ~targets ()))

  let rec target_status 
      ~state ?(viewer=`Inline) ?(add_info=Log.empty) exploration_state target =
    let sentence =
      let rec log_of_status (status: Target.workflow_state) =
        let open Log in
        match status with
        | `Created time -> s "Created: " % Time.log time
        | `Activated (time, subm, why) ->
          log_of_status (subm :> Target.workflow_state) % n
          % s "Activated: " % Time.log time % sp
          % parens 
            (match why with `User -> s "user" | `Dependency -> s "dependency")
        | `Running ({Target.plugin_name; run_parameters; run_history}, act) ->
          log_of_status (act :> Target.workflow_state) % n
          % s "Running " % parens (s plugin_name) % s ":" % n
          % indent (
            separate n
              (List.map
                 ~f:(fun (key, value) -> s "* " % s key % s ": " % value)
                 (Ketrew_state.long_running_log
                    ~state plugin_name run_parameters)))
        | `Dead (time, prev, why) ->
          log_of_status (prev :> Target.workflow_state) % n
          % s "Dead: " % Time.log time % n
          % parens (match why with
            | `Failed reason -> s "Failed: " % s reason
            | `Killed reason -> s "Killed: " % s reason)
        | `Successful (time, prev, arti) ->
          log_of_status (prev :> Target.workflow_state) % n
          % s "Successful: " % Time.log time % s "→ " % Artifact.log arti
      in
      let open Log in
      Document.target ~state target
      % s "Status:" % n
      % indent (log_of_status target.Target.history)
      % n % add_info
    in
    let open Interaction in
    let additional =
      Ketrew_state.additional_queries ~state target
      |> List.map ~f:(fun (key, log) -> menu_item ~log (`Call (key, log)))
    in
    let always_there =
      let viewer_items =
        let char = 'v' in
        match viewer with
        | `Inline -> 
          [menu_item ~char ~log:Log.(s "Use $EDITOR as viewer")
             (`Set_viewer `Dollar_editor)]
        | `Dollar_editor ->
          [menu_item ~char ~log:Log.(s "View stuff inline")
             (`Set_viewer `Inline)]
      in
      cancel_menu_items @  viewer_items  in
    menu ~sentence ~always_there additional
    >>= function
    | `Set_viewer viewer ->
      target_status ~state ~viewer ~add_info exploration_state target
    | `Call (key, log) ->
      Log.(s "Calling query " % sf "%S" key % n
           % s "Press " % bold_red (s "'K'") % s " for cancelling"
           @ warning);
      begin Deferred_list.pick_and_cancel [
          Ketrew_state.call_query ~state ~target key;
          begin 
            let rec loop () =
              get_key ()
              >>< function
              | `Error (`Failure failure) ->
                fail Log.(s "Interface fails: " % s failure)
              | `Ok 'K' -> fail Log.(s "Cancelled by user")
              | `Ok other -> loop () in
            loop ()
          end;
        ]
        >>< function 
        | `Ok qlog -> 
          begin match viewer with
          | `Inline ->
            let formatted =
              let line = String.make 80 '`' in
              String.concat ~sep:"\n" [line; qlog; line] in
            return (Some Log.(log % s ":" % n 
                              % verbatim ("\n" ^ formatted ^ "\n") % n))
          | `Dollar_editor ->
            view_in_dollar_editor ~state qlog
            >>= fun () ->
            return None
          end
        | `Error e ->
          return (Some Log.(log % s ": ERROR -> " % n % e % n))
      end
      >>= fun add_info ->
      target_status ~state ~viewer ?add_info exploration_state target
    | `Cancel | `Quit as up -> return up

  let rec explore ~state exploration_state_stack =
    let go_back ~state history =
      match history with
      | [] -> return ()
      | some -> explore ~state some in
    begin match exploration_state_stack with
    | [] -> explore ~state [create_state ()]
    | one :: history ->
      begin match one.current_target with
      | None ->
        begin pick_a_target ~state one
          >>= function
          | `Cancel -> go_back ~state history (* go back in history *)
          | `Quit -> return ()
          | `Set_with_archived b ->
            explore ~state ({one with show_archived = b } :: history)
          | `Filter ->
            get_filter () >>= fun f ->
            explore ~state ({one with target_filter = f } :: one :: history)
          | `Go t ->
            explore ~state ({one with current_target = Some t } :: one :: history)
        end
      | Some chosen_id -> 
        Ketrew_state.get_target state chosen_id >>= fun chosen ->
        begin explore_single_target ~state one chosen
          >>= function
          | `Cancel -> go_back ~state history
          | `Quit -> return ()
          | `Show_make ->
            explore ~state 
              ({ one with build_process_details = true }  :: history)
          | `Hide_make ->
            explore ~state 
              ({ one with build_process_details = false }  :: history)
          | `Status ->
            begin target_status ~state one chosen
              >>= function
              | `Cancel -> go_back ~state (one :: history)
              | `Quit -> return ()
            end
          | `Kill ->
            Log.(s "Killing target …" @ warning);
            Ketrew_state.kill state (Target.id chosen)
            >>= fun what_happened ->
            Log.(s "→ " % s (Target.name chosen) % n
                 % (separate n 
                      (List.map ~f:Ketrew_state.log_what_happened what_happened) 
                    |> indent)
                 @ warning);
            explore ~state (one :: history)
          | `Archive ->
            Ketrew_state.archive_target state (Target.id chosen) >>= fun () ->
            explore ~state (one :: history)
          | `View_json ->
            view_json ~state chosen >>= fun () ->
            explore ~state (one :: history)
          | `Follow_dependencies ->
            begin pick_a_dependency ~state chosen
              >>= function
              | `Cancel -> go_back ~state (one :: history)
              | `Quit -> return ()
              | `Go t ->
                explore ~state 
                  ({one with current_target = Some t} :: one :: history)
            end
        end
      end
    end
end

let interact ~state =
  let rec main_loop () =
    Interaction.(
      menu ~sentence:Log.(s "Main menu")
        ~always_there:[menu_item ~char:'q' ~log:Log.(s "Quit") `Quit]
        [
          menu_item ~char:'s' ~log:Log.(s "Display current status(es)") `Status;
          menu_item ~char:'k' ~log:Log.(s "Kill targets") `Kill;
          menu_item ~char:'r' ~log:Log.(s "Run fix-point") (`Run ["fix"]);
          menu_item ~char:'l' ~log:Log.(s "Run loop") (`Run ["loop"]);
          menu_item ~char:'a' ~log:Log.(s "Archive targets") `Archive;
          menu_item ~char:'e' ~log:Log.(s "The Target Explorer™") `Explore;
        ]
    )
    >>= function
    | `Quit -> return ()
    | `Status -> 
      display_status ~item_format:default_item_format ~all:true ~state
      >>= fun () ->
      main_loop ()
    | `Kill ->
      kill ~interactive:true ~state []
      >>= fun () ->
      main_loop ()
    | `Run how ->
      run_state ~state  ~max_sleep:120. ~how
      >>= fun () ->
      main_loop ()
    | `Archive ->
      archive ~interactive:true ~state []
      >>= fun () ->
      main_loop ()
    | `Explore ->
      Explorer.explore ~state []
      >>= fun () ->
      main_loop ()
  in
  main_loop ()

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
  (term, Term.info name ~docs:"COMMAND ALIASES" ~doc ~man)

let cmdliner_main ?plugins ?override_configuration ?argv ?additional_term () =
  let open Cmdliner in
  let version = Ketrew_version.version in
  let sub_command ~info ~term = (term, info) in
  let config_file_argument =
    let default = 
      (try Sys.getenv "KETREW_CONFIGURATION" with _ -> 
         (try Sys.getenv "KETREW_CONFIG" with _ ->
            Configuration.default_configuration_path)) in
    let docv = "FILE" in
    let doc = "Use $(docv) as configuration file (can be overriden also \
               with `$KETREW_CONFIGURATION`)." in
    Arg.(value & opt string default
         & info ["C"; "configuration-file"] ~docv ~doc)
  in
  let init_cmd =
    sub_command
      ~info:(Term.info "init" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
               ~doc:"Initialize the client (create config-file)")
      ~term:Term.(
          pure (fun config_path database_path ->
              System.ensure_directory_path (Filename.dirname config_path)
              >>= fun () ->
              let content =
                fmt "# Ketrew configuration file\n\n[database]\n\
                    \  path = %S\n" database_path
              in
              IO.write_file ~content config_path)
          $ config_file_argument
          $ Arg.(value & opt string Configuration.default_database_path
                 & info ["database"] ~docv:"FILE"
                   ~doc:"Use $(docv) as database.")
        ) in
  let status_cmd =
    sub_command
      ~info:(Term.info "status" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
               ~doc:"Get info about this instance.")
      ~term: Term.(
          pure (fun config_path all item_format ->
              Configuration.get_configuration ?override_configuration config_path
              >>= fun configuration ->
              Ketrew_state.with_state ?plugins ~configuration
                (display_status ~item_format ~all))
          $ config_file_argument
          $ Arg.(value & flag & info ["A"; "all"] 
                   ~doc:"Display all processes even the completed ones.")
          $ Arg.(value
                 & opt string default_item_format
                 & info ["F"; "item-format"] ~docv:"FORMAT-STRING"
                   ~doc:"Use $(docv) as format for displaying jobs")
        ) in
  let run_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path max_sleep how ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_state.with_state ?plugins ~configuration 
              (run_state ~max_sleep ~how))
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
          `S "The HOW argument";
          `P "The following “run” methods are available:";
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
        pure (fun config_path interactive ids ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_state.with_state ?plugins ~configuration 
              (kill ~interactive ids))
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
  let archive_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path interactive ids ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_state.with_state ?plugins ~configuration 
              (archive ~interactive ids))
        $ config_file_argument
        $ interactive_flag "Go through running targets and kill them with 'y' \
                            or 'n'."
        $ Arg.(value @@ pos_all string [] @@
               info [] ~docv:"Target-Id" ~doc:"Archive target $(docv)"))
      ~info:(
        info "archive" ~version ~sdocs:"COMMON OPTIONS" 
          ~doc:"Archive targets." ~man:[])
  in
  let print_conf_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Log.(s "From " %
                 (match override_configuration with 
                  | None -> sf "%S" config_path
                  | Some _ -> s "user-overriden")
                 % s ":" % n
                 % concat
                   (List.map (Configuration.log configuration)
                      ~f:(fun conflog ->  s "* " % conflog % n))
                 @ normal); return ())
        $ config_file_argument)
      ~info:(info "print-configuration" ~version 
               ~doc:"Display current configuration." ~man:[])
  in
  let interact_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_state.with_state ?plugins ~configuration interact)
        $ config_file_argument)
      ~info:(
        info "interact" ~version ~sdocs:"COMMON OPTIONS" 
          ~doc:"Run the interactive menu." ~man:[])
  in
  let explore_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_state.with_state ?plugins ~configuration 
              (Explorer.explore []))
        $ config_file_argument)
      ~info:(
        info "explore" ~version ~sdocs:"COMMON OPTIONS" 
          ~doc:"Run the interactive Target Explorer." ~man:[])
  in
  let default_cmd = 
    let doc = "A Workflow Engine for Complex Experimental Workflows" in 
    let man = [] in
    sub_command
      ~term:Term.(ret (pure (`Help (`Plain, None))))
      ~info:(Term.info "ketrew" ~version ~doc ~man) in
  let cmds = [
    init_cmd; status_cmd; run_cmd; kill_cmd; archive_cmd;
    interact_cmd;
    explore_cmd;
    print_conf_cmd; make_command_alias print_conf_cmd "pc";
  ] in
  match Term.eval_choice ?argv default_cmd cmds with
  | `Ok f -> f
  | `Error _ -> exit Return_code.cmdliner_error
  | `Version | `Help -> exit 0


let run_main ?plugins ?argv ?override_configuration () =
  match Lwt_main.run (
      cmdliner_main ?plugins ?argv ?override_configuration ()
      >>< Return_code.transform_error
    ) with
  | `Ok () -> exit 0
  | `Error n -> exit n
