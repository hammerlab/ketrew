(**************************************************************************)
(*  Copyright 2014, Sebastien Mondet <seb@mondet.org>                     *)
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
module Target = Ketrew_target
module Artifact = Ketrew_artifact
module Error = Ketrew_error
module Configuration = Ketrew_configuration

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

(** Transform complex Ketrew values into display-friendly {!Log.t} values. *)
module Document = struct

  let log_list ~empty l =
    let empty_log = empty in (** renaming because of {!Log.empty} *)
    let open Log in
    let if_empty = sp % empty_log in
    match l with 
    | [] -> if_empty
  | more ->
    n % indent (separate n (List.map more ~f:(fun item -> s "- " % item)))

  let build_process ?(with_details=false)  =
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
              (Ketrew_plugin.long_running_log name content)
              ~f:(fun (title, descr) -> s title % s ": " % descr % n)
        ))
      else empty
        
  let condition ?(with_details=false) =
    let open Log in
    function
    | None -> s "Always"
    | Some c ->
      if with_details 
      then Target.Condition.log c
      else s "Conditionally"

  let target ?build_process_details ?condition_details t =
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
      "Fallbacks", OCaml.list s t.if_fails_activate;
      "Metadata", Artifact.Value.log t.metadata;
      "Build-process", 
      build_process  ?with_details:build_process_details t.make;
      "Runs", condition  ?with_details:condition_details t.condition;
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

(** Keyboard interaction functions (build “menus”, ask questions, etc.) *)
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

  let format_target_for_menu t =
    let open Log in
    let if_color f x = if !global_with_color then f x else x in
    if_color bold_yellow (s (Target.name t)) % n
    % if_color greyish (s (Target.id t)) % n
    % (
      begin match t.Target.history with
      | `Created _ -> s "Created"
      | `Activated _ -> s "Activated"
      | `Running _ -> if_color bold_red (s "Running")
      | `Dead (_, _, `Killed reason) -> if_color bold_red (sf "Killed: %s" reason)
      | `Dead (_, _, `Failed reason) -> if_color bold_red (sf "Failed: %s" reason)
      | `Successful _ -> if_color bold_green (s "Successful")
      end
    )

  let build_sublist_of_targets 
      ~client ~list_name ~all_log ~go_verb ~filter =
    Ketrew_client.current_targets client
    >>= fun all_targets ->
    let to_process = ref [] in
    let all_valid_targets () =
      List.filter all_targets ~f:(fun target ->
          filter target 
          && not (List.exists !to_process ~f:(fun id -> id = Target.id target)))
    in
    let target_menu () =
      List.map (all_valid_targets ()) ~f:(fun t ->
          menu_item 
            ~log:Log.(s "Add: " % format_target_for_menu t)
            (`Add (Target.id t)))
    in
    let rec loop () =
      let all_valid_ids = all_valid_targets () |> List.map ~f:Target.id in
      let always_there =
        let go =
          if !to_process = [] then []
          else 
            let log = Log.(s "Go; " % if_color bold_red go_verb % s " the "
                           % (match !to_process with
                             | [one] -> s "target"
                             | more -> i (List.length more) % s " targets")) in
            [menu_item ~char:'G' ~log `Done]
        in
        go @ [ menu_item ~char:'q' ~log:Log.(s "Cancel") `Cancel ]
        @ (if all_valid_ids = [] then []
           else [ menu_item ~char:'A' ~log:all_log `All; ])
      in
      let sentence = 
        if all_valid_ids = [] then Log.(s "Nothing to " % go_verb)
        else Log.(s "Add targets to “" % s list_name % s "”") in
      menu ~sentence ~always_there (target_menu ())
      >>= function
      | `Add id -> to_process := id :: !to_process; loop ()
      | `All -> to_process := all_valid_ids @ !to_process; loop ()
      | `Cancel -> return `Cancel
      | `Done -> return (`Go !to_process)
    in
    loop ()

  let make_target_menu ~targets ?(filter_target=fun _ -> true) () =
    List.filter_map targets ~f:(fun target ->
        match filter_target target with
        | false -> None
        | true ->
          Some (
            menu_item 
              ~log:Log.(format_target_for_menu target)
              (`Go (Target.id target))))

end

(** The function behind the [ketrew status] sub-command (and the equivalent
    command in [ketrew interactive]). *)
let display_status ~client  =
  begin
    Log.(s "display_info !" @ verbose);
    Ketrew_client.current_targets client
    >>= fun targets ->
    let `Running rr, `Created cc, `Activated aa =
      List.fold targets ~init:(`Running 0, `Created 0, `Activated 0)
        ~f:(fun ((`Running r, `Created c, `Activated a) as prev) t ->
            let open Target in
            match t.history with
            | `Dead _       -> prev
            | `Successful _ -> prev
            | `Activated _  -> (`Running r, `Created c, `Activated (a + 1))
            | `Created _    -> (`Running r, `Created (c + 1), `Activated a)
            | `Running (_, _) -> (`Running (r + 1), `Created c, `Activated a))
    in
    Log.(s "Current targets: "
           % i rr % s " Running, "
           % i aa % s " Activated, "
           % i cc % s " Created." %n
           @ normal);
    return ()
  end
      (*
    begin match
      Configuration.server_configuration (Ketrew_client.configuration client)
    with
    | Some server_config ->
      let local_server_uri =
        match Configuration.listen_to server_config with
        | `Tls (_, _, port) ->
          Uri.make ~scheme:"https" ~host:"127.0.0.1" ~path:"/hello" () ~port in
      Log.(s "Trying GET on " % uri local_server_uri @ verbose);
      begin
        System.with_timeout 5. ~f:(fun () ->
            wrap_deferred ~on_exn:(fun e -> `Client (`Get_exn e)) (fun () ->
                Cohttp_lwt_unix.Client.get local_server_uri))
        >>< function
        | `Ok (response, body) ->
          Log.(s "Response: " 
               % sexp Cohttp.Response.sexp_of_t response @ verbose);
          begin match Cohttp.Response.status response with
          | `OK ->
            Log.(s "The Server seems to be doing well on "
                 % uri local_server_uri @ normal);
            return ()
          | other ->
            Log.(s "There is a server at " % uri local_server_uri
                 %s " but it did not reply `OK`: " 
                 % sexp Cohttp.Response.sexp_of_t response @ warning);
            return ()
          end
        | `Error (`Client (`Get_exn
                             (Unix.Unix_error (Unix.ECONNREFUSED,
                                               "connect", "")))) ->
          Log.(s "No server seems to be listening at " % uri local_server_uri
               @ warning);
          return ()
        | `Error (`System (`With_timeout t, `Exn except)) -> 
          Log.(s "Could not perform a GET request because Timeout failed! "
               %s "Exn: " % exn except @ error);
          return ()
        | `Error (`Timeout _) ->
          Log.(s "Could not perform a GET request at " % uri local_server_uri
               % s " the operation timeouted, some server must be listenting\
                   on the port but it does not sound like Ketrew"
               @ error);
          return ()
        |  `Error (`Client (`Get_exn other_exn)) ->
          Log.(s "Could not perform a GET request at " % uri local_server_uri
               %s " exception: " % exn other_exn @ error);
          return ()
      end
    | None -> Log.(s "No local server configured." @ normal); return ()
    end
  end
*)

(** The function behind the [ketrew run <how>] sub-command (and the equivalent
    command in [ketrew interactive]). *)
let run_state ~client ~max_sleep ~how =
  let log_happening ~what_happened =
    let open Log in
    let step_count = List.length what_happened in
    let happening_list =
      List.mapi what_happened ~f:(fun step_index happening_list ->
          List.map happening_list ~f:(fun happening ->
              brakets (s "step " % i (step_index + 1)) % sp % 
              s (Ketrew_engine.what_happened_to_string happening)))
      |> List.concat
    in
    let step_sentence =
      match step_count with
      | 0 -> s "No step was executed"
      | 1 -> s "One step was executed" 
      | more -> i more % s " steps were executed"
    in
    Log.(step_sentence % s ":"  %
         Document.log_list ~empty:(s "Nothing happened") happening_list
         @ normal); 
    return ()
  in
  begin match Ketrew_client.get_local_engine client with
  | None ->
    Log.(s "Cannot get local-engine to run things." @ warning);
    return ()
  | Some state ->
    Log.(s "Running " % OCaml.list s how % s " with max-spleep: " % f max_sleep 
         @ warning);
    begin match how with
    | ["step"] ->
      Ketrew_engine.step state
      >>= fun what_happened ->
      log_happening ~what_happened:[what_happened]
    | ["fix"] ->
      Ketrew_engine.fix_point state
      >>= fun (`Steps step_count, what_happened) ->
      log_happening ~what_happened:List.(rev what_happened)
    | ["loop"] ->
      let keep_going = ref true in
      let traffic_light = Light.create () in
      let rec loop previous_sleep happenings =
        Ketrew_engine.fix_point state
        >>= fun (`Steps step_count, what_happened) ->
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
          Ketrew_client.kill client ~id
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
                 OCaml.list Ketrew_engine.log_what_happened more @ error); 
            return ()
        )
      >>= fun (_ : unit list) ->
      return ()
    | `Cancel -> 
      Log.(s "Cancelling murder plans." @ normal);
      return ()
  end

(** Archive targets (command line, ["--interactive"], or within 
    [ketrew interactive]. *)
let archive ~client ~interactive ids =
  begin 
    begin if interactive then
        Interaction.build_sublist_of_targets ~client ~list_name:"Archival list"
          ~all_log:Log.(s "Archive them all") ~go_verb:Log.(s "archive")
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
          Ketrew_client.archive client ~id)
      >>| List.concat
      >>= fun what_happened ->
      Log.(s "→ " % n
           % (separate n 
                (List.map ~f:Ketrew_engine.log_what_happened what_happened) 
              |> indent)
           @ warning);
      return ()
    | `Cancel ->
      Log.(s "Cancelling archival" @ normal);
      return ()
  end

(** Kill and archive targets that are done or useless. *)
let autoclean ~client ~how_much ~interactive () =
  let module G = Ketrew_engine.Target_graph in
  Ketrew_client.targets_to_clean_up client ~how_much
  >>= fun (`To_kill to_kill, `To_archive to_archive) ->
  let proceed () =
    kill ~client ~interactive:false to_kill
    >>= fun () ->
    archive ~client ~interactive:false (to_kill @ to_archive) in 
  begin match interactive, to_kill, to_archive with
  | _, [], [] -> Log.(s "Nothing to do" @ normal); return ()
  | true, _, _ -> 
    let sentence =
      let open Log in
      s "Going to"
      % (match to_kill with
        | [] -> empty
        | more ->
          s " kill & archive: " % OCaml.list string to_kill)
      % (match to_archive with
        | [] -> empty
        | more -> 
          (if to_kill = [] then empty else s " and ")
          % s " archive: " % OCaml.list string to_archive)
      % n % s "Proceed?"
    in
    Interaction.(
      menu ~sentence ~always_there:[
        menu_item ~char:'n' ~log:Log.(s "No") `No;
        menu_item ~char:'Y' ~log:Log.(s "Yes") `Yes;
      ] []
      >>= function
      | `Yes -> proceed ()
      | `No ->
        Log.(s "Cancelling" @ normal);
        return ()
    )
  | false, _, _ -> 
    kill ~client ~interactive:false to_kill
    >>= fun () ->
    archive ~client ~interactive:false to_archive
  end

(** The “Target Explorer™“ *)
module Explorer = struct
  type exploration_state = {
    build_process_details: bool;
    show_archived: bool;
    target_filter: (Target.t -> bool) * Log.t;
    current_target: Target.id option;
    condition_details: bool;
  }
  let create_state () =
    {build_process_details = false;
     condition_details = false;
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

  let pick_a_target_from_list ~client target_ids =
    Deferred_list.while_sequential target_ids
      (fun id -> Ketrew_client.get_target client ~id)
    >>= fun targets ->
    Interaction.(
      menu ~sentence:Log.(s "Pick a target")
        ~always_there:cancel_menu_items
        (make_target_menu ~targets ()))

  let pick_a_target ~client (es : exploration_state) =
    Ketrew_client.current_targets ~archived:es.show_archived client
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

  let explore_single_target ~client (es: exploration_state) target =
    let sentence =
      let build_process_details = es.build_process_details in
      let condition_details = es.condition_details in
      Log.(s "Exploring " 
           % Document.target ~build_process_details ~condition_details target)
    in
    Ketrew_client.is_archived client ~id:(Target.id target)
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
      let boolean_item ~value ~char ~to_false ~to_true =
        match value with
        | true -> [menu_item ~char ~log:(fst to_false) (snd to_false)]
        | false -> [menu_item ~char ~log:(fst to_true) (snd to_true)]
      in
      let build_process_details_item =
        boolean_item ~value:es.build_process_details ~char:'b'
          ~to_false:(Log.(s "Hide build process details"), `Show_make false)
          ~to_true:(Log.(s "Show build process details"), `Show_make true) in
      let condition_details_item =
        boolean_item ~value:es.condition_details ~char:'c'
          ~to_false:(Log.(s "Hide condition details"), `Show_condition false)
          ~to_true:(Log.(s "Show condition details"), `Show_condition true) in
      let follow_deps_item =
        match target.Target.dependencies with
        | [] -> []
        | some -> 
          [menu_item ~char:'d'
             ~log:Log.(s "Follow a dependency") `Follow_dependencies]
      in
      let follow_fbacks_item =
        match target.Target.if_fails_activate with
        | [] -> []
        | some -> 
          [menu_item ~char:'f'
             ~log:Log.(s "Follow a fallback") `Follow_fallbacks]
      in
      let restart_item =
        if Target.Is.finished target
        then [menu_item ~char:'r' ~log:Log.(s "Restart (clone & activate)")
                `Restart]
        else [] in
      menu ~sentence ~always_there:cancel_menu_items (
        [menu_item ~char:'s' ~log:Log.(s "Show status") `Status]
        @ build_process_details_item
        @ condition_details_item
        @ follow_deps_item
        @ follow_fbacks_item
        @ kill_item
        @ archive_item
        @ restart_item
        @ [menu_item ~char:'O' ~log:Log.(s "See JSON in $EDITOR") `View_json]
      ))

  let view_in_dollar_editor ?(extension="txt") ~client content =
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

  let view_json ~client target =
    let content = Target.serialize target in 
    view_in_dollar_editor ~extension:"json" ~client content

  let rec target_status 
      ~client ?(viewer=`Inline) ?(add_info=Log.empty) exploration_state target =
    let sentence =
      let rec log_of_status (status: Target.workflow_state) =
        let open Log in
        match status with
        | `Created time -> s "Created: " % Time.log time
        | `Activated (time, subm, why) ->
          log_of_status (subm :> Target.workflow_state) % n
          % s "Activated: " % Time.log time % sp
          % parens 
            (match why with
             | `User -> s "user"
             | `Dependency -> s "dependency"
             | `Fallback -> s "fallback")
        | `Running ({Target.plugin_name; run_parameters; run_history}, act) ->
          log_of_status (act :> Target.workflow_state) % n
          % s "Running " % parens (s plugin_name) % s ":" % n
          % indent (
            separate n
              (List.map
                 ~f:(fun (key, value) -> s "* " % s key % s ": " % value)
                 (Ketrew_plugin.long_running_log plugin_name run_parameters)))
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
      Document.target target
      % s "Status:" % n
      % indent (log_of_status target.Target.history)
      % n % add_info
    in
    let open Interaction in
    let additional =
      Ketrew_plugin.additional_queries target
      |> List.map ~f:(fun (key, log) -> menu_item ~log (`Call (key, log)))
    in
    let always_there =
      let viewer_items =
        let char = 'v' in
        match additional, viewer with
        | [], _ -> [] (* No additional → no need for this menu-item. *)
        | _, `Inline -> 
          [menu_item ~char ~log:Log.(s "Use $EDITOR as viewer")
             (`Set_viewer `Dollar_editor)]
        | _, `Dollar_editor ->
          [menu_item ~char ~log:Log.(s "View stuff inline")
             (`Set_viewer `Inline)]
      in
      cancel_menu_items @  viewer_items  in
    menu ~sentence ~always_there additional
    >>= function
    | `Set_viewer viewer ->
      target_status ~client ~viewer exploration_state target
    | `Call (key, log) ->
      Log.(s "Calling query " % sf "%S" key % n
           % s "Press " % bold_red (s "'K'") % s " for cancelling"
           @ warning);
      begin Deferred_list.pick_and_cancel [
          Ketrew_client.call_query client ~target key;
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
            view_in_dollar_editor ~client qlog
            >>= fun () ->
            return None
          end
        | `Error e ->
          return (Some Log.(log % s ": ERROR -> " % n % e % n))
      end
      >>= fun add_info ->
      target_status ~client ~viewer ?add_info exploration_state target
    | `Cancel | `Quit as up -> return up

  let rec explore ~client exploration_state_stack =
    let go_back ~client history =
      match history with
      | [] -> return ()
      | some -> explore ~client some in
    begin match exploration_state_stack with
    | [] -> explore ~client [create_state ()]
    | one :: history ->
      begin match one.current_target with
      | None ->
        begin pick_a_target ~client one
          >>= function
          | `Cancel -> go_back ~client history (* go back in history *)
          | `Quit -> return ()
          | `Set_with_archived b ->
            explore ~client ({one with show_archived = b } :: history)
          | `Filter ->
            get_filter () >>= fun f ->
            explore ~client ({one with target_filter = f } :: one :: history)
          | `Go t ->
            explore ~client ({one with current_target = Some t } :: one :: history)
        end
      | Some chosen_id -> 
        Ketrew_client.get_target client chosen_id >>= fun chosen ->
        begin explore_single_target ~client one chosen
          >>= function
          | `Cancel -> go_back ~client history
          | `Quit -> return ()
          | `Show_make build_process_details ->
            explore ~client ({ one with build_process_details }  :: history)
          | `Show_condition condition_details ->
            explore ~client ({ one with condition_details }  :: history)
          | `Status ->
            begin target_status ~client one chosen
              >>= function
              | `Cancel -> go_back ~client (one :: history)
              | `Quit -> return ()
            end
          | `Kill ->
            Log.(s "Killing target …" @ warning);
            Ketrew_client.kill client (Target.id chosen)
            >>= fun what_happened ->
            Log.(s "→ " % s (Target.name chosen) % n
                 % (separate n 
                      (List.map ~f:Ketrew_engine.log_what_happened what_happened) 
                    |> indent)
                 @ warning);
            explore ~client (one :: history)
          | `Archive ->
            Ketrew_client.archive client (Target.id chosen)
            >>= fun what_happened ->
            Log.(s "Archival of " % s (Target.name chosen) % n
                 % (separate n 
                      (List.map ~f:Ketrew_engine.log_what_happened what_happened) 
                    |> indent)
                 @ warning);
            explore ~client (one :: history)
          | `Restart ->
            Ketrew_client.restart_target client ~target:chosen
            >>= fun (this_target, new_upper_dag) ->
            Log.(s "New Targets:" % n
                 % indent (Interaction.format_target_for_menu this_target) % n
                 % s "and its reverse dependencies:" % n
                 % indent (separate n (List.map new_upper_dag
                                         ~f:Interaction.format_target_for_menu))
                 @ normal);
            explore ~client (one :: history)
          | `View_json ->
            view_json ~client chosen >>= fun () ->
            explore ~client (one :: history)
          | `Follow_dependencies | `Follow_fallbacks as follow ->
            let target_ids =
              match follow with
              | `Follow_fallbacks -> chosen.Target.if_fails_activate
              | `Follow_dependencies -> chosen.Target.dependencies in
            begin pick_a_target_from_list ~client target_ids
              >>= function
              | `Cancel -> go_back ~client (one :: history)
              | `Quit -> return ()
              | `Go t ->
                explore ~client 
                  ({one with current_target = Some t} :: one :: history)
            end
        end
      end
    end
end

(** The function behind [ketrew interact]. *)
let interact ~client =
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
          menu_item ~char:'c'
            ~log:Log.(s "Auto-clean-up: orphans, successes")
            (`Autoclean `Soft);
          menu_item ~char:'C'
            ~log:Log.(s "Auto-clean-up: orphans, successes, and failures")
            (`Autoclean `Hard);
          menu_item ~char:'e' ~log:Log.(s "The Target Explorer™") `Explore;
        ]
    )
    >>= function
    | `Quit -> return ()
    | `Status -> 
      display_status ~client
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
    | `Archive ->
      archive ~interactive:true ~client []
      >>= fun () ->
      main_loop ()
    | `Autoclean how_much ->
      autoclean ~client ~how_much ~interactive:true ()
      >>= fun () ->
      main_loop ()
    | `Explore ->
      Explorer.explore ~client []
      >>= fun () ->
      main_loop ()
  in
  main_loop ()

let daemonize_if_applicable config =
  let module Conf = Ketrew_configuration in
  match Conf.server_configuration config with
  | Some server_config when Conf.daemon server_config ->
    let syslog = false in
    let stdin = `Dev_null in
    let (stdout, stderr) =
      begin match Conf.log_path server_config with
      | None -> (`Dev_null,`Dev_null)
      | Some file_name ->
        global_with_color := false;
        Log.(s "Creating logger" @ very_verbose);
        let logger =
          Lwt_log.channel  ()
          ~template:"[$(date):$(milliseconds)] $(message)"
          ~close_mode:`Keep
          ~channel:(Lwt_io.of_unix_fd 
                      ~mode:Lwt_io.output
                      (UnixLabels.(
                          openfile 
                            ~perm:0o600 file_name
                            ~mode:[O_APPEND; O_CREAT; O_WRONLY])))
          in
          (`Log logger, `Log logger)
      end
    in
    let directory = Sys.getcwd () in
    let umask = None in (* we keep the default *)
    Log.(s "Going to the background, now!" @ normal);
    Lwt_daemon.daemonize ~syslog ~stdin ~stdout ~stderr ~directory ?umask ();
    Log.(s "Daemonized!" @ very_verbose);
    ()
  | None | Some _ -> ()

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
  (term, Term.info name ~docs:"COMMAND ALIASES" ~doc ~man)

(** The configuration of the command line, using the [Cmdliner] library. *)
let cmdliner_main ?override_configuration ?argv ?(additional_commands=[]) () =
  let open Cmdliner in
  let version = Ketrew_metadata.version in
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
          pure (fun config_path  ->
              Configuration.get_configuration ?override_configuration config_path
              >>= fun configuration ->
              Ketrew_client.as_client ~configuration ~f:(display_status))
          $ config_file_argument
        ) in
  let run_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path max_sleep how ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_client.as_client ~configuration 
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
            Ketrew_client.as_client ~configuration 
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
  let archive_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path interactive ids ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_client.as_client ~configuration 
              ~f:(archive ~interactive ids))
        $ config_file_argument
        $ interactive_flag "Go through running targets and kill them with 'y' \
                            or 'n'."
        $ Arg.(value @@ pos_all string [] @@
               info [] ~docv:"Target-Id" ~doc:"Archive target $(docv)"))
      ~info:(
        info "archive" ~version ~sdocs:"COMMON OPTIONS" 
          ~doc:"Archive targets." ~man:[])
  in
  let autoclean_command =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path interactive how_much ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            Ketrew_client.as_client ~configuration 
              ~f:(autoclean ~interactive ~how_much ()))
        $ config_file_argument
        $ interactive_flag "Ask before proceeding."
        $ (pure (fun hard -> if hard then `Hard else `Soft)
           $ Arg.(value & flag & info ["H"; "hard"] 
                    ~doc:"Also clean-up failed/killed targets")))
      ~info:(
        info "autoclean" ~version ~sdocs:"COMMON OPTIONS" 
          ~doc:"Kill & Archive orphan and finished targets." ~man:[])
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
                 % Configuration.log configuration
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
            Ketrew_client.as_client ~configuration ~f:interact)
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
            Ketrew_client.as_client ~configuration 
              ~f:(Explorer.explore []))
        $ config_file_argument)
      ~info:(
        info "explore" ~version ~sdocs:"COMMON OPTIONS" 
          ~doc:"Run the interactive Target Explorer." ~man:[])
  in
  let start_server_cmd =
    let open Term in
    sub_command
      ~term:(
        pure (fun config_path ->
            (* We need a Lwt-less processing until the potential 
               daemonization: *)
            let configuration =
              Configuration.get_configuration_non_deferred_exn
                ?override_configuration config_path in
            Log.(s "Got configuration: " % Configuration.log configuration
                 @ very_verbose);
            daemonize_if_applicable configuration;
            match Configuration.mode configuration with
            | `Server srv -> Ketrew_server.start srv
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
        pure (fun config_path ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            begin match Configuration.mode configuration with
            | `Server srv -> Ketrew_server.stop srv
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
    let man = [] in
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
      init_cmd; status_cmd; run_cmd; kill_cmd; archive_cmd;
      interact_cmd;
      explore_cmd;
      autoclean_command;
      start_server_cmd; stop_server_cmd;
      print_conf_cmd; make_command_alias print_conf_cmd "pc";
    ] in
  match Term.eval_choice ?argv default_cmd cmds with
  | `Ok f -> f
  | `Error _ -> exit Return_code.cmdliner_error
  | `Version | `Help -> exit 0


let run_main ?argv ?override_configuration ?additional_commands () =
  let main_lwt_thread =
    cmdliner_main ?argv ?override_configuration ?additional_commands ()
  in
  Log.(s "Calling Lwt_main.run" @ very_verbose);
  match Lwt_main.run (main_lwt_thread >>< Return_code.transform_error) with
  | `Ok () -> exit 0
  | `Error n -> exit n



