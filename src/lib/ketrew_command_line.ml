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
    function
    | `No_operation -> s "No-op"
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
    | None -> s "Always Runs"
    | Some c ->
      if with_details
      then Target.Condition.log c
      else s "Runs When “Not Done”"

  let if_color f x = if !global_with_color then f x else x

  let short_status t =
    let open Log in
    let state = Target.state t in
    let add_color =
      match Target.State.simplify state with
      | `In_progress -> if_color bold_yellow
      | `Failed -> if_color bold_red
      | `Activable -> if_color greyish
      | `Successful -> if_color bold_green
    in
    let (`Time time, `Log log, `Info info) = Target.State.summary state in
    add_color (s (Target.State.name state))
    %sp % braces (Time.log time
                  % Option.value_map
                    ~default:empty log ~f:(fun m -> sp % parens (s m))
                  % separate empty (List.map ~f:(fun m -> s ", " % s m) info))


  let target_for_menu t =
    let open Log in
    let if_color f x = if !global_with_color then f x else x in
    if_color bold_yellow (s (Target.name t)) % n
    % if_color greyish (s (Target.id t)) % n
    % short_status t

  let target ?build_process_details ?condition_details t =
    let open Log in
    let doc_build_process = build_process in
    let doc_condition = condition in
    let open Target in
    let itemize l =
      indent (concat (List.map l ~f:(fun (name, log) ->
          s "* " % s name % s ": " % log %n))) in
    s "Target " % a name t % n
    % itemize [
      "ID", a id t;
      "Dependencies", OCaml.list s (dependencies t);
      "Fallbacks", OCaml.list s (fallbacks t);
      "On Success trigger", OCaml.list s (success_triggers t);
      "Metadata", OCaml.option (function `String m -> s m) (metadata t);
      "Build-process",
      doc_build_process ?with_details:build_process_details
        (Target.build_process t);
      "Condition",
      doc_condition ?with_details:condition_details (Target.condition t);
      "Equivalence", (match equivalence t with
        | `None -> s "None"
        | `Same_active_condition -> s "Same active condition");
      "Tags", OCaml.list quote (tags t);
      "Status", short_status t;
      "Additional Log",
      OCaml.list (fun (time, msg) ->
          brakets (Time.log time) % s ": " % s msg) (additional_log t);
    ]
end

(** Keyboard interaction functions (build “menus”, ask questions, etc.) *)
module Interaction = struct

  let initialized = ref false
  let init () =
    if not !initialized then (
      let open Unix in
      let term_init = tcgetattr stdin in
      at_exit (fun () -> tcsetattr stdin TCSADRAIN term_init);
      initialized := true;
      ()
    )

  let verbose_r = ref false
  let toggle_verbose () =
    verbose_r := not !verbose_r

  (** [with_cbreak f] calls with the terminal in “get key” mode.
         It comes from
         http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html
  *)
  let with_cbreak (f: unit -> (_, _) t) =
    init ();
    let open Lwt_unix in
    Lwt.(tcgetattr stdin
         >>= fun term_init ->
         let go_back_to_normal () = tcsetattr stdin TCSADRAIN term_init in
         let term_cbreak = { term_init with c_icanon = false; c_echo = false } in
         tcsetattr stdin TCSANOW term_cbreak
         >>= fun () ->
         catch f (fun e ->
             Log.(s "with_cbreak exn: " % exn e @ warning);
             return (`Error (`Failure "with_cbreak")))
         >>= fun res ->
         go_back_to_normal ()
         >>= fun () ->
         return res)

  let use_cbreak () =
    try Sys.getenv "WITH_CBREAK" <> "no" with _ -> true

  let get_key () =
    match use_cbreak () with
    | true ->
      with_cbreak (fun () ->
          wrap_deferred (fun () -> Lwt_io.read_char Lwt_io.stdin)
            ~on_exn:(fun e -> (`Failure "get_key")))
    | false ->
      wrap_deferred
        ~on_exn:(fun e -> (`Failure "get_key"))
        begin fun () ->
          let open Lwt in
          Lwt_io.read_line Lwt_io.stdin
          >>= fun l ->
          begin match String.get l 0 with
          | Some c -> return c
          | None -> return '\n'
          end
        end

  let get_key_question () =
    match use_cbreak () with
    | true -> "Press a single key:"
    | false -> "Enter a character and <enter>:"

  let open_in_dollar_editor file =
    let editor =
      try Sys.getenv "EDITOR"
      with _ ->
        Log.(s "Using `vi` since $EDITOR is not defined" @ warning);
        "vi" in
    let command = fmt "%s %s" editor file in
    Log.(s "Running " % s command @ verbose);
    (* We actually want (for now) to block the whole process and wait for
       the editor to end. *)
    ignore (Sys.command command);
    return ()

  let view_in_dollar_editor ?(extension="txt") content =
    let tmp =
      Filename.(concat temp_dir_name
                  (fmt "%s.%s" (Unique_id.create ()) extension))
    in
    IO.write_file ~content tmp
    >>= fun () ->
    open_in_dollar_editor tmp

  let interaction_chars =
    List.init 10 (fun i -> Char.chr (48 + i))
    @ List.init 26 (fun i -> Char.chr (97 + i))

  let menu_item ?char ?(log=Log.empty) v =
    (char, log, v)

  let menu ?(max_per_page=15) ?(always_there=[]) ~sentence items =
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
           % s (get_key_question ()) % n
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
      if !verbose_r then Log.(sf "%c pressed" key @ normal);
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


  (** Sort a list of targets from the most recent to the oldest
      (using the unique IDs which is hackish …). *)
  let sort_target_list =
    List.sort ~cmp:(fun ta tb -> compare (Target.id tb) (Target.id ta))

  let build_sublist_of_targets
      ~client ~list_name ~all_log ~go_verb ~filter =
    Ketrew_client.current_targets client
    >>| List.filter ~f:(fun target -> filter target)
    >>| sort_target_list
    >>= fun all_targets ->
    let to_process = ref [] in
    let all_valid_targets () =
      List.filter all_targets ~f:(fun target ->
          not (List.exists !to_process ~f:(fun id -> id = Target.id target)))
    in
    let target_menu () =
      List.map (all_valid_targets ()) ~f:(fun t ->
          menu_item
            ~log:Log.(s "Add: " % Document.target_for_menu t)
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
    List.filter targets ~f:filter_target
    |> sort_target_list
    |> List.map ~f:(fun target ->
        menu_item ~log:Log.(Document.target_for_menu target)
          (`Go (Target.id target)))

end

let get_status ~client =
  Ketrew_client.current_targets client
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
  get_status ~client
  >>= fun (`In_progress inp) ->
  Log.(s "Current targets “in-progress”: " % i inp @ normal);
  begin match loop, inp with
  | Some _, 0 ->
    Log.(s "Nothing left to do" @ verbose);
    return ()
  | Some seconds, _ ->
    (System.sleep seconds >>< fun _ -> return ())
    >>= fun () ->
    display_status ~client ~loop
  | None, _ ->
    return ()
  end

(** The function behind the [ketrew run <how>] sub-command (and the equivalent
    command in [ketrew interactive]). *)
let run_state ~client ~max_sleep ~how =
  begin match Ketrew_client.get_local_engine client with
  | None ->
    fail (`Failure "This client is not Standalone, it can not run stuff.")
  | Some state ->
    begin match how with
    | ["step"] ->
      Ketrew_engine.Run_automaton.step state
      >>= fun (_ : bool) ->
      return ()
    | ["fix"] ->
      Ketrew_engine.Run_automaton.fix_point state
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
        Ketrew_engine.Run_automaton.fix_point state
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
      Deferred_list.pick_and_cancel [
        begin loop 2. end;
        begin
          let rec kbd_loop () =
            Log.(s "Press the 'q' key to stop looping." @ normal);
            Interaction.get_key ()
            >>= function
            | 'q' | 'Q' ->
              stop_it ();
              return ()
            | _ -> kbd_loop ()
          in
          kbd_loop ()
        end;
      ]
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
      Ketrew_client.kill client to_kill
    | `Cancel ->
      Log.(s "Cancelling murder plans." @ normal);
      return ()
  end

(** Kill targets that are useless. *)
let autoclean ~client ~how_much ~interactive () =
  let module G = Ketrew_engine.Target_graph in
  Ketrew_client.targets_to_clean_up client ~how_much
  >>= fun (to_kill) ->
  let proceed () =
    kill ~client ~interactive:false to_kill
  in
  begin match interactive, to_kill with
  | _, [] -> Log.(s "Nothing to do" @ normal); return ()
  | true, _ ->
    let sentence =
      let open Log in
      s "Going to"
      % (match to_kill with
        | [] -> empty
        | more ->
          s " kill: " % OCaml.list string to_kill)
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
  | false, _ -> proceed ()
  end

(** The “Target Explorer™“ *)
module Explorer = struct
  type exploration_state = {
    build_process_details: bool;
    target_filter: (Target.t -> bool) * Log.t;
    current_target: Target.id option;
    condition_details: bool;
  }
  let create_state () =
    {build_process_details = false;
     condition_details = false;
     target_filter = (fun _ -> true), Log.(s "No-filter, see them all");
     current_target = None}


  let cancel_menu_items =
    Interaction.([
        menu_item ~char:'Q' ~log:Log.(s "Quit explorer") `Quit;
        menu_item ~char:'q' ~log:Log.(s "Cancel/Go-back") `Cancel;
        menu_item ~char:'R' ~log:Log.(s "Reload") `Reload;
      ])


  let filter ~log ~char f =
    (char, log, `Set (f, log))
  let simple_filter ~log ~char simple =
    filter ~log ~char (fun t -> Target.state t |> Target.State.simplify = simple)
  let finished t =
    let simple = Target.state t |> Target.State.simplify in
    match simple with
    | `Failed 
    | `Successful -> true
    | `In_progress
    | `Activable -> false

  let filters = [
    filter (fun _ -> true)      ~char:'n' ~log:Log.(s "No-filter, see them all");
    simple_filter `Activable ~char:'p' ~log:Log.(s "Passive/Activable");
    simple_filter `In_progress ~char:'r' ~log:Log.(s "Running/In-progress");
    simple_filter `Successful ~char:'s' ~log:Log.(s "Successful");
    simple_filter `Failed ~char:'f' ~log:Log.(s "Failed");
    filter finished ~char:'n' ~log:Log.(s "Finished (success or failure)");
  ]

  let initial_ask_tags_content =
    "# Enter regular expressions on `tags` of the targets\n\
     # Lines beginning with '#' are thrown aways\n\
     # The syntax of the regular expressions is “POSIX”\n\
    "

  let get_filter () =
    Interaction.(
      menu ~sentence:Log.(s "Pick a filter")
        (List.map filters (fun (char, log, tag) ->
             menu_item ~char ~log tag)
         @ [menu_item ~char:'T'
              ~log:Log.(s "Enter tag regular-expression(s)") `Ask_tags]
        )
    )
    >>= function
    | `Ask_tags ->
      let tmpfile = Filename.temp_file "ketrew" "tags.conf" in
      IO.write_file tmpfile ~content:initial_ask_tags_content
      >>= fun () ->
      Interaction.open_in_dollar_editor tmpfile
      >>= fun () ->
      IO.read_file tmpfile
      >>= fun content ->
      let tag_regs =
        String.split ~on:(`Character '\n') content
        |> List.filter_map ~f:(fun line ->
            let stripped = String.strip line in
            match String.get ~index:0 stripped with
            | Some '#' -> None
            | None -> None
            | Some other ->
              (try Some (stripped,
                         Re_posix.compile_pat stripped)
              with _ -> None))
      in
      return (
        (fun trgt ->
           List.for_all tag_regs ~f:(fun (_, reg) ->
               List.exists Target.(tags trgt) ~f:(fun tag ->
                   Re.execp reg tag))),
        Log.(s "Tags matching "
             % OCaml.list (fun (s, _) -> quote s) tag_regs))
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
    Ketrew_client.current_targets client
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
          ])
        (make_target_menu ~targets ~filter_target:(fst es.target_filter) ()))

  let explore_single_target ~client (es: exploration_state) target =
    let sentence =
      let build_process_details = es.build_process_details in
      let condition_details = es.condition_details in
      Log.(s "Exploring "
           % Document.target ~build_process_details ~condition_details target)
    in
    (* Ketrew_client.is_archived client ~id:(Target.id target) *)
    (* >>= fun is_archived -> *)
    Interaction.(
      let kill_item =
        if Target.state target |> Target.State.Is.killable
        then [menu_item ~char:'k' ~log:Log.(s "Kill") `Kill]
        else [] in
      (* let archive_item = *)
      (*   if not is_archived && Target.Is.finished target *)
      (*   then [menu_item ~char:'a' ~log:Log.(s "Archive") `Archive] *)
      (*   else [] in *)
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
      let menu_item_of_id_list ~char ~log ~result ids =
        match ids with
        | [] -> []
        | some -> [menu_item ~char ~log result] in
      let follow_deps_item =
        menu_item_of_id_list ~char:'d' ~log:Log.(s "Follow a dependency")
          ~result:`Follow_dependencies (Target.dependencies target) in
      let follow_fbacks_item =
        menu_item_of_id_list ~char:'f' ~log:Log.(s "Follow a fallback")
          ~result:`Follow_fallbacks (Target.fallbacks target) in
      let follow_success_triggers_item =
        menu_item_of_id_list ~char:'t' ~log:Log.(s "Follow a success-trigger")
          ~result:`Follow_success_triggers (Target.success_triggers target) in
      let restart_item =
        if Target.state target |> Target.State.Is.finished
        then [menu_item ~char:'r' ~log:Log.(s "Restart (clone & activate)")
                `Restart]
        else [] in
      menu ~sentence ~always_there:cancel_menu_items (
        [menu_item ~char:'s' ~log:Log.(s "Show status") `Status]
        @ build_process_details_item
        @ condition_details_item
        @ follow_deps_item
        @ follow_fbacks_item
        @ follow_success_triggers_item
        @ kill_item
        (* @ archive_item *)
        @ restart_item
        @ [menu_item ~char:'O' ~log:Log.(s "See JSON in $EDITOR") `View_json]
      ))


  let view_json ~client target =
    let content = Target.Stored_target.(of_target target |> serialize) in
    Interaction.view_in_dollar_editor ~extension:"json" content

  let rec target_status
      ~client ?(viewer=`Inline) ?(add_info=Log.empty) exploration_state target =
    let sentence =
      let open Log in
      let log_of_status (status: Target.State.t) =
        Target.State.log ~depth:4 status % n % s "..." in
      Document.target target
      % s "Status:" % n
      % indent (log_of_status (Target.state target))
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
            Interaction.view_in_dollar_editor qlog
            >>= fun () ->
            return None
          end
        | `Error e ->
          return (Some Log.(log % s ": ERROR -> " % n % e % n))
      end
      >>= fun add_info ->
      target_status ~client ~viewer ?add_info exploration_state target
    | `Reload ->
      Ketrew_client.get_target client (Ketrew_target.id target) >>= fun chosen ->
      target_status ~client ~viewer exploration_state chosen
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
          | `Reload -> explore ~client exploration_state_stack
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
          | `Reload -> explore ~client exploration_state_stack
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
            Ketrew_client.kill client [Target.id chosen]
            >>= fun () ->
            explore ~client (one :: history)
          | `Restart ->
            Ketrew_client.restart_target client [Ketrew_target.id chosen]
            >>= fun () ->
            explore ~client (one :: history)
          | `View_json ->
            view_json ~client chosen >>= fun () ->
            explore ~client (one :: history)
          | `Follow_dependencies | `Follow_fallbacks | `Follow_success_triggers
            as follow ->
            let target_ids t =
              match follow with
              | `Follow_fallbacks -> (Target.fallbacks t)
              | `Follow_success_triggers -> (Target.success_triggers t)
              | `Follow_dependencies -> (Target.dependencies t) in
            let rec next_target ids =
              begin pick_a_target_from_list ~client ids
                >>= function
                | `Cancel -> return `Cancel
                | `Quit -> return `Quit
                | `Reload ->
                  Ketrew_client.get_target client chosen_id >>= fun chosen ->
                  next_target (target_ids chosen)
                | `Go t -> return (`Go t)
              end
            in
            begin next_target (target_ids chosen)
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

let inspect
    ~(client:Ketrew_client.t)
    ~(in_dollar_editor: bool)
    ~(format: [ `Tsv | `Csv ])
    ?since (* String representing a date for lexicographic comparison *)
    how (* list of strings: how to inspect *) =
  let module Mtem = Ketrew_gen_base_v0.Measurement_item in
  let get_all () =
    match Ketrew_client.get_local_engine client with
    | None ->
      Log.(s "HTTP Client cannot inspect for now." @ error);
      fail (`Not_implemented "inspect")
    | Some engine ->
      Ketrew_engine.Measurements.get_all engine
      >>= fun all ->
      List.sort
        ~cmp:(fun ma mb -> Float.compare ma.Mtem.time mb.Mtem.time)
        (match since with
         | None -> all
         | Some stime ->
           List.filter ~f:(fun m -> Time.to_filename m.Mtem.time >= stime) all)
      |> return
  in
  let is s ~prefix_of =
    String.(sub prefix_of ~index:0 ~length:(length s) = Some s) in
  let display document =
    match in_dollar_editor with
    | true ->
      let str =
        match format with
        | `Tsv ->
          List.map document ~f:(fun row ->
              (List.map row ~f:(String.map ~f:(function '\t' -> ' ' | c -> c))
               |> String.concat ~sep:"\t") ^ "\n")
          |> String.concat ~sep:""
        | `Csv ->
          List.map document ~f:(fun row ->
              (List.map row ~f:(fun cell ->
                   match String.index_of_character cell ',' with
                   | Some _ -> fmt "%S" cell
                   | None -> cell)
               |> String.concat ~sep:",") ^ "\n")
          |> String.concat ~sep:""
      in
      Interaction.view_in_dollar_editor str
    | false ->
      Log.(s "Measurements:" % n %
           separate n
             (List.map document ~f:(fun str ->
                  separate (s "\t") (List.map ~f:s str)))
           @ normal);
      return ()
  in
  begin match how with
  | [all; mea] when is all ~prefix_of:"all"
                 && is mea ~prefix_of:"measurements" ->
    get_all ()
    >>= fun measurements ->
    let document =
      List.map measurements ~f:(fun item ->
          let date =
            Time.to_filename item.Ketrew_gen_base_v0.Measurement_item.time in
          match item.Ketrew_gen_base_v0.Measurement_item.content with
          | `Creation -> [date; "Creation"]
            | `Incoming_request hr ->
              [date; "Incomming HTTP request";
               hr.Ketrew_gen_base_v0.Http_request.uri]
            | `End_of_request (hr, rl) ->
              [date; "End of HTTP request";
               hr.Ketrew_gen_base_v0.Http_request.uri;
               Int.to_string rl.Ketrew_gen_base_v0.Response_log.body_length;
               rl.Ketrew_gen_base_v0.Response_log.response; ]
            | `Tag t -> [date; "Tag"; t])
    in
    display document
  | [ht; dur] when is ht ~prefix_of:"http-request"
                && is dur ~prefix_of:"benchmark" ->
    get_all ()
    >>= fun measurements ->
    let all_reqs =
      let r = ref [] in
      List.iter measurements ~f:(fun item ->
          let date = item.Ketrew_gen_base_v0.Measurement_item.time in
          match item.Ketrew_gen_base_v0.Measurement_item.content with
          | `Tag _ | `Creation -> ()
          | `Incoming_request hr ->
            r := (hr, date, None) :: !r
          | `End_of_request (hr, rl) ->
            r := List.map !r ~f:(function
              | (h, i, None) when h = hr -> (h, i, Some (date, rl))
              | other -> other)
        );
      List.filter_map !r ~f:(function
        | (hr, t, Some (t2, rl)) -> Some (hr, t, (t2 -. t), rl)
        | _ -> None)
      |> List.sort ~cmp:(fun (_, _, a, _) (_, _, b, _) -> Float.compare b a)
    in
    let document =
      List.map all_reqs ~f:(fun (hr, date, duration, response_log) ->
          [Time.to_filename date; hr.Ketrew_gen_base_v0.Http_request.uri;
           Float.to_string duration;
           Int.to_string
             response_log.Ketrew_gen_base_v0.Response_log.body_length; ])
    in
    display document
  | other ->
    Log.(s "Don't know what to do with " % OCaml.list quote other @ error);
    fail (`Failure "command line")
  end


(** The function behind [ketrew interact]. *)
let interact ~client =
  let can_run_stuff =
    Ketrew_client.get_local_engine client <> None
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
            menu_item ~char:'c'
              ~log:Log.(s "Auto-clean-up: orphans, successes")
              (`Autoclean `Soft);
            menu_item ~char:'C'
              ~log:Log.(s "Auto-clean-up: orphans, successes, and failures")
              (`Autoclean `Hard);
            menu_item ~char:'e' ~log:Log.(s "The Target Explorer™") `Explore;
          ]
        )
    )
    >>= function
    | `Quit -> return ()
    | `Verbose ->
        Interaction.toggle_verbose ();
        if !Interaction.verbose_r then Log.(s "v pressed" @ normal);
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
  match config with
  | `Daemonize_with log_path_opt ->
    let syslog = false in
    let stdin = `Dev_null in
    let (stdout, stderr) = (`Dev_null, `Dev_null) in
    let directory = Sys.getcwd () in
    let umask = None in (* we keep the default *)
    Log.(s "Going to the background, now!" @ normal);
    begin match log_path_opt with
    | None -> ()
    | Some file_name ->
      let unix_fd =
        UnixLabels.(
          openfile ~perm:0o600 file_name ~mode:[O_APPEND; O_CREAT; O_WRONLY])
      in
      let channel =
        Lwt_io.of_unix_fd unix_fd ~buffer_size:1024 ~mode:Lwt_io.output in
      Lwt_main.at_exit (fun () -> Lwt_io.close channel);
      global_with_color := false;
      global_log_print_string := Lwt.(fun s ->
          async (fun () ->
              Lwt_io.fprint channel s
              >>= fun () ->
              Lwt_io.flush channel)
        );
    end;
    Lwt_daemon.daemonize ~syslog ~stdin ~stdout ~stderr ~directory ?umask ();
    Log.(s "Daemonized!" @ very_verbose);
    ()
  | `Do_not_daemonize -> ()

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
  let version = Ketrew_metadata.version in
  let common_options_section = "COMMON OPTIONS" in
  let sub_command ~info ~term = (term, info) in
  let config_file_argument =
    let default = Configuration.get_path () in
    let docv = "FILE" in
    let doc = "Use $(docv) as configuration file (can be overriden also \
               with `$KETREW_CONFIGURATION`)." in
    Arg.(value & opt string default
         & info ["C"; "configuration-file"]
           ~docs:common_options_section ~docv ~doc)
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
          pure (fun config_path loop  ->
              Configuration.get_configuration ?override_configuration config_path
              >>= fun configuration ->
              match Configuration.mode configuration  with
              | `Client _ | `Standalone _ ->
                let loop = if loop then Some 2. else None in
                Ketrew_client.as_client ~configuration ~f:(display_status ~loop)
              | `Server s ->
                Ketrew_server.status ~configuration:s
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
  let inspect_cmd =
    sub_command
      ~term:Term.(
        pure (fun config_path in_dollar_editor csv since how ->
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
            let in_dollar_editor = in_dollar_editor || csv in
            let format = if csv then `Csv else `Tsv in
            Ketrew_client.as_client ~configuration
              ~f:(inspect ~in_dollar_editor ~format ?since how))
        $ config_file_argument
        $ Arg.(value @@ flag
               @@ info ["e"; "view-in-editor"]
                 ~doc:"Open stuff in $EDITOR (by default in TSV).")
        $ Arg.(value @@ flag
               @@ info ["csv"]
                 ~doc:"Output CSV instead of TSV (implies `--view-in-editor`).")
        $ Arg.(value & opt (some string) None
               & info ["S"; "since"] ~docv:"TIME-STRING"
                 ~doc:(fmt
                         "Get measurements that are younger than $(docv); \
                          the date-format is (any prefix of) `%s`"
                         Time.(now () |> to_filename)))
        $ Arg.(non_empty @@ pos_all string [] @@
               info [] ~docv:"HOW"
                 ~doc:"How to do the inspection")
      )
      ~info:(
        let man = [
          `S "THE HOW ARGUMENT";
          `P "The following $(b,HOW) arguments are possible:";
          `I ("`all measurements`", "display all the known measurements");
          `Noblank;
          `I ("`http-request benchmark`",
              "display durations and response-sizes of HTTP");
          `P "Note that one can use unambiguous prefixes, e.g.:";
          `P "    ketrew insp h b -e";
          `P "to open all HTTP benchmarks in `$EDITOR`";
        ] in
        Term.info "inspect" ~version ~sdocs:"COMMON OPTIONS"
          ~doc:"Run steps of the engine."  ~man)
  in
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
            let configuration_extract =
              Configuration.get_configuration_for_daemon_exn
                ?override_configuration config_path in
            daemonize_if_applicable configuration_extract;
            Configuration.get_configuration ?override_configuration config_path
            >>= fun configuration ->
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
      init_cmd; status_cmd; run_cmd; kill_cmd;
      inspect_cmd;
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



