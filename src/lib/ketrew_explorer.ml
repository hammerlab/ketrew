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
module Document = Ketrew_document
module Interaction = Ketrew_interaction

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
  filter        (fun _ -> true) ~char:'n' ~log:Log.(s "No-filter, see them all");
  simple_filter `Activable      ~char:'p' ~log:Log.(s "Passive/Activable");
  simple_filter `In_progress    ~char:'r' ~log:Log.(s "Running/In-progress");
  simple_filter `Successful     ~char:'s' ~log:Log.(s "Successful");
  simple_filter `Failed         ~char:'f' ~log:Log.(s "Failed");
  filter        finished        ~char:'n' ~log:Log.(s "Finished (success or failure)");
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

let pick_a_target ~client (es : exploration_state) :
    (([> `Cancel | `Quit | `Reload | `Filter | `Go of string]), 'e) t =
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
    Ketrew_client.get_target client (Target.id target) >>= fun chosen ->
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
          Ketrew_client.restart_target client [Target.id chosen]
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
