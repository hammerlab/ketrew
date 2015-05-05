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

open Ketrew_pervasives
open Ketrew_unix_io
module Target = Ketrew_target
module Document = Ketrew_document
module Interaction = Ketrew_interaction

module Target_cache  = struct
  (**
     For now we implement the cache with a [Hashtbl.t] but in the
     future this maybe using a proper DB or cache. Hence the I/O types
     ([Deferred_result.t]) given to the functions. *)

  type t = {
    targets: (Target.id, Target.t) Hashtbl.t;
  }

  let create () = {targets = Hashtbl.create 42}

  let get {targets} ~id =
    try return (Some (Hashtbl.find targets id))
    with _ ->
      Log.(s "Target-cache: miss on " % s id @ verbose);
      return None

  let add {targets} ~id ~value =
    Hashtbl.replace targets id value;
    return ()

  let clear {targets} = Hashtbl.clear targets

end

type exploration_state = {
  build_process_details: bool;
  target_filter: (Target.t -> bool) * Log.t;
  condition_details: bool;
  metadata_details: bool;
  currently_seeing: [
    | `Target of Target.id
    | `Target_menu of [`From of int]
  ];
}

let create_state () = {
  build_process_details = false;
  condition_details = false;
  metadata_details = false;
  target_filter = (fun _ -> true), Log.(s "No-filter, see them all");
  currently_seeing = `Target_menu (`From 0);
}

type t = {
  ketrew_client: Ketrew_client.t;
  mutable target_ids: string list;
  target_cache: Target_cache.t;
  mutable state_stack: exploration_state list;
  mutable request_targets_ids: [ `All | `Younger_than of [ `Days of float ]];
  mutable targets_per_page: int;
  mutable targets_to_prefetch: int;
}
let create ~client () =
  let conf = Ketrew_client.configuration client in
  {
  ketrew_client = client;
  target_ids = [];
  target_cache = Target_cache.create ();
  state_stack = [];
  request_targets_ids = Ketrew_configuration.request_targets_ids conf;
  targets_per_page = Ketrew_configuration.targets_per_page conf;
  targets_to_prefetch = Ketrew_configuration.targets_to_prefetch conf;
}

let reload_list_of_ids explorer =
  let query =
    match explorer.request_targets_ids with
    | `All -> `All
    | `Younger_than (`Days days) ->
      let now_in_seconds = Time.now () in
      let limit_in_seconds = 60. *. 60. *. 24. *. days in
      `Created_after (now_in_seconds -. limit_in_seconds)
  in
  Ketrew_client.get_list_of_target_ids explorer.ketrew_client query 
  >>| List.sort  ~cmp:(fun a b -> String.compare b a) (* reverse order *)
  >>= fun id_list ->
  Log.(s "Explorer.reload got " % i (List.length id_list) % s " ids"
       @ verbose);
  explorer.target_ids <- id_list;
  Target_cache.clear explorer.target_cache;
  return ()

let rec get_target ?(force_reload=false) ?prefetching explorer ~id =
  begin match force_reload with
  | false -> Target_cache.get explorer.target_cache ~id
  | true -> return None
  end
  >>= begin function
  | Some e -> return e
  | None ->
    begin match prefetching with
    | None  ->
      Log.(s "Explorer getting target " % s id
           % (if force_reload then s " (forced)" else s " (cache miss)")
           @ verbose);
      Ketrew_client.get_target explorer.ketrew_client ~id
      >>= fun value ->
      Target_cache.add explorer.target_cache ~id ~value
      >>= fun () ->
      return value
    | Some (`Take_from (nb, ids)) ->
      let rec find_cache_misses cache_misses = function
      | (zero, _) when zero <= 0 -> return cache_misses
      | (_, []) -> return cache_misses
      | (more, next :: ids) ->
        Target_cache.get explorer.target_cache ~id:next
        >>= fun opt ->
        begin match opt with
        | None -> find_cache_misses (next :: cache_misses) (more - 1, ids)
        | Some _ -> find_cache_misses cache_misses (more, ids)
        end
      in
      find_cache_misses [] (nb, id :: ids)
      >>= fun id_list ->
      Log.(s "Explorer getting targets " % OCaml.list s id_list @ verbose);
      Ketrew_client.get_targets explorer.ketrew_client ~id_list
      >>= fun targets ->
      Deferred_list.while_sequential targets ~f:(fun value ->
          let id = Ketrew_target.id value in
          Target_cache.add explorer.target_cache ~id ~value)
      >>= fun (_ : unit list) ->
      get_target ~force_reload:false ?prefetching:None explorer ~id
    end
  end

(*
   Items that would be in every menu
*)
let common_menu_items =
  Interaction.([
      menu_item ~char:'Q' ~log:Log.(s "Quit explorer") `Quit;
      menu_item ~char:'q' ~log:Log.(s "Cancel/Go-back") `Cancel;
      menu_item ~char:'R' ~log:Log.(s "Reload") `Reload;
      menu_item ~char:'S' ~log:Log.(s "Settings") `Settings;
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

let failed_but_not_because_of_dependencies t =
  let state = Target.state t in
  Target.State.simplify state = `Failed
  && not (Target.State.Is.finished_because_dependencies_died state)

let really_running t =
  let state = Target.state t in
  let open Target.State in
  Is.started_running state || Is.still_running state
  || Is.ran_successfully state

let waiting_on_dependencies t =
  let state = Target.state t in
  let open Target.State in
  Is.building state || Is.still_building state

let filters = [
  filter        (fun _ -> true) ~char:'n' ~log:Log.(s "No-filter, see them all");
  simple_filter `Activable      ~char:'p' ~log:Log.(s "Passive/Activable");
  simple_filter `In_progress    ~char:'r' ~log:Log.(s "Running/In-progress");
  filter really_running         ~char:'R' ~log:Log.(s "Really running, not waiting");
  filter waiting_on_dependencies ~char:'d' ~log:Log.(s "Waiting on dependencies");
  simple_filter `Successful     ~char:'s' ~log:Log.(s "Successful");
  simple_filter `Failed         ~char:'f' ~log:Log.(s "Failed");
  filter finished ~char:'n' ~log:Log.(s "Finished (success or failure)");
  filter failed_but_not_because_of_dependencies
    ~char:'D' ~log:Log.(s "Failed but not because of its depedencies");
]

let initial_ask_tags_content =
  "# Enter regular expressions on `tags` of the targets\n\
   # Lines beginning with '#' are thrown aways\n\
   # The syntax of the regular expressions is “POSIX”\n\
  "

let get_filter () =
  Interaction.(
    menu ~sentence:Log.(s "Pick a filter")
      (List.map filters ~f:(fun (char, log, tag) ->
            menu_item ~char ~log tag)
        @ [menu_item ~char:'T'
            ~log:Log.(s "Enter tag regular-expression(s)") `Ask_tags]
      )
  )
  >>= function
  | `Ask_tags ->
    Interaction.ask_for_edition initial_ask_tags_content
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

let rec settings_menu explorer =
  Interaction.(
    menu ~sentence:Log.(s "Pick an action")
      ~always_there:[
        menu_item ~char:'q' ~log:Log.(s "Quit") `Quit;
      ]
      [
        menu_item ~char:'d' `Edit ~log:Log.(s "Edit settings with a DSL");
      ]
  )
  >>= function
  | `Quit -> return ()
  | `Edit ->
    Interaction.ask_for_edition
      (fmt
         "# Edit/Uncomment the lines you're interested\n\
          # (lines starting with '#' are ignored)\n\
          \n\
          # Set the “listing query”:\n\
          # type (or uncomment):\n\
          #   listing-query = all\n\
          # to see ALL targets since the beginning of time, or limit with:\n\
          #   listing-query = younger than <some-float> days\n\
          # Current value: %s\n\
          \n\
          # Set the number of targets per page:\n\
          #   targets-per-page = %d\n\
          \n\
          # Set the number of targets that the client requests at once (for prefetching/speed)):\n\
          #   prefetch-targets = %d\n\
         "
         (match explorer.request_targets_ids with
         | `All -> "all"
         | `Younger_than (`Days days) -> fmt "younger than %F days" days)
         explorer.targets_per_page
         explorer.targets_to_prefetch)
    >>| String.split ~on:(`Character '\n')
    >>= fun lines ->
    List.iter lines ~f:(fun line ->
        let stripped = String.strip line in
        match String.get ~index:0 stripped with
        | Some '#' | None -> ()
        | Some other ->
          begin match String.split ~on:(`Character ' ') stripped
                      |> List.map ~f:String.strip
                      |> List.filter ~f:((<>) "") with
          | "listing-query" :: "=" :: listing_query ->
            begin match listing_query with
            | ["all"] | ["All"] | ["ALL"] ->
              explorer.request_targets_ids <- `All;
            | "younger" :: "than" :: v :: "days" :: [] ->
              begin match Float.of_string v with
              | None -> Log.(s "Can't parse float: " % quote v @ error);
              | Some days ->
                explorer.request_targets_ids <- (`Younger_than (`Days days))
              end
            | _ -> Log.(s "Can't parse listing query: " % quote line @ error);
            end
          | "targets-per-page" :: "=" :: v :: [] ->
            begin match Int.of_string v with
            | None -> Log.(s "Can't parse int: " % quote v @ error);
            | Some nb -> explorer.targets_per_page <- nb
            end
          | "prefetch-targets" :: "=" :: v :: [] ->
            begin match Int.of_string v with
            | None -> Log.(s "Can't parse int: " % quote v @ error);
            | Some nb -> explorer.targets_to_prefetch <- nb
            end
          | _ -> Log.(s "Can't parse line: " % quote line @ error);
          end);
    reload_list_of_ids explorer
    >>= fun () ->
    settings_menu explorer

let pick_a_target_from_list explorer target_ids =
  Deferred_list.while_sequential target_ids (fun id -> get_target explorer ~id)
  >>= fun targets ->
  Interaction.(
    menu ~sentence:Log.(s "Pick a target")
      ~always_there:common_menu_items
      (make_target_menu ~targets ()))

let pick_a_target explorer (es : exploration_state) ~how =
  (* Ketrew_client.current_targets explorer.ketrew_client >>= fun targets -> *)
  let targets_to_display = explorer.targets_per_page in
  let max_per_page =
    targets_to_display + List.length common_menu_items
    + 1 (* = filter *)
    + 3 (* prev, next, and home *)
  in
  begin match how with
  | `From from ->
    let rec find_targets in_list acc found_count passed_count =
      match in_list with
      | [] -> return (List.rev acc, None)
      | _  when found_count = targets_to_display ->
        return (List.rev acc, Some (`Pick (`From passed_count)))
      | one :: more ->
        let prefetching = `Take_from (explorer.targets_to_prefetch, more) in
        get_target explorer ~id:one ~prefetching
        >>= fun target ->
        begin match (fst es.target_filter) target with
        | true ->
          find_targets more (target :: acc) (found_count + 1) (passed_count + 1)
        | false -> find_targets more acc found_count (passed_count + 1)
        end
    in
    let init = List.drop explorer.target_ids from in
    Log.(s "pick_a_target from " % i from % s " among " % i (List.length init)
         @ verbose);
    find_targets init [] 0 from
  end
  >>= fun (sub_list, next_menu_option) ->
  Interaction.(
    menu ~sentence:Log.(s "Pick a target") ~max_per_page
      ~always_there:(
        common_menu_items
        @ [
          menu_item ~char:'f'
            ~log:Log.(s "Change filter "
                      % parens (s "current: " % snd es.target_filter))
            `Filter;
        ]
        @ (match next_menu_option with
          | None -> [] | Some next ->
            [menu_item ~char:'n' ~log:Log.(s "Next targets") next])
        @ (match how with
          | `From 0 -> []
          | `From other ->
            [menu_item ~char:'p' ~log:Log.(s "Previous view") `Cancel;
             menu_item ~char:'h' ~log:Log.(s "Back to top/home")
               (`Pick (`From 0))])
      )
      (List.map sub_list ~f:(fun target ->
           menu_item ~log:Log.(Ketrew_document.target_for_menu target)
             (`Go (Target.id target)))))

let explore_single_target ~client (es: exploration_state) target =
  let sentence =
    let build_process_details = es.build_process_details in
    let condition_details = es.condition_details in
    let metadata_details = es.metadata_details in
    Log.(s "Exploring "
         % Document.target
           ~build_process_details ~condition_details ~metadata_details target)
  in
  Interaction.(
    let kill_item =
      if Target.state target |> Target.State.Is.killable
      then [menu_item ~char:'k' ~log:Log.(s "Kill") `Kill]
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
    let metadata_details_item =
      match Target.metadata target with
      | None -> []
      | Some _ ->
        boolean_item ~value:es.metadata_details ~char:'m'
          ~to_false:(Log.(s "Hide metadata details"), `Show_metadata false)
          ~to_true:(Log.(s "Show metadata details"), `Show_metadata true)
        @ [menu_item ~char:'M'
             ~log:Log.(s "View Metadata in $EDITOR") `View_metadata]
    in
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
    menu ~sentence ~always_there:common_menu_items (
      [menu_item ~char:'s' ~log:Log.(s "Show status") `Status]
      @ build_process_details_item
      @ condition_details_item
      @ metadata_details_item
      @ follow_deps_item
      @ follow_fbacks_item
      @ follow_success_triggers_item
      @ kill_item
      @ restart_item
      @ [menu_item ~char:'O' ~log:Log.(s "See JSON in $EDITOR") `View_json]
    ))


let view_json ~client target =
  let content = Target.Stored_target.(of_target target |> serialize) in
  Interaction.view_in_dollar_editor ~extension:"json" content

let view_metadata ~client target =
  let content =
    match Target.metadata target with
    | Some (`String s) -> s
    | None -> "" in
  Interaction.view_in_dollar_editor ~extension:"json" content

let rec target_status
    ?(viewer=`Inline) ?(add_info=Log.empty) explorer exploration_state target =
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
    common_menu_items @  viewer_items  in
  menu ~sentence ~always_there additional
  >>= function
  | `Set_viewer viewer ->
    target_status ~viewer explorer exploration_state target
  | `Call (key, log) ->
    Log.(s "Calling query " % sf "%S" key % n
         % s "Press " % bold_red (s "'K'") % s " for cancelling"
         @ warning);
    begin Deferred_list.pick_and_cancel [
        Ketrew_client.call_query explorer.ketrew_client ~target key;
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
    target_status explorer ~viewer ?add_info exploration_state target
  | `Reload ->
    get_target explorer ~force_reload:true ~id:(Target.id target)
    >>= fun chosen ->
    target_status explorer ~viewer exploration_state chosen
  | `Settings ->
    settings_menu explorer
    >>= fun () ->
    target_status explorer ~viewer exploration_state target
  | `Cancel | `Quit as up -> return up

let rec exploration_loop explorer state =
  let go_back explorer state =
    match state with
    | [] -> return ()
    | some -> exploration_loop explorer some in
  begin match state with
  | [] -> exploration_loop explorer [create_state ()]
  | (current :: previous) as history ->
    begin match current.currently_seeing with
    | `Target_menu how ->
      begin pick_a_target explorer current ~how
        >>= function
        | `Cancel -> go_back explorer previous
        | `Quit -> return ()
        | `Reload ->
          reload_list_of_ids explorer
          >>= fun () ->
          exploration_loop explorer history
        | `Settings ->
          settings_menu explorer
          >>= fun () ->
          exploration_loop explorer history
        | `Filter ->
          get_filter () >>= fun f ->
          exploration_loop explorer ({current with target_filter = f } :: history)
        | `Go t ->
          exploration_loop explorer ({current with currently_seeing = `Target t } :: history)
        | `Pick how ->
          exploration_loop explorer ({current with currently_seeing = `Target_menu how } :: history)
      end
    | `Target chosen_id ->
      get_target explorer ~id:chosen_id
      >>= fun chosen ->
      begin explore_single_target ~client:explorer.ketrew_client current chosen
        >>= function
        | `Cancel -> go_back explorer previous
        | `Quit -> return ()
        | `Reload ->
          get_target explorer ~id:chosen_id ~force_reload:true
          >>= fun reloaded ->
          exploration_loop explorer history
        | `Settings ->
          settings_menu explorer
          >>= fun () ->
          exploration_loop explorer history
        | `Show_make build_process_details ->
          exploration_loop explorer ({ current with build_process_details }  :: history)
        | `Show_condition condition_details ->
          exploration_loop explorer ({ current with condition_details } :: history)
        | `Show_metadata metadata_details ->
          exploration_loop explorer ({ current with metadata_details } :: history)
        | `Status ->
          begin target_status explorer current chosen
            >>= function
            | `Cancel -> go_back explorer history
            | `Quit -> return ()
          end
        | `Kill ->
          Log.(s "Killing target …" @ warning);
          Ketrew_client.kill explorer.ketrew_client [Target.id chosen]
          >>= fun () ->
          exploration_loop explorer history
        | `Restart ->
          Ketrew_client.restart explorer.ketrew_client [Target.id chosen]
          >>= fun () ->
          exploration_loop explorer history
        | `View_json ->
          view_json explorer.ketrew_client chosen >>= fun () ->
          exploration_loop explorer history
        | `View_metadata ->
          view_metadata ~client:explorer.ketrew_client chosen >>= fun () ->
          exploration_loop explorer history
        | `Follow_dependencies | `Follow_fallbacks | `Follow_success_triggers
          as follow ->
          let target_ids t =
            match follow with
            | `Follow_fallbacks -> (Target.fallbacks t)
            | `Follow_success_triggers -> (Target.success_triggers t)
            | `Follow_dependencies -> (Target.dependencies t) in
          let rec next_target ids =
            begin pick_a_target_from_list explorer ids
              >>= function
              | `Cancel -> return `Cancel
              | `Quit -> return `Quit
              | `Reload ->
                get_target explorer ~id:chosen_id ~force_reload:true
                >>= fun chosen ->
                next_target (target_ids chosen)
              | `Settings ->
                settings_menu explorer
                >>= fun () ->
                next_target ids
              | `Go t -> return (`Go t)
            end
          in
          begin next_target (target_ids chosen)
            >>= function
            | `Cancel -> go_back explorer history
            | `Quit -> return ()
            | `Go t ->
              exploration_loop explorer
                ({current with currently_seeing = `Target t} :: history)
          end
      end
    end
  end

(* The exported function just “starts” the exploration with `[]`: *)
let explore state =
  reload_list_of_ids state
  >>= fun () ->
  exploration_loop state []
