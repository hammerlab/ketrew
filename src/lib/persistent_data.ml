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

include Logging.Global.Make_module_error_and_info(struct
    let module_name = "Persistence"
  end)
open Logging.Global

module Database = Trakeva_of_uri
module Database_action = Trakeva.Action
module Database_error = Trakeva.Error

module Error = struct
  type fetching_node = [
    | `Get_stored_target
    | `Pointer_loop_max_depth of int
    | `Target_to_add
  ] * [ `Id of string ]
end

module With_database = struct

  type t = {
    mutable database_handle: Database.t option;
    database_parameters: string;
    archival_age_threshold: [ `Days of float ];
    activation_mutex : Lwt_mutex.t;
    equivalence_checking_mode : [ `When_adding | `When_activating ];
  }
  let create ~database_parameters ~archival_age_threshold =
    let activation_mutex = Lwt_mutex.create () in
    return {
      database_handle = None; database_parameters; archival_age_threshold;
      activation_mutex;
      equivalence_checking_mode =
        begin try
          (* This is an undocumented way of changing how Ketrew check for
             equivalence of targets.

             The default one “when-adding” is the “historical” one.

             The other one only checks for equivalence while activating the
             node against the other active nodes. It makes workflows show-up as
             active quicker. In some use-cases, this may be faster, in some
             others it will be slower.

             Both modes do not cooperate; if Ketrew restarts while changing
             mode, you'd better know what you're doing with your workflows.
          *)
          assert (Sys.getenv "KETREW_EQUIVALENCE_TEST" = "activating");
          `When_activating
        with _ -> `When_adding
        end;
    }

  let database t =
    match t.database_handle with
    | Some db -> return db
    | None ->
      Database.load t.database_parameters
      >>= fun db ->
      t.database_handle <- Some db;
      return db

  let unload t =
    match t.database_handle with
    | Some s ->
      Database.close s
    | None -> return ()


  (* These are the names of the collections used for storing targets
     with Trakeva: *)
  let passive_targets_collection = "passive-targets"
  let active_targets_collection = "active-targets"
  let finished_targets_collection = "finished-targets"
  let archived_targets_collection = "archived-targets"

  let all_collections = [
    passive_targets_collection;
    active_targets_collection;
    finished_targets_collection;
    archived_targets_collection;
  ]

  let set_target_db_action target =
    let key = Target.id target in
    Database_action.(set ~collection:active_targets_collection ~key
                       Target.Stored_target.(of_target target |> serialize))

  let run_database_action ?(msg="NO INFO") t action =
    database t
    >>= fun db ->
    Log.(s "Going to to run DB action: " % s msg @ very_verbose);
    begin Database.(act db  action)
      >>= function
      | `Done ->
        return ()
      | `Not_done ->
        (* TODO: try again a few times instead of error *)
        fail (`Database_unavailable (fmt "running DB action: %s" msg))
    end

  let make_pointer_of_passive_target t ~target ~pointing_to =
    let key = Target.id target in
    let src = passive_targets_collection in
    let dest = archived_targets_collection in
    let pointer =
      Target.Stored_target.make_pointer ~from:target ~pointing_to in
    run_database_action ~msg:(fmt "pointerify-%s-from-%s-to-%s" key src dest) t
      Database_action.(
        seq [
          unset ~collection:src key;
          set ~collection:dest ~key Target.Stored_target.(serialize pointer)
        ])
    >>= fun () ->
    return pointer


  let move_target t ~target ~src ~dest =
    (* Caller will assume `target` is the new value; it may have changed *)
    let key = Target.id target in
    run_database_action ~msg:(fmt "move-%s-from-%s-to-%s" key src dest) t
      Database_action.(
        seq [
          unset ~collection:src key;
          set ~collection:dest ~key
            Target.Stored_target.(of_target target |> serialize)
        ])

  let debug_archival =
    try Sys.getenv "DEBUG_ARCHIVAL" = "true"
    with _ -> false

  let get_archival_threshold t  =
    match debug_archival, t.archival_age_threshold with
    | true, _ -> 60. *. 2.
    | false, `Days d -> Time.day *. d

  (* This will be called after an update and after fetching to
     make sure it gets done even when there are crashes or weirdly sync-ed DBs
  *)
  let clean_up_finished t ~from target =
    let archival_threshold = get_archival_threshold t in
    let now = Time.now () in
    begin match Target.(state target |> State.finished_time) with
    | None -> return ()
    | Some time when
        (* finished, but not old enough to go to archive *)
        time +. archival_threshold > now ->
      if debug_archival then
        Printf.eprintf "debug archival\n   target %s (from %s) is YOUNG\n   (%s + %f Vs %s)\n%!"
          (Target.id target) from (Time.to_string_hum time) archival_threshold
          (now |>Time.to_string_hum);
      begin if from <> finished_targets_collection
        then (
          log_info Log.(Target.log target % s " moves to the finsihed collection");
          move_target t ~target ~src:from ~dest:finished_targets_collection
        ) else (
          (* already in "finished" *)
          return ()
        )
      end
    | Some time
      when time +. archival_threshold < Time.now ()
        && from = finished_targets_collection ->
      if debug_archival then
        Printf.eprintf "debug archival target %s is OLD\n%!" (Target.id target);
      (* old enough to be archived and not yet archived *)
      move_target t ~target ~src:from ~dest:archived_targets_collection
    | Some _ ->
      return ()
    end

  let add_or_update_targets t target_list =
    run_database_action t
      Database_action.(seq (List.map target_list ~f:set_target_db_action))
      ~msg:(fmt "add_or_update_targets [%s]"
              (List.map target_list ~f:Target.id |> String.concat ~sep:", "))

  let update_target t trgt =
    add_or_update_targets t [trgt]
    >>= fun () ->
    clean_up_finished t ~from:active_targets_collection trgt

  let raw_add_or_update_stored_target t ~collection ~stored_target =
    let key = Target.Stored_target.id stored_target in
    run_database_action t
      Database_action.(
        set ~collection  ~key (Target.Stored_target.serialize stored_target)
      )
      ~msg:(fmt "raw_add_or_update_stored_target: %s" key)

  let get_stored_target t key =
    begin
      database t >>= fun db ->
      List.fold all_collections ~init:(return None) ~f:(fun prev_m collection ->
          prev_m
          >>= begin function
          | Some s -> return (Some s)
          | None ->
            Database.get db ~collection ~key
            >>= begin function
            | Some serialized_stored ->
              of_result (Target.Stored_target.deserialize serialized_stored)
              >>= fun st ->
              (* It may happen that finished targets are in the active collection,
                 - the application may have been stopped between the
                   Finished save and the move to the finsihed collection
                 - the user may have played with `ketrew sync` and backup
                   directories
                 so when we come accross it, we clean up *)
              begin match Target.Stored_target.get_target st with
              | `Target target ->
                clean_up_finished t target ~from:collection
                >>= fun () ->
                return (Some st)
              | `Pointer _ ->
                (* pointers are already archived *)
                return (Some st)
              end
            | None ->
              return None
            end
          end
        )
      >>= function
      | Some st -> return st
      | None -> fail (`Fetching_node (`Get_stored_target, `Id key))
    end

  let get_target t id =
    let rec get_following_pointers ~key ~count =
      get_stored_target t key
      >>= fun stored ->
      begin match Target.Stored_target.get_target stored with
      | `Pointer _ when count >= 30 ->
        fail (`Fetching_node (`Pointer_loop_max_depth 30, `Id id))
      | `Pointer key ->
        get_following_pointers ~count:(count + 1) ~key
      | `Target t -> return t
      end
    in
    get_following_pointers ~key:id ~count:0

  let get_collections_of_stored_targets t ~from =
    database t
    >>= fun db ->
    Deferred_list.while_sequential from ~f:(fun collection ->
        Database.get_all db ~collection
        >>= fun ids ->
        return (collection, ids))
    >>= fun col_ids ->
    log_info
      Log.(s "Getting : "
           % separate (s " + ")
             (List.map col_ids ~f:(fun (col, ids) ->
                  i (List.length ids) % sp % parens (quote col)
                ))
           % s " stored targets");
    Deferred_list.while_sequential col_ids ~f:(fun (_, ids) ->
        Deferred_list.while_sequential ids ~f:(fun key ->
            get_stored_target t key
          ))
    >>| List.concat

  let get_collections_of_targets t ~from =
    database t
    >>= fun db ->
    Deferred_list.while_sequential from ~f:(fun collection ->
        Database.get_all db ~collection
        >>= fun ids ->
        return (collection, ids))
    >>= fun col_ids ->
    log_info
      Log.(s "Getting : "
           % separate (s " + ")
             (List.map col_ids ~f:(fun (col, ids) ->
                  i (List.length ids) % sp % parens (quote col)
                ))
           % s " targets");
    Deferred_list.while_sequential col_ids ~f:(fun (_, ids) ->
        Deferred_list.while_sequential ids ~f:(fun key ->
            get_stored_target t key
            >>| Target.Stored_target.get_target
            >>= fun topt ->
            begin match topt with
            | `Pointer _ ->
              return None
            | `Target target -> return (Some target)
            end)
        >>| List.filter_opt)
    >>| List.concat

  let alive_stored_targets t =
    get_collections_of_stored_targets t ~from:[
      passive_targets_collection;
      active_targets_collection;
    ]
  let get_all_finished_ids t =
    database t
    >>= fun db ->
    Database.get_all db ~collection:finished_targets_collection

  let active_targets t =
    get_collections_of_targets t ~from:[active_targets_collection]
    >>= fun targets ->
    let filtered =
      List.filter_map targets ~f:(fun target ->
          match Target.State.simplify (Target.state target) with
          | `Failed
          | `Successful
          | `Activable -> None
          | `In_progress -> Some target)
    in
    Logger.(
      description_list [
        "module", text "With_database";
        "function", text "active_targets";
        "result", textf "%d targets, %d after filter"
          (List.length targets) (List.length filtered);
      ] |> log);
    return filtered

  (** Get the targets are alive, so activable or in_progress *)
  let alive_targets t =
    get_collections_of_targets t ~from:[passive_targets_collection;
                                        active_targets_collection]
    >>= fun targets ->
    let filtered =
      List.filter_map targets ~f:(fun target ->
          match Target.State.simplify (Target.state target) with
          | `Failed
          | `Successful -> None
          | `Activable
          | `In_progress -> Some target)
    in
    Logger.(
      description_list [
        "module", text "With_database";
        "function", text "alive_targets";
        "result", textf "%d targets, %d after filter"
          (List.length targets) (List.length filtered);
      ] |> log);
    return filtered

  let activate_target t ~target ~reason =
    let starts = Time.now () in
    let log = ref Display_markup.[
        "module", text "With_database";
        "function", text "activate_target";
        "database_parameters", path t.database_parameters;
        "target", textf "%s (%s)" (Target.name target) (Target.id target);
        "starts", date starts;
      ] in
    begin match t.equivalence_checking_mode with
    | `When_activating ->
      Lwt_mutex.with_lock t.activation_mutex begin fun () ->
        let enters_mutex = Time.now () in
        begin
          get_target t (Target.id target)
          >>= fun fresh_target ->
          begin match Target.state fresh_target |> Target.State.simplify with
          | `Activable ->
            log := Display_markup.("is-activable", date_now ()) :: !log;
            active_targets t
            >>= fun current_living_targets ->
            log := Display_markup.("get-living-targets", date_now ()) :: !log;
            log := Display_markup.(
                "living-targets",
                textf "%d" (List.length current_living_targets)
              ) :: !log;
            begin match
              List.find current_living_targets (Target.is_equivalent target)
            with
            | Some pointing_to ->
              (* We found one, need to make a pointer *)
              log := Display_markup.("equivalent-to",
                                     textf "%s (%s)"
                                       (Target.name pointing_to)
                                       (Target.id pointing_to)) :: !log;
              make_pointer_of_passive_target t ~target ~pointing_to
            | None ->
              log := Display_markup.("certified-fresh", date_now ()) :: !log;
              let newone = Target.(activate_exn target ~reason) in
              move_target t ~target:newone ~src:passive_targets_collection
                ~dest:active_targets_collection
              >>= fun () ->
              return (Target.Stored_target.of_target newone)
            end
          | `In_progress
          | `Successful
          | `Failed ->
            log := Display_markup.("is-not-activable", date_now ()) :: !log;
            return (Target.Stored_target.of_target fresh_target)
          end
        end
        |> Lwt.map (fun x ->
            log := !log @ Display_markup.[
                "enters_mutex", time_span (enters_mutex -. starts);
                "exit_mutex", time_span (Time.now () -. enters_mutex);
              ];
            x)
      end
    | `When_adding ->
      let newone = Target.(activate_exn target ~reason) in
      move_target t ~target:newone ~src:passive_targets_collection
        ~dest:active_targets_collection
      >>= fun () ->
      return (Target.Stored_target.of_target newone)
    end
    >>= fun stored ->
    log := !log @ Display_markup.[
        "total-time", time_span (Time.now () -. starts);
      ];
    Logger.log (Display_markup.description_list !log);
    return stored


  module Killing_targets = struct
    let targets_to_kill_collection = "targets-to-kill"
    let add_target_ids_to_kill_list t id_list =
      let action =
        let open Database_action in
        List.map id_list ~f:(fun id ->
            set ~collection:targets_to_kill_collection ~key:id id)
        |> seq in
      run_database_action t action
        ~msg:(fmt "add_target_ids_to_kill_list [%s]"
                (String.concat ~sep:", " id_list))

    let get_all_targets_to_kill t : (Target.id list, _) Deferred_result.t =
      database t
      >>= fun db ->
      Database.get_all db ~collection:targets_to_kill_collection
      >>= fun all_keys ->
      (* Keys are equal to values, so we can take short cut *)
      return all_keys

    let remove_from_kill_list_action id =
      Database_action.(unset ~collection:targets_to_kill_collection id)

    let proceed_to_mass_killing t =
      get_all_targets_to_kill t
      >>= fun to_kill_list ->
      log_info
        Log.(s "Going to actually kill: " % OCaml.list quote to_kill_list);
      List.fold to_kill_list ~init:(return []) ~f:(fun prev id ->
          prev >>= fun prev_list ->
          get_target t id
          >>= fun target ->
          let pull_out_from_passiveness =
            if Target.state target |> Target.State.Is.passive then
              [Database_action.unset ~collection:passive_targets_collection id]
            else [] in
          begin match Target.kill target with
          | Some t ->
            return (t, pull_out_from_passiveness @ [
                remove_from_kill_list_action id;
                set_target_db_action t;
              ])
          | None ->
            return
              (target,
               pull_out_from_passiveness @ [remove_from_kill_list_action id])
          end
          >>= fun (target_actions) ->
          return (target_actions :: prev_list)
        )
      >>= begin function
      | [] -> return []
      | target_actions ->
        let actions =
          List.rev_map target_actions ~f:(fun (trgt, act) -> act)
          |> List.concat in
        run_database_action t Database_action.(seq actions)
          ~msg:(fmt "killing %d targets" (List.length to_kill_list))
        >>= fun () ->
        return (List.rev_map target_actions ~f:fst)
      end

  end



  module Adding_targets = struct
    let targets_to_add_collection = "targets-to-add"
    let register_targets_to_add t t_list =
      let action =
        let open Database_action in
        List.map t_list ~f:(fun trgt ->
            let st = Target.Stored_target.of_target trgt in
            let key = Target.Stored_target.id st in
            set ~collection:targets_to_add_collection ~key
              (Target.Stored_target.serialize st))
        |> seq in
      log_info
        Log.(s "Storing " % i (List.length t_list)
             % s " targets to be added at next step");
      run_database_action t action
        ~msg:(fmt "store_targets_to_add [%s]"
                (List.map t_list ~f:Target.id
                 |> String.concat ~sep:", "))

    let get_all_targets_to_add t =
      database t
      >>= fun db ->
      Database.get_all db ~collection:targets_to_add_collection
      >>= fun keys ->
      Deferred_list.while_sequential keys ~f:(fun key ->
          Database.get db ~collection:targets_to_add_collection ~key
          >>= begin function
          | Some blob ->
            of_result (Target.Stored_target.deserialize blob)
          | None ->
            fail (`Fetching_node (`Target_to_add, `Id key))
          end
          >>= fun st ->
          begin match Target.Stored_target.get_target st with
          | `Target t -> return t
          | `Pointer p ->
            get_target t p (* We don't use this case in practice maybe
                              it would be better to fail there *)
          end)

    let check_and_really_add_targets t =
      get_all_targets_to_add t
      >>= fun tlist ->
      begin match tlist with
      | [] -> return []
      | _ :: _ ->
        begin match t.equivalence_checking_mode with
        | `When_activating -> active_targets t
        | `When_adding -> alive_targets t
        end
        >>= fun current_interesting_targets ->
        let stuff_to_actually_add =
          List.fold ~init:[] tlist ~f:begin fun to_store_targets target ->
            let equivalences =
              let we_kept_so_far =
                List.filter_map to_store_targets
                  ~f:(fun st ->
                      match Target.Stored_target.get_target st with
                      | `Target t -> Some t
                      | `Pointer _ -> None) in
              List.filter (current_interesting_targets @ we_kept_so_far)
                ~f:(fun t -> Target.is_equivalent target t) in
            Log.(Target.log target % s " is "
                 % (match equivalences with
                   | [] -> s "pretty fresh"
                   | more ->
                     s " equivalent to " % OCaml.list Target.log equivalences)
                 @ very_verbose);
            match equivalences with
            | [] ->
              (Target.Stored_target.of_target target :: to_store_targets)
            | at_least_one :: _ ->
              (
                if Target.State.Is.activated_by_user (Target.state target)
                then
                  Logging.User_level_events.root_workflow_equivalent_to
                    ~name:(Target.name target)
                    ~id:(Target.id target)
                    (List.map equivalences ~f:(fun st ->
                         (Target.name st, Target.id st)))
              );
              (Target.Stored_target.make_pointer
                 ~from:target ~pointing_to:at_least_one :: to_store_targets)
          end
        in
        log_info
          Log.(s "Adding new " % i (List.length stuff_to_actually_add)
               % s " targets to the DB"
               % (parens (i (List.length tlist) % s " were submitted")));
        let action =
          let open Database_action in
          List.map tlist ~f:(fun trgt ->
              let key = Target.id trgt in
              unset ~collection:targets_to_add_collection key)
          @ List.map stuff_to_actually_add ~f:(fun st ->
              let key = Target.Stored_target.id st in
              let collection =
                match Target.Stored_target.get_target st with
                | `Pointer _ -> archived_targets_collection
                | `Target t when Target.state t |> Target.State.Is.passive ->
                  passive_targets_collection
                | `Target t -> active_targets_collection
              in
              set ~collection ~key (Target.Stored_target.serialize st))
          |> seq in
        run_database_action t action
          ~msg:(fmt "check_and_really_add_targets [%s]"
                  (List.map tlist ~f:Target.id |> String.concat ~sep:", "))
        >>= fun () ->
        return stuff_to_actually_add
      end

    let force_add_passive_target t trgt =
      let st = Target.Stored_target.of_target trgt in
      let action =
        let open Database_action in
        let key = Target.id trgt in
        set ~collection:passive_targets_collection
          ~key (Target.Stored_target.serialize st)
      in
      run_database_action t action
        ~msg:(fmt "force_add_target: %s (%s)"
                (Target.id trgt) (Target.name trgt))
      >>= fun () ->
      return st


  end

end

module Cache_table = struct

  type 'a t = (string, 'a) Hashtbl.t

  let create () = Hashtbl.create 42

  let add_or_replace t id st =
    Hashtbl.replace t id st;
    return ()

  let get t id =
    try return (Some (Hashtbl.find t id))
    with e ->
      return None

  let fold t ~init ~f =
    Hashtbl.fold (fun id elt previous -> f ~previous ~id elt) t init

  let reset t = Hashtbl.reset t; return ()

  let remove t id = Hashtbl.remove t id

end

module String_mutable_set = struct
  module String_set = Set.Make(String)

  type t = String_set.t ref
  let of_list l = ref (String_set.of_list l)
  let add t s =
    t := String_set.add s !t
  let fold t ~init ~f =
    String_set.fold (fun elt prev -> f prev elt) !t init
  let length t = String_set.cardinal !t

  let remove_list t l =
    t := String_set.diff !t (String_set.of_list l)

  (*
    let x = of_list ["a"; "b"; "c"; "d"; "e"]
    let () = remove_list x ["b"; "d"]
    let l = String_set.elements !x
  *)

end

module Event_source = struct
  type 'a t = {
    stream: 'a list Lwt_stream.t;
    trigger: 'a -> unit;
  }
  let create () =
    let base_event, trigger = React.E.create () in
    let stream =
      let last_return = ref None in
      let stream = Lwt_react.E.to_stream base_event in
      let rate_limit = 2.0 in
      let max_wait = 1.0 in
      Lwt_stream.from Lwt.(fun () ->
          let rec loop count acc =
            (* Count is used only for debug printing *)
            Lwt.pick [
              begin
                Lwt_stream.next stream
                >>= fun evalue ->
                return (evalue :: acc)
              end;
              begin
                Lwt_unix.sleep max_wait >>= fun _ -> return acc
              end;
            ]
            >>= fun new_values ->
            begin match !last_return with
            | None ->
              last_return := Some (Time.now ());
              loop (count + 1) new_values
            | Some t ->
              let now = Time.now () in
              begin match now -. t < rate_limit with
              | true ->
                loop (count + 1) new_values
              | false when new_values = [] ->
                loop (count + 1) new_values
              | false ->
                last_return := Some now;
                return (Some (new_values |> List.dedup))
              end
            end
          in
          loop 0 []
        )
    in
    {stream; trigger}
  let trigger {trigger; _} e =
    trigger e
  let stream {stream; _} = stream
end

module Change = struct
  type t = [ `Started | `New_nodes of string list | `Nodes_changed of string list ]
    [@@deriving show]
end

type t = {
  db: With_database.t;
  cache: Target.Stored_target.t Cache_table.t;
  all_ids: String_mutable_set.t;
  changes : Change.t Event_source.t;
}

let log_markup t mu =
  Logger.(
    description_list (
      ("Module", text "Persistent_data")
      ::
      ("Cache",
       textf "%d targets"
         (Cache_table.fold t.cache ~init:0
            ~f:(fun ~previous ~id _ -> previous + 1)))
      :: mu
    ) |> log)

let log_info t fmt =
  Printf.ksprintf (fun msg ->
      log_markup t ["Info", Logger.text msg]
    ) fmt

let create ~database_parameters ~archival_age_threshold =
  let starts = Time.now () in
  With_database.create ~database_parameters ~archival_age_threshold
  >>= fun db ->
  (* Heuristic: we cache the alive targets and all the finished IDs *)
  With_database.alive_stored_targets db
  >>= fun all ->
  let cache = Cache_table.create () in
  List.fold ~init:(return []) all  ~f:(fun prev_m st ->
      prev_m >>= fun prev ->
      let id = Target.Stored_target.id st in
      Cache_table.add_or_replace cache id st
      >>= fun () ->
      return (id :: prev)
    )
  >>= fun active_ids ->
  With_database.get_all_finished_ids db
  >>= fun more_ids ->
  let t = {
    db; cache;
    all_ids = String_mutable_set.of_list (List.rev_append active_ids more_ids);
    changes = Event_source.create ();
  } in
  log_markup t Display_markup.[
      "function", text "create";
      "database_parameters", path database_parameters;
      "archival_age_threshold",
      begin match archival_age_threshold with
      | `Days d -> time_span (Time.day *. d)
      end;
      "starts", date starts;
      "returns", date (Time.now ());
      "active_ids", textf "%d" (List.length active_ids);
      "more_ids", textf "%d" (List.length more_ids);
    ];
  return t

let next_changes t =
  Lwt.(
    Lwt_stream.next (Event_source.stream t.changes)
    >>= fun change ->
    return (`Ok change)
  )

let unload t =
  log_info t "unloading";
  With_database.unload t.db
  >>= fun () ->
  Cache_table.reset t.cache

let get_stored_target t ~key =
  Cache_table.get t.cache key
  >>= function
  | Some stored -> return stored
  | None ->
    With_database.get_stored_target t.db key
    >>= fun stored ->
    Cache_table.add_or_replace t.cache key stored
    >>= fun () ->
    return stored

let get_target t id =
  let rec get_following_pointers ~key ~count =
    get_stored_target t ~key
    >>= fun stored ->
    begin match Target.Stored_target.get_target stored with
    | `Pointer _ when count >= 30 ->
      fail (`Fetching_node (`Pointer_loop_max_depth 30, `Id  id))
    | `Pointer key ->
      String_mutable_set.add t.all_ids key;
      get_following_pointers ~count:(count + 1) ~key
    | `Target t -> return t
    end
  in
  get_following_pointers ~key:id ~count:0

(**

   [should_garbage_collect] returns {[
     [
       | `No
       | `Yes of (target_id * justification) list
     ]
   ]}
   where [justification] is {[
     [
       | `Points_to of target_id  (* poits to another target that should be
                                     collected *)
       | `Time of Time.t (* Has the given finished-time *)
     ]
   ]}
*)
let rec should_garbage_collect ?(acc = []) t stored_target =
  let check_target trgt =
    let threshold = With_database.get_archival_threshold t.db in
    let now = Time.now () in
    begin match Target.(state trgt |> State.finished_time) with
    | None -> return `No
    | Some time when time +. threshold > now ->
      (* finished, but not old enough to be garbage collected *)
      return `No
    | Some time -> return (`Yes ((Target.id trgt, `Time time) :: acc))
    end
  in
  begin match Target.Stored_target.get_target stored_target with
  | `Pointer id ->
    get_stored_target t id
    >>= fun points_to ->
    let item = id, `Points_to (Target.Stored_target.id points_to) in
    should_garbage_collect ~acc:(item  :: acc) t points_to
  | `Target t ->
    check_target t
  end

(** Go through the [to_gc] list and:

    - remove them from the list-of-ids cache,
    - remove the corresponding stored-target from the cache-table,
    - and use {!With_database.get_stored_target} to make sure
    {!With_database.clean_up_finished} is also called.

    In the function {!all_visible_targets}, we call
    {!perform_garbage_collection} inside a {!Lwt.async} call, so that garbage
    collection does not block the UIs that depend on it.
*)
let perform_garbage_collection t to_gc : (unit, unit) Deferred_result.t =
  let start_date = Time.now () in
  let to_remove =
    List.fold to_gc ~init:[] ~f:(fun prev (id, why) -> id :: prev) in
  String_mutable_set.remove_list t.all_ids to_remove;
  List.fold to_gc
    ~init:(return (`P 0, `Min infinity, `Max 0., `Errors []))
    ~f:begin fun prev_m (id, why) ->
      prev_m
      >>= fun (`P p, `Min mi, `Max ma, `Errors errors) ->
      Cache_table.remove t.cache id;
      begin
        (* This will trigger archival/clean-up in the database too: *)
        With_database.get_stored_target t.db id
        >>< function
        | `Ok _ ->
          begin match why with
          | `Points_to _ ->
            return (`P (p + 1), `Min mi, `Max ma, `Errors errors)
          | `Time t ->
            return (`P p, `Min (min t mi), `Max (max t ma), `Errors errors)
          end
        | `Error e ->
          return (`P p, `Min mi, `Max ma, `Errors (e :: errors))
      end
    end
  >>= fun (`P pointers, `Min min_time, `Max max_time, `Errors errors) ->
  log_markup t Display_markup.([
      "function", text "perform_garbage_collection";
      "to-garbage-collect", (
        let lgth = List.length to_gc in
        description_list [
          "stored-targets", textf "%d" lgth;
          "pointers", textf "%d" pointers;
          "finished-min", textf "%f" min_time;
          "finished-max", textf "%f" max_time;
          "threshold", time_span (With_database.get_archival_threshold t.db);
          "errors", textf "%d" (List.length errors);
          "details", (
            match lgth with
            | 0 -> text "None"
            | lgth when lgth > 20 -> text "HIDDEN"
            | less ->
              description_list (List.map to_gc ~f:(fun (id, why) ->
                  id, (match why with
                    | `Points_to id -> textf "-> %s" id
                    | `Time t -> concat [textf "finsihed on "; date t]
                    )))
          );
        ]
      );
      "start-date", date start_date;
      "total-time", time_span (Time.now () -. start_date);
    ]);
  return ()

let all_visible_targets t =
  let start_date = Time.now () in
  String_mutable_set.fold t.all_ids ~init:(return (`All [], `Gc []))
    ~f:begin fun prev_m id ->
      prev_m >>= fun (`All prev_list, `Gc prev_gc) ->
      get_stored_target t ~key:id
      >>= fun stored ->
      should_garbage_collect t stored
      >>= fun gc_result ->
      let gc =
        match gc_result with
        | `No -> `Gc prev_gc
        | `Yes l -> `Gc (prev_gc @ l)
      in
      begin match Target.Stored_target.get_target stored with
      | `Pointer _ -> return (`All prev_list, gc)
      | `Target t -> return (`All (t :: prev_list), gc)
      end
    end
  >>= fun (`All all_targets, `Gc to_gc) ->
  Lwt.async (fun () ->
      perform_garbage_collection t to_gc
    );
  log_markup t Display_markup.([
      "function", text "all_visible_targets";
      "all_visible_targets", textf "%d targets" (List.length all_targets);
      "all_ids", textf "%d ids" (String_mutable_set.length t.all_ids);
      "to-garabage-collect", textf "%d" (List.length to_gc);
      "start-date", date start_date;
      (* "iterator", time_span (iterator_date -. start_date); *)
      "total-time", time_span (Time.now () -. start_date);
    ]);
  return all_targets

let target_strict_state trgt =
  let is_finished = Target.(state trgt |> State.Is.finished) in
  let is_passive = Target.(state trgt |> State.Is.passive) in
  begin match is_finished, is_passive with
  | true, true -> assert false
  | true, false -> `Finished
  | false, true -> `Passive
  | false, false -> `Active
  end

(** [find_all_orphans] goes through the cache and returns all the targets that
    are passive but not reachable, i.e. that can't be activated, ever.

    The implementation follows 3 steps:

    - Go through [t.all_ids] and collect all the active and passive targets;
    - Follow all edges from the active ones, to find the reachable passives;
    - Substract the above from all the passives.

    The definition of active is here quite conservative, cf.
    {!target_strict_state}.

    The function logs at the end; one can trace it with
    ["debug_log_functions=find_all_orphans"].

*)
let find_all_orphans t =
  let log_items = ref Display_markup.[
      "function", text "find_all_orphans";
      "start", date_now ();
    ] in
  String_mutable_set.fold t.all_ids
    ~init:(return (`Passives [], `Actives []))
    ~f:begin fun prev_m id ->
      prev_m >>= fun ((`Passives pl, `Actives al) as prev) ->
      get_stored_target t ~key:id
      >>= fun stored ->
      begin match Target.Stored_target.get_target stored with
      | `Pointer id ->
        return prev
      | `Target trgt ->
        begin match target_strict_state trgt with
        | `Finished -> return prev
        | `Passive -> return (`Passives (trgt :: pl), `Actives al)
        | `Active -> return (`Passives pl, `Actives (trgt :: al))
        end
      end
    end
  >>= fun (`Passives passives, `Actives actives)->
  log_items := !log_items @ Display_markup.[
      "actives", big_itemize actives
        ~render:Target.(fun st -> textf "%s (%s)" (id st) (name st));
      "passives", textf "%d targets" (List.length passives);
    ];
  (* To find all the reachable-passives, we use [to_check] as a stack of
     targets to explore; and [acc] as the accumulation of results.

     The list [checked] is used to control against infinite loops (depending on
     the amount of equivalent targets, this may also be speeding up the
     search).
  *)
  let to_check = ref actives in
  let checked = ref [] in
  let rec reachable_passives acc () =
    match !to_check with
    | [] -> return acc
    | one :: more when List.exists !checked ~f:Target.(fun c -> id c = id one) ->
      to_check := more;
      reachable_passives acc ()
    | one :: more ->
      checked := one :: !checked;
      to_check := more;
      let all_edges =
        Target.depends_on one
        @ Target.on_failure_activate one
        @ Target.on_success_activate one
      in
      List.fold all_edges ~init:(return []) ~f:(fun prev_m id ->
          prev_m >>= fun prev ->
          get_target t id (* we actively want to follow pointers to find them
                             all *)
          >>= fun trgt ->
          begin match target_strict_state trgt with
          | `Finished -> return prev
          | `Passive ->
            to_check := trgt :: !to_check;
            return (trgt :: prev)
          | `Active ->
            to_check := trgt :: !to_check;
            return prev
          end
        )
      >>= fun passives ->
      reachable_passives (acc @ passives) ()
  in
  reachable_passives [] ()
  >>| List.dedup ~compare:(fun a b -> compare (Target.id a) (Target.id b))
  >>= fun reachable ->
  log_items := !log_items @ Display_markup.[
      "reachable", big_itemize reachable
        ~render:Target.(fun st -> textf "%s (%s)" (id st) (name st));
    ];
  let unreachable_passives =
    List.filter passives ~f:(fun p ->
        List.for_all reachable ~f:(fun rp -> Target.id rp <> Target.id p))
  in
  log_items := !log_items @ Display_markup.[
      "unreachable", big_itemize unreachable_passives
        ~render:Target.(fun st -> textf "%s (%s)" (id st) (name st));
      "end", date_now ();
    ];
  Logger.log Display_markup.(description_list !log_items);
  return unreachable_passives



let activate_target t ~target ~reason =
  With_database.activate_target t.db ~target ~reason
  >>= fun new_one ->
  (* let stored = Target.Stored_target.of_target new_one in *)
  Cache_table.add_or_replace t.cache (Target.Stored_target.id new_one) new_one
  >>= fun () ->
  Event_source.trigger t.changes (`Nodes_changed [Target.id target]);
  return ()

let fold_active_targets t ~init ~f =
  Cache_table.fold t.cache ~init:(return init) ~f:(fun ~previous ~id st ->
      previous >>= fun previous ->
      let active t =
        let s = Target.state t in
        not Target.State.Is.(passive s || finished s) in
      match Target.Stored_target.get_target st with
      | `Target t when active t ->
        f previous ~target:t
      | `Pointer _ | `Target _ -> return previous
    )

let update_target t trgt =
  With_database.update_target t.db trgt
  >>= fun () ->
  Cache_table.add_or_replace t.cache
    (Target.id trgt) (Target.Stored_target.of_target trgt)
  >>= fun () ->
  String_mutable_set.add t.all_ids (Target.id trgt);
  Event_source.trigger t.changes (`Nodes_changed [Target.id trgt]);
  return ()

module Killing_targets = struct

  let add_target_ids_to_kill_list t ids =
    With_database.Killing_targets.add_target_ids_to_kill_list t.db ids

  let proceed_to_mass_killing t =
    With_database.Killing_targets.proceed_to_mass_killing t.db
    >>= fun res ->
    List.fold ~init:(return ()) res ~f:(fun unit_m trgt ->
        unit_m >>= fun () ->
        Cache_table.add_or_replace t.cache
          (Target.id trgt) (Target.Stored_target.of_target trgt)
      )
    >>= fun () ->
    begin match res with
    | [] -> return false
    | more ->
      Event_source.trigger t.changes (`Nodes_changed (List.map ~f:Target.id more));
      return true
    end

end

module Adding_targets = struct

  let force_add_passive_target t ts =
    With_database.Adding_targets.force_add_passive_target t.db ts
    >>= fun st ->
    let id = Target.Stored_target.id st in
    String_mutable_set.add t.all_ids id;
    Cache_table.add_or_replace t.cache id st

  let register_targets_to_add t ts =
    With_database.Adding_targets.register_targets_to_add t.db ts
  let check_and_really_add_targets t =
    With_database.Adding_targets.check_and_really_add_targets t.db
    >>= fun res ->
    List.fold ~init:(return ()) res ~f:(fun unit_m st ->
        unit_m >>= fun () ->
        let id = Target.Stored_target.id st in
        String_mutable_set.add t.all_ids id;
        Cache_table.add_or_replace t.cache id st
      )
    >>= fun () ->
    begin match res with
    | [] -> return false
    | more ->
      Event_source.trigger t.changes
        (`New_nodes (List.map ~f:Target.Stored_target.id more));
      return true
    end

end

module Synchronize = struct

  let make_input spec =
    let uri = Uri.of_string spec in
    match Uri.scheme uri with
    | Some "backup" ->
      let path  = Uri.path uri in
      return (object
        method get_collection collection =
          let dir = path // collection in
          System.file_info dir
          >>= begin function
          | `Symlink _
          | `Socket
          | `Fifo
          | `Regular_file _
          | `Block_device
          | `Character_device -> fail (`Not_a_directory dir)
          | `Absent -> return []
          | `Directory ->
            let `Stream next = System.list_directory dir in
            let rec go acc =
              next ()
              >>= function
              | None -> return acc
              | Some s ->
                let file = path // collection // s in
                System.file_info file
                >>= begin function
                | `Regular_file _ ->
                  IO.read_file file
                  >>= fun d ->
                  of_result (Target.Stored_target.deserialize d)
                  >>= fun st ->
                  go (st :: acc)
                | _ -> go acc
                end
            in
            go []
          end
        method close =
          return ()
      end)
    | _ ->
      With_database.create
        ~database_parameters:spec
        ~archival_age_threshold:(`Days infinity)
      >>= fun src_db ->
      return (object
        method get_collection collection =
          With_database.get_collections_of_stored_targets src_db ~from:[collection]
        method close =
          With_database.unload src_db
      end)

  let make_output spec =
    let uri = Uri.of_string spec in
    match Uri.scheme uri with
    | Some "backup" ->
      let path  = Uri.path uri in
      System.ensure_directory_path path
      >>= fun () ->
      return (object
        method store ~collection ~stored_target =
          System.ensure_directory_path (path // collection)
          >>= fun () ->
          IO.write_file (path // collection //
                         Target.Stored_target.id stored_target ^ ".json")
            ~content:(Target.Stored_target.serialize stored_target)
        method close =
          return ()
      end)
    | _ ->
      With_database.create
        ~database_parameters:spec
        ~archival_age_threshold:(`Days infinity)
      >>= fun dst_db ->
      return (object
        method store ~collection ~stored_target =
          With_database.raw_add_or_update_stored_target dst_db
            ~collection ~stored_target
        method close =
          With_database.unload dst_db
      end)

  let copy src dst =
    make_input src
    >>= fun input ->
    make_output dst
    >>= fun output ->
    Deferred_list.while_sequential
      With_database.all_collections ~f:(fun collection ->
          input#get_collection collection
          >>= fun all_for_collection ->
          Deferred_list.while_sequential all_for_collection ~f:(fun stored_target ->
              output#store  ~collection ~stored_target)
          >>= fun _ ->
          return ())
    >>= fun _ ->
    input#close
    >>= fun () ->
    output#close



end
