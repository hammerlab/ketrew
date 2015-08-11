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

module Database = Trakeva_cache.Add(Trakeva_sqlite)
module Database_action = Trakeva.Action
module Database_error = Trakeva.Error

module With_database = struct

  type t = {
    mutable database_handle: Database.t option;
    database_parameters: string;
  }
  let create ~database_parameters =
    return {
      database_handle = None; database_parameters;
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

  let move_target_to_finished_collection t ~target =
    move_target t ~target
      ~src:active_targets_collection
      ~dest:finished_targets_collection

  let activate_target t ~target ~reason =
    let newone = Target.(activate_exn target ~reason) in
    move_target t ~target:newone ~src:passive_targets_collection
      ~dest:active_targets_collection
    >>= fun () ->
    return newone

  let add_or_update_targets t target_list =
    run_database_action t
      Database_action.(seq (List.map target_list ~f:set_target_db_action))
      ~msg:(fmt "add_or_update_targets [%s]"
              (List.map target_list ~f:Target.id |> String.concat ~sep:", "))

  let update_target t trgt = add_or_update_targets t [trgt]

  let get_stored_target t key =
    database t >>= fun db ->
    Database.get db ~collection:active_targets_collection ~key
    >>= begin function
    | Some serialized_stored ->
      of_result (Target.Stored_target.deserialize serialized_stored)
    | None ->
      Database.get db ~collection:finished_targets_collection ~key
      >>= begin function
      | Some serialized_stored ->
        of_result (Target.Stored_target.deserialize serialized_stored)
      | None ->
        Database.get db ~collection:passive_targets_collection ~key
        >>= begin function
        | Some serialized_stored ->
          of_result (Target.Stored_target.deserialize serialized_stored)
        | None ->
          fail (`Missing_data (fmt "get_stored_target %S" key))
        end
      end
    end

  let get_target t id =
    let rec get_following_pointers ~key ~count =
      get_stored_target t key
      >>= fun stored ->
      begin match Target.Stored_target.get_target stored with
      | `Pointer _ when count >= 30 ->
        fail (`Missing_data (fmt "there must be a loop or something (from %s)" id))
      | `Pointer key ->
        get_following_pointers ~count:(count + 1) ~key
      | `Target t -> return t
      end
    in
    get_following_pointers ~key:id ~count:0

  let fold_active_targets t ~init ~f =
    database t
    >>= fun db ->
    let target_stream =
      Database.iterator db ~collection:active_targets_collection in
    let rec iter_stream previous =
      target_stream ()
      >>= begin function
      | Some key ->
        get_stored_target t key
        >>| Target.Stored_target.get_target
        >>= fun topt ->
        begin match topt with
        | `Pointer _ -> (* it's a pointer, keep going *) iter_stream previous
        | `Target target ->
          f previous ~target
          >>= fun next ->
          iter_stream next
        end
      | None -> return previous (* done with the stream *)
      end
    in
    iter_stream init

  let get_collections_of_stored_targets t ~from =
    database t
    >>= fun db ->
    Deferred_list.while_sequential from ~f:(fun collection ->
        Database.get_all db ~collection
        >>= fun ids ->
        return (collection, ids))
    >>= fun col_ids ->
    Log.(s "Getting : "
         % separate (s " + ")
           (List.map col_ids ~f:(fun (col, ids) ->
                i (List.length ids) % sp % parens (quote col)
              ))
         % s " stored targets" @verbose);
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
    Log.(s "Getting : "
         % separate (s " + ")
           (List.map col_ids ~f:(fun (col, ids) ->
                i (List.length ids) % sp % parens (quote col)
              ))
         % s " targets" @verbose);
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

  (* Alive should mean In-progess or activable *)
  let alive_targets t =
    get_collections_of_targets t ~from:[passive_targets_collection; active_targets_collection]
    >>= fun targets ->
    let filtered =
      List.filter_map targets ~f:(fun target ->
          match Target.State.simplify (Target.state target) with
          | `Failed
          | `Successful -> None
          | `In_progress
          | `Activable -> Some target)
    in
    return filtered

  let all_targets t =
    get_collections_of_targets t ~from:[
      passive_targets_collection;
      active_targets_collection;
      finished_targets_collection;
    ]

  let all_stored_targets t =
    get_collections_of_stored_targets t ~from:[
      passive_targets_collection;
      active_targets_collection;
      finished_targets_collection;
    ]
    

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
      Log.(s "Going to actually kill: "
           % OCaml.list (sf "{%S}") to_kill_list @ verbose);
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
      Log.(s "Storing " % i (List.length t_list) % s " to be added at next step"
           @ verbose);
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
            fail (`Missing_data (fmt "target to add: %s" key))
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
        alive_targets t
        >>= fun current_living_targets ->
        (* current targets are alive, so activable or in_progress *)
        let stuff_to_actually_add =
          List.fold ~init:[] tlist ~f:begin fun to_store_targets target ->
            let equivalences =
              let we_kept_so_far =
                List.filter_map to_store_targets
                  ~f:(fun st ->
                      match Target.Stored_target.get_target st with
                      | `Target t -> Some t
                      | `Pointer _ -> None) in
              List.filter (current_living_targets @ we_kept_so_far)
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
              (Target.Stored_target.make_pointer
                 ~from:target ~pointing_to:at_least_one :: to_store_targets)
          end
        in
        Log.(s "Adding new " % i (List.length stuff_to_actually_add)
             % s " things to the DB" @ verbose);
        let action =
          let open Database_action in
          List.map tlist ~f:(fun trgt ->
              let key = Target.id trgt in
              unset ~collection:targets_to_add_collection key)
          @ List.map stuff_to_actually_add ~f:(fun st ->
              let key = Target.Stored_target.id st in
              let collection =
                match Target.Stored_target.get_target st with
                | `Pointer _ -> finished_targets_collection
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


  end

end

module Cache_table = struct

  type 'a t = (string, 'a) Hashtbl.t

  let create () = Hashtbl.create 42

  let add_or_replace t id st =
    Hashtbl.replace t id st;
    return ()

  let get t id =
    try return (Hashtbl.find t id)
    with e ->
      fail (`Missing_data (fmt "cache_table: %s" id))

  let filter_map t ~f =
    let red _ elt prev =
      match f elt with
      | Some x -> x :: prev
      | None -> prev in
    return (Hashtbl.fold red t []) 

  let fold t ~init ~f =
    Hashtbl.fold (fun id elt previous -> f ~previous ~id elt) t init

  let reset t = Hashtbl.reset t; return ()

end

type t = {
  db: With_database.t;
  cache: Target.Stored_target.t Cache_table.t
}

let create ~database_parameters =
  With_database.create ~database_parameters
  >>= fun db ->
  With_database.all_stored_targets db
  >>= fun all ->
  let cache = Cache_table.create () in
  List.fold ~init:(return ()) all  ~f:(fun prev_m st ->
      prev_m >>= fun () ->
      let id = Target.Stored_target.id st in
      Cache_table.add_or_replace cache id st)
  >>= fun () ->
  return {db; cache}

let unload t =
  With_database.unload t.db
  >>= fun () ->
  Cache_table.reset t.cache

let get_target t id =
  let rec get_following_pointers ~key ~count =
    Cache_table.get t.cache key
    >>= fun stored ->
    begin match Target.Stored_target.get_target stored with
    | `Pointer _ when count >= 30 ->
      fail (`Missing_data (fmt "there must be a loop or something (from %s)" id))
    | `Pointer key ->
      get_following_pointers ~count:(count + 1) ~key
    | `Target t -> return t
    end
  in
  get_following_pointers ~key:id ~count:0


let all_targets t =
  Cache_table.filter_map t.cache
    ~f:(fun trgt ->
        match Target.Stored_target.get_target trgt with
        | `Pointer _ -> None
        | `Target t -> Some t)

let activate_target t ~target ~reason =
  With_database.activate_target t.db ~target ~reason
  >>= fun new_one ->
  let stored = Target.Stored_target.of_target new_one in
  Cache_table.add_or_replace t.cache (Target.Stored_target.id stored) stored

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
      
let move_target_to_finished_collection t ~target =
  With_database.move_target_to_finished_collection t.db ~target

let update_target t trgt =
  With_database.update_target t.db trgt
  >>= fun () ->
  Cache_table.add_or_replace t.cache
    (Target.id trgt) (Target.Stored_target.of_target trgt)

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
    return (res <> [])

end

module Adding_targets = struct

  let register_targets_to_add t ts =
    With_database.Adding_targets.register_targets_to_add t.db ts
  let check_and_really_add_targets t =
    With_database.Adding_targets.check_and_really_add_targets t.db
    >>= fun res ->
    List.fold ~init:(return ()) res ~f:(fun unit_m st ->
        unit_m >>= fun () ->
        Cache_table.add_or_replace t.cache (Target.Stored_target.id st) st
      )
    >>= fun () ->
    return (res <> [])

end
