
open Ketrew_pervasives


module File_tree = struct

  type file
  type t

end

type value
type volume

module Metadata = struct
  type t = string
  let empty = ""
end

module Host = struct

  type t = [ `Localhost ]
  let localhost : t = `Localhost

  let to_string = function `Localhost -> "localhost"

  let fail_exec t ?(out="") ?(err="") msg =
    fail (`Host (`Execution (to_string t, out, err, msg)))

  let get_shell_command_output t cmd =
    match t with
    | `Localhost ->
      begin System.Shell.execute cmd
        >>< function
        | `Ok (out, err, `Exited 0) -> return (out, err)
        | `Ok (out, err, other) -> 
          fail_exec t ~out ~err (System.Shell.status_to_string other)
        | `Error (`Shell _ as e) ->
          fail_exec t (System.error_to_string e)
      end
end


module Command = struct

  type t = {
    host: Host.t;
    action: [ `Shell of string ];
  }
  let shell ?(host=Host.localhost) s = { host; action = `Shell s}

  let get_output {host; action} =
    match action with
    | `Shell cmd ->
      Host.get_shell_command_output host cmd

end
module Data = struct

  type 'a pointer = { id: Unique_id.t }
  let pointer id = {id}

  type value_type = [`String | `Number]
  type value = [ `String of string | `Number of float ]

end
module Process = struct

  type t = [
    | `Nop
    | `Get_output of Command.t
  ]
  let nop = `Nop
end

module Artefact = struct
  type specification = [
    | `Tree of File_tree.t
    | `File of File_tree.file
    | `Fresh_file
    | `Value of Data.value_type
  ]
  let value vt : specification = `Value vt
  let string_value : specification = `Value `String
  type t = [
    | `Tree of File_tree.t
    | `File of File_tree.file
    | `Value of Data.value
  ]
end

module Target = struct

  type submitted_state = [`Created of Time.t]
  type activated_state =
    [`Activated of Time.t * submitted_state * [ `User | `Dependency ] ]
  type run_history = (Time.t * string) list (* complexify *)
  type running_state = [ `Running of run_history * activated_state ]
  type death_reason = [`Killed of string | `Failed of string]
  type finished_state = [ 
    | `Dead of Time.t * [activated_state | running_state] * death_reason
    | `Successful of Time.t * [activated_state | running_state ] * Artefact.t
  ]
  type workflow_state = [ submitted_state | activated_state | running_state | finished_state]
  type t = {
    id: Unique_id.t;
    name: string;
    persistance: [ `Input_data | `Recomputable of float | `Result ];
    metadata: Metadata.t;
    dependencies: t Data.pointer list;
    make: Process.t;
    artefact: Artefact.specification;
    history: workflow_state;
  }
  let create
      ?name ?(persistance=`Input_data) ?(metadata=Metadata.empty)
      ?(dependencies=[]) ?(make= Process.nop)
      artefact = 
    let history = `Created Time.(now ()) in
    let id = Unique_id.create () in
    { id; name = Option.value name ~default:id; persistance; metadata;
      dependencies; make; artefact; history }

  (** Create a new  target but activated from a created one; 
    raises [Invalid_argument _] if current status is not [`Created _]. *)
  let activate_exn t ~by = 
    match t.history with 
    | `Created _ as c ->
      { t with history = `Activated (Time.now (), c, by) }
    | _ -> raise (Invalid_argument "activate_exn")

  let make_succeed_exn t artefact =
    match t.history with
    | `Activated _ | `Running _ as state -> 
      { t with history = `Successful (Time.now (), state, artefact) }
    | _ -> raise (Invalid_argument "make_succeed_exn")

  let kill_exn ?(msg="") t =
    match t.history with
    | `Activated _ | `Running _ as state -> 
      { t with history = `Dead (Time.now (), state, `Killed msg) }
    | _ -> raise (Invalid_argument "kill_exn")

  let make_fail_exn ?(msg="") t =
    match t.history with
    | `Activated _ | `Running _ as state -> 
      { t with history = `Dead (Time.now (), state, `Failed msg) }
    | _ -> raise (Invalid_argument "kill_exn")

  let active 
      ?name ?persistance ?metadata
      ?dependencies ?make
      artefact = 
    activate_exn ~by:`User (create ?name ?persistance ?metadata
                    ?dependencies ?make artefact)

  let pointer t : t Data.pointer = Data.pointer t.id
  let serialize t = Marshal.to_string t []
  let deserialize s : (t, _) Result.t =
    let open Result in
    try return (Marshal.from_string s 0)
    with e -> fail (`Target (`Deserilization (Printexc.to_string e)))

  let log t = Log.(brakets (sf "Target: %s (%s)" t.name t.id))

end

module Database = struct

  type stupid_db = (string * string) list
  type action =
    | Set of string * string
    | Sequence of action list
    | Check of string * string option
  let set ~key value = Set (key, value)
  let seq l = Sequence l
  let contains ~key v = Check (key, Some v) 
  let is_not_set key = Check (key, None)

  type t = {
    mutable db: stupid_db;
    (* mutable history: (action * stupid_db) list; *)
    parameters: string;
  }
  let create parameters = {db = []; parameters} 

  let load parameters =
    IO.read_file parameters
    >>= fun content ->
    begin try return (Marshal.from_string content 0 : t) with
    | e -> fail (`Database (`Load, parameters))
    end
  let save t =
    let content = Marshal.to_string t [] in
    IO.write_file t.parameters ~content

  let get t ~key =
    List.find_map t.db ~f:(fun (k, v) -> if k = key then Some v else None)
    |> return
  let act t ~action =
    let current = ref t.db in
    let rec go = 
      function
      | Set (key, value) ->
        current := (key, value) :: !current;
        true
      | Check (key, value) ->
        begin match List.find !current (fun (k, v) -> key = k) with
        | Some (k, v) when Some v = value -> true
        | None when value = None -> true
        | _ -> false
        end
      | Sequence actions ->
        List.for_all actions go
    in
    if go action 
    then begin
      t.db <- !current;
      save t >>= fun () ->
      return `Done
    end
    else return `Not_done

end

module Persistent_state = struct
  type t = {
    current_targets: Target.t Data.pointer list;
    (* keep db id of a list of all "archived" targets *)
  }
  let create () = {current_targets = [];}

  let serialize t = Marshal.to_string t []
  let unserialize s =
    try return (Marshal.from_string s 0 : t)
    with e -> fail (`Persistent_state (`Deserilization (Printexc.to_string e)))

  let add t target = { current_targets = Target.pointer target :: t.current_targets }

  let current_targets t = t.current_targets
end

module Configuration = struct
  type t = {
    database_parameters: string;
    persistent_state_key: string;
  }
  let default_persistent_state_key = "ketrew_persistent_state"
  let create 
      ?(persistent_state_key=default_persistent_state_key) ~database_parameters () =
    { database_parameters; persistent_state_key }
end

(** The “application” state *)
module State = struct
  type t = {
    mutable database_handle: Database.t option;
    configuration: Configuration.t;
  }
  let create configuration =
    return {database_handle = None; configuration}

  let database t =
    match t.database_handle with
    | Some db -> return db
    | None -> 
      let path = t.configuration.Configuration.database_parameters in
      begin System.file_info ~follow_symlink:true path
        >>= function
        | `Regular_file _ ->
          Log.(s "Loading database at " % s path @ very_verbose);
          Database.load path
          >>= fun db ->
          t.database_handle <- Some db;
          return db
        | _ -> 
          Log.(s "Creating new database at " % s path @ very_verbose);
          let db = Database.create path in
          Database.save db (* should create + save be in `Database`? *)
          >>= fun () ->
          t.database_handle <- Some db;
          return db
      end

  let get_persistent t =
    database t >>= fun db ->
    begin Database.get db ~key:t.configuration.Configuration.persistent_state_key
      >>= function
      | Some persistent_serialized ->
        Persistent_state.unserialize persistent_serialized
      | None ->
        let e = Persistent_state.create () in
        return e
    end

  let save_persistent t persistent =
    database t >>= fun db ->
    let key = t.configuration.Configuration.persistent_state_key in
    let action = Database.(set ~key (Persistent_state.serialize persistent)) in
    begin Database.act db ~action
      >>= function
      | `Done -> return ()
      | `Not_done -> fail (`State (`Database_unavailable key))
    end

  let add_or_update_target t target =
    database t
    >>= fun db ->
    begin Database.(act db (set target.Target.id Target.(serialize target)))
      >>= function
      | `Done -> return ()
      | `Not_done ->
        (* TODO: try again a few times instead of error *)
        fail (`State (`Database_unavailable target.Target.id))
    end

  let add_target t target =
    add_or_update_target t target
    >>= fun () ->
    get_persistent t
    >>= fun persistent ->
    let new_persistent = Persistent_state.add persistent target in
    save_persistent t new_persistent
      (* TODO: remove target if this fails, or put in same transaction *)

  let follow_pointer db (p : 'a Data.pointer) (f: string -> ('a, _) Result.t) =
    Database.get db p.Data.id
    >>= function
    | Some t -> of_result (f t)
    | None ->
      fail (`State (`Missing_data p.Data.id))


  let current_targets t =
    database t >>= fun db ->
    get_persistent t >>= fun persistent ->
    let pointers = Persistent_state.current_targets persistent in
    Deferred_list.for_concurrent pointers ~f:(fun target_pointer ->
        follow_pointer db target_pointer Target.deserialize)
    >>= fun (targets, errors) ->
    begin match errors with
    | [] -> return targets
    | some :: more -> fail some (* TODO do not forget other errors *)
    end

  let step t = (* TODO return a log of what happened *)
    current_targets t
    >>= fun targets ->
    Deferred_list.while_sequential targets ~f:(fun target ->
        match target.Target.history with
        | `Created _ ->
          Log.(s "Target " % Target.log target 
               % s " is inactive" @ very_verbose);
          (* nothing to do *) return ()
        | `Activated _ ->
          (* TODO check deps *)
          begin match target.Target.make with
          | `Nop ->
            (* TODO if "result" is there succeed, if not fail *)
            return ()
          | `Get_output cmd ->
            begin Command.get_output cmd
              >>< function
              | `Ok (out, _) ->
                Log.(s "Cmd output: " % s out @ very_verbose);
                let new_target =
                  Target.make_succeed_exn target (`Value (`String out)) in
                add_or_update_target t new_target
              | `Error (`Host (`Execution (where, out, err, msg))) ->
                Log.(s "Cmd error: " % s err @ very_verbose);
                add_or_update_target t Target.(
                    make_fail_exn target  
                      ~msg:(fmt "On %S, out: %S, err: %S, msg: %S" where out err msg))
            end
          end
          (* start or run *)
        | `Running _ -> (* check/update status *) return ()
        | `Dead _ | `Successful _ -> return ())
    >>= fun (_ : unit list) ->
    return ()




end
