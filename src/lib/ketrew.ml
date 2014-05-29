
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
type dependency
type job = <
  kind: string;
>
type process = [
  | `Get_command_output of string
  | `Run_job of job
]
type target_value = [ `String of string | `Number of float ]

module Data = struct

  type 'a pointer = { id: Unique_id.t }
  let pointer id = {id}

  type value_type = [`String | `Number]
end
module Process = struct

  type t = [ `Nop ]
  let nop = `Nop
end
module Target = struct
  type t = [
    | `Tree of File_tree.t
    | `File of File_tree.file
    | `Value of Data.value_type
  ]
  let value vt : t = `Value vt
  let string_value : t = `Value `String
end

module Task = struct

  type running_state = {
    identification: string; (* job id ? *)
    stdout: string option;
    stderr: string option;
  }
  type workflow_state = [
    | `Submitted
    | `Started of running_state
    | `Dead of running_state
    | `Successful of target_value
  ]
  type t = {
    id: Unique_id.t;
    name: string;
    persistance: [ `Input_data | `Recomputable of float | `Result ];
    metadata: Metadata.t;
    dependencies: t Data.pointer list;
    make: Process.t;
    target: Target.t;
    history: workflow_state list;
  }
  let create
      ?name ?(persistance=`Input_data) ?(metadata=Metadata.empty)
      ?(dependencies=[]) ?(make= Process.nop) ~target
      ?(history=[])
      id = 
    { id; name = Option.value name ~default:id; persistance; metadata;
      dependencies; make; target; history }

  let pointer t : t Data.pointer = Data.pointer t.id
  let serialize t = Marshal.to_string t []
  let deserialize s : (t, _) Result.t =
    let open Result in
    try return (Marshal.from_string s 0)
    with e -> fail (`Task (`Deserilization (Printexc.to_string e)))

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
    Pvem_lwt_unix.IO.read_file parameters
    >>= fun content ->
    begin try return (Marshal.from_string content 0 : t) with
    | e -> fail (`Database (`Load, parameters))
    end
  let save t =
    let content = Marshal.to_string t [] in
    Pvem_lwt_unix.IO.write_file t.parameters ~content

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

module Engine = struct
  type t = {
    current_tasks: Unique_id.t list;
    (* keep db id of a list of all "archived" tasks *)
  }
  let create () = {current_tasks = [];}

  let serialize t = Marshal.to_string t []
  let unserialize s =
    try return (Marshal.from_string s 0 : t)
    with e -> fail (`Engine (`Deserilization (Printexc.to_string e)))
  let add t task = { current_tasks = task.Task.id :: t.current_tasks }
  let current_tasks t = t.current_tasks
end

module Configuration = struct
  type t = {
    database_parameters: string;
    engine_key: string;
  }
  let default_engine_key = "ketrew_engine"
  let create 
      ?(engine_key=default_engine_key) ~database_parameters () =
    { database_parameters; engine_key }
end

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
      begin Pvem_lwt_unix.System.file_info ~follow_symlink:true path
        >>= function
        | `Regular_file _ ->
          Log.(s "Loading database at " % s path @ very_verbose);
          Database.load path
        | _ -> 
          Log.(s "Creating new database at " % s path @ very_verbose);
          let db = Database.create path in
          Database.save db (* should create + save be in `Database`? *)
          >>= fun () ->
          return db
      end

  let get_engine t =
    database t >>= fun db ->
    begin Database.get db ~key:t.configuration.Configuration.engine_key
      >>= function
      | Some engine_serialized ->
        Engine.unserialize engine_serialized
      | None ->
        let e = Engine.create () in
        return e
    end

  let save_engine t engine =
    database t >>= fun db ->
    let key = t.configuration.Configuration.engine_key in
    let action = Database.(set ~key (Engine.serialize engine)) in
    begin Database.act db ~action
      >>= function
      | `Done -> return ()
      | `Not_done -> fail (`State (`Database_unavailable key))
    end

  let add_task t task_fun =
    let id = Unique_id.create () in
    let actual_task = task_fun id in
    database t
    >>= fun db ->
    begin Database.(act db (set id Task.(serialize actual_task)))
      >>= function
      | `Done -> return ()
      | `Not_done ->
        (* TODO: loop instead of error *) fail (`State (`Database_unavailable id))
    end
    >>= fun () ->
    get_engine t
    >>= fun engine ->
    let new_engine = Engine.add engine actual_task in
    save_engine t new_engine (* TODO: remove task if this fails *)

  let current_tasks t =
    database t >>= fun db ->
    get_engine t >>= fun engine ->
    let pointers = Engine.current_tasks engine in
    Pvem_lwt_unix.Deferred_list.for_concurrent pointers ~f:(fun task_id ->
        Database.get db task_id
        >>= function
        | Some t -> of_result (Task.deserialize t)
        | None ->
          fail (`State (`Database_unavailable (fmt "Task missing: %s" task_id)))
      )
    >>= fun (tasks, errors) ->
    begin match errors with
    | [] -> return tasks
    | some :: more -> fail some (* TODO do not forget other errors *)
    end


end
