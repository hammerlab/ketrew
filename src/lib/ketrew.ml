
open Ketrew_pervasives


module File_tree = struct

  type file
  type t

end

type value
type volume
type metadata
type dependency
type job = <
  kind: string;
>
type process = [
  | `Get_command_output of string
  | `Run_job of job
]
type value_type = [`String | `Number]
type target_value = [ `String of string | `Number of float ]

type target = [
  | `Tree of File_tree.t
  | `File of File_tree.file
  | `Value of value_type
]
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
type workflow = {
  id: Unique_id.t;
  name: string;
  persistance: [ `Input_data | `Recomputable of float | `Result ];
  metadata: metadata;
  dependencies: dependency list;
  make: process;
  target: target;
  mutable history: workflow_state list;
}

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
    current_workflows: Unique_id.t list;
  }
  let create () = {current_workflows = [];}

  let serialize t = Marshal.to_string t []
  let unserialize s =
    try return (Marshal.from_string s 0 : t)
    with e -> fail (`Engine (`Deserilization (Printexc.to_string e)))
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


end
