
open Ketrew_pervasives


module Path = struct

  type abs
  type rel
  type 'a t = string
  type relative = rel t
  type absolute = abs t

  let root : absolute = "/"
  let absolute_exn s : absolute t =
    if Filename.is_relative s
    then invalid_argument_exn ~where:"Path" "absolute_exn"
    else s
  let relative_exn s : relative t =
    if not (Filename.is_relative s)
    then invalid_argument_exn ~where:"Path" "relative_exn"
    else s
  let concat: 'a t -> relative -> 'a t = Filename.concat 

  let to_string: 'a t -> string = fun x -> x

end

module Host = struct

  module Ssh = struct

    let _configuration_ssh_batch_option = ref ""

    let configure_ssh_batch_option spec =
      let op =
        match spec with
        | `Openssh -> "-oBatchMode=yes"
        | `Dropbear -> "-s"
        | `Custom s -> s
      in
      _configuration_ssh_batch_option := op

    let () = configure_ssh_batch_option `Openssh

    type t = {
      address: string;
      port: int option;
      user: string option;
    }

    (** Generate a proper SSH command for the given host. *)
    let do_ssh ssh command =
      ["ssh"; !_configuration_ssh_batch_option]
      @ (match ssh.port with
        | Some p -> ["-p"; "port"]
        | None -> [])
      @ (match ssh.user with
        | None -> [ssh.address]
        | Some u -> [fmt "%s@%s" u ssh.address])
      @ [command]

  end
  type connection = [
    | `Localhost
    | `Ssh of Ssh.t
  ]
  type t = {
    name: string;
    connection: connection;
    playground: string option;
  }
  let create ?(connection=`Localhost) ?playground name =
    {name; connection; playground}
  let localhost = create "localhost"

  let ssh ?playground ?port ?user ?name address =
    create ?playground Option.(value name ~default:address)
      ~connection:(`Ssh {Ssh. address; port; user})

  let to_string t = t.name

  let fail_exec t ?(out="") ?(err="") msg =
    fail (`Host (`Execution (to_string t, out, err, msg)))

  let get_shell_command_output t cmd =
    match t.connection with
    | `Localhost ->
      begin System.Shell.execute cmd
        >>< function
        | `Ok (out, err, `Exited 0) -> return (out, err)
        | `Ok (out, err, other) -> 
          fail_exec t ~out ~err (System.Shell.status_to_string other)
        | `Error (`Shell _ as e) ->
          fail_exec t (System.error_to_string e)
      end
    | `Ssh ssh ->
      let ssh_cmd = Ssh.(do_ssh ssh cmd) in
      begin Ketrew_unix_process.succeed ssh_cmd
        >>< function
        | `Ok (out, err) -> return (out, err)
        | `Error (`Process _ as process_error) ->
          let msg = Ketrew_unix_process.error_to_string process_error in
          Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_cmd 
               % s " failed: " %s msg @ verbose);
          fail_exec t msg
      end

  let do_files_exist t paths =
    let cmd =
      List.map paths ~f:(fmt "[ -f %S ]") |> String.concat ~sep:" && " in
    match t.connection with
    | `Localhost ->
      begin System.Shell.execute cmd
        >>< function
        | `Ok (_, _, `Exited 0) -> return true
        | `Ok (_, _, `Exited 1) -> return false
        | `Ok (out, err, other) -> 
          fail_exec t ~out ~err (System.Shell.status_to_string other)
        | `Error (`Shell _ as e) -> fail_exec t (System.error_to_string e)
      end
    | `Ssh ssh ->
      let ssh_cmd = Ssh.(do_ssh ssh cmd) in
      begin Ketrew_unix_process.exec ssh_cmd
        >>< function
        | `Ok (_, _, `Exited 0) -> return true
        | `Ok (_, _, `Exited 1) -> return false
        | `Ok (out, err, other) -> 
          fail_exec t ~out ~err (System.Shell.status_to_string other)
        | `Error (`Process _ as process_error) ->
          let msg = Ketrew_unix_process.error_to_string process_error in
          Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_cmd 
               % s " failed: " %s msg @ verbose);
          fail_exec t msg
      end
end

module Command = struct

  type t = {
    host: Host.t;
    action: [ `Shell of string ];
  }
  let shell ?(host=Host.localhost) s = { host; action = `Shell s}

  let to_string {host; action = `Shell cmd} =
    fmt "Shell[%S] on %s" cmd (Host.to_string host)

  let get_output {host; action} =
    match action with
    | `Shell cmd ->
      Host.get_shell_command_output host cmd
  let run t =
    get_output t (* TODO optimize to not record the output *)
    >>= fun (_, _) ->
    return ()

end

module Volume = struct

  type structure =   
    | File of string
    | Directory of string * structure list

  type t = {
    host: Host.t;
    root: Path.absolute;
    structure: structure;
  }
  let create ~host ~root structure = {host; root; structure}
  let file s = File s
  let dir name contents = Directory (name, contents)

  let rec all_structure_paths s =
    match s with
    | File s -> [Path.relative_exn s]
    | Directory (name, children) ->
      let children_paths = 
        List.concat_map ~f:all_structure_paths children in
      List.map ~f:(Path.concat name) children_paths

  let all_paths t =
    List.map ~f:(Path.concat t.root) (all_structure_paths t.structure)

  let exists t = (* for now, like git, we check only files, not directories *)
    let paths = all_paths t in
    Host.do_files_exist t.host paths

  let to_string {host; root; structure} =
    fmt "Vol(%s:%s)" (Host.to_string host) (Path.to_string root)
end

module Data = struct

  type 'a pointer = { id: Unique_id.t }
  let pointer id = {id}

end
module Artifact_type = struct

  type value_type = [`Unit | `String | `Number]
  let value_type_to_string = function
  | `Unit -> "Unit"
  | `String -> "String"
  | `Number -> "Number"

  type t = [
    (* | `Fresh_file *)
    | `Value of value_type
    | `Volume of Volume.t
  ]
  let value vt : t = `Value vt
  let string_value : t = `Value `String
  let volume v = `Volume v

  let to_string = function
  | `Value v -> fmt "Value %s" (value_type_to_string v)
  | `Volume v -> fmt "Volume %s" (Volume.to_string v)

end
module Artifact = struct

  type value = [ `Unit | `String of string | `Number of float ]
  
  let unit : value = `Unit

  type t = [
    (* | `Tree of File_tree.t *)
    (* | `File of File_tree.file *)
    | `Value of value
    | `Volume of Volume.t
  ]


  (* TODO those two functions should more type-safe *)
  let is_ready specification =
    match specification with
    | `Value _ -> return false
    | `Volume v -> Volume.exists v

  let of_type: Artifact_type.t -> t = function
  | `Value v -> invalid_argument_exn ~where:"Artifact" "specification_to_value"
  | `Volume v -> `Volume v

end

module Process = struct

  type t = [
    | `Artifact of Artifact.t
    | `Get_output of Command.t
    | `Direct_command of Command.t
  ]
  let nop = `Artifact (`Value `Unit)
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
    | `Successful of Time.t * [activated_state | running_state ] * Artifact.t
  ]
  type workflow_state = [ submitted_state | activated_state | running_state | finished_state]
  type t = {
    id: Unique_id.t;
    name: string;
    persistance: [ `Input_data | `Recomputable of float | `Result ];
    metadata: Artifact.value;
    dependencies: t Data.pointer list;
    make: Process.t;
    result_type: Artifact_type.t;
    history: workflow_state;
  }
  let create
      ?name ?(persistance=`Input_data) ?(metadata=Artifact.unit)
      ?(dependencies=[]) ?(make= Process.nop)
      result_type = 
    let history = `Created Time.(now ()) in
    let id = Unique_id.create () in
    { id; name = Option.value name ~default:id; persistance; metadata;
      dependencies; make; result_type; history }

  (** Create a new  target but activated from a created one; 
    raises [Invalid_argument _] if current status is not [`Created _]. *)
  let activate_exn t ~by = 
    match t.history with 
    | `Created _ as c ->
      { t with history = `Activated (Time.now (), c, by) }
    | _ -> raise (Invalid_argument "activate_exn")

  let make_succeed_exn t artifact =
    match t.history with
    | `Activated _ | `Running _ as state -> 
      { t with history = `Successful (Time.now (), state, artifact) }
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
      artifact = 
    activate_exn ~by:`User (create ?name ?persistance ?metadata
                    ?dependencies ?make artifact)

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

  let not_implemented msg = 
    Log.(s "Going through not implemented stuff: " % s msg @ verbose);
    fail (`Not_implemented msg)

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
      | `Not_done -> fail (`Database_unavailable key)
    end

  let add_or_update_target t target =
    database t
    >>= fun db ->
    begin Database.(act db (set target.Target.id Target.(serialize target)))
      >>= function
      | `Done -> return ()
      | `Not_done ->
        (* TODO: try again a few times instead of error *)
        fail (`Database_unavailable target.Target.id)
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
      fail (`Missing_data p.Data.id)


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
    begin
      current_targets t >>= fun targets ->
      database t >>= fun db ->
      Deferred_list.while_sequential targets ~f:(fun target ->
          match target.Target.history with
          | `Created _ ->
            Log.(s "Target " % Target.log target 
                 % s " is inactive" @ very_verbose);
            (* nothing to do *) return ()
          | `Activated _ ->
            begin Artifact.is_ready target.Target.result_type
              >>= function
              | false ->
                Deferred_list.while_sequential target.Target.dependencies
                  ~f:begin fun dep ->
                    follow_pointer db dep Target.deserialize
                    >>= fun dependency ->
                    match target.Target.history with
                    | `Created _  ->
                      let newdep =
                        Target.(activate_exn target ~by:`Dependency) in
                      add_or_update_target t newdep
                      >>= fun () ->
                      return `Wait
                    | `Activated _ | `Running _ -> return `Wait
                    | `Dead _ -> return (`Die dep)
                    | `Successful _ -> return `Go
                  end
                >>= fun statuses ->
                begin match statuses with
                | some_list when List.for_all some_list ~f:((=) `Go) ->
                  begin match target.Target.make with
                  | `Artifact a ->
                    add_or_update_target t Target.(make_succeed_exn target a)
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
                  | `Direct_command cmd ->
                    begin Command.run cmd
                      >>< function
                      | `Ok () -> 
                        begin Artifact.is_ready target.Target.result_type
                          >>= function
                          | false ->
                            add_or_update_target t Target.(
                                make_fail_exn target  
                                  ~msg:(fmt "command %S did not create %S" 
                                          (Command.to_string cmd)
                                          (Artifact_type.to_string target.Target.result_type)))
                          | true ->
                            (* result_type must be a Volume: *)
                            let v = Artifact.of_type target.Target.result_type in
                            add_or_update_target t Target.(make_succeed_exn target v)
                        end
                      | `Error (`Host (`Execution (where, out, err, msg))) ->
                        Log.(s "Cmd error: " % s err @ very_verbose);
                        add_or_update_target t Target.(
                            make_fail_exn target  
                              ~msg:(fmt "On %S, out: %S, err: %S, msg: %S" 
                                      where out err msg))
                    end
                  end
                | some_dependency_died
                  when List.exists some_dependency_died
                      ~f:(function `Die _ -> true | _ -> false) ->
                  let msg =
                    List.filter_map some_dependency_died
                      ~f:(function `Die d -> Some d.Data.id | _ -> None)
                    |> String.concat ~sep:", "
                  in
                  add_or_update_target t Target.(
                      make_fail_exn target  ~msg:(fmt "Dependencies died: %s" msg))
                | no_death_not_all_go -> 
                  (* there must be some `Wait :) *)
                  return ()
                end
              | true ->
                (* result_type must be a Volume: *)
                let v = Artifact.of_type target.Target.result_type in
                add_or_update_target t Target.(make_succeed_exn target v)
            end
          (* start or run *)
          | `Running _ -> (* check/update status *) return ()
          | `Dead _ | `Successful _ -> return ())
      >>= fun (_ : unit list) ->
      return ()
    end 

  let get_status t pointer =
    database t >>= fun db ->
    follow_pointer db pointer Target.deserialize
    >>= fun target ->
    return target.Target.history 

end

module Error = struct

  let to_string = function
  | `IO _ as io -> IO.error_to_string io
  | `System _ as s -> System.error_to_string s
  | `Database (`Load, path) -> fmt "DB-load: %S" path
  | `Host (`Execution (one, two, three, four)) ->
    fmt "Host-exec(%s, %s, %s, %s)" one two three four
  | `Persistent_state (`Deserilization s) ->
    fmt "Persistent_state-Deserilization: %S" s
  | `Target (`Deserilization s) -> fmt "target-deserialization: %s" s
  | `Database_unavailable s -> fmt "DB %s" s
  | `Not_implemented s -> fmt "Not-impl %S" s
  | `Missing_data p -> fmt "pointer: %s" p

end
