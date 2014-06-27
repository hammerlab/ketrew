open Ketrew_pervasives


module Host = Ketrew_host
module Path = Ketrew_path
module Target = Ketrew_target
module Artifact = Ketrew_artifact

 
type host = Ketrew_host.t

class type user_artifact = object

  method path : string
  method exists: Target.Condition.t
  (** Return the path of the artifact if the artifact is a volume containing
      a single file or directory. *)

end

let unit = object
  method path = failwith "Unit has no path"
  method exists = `False
end

let file ?(host= Host.tmp_on_localhost) path  =
  let basename = Filename.basename path in
  object
    val vol =
      Artifact.Volume.(
        create ~host
          ~root:(Path.absolute_directory_exn (Filename.dirname path))
          (file basename))
    method path = path
    method exists = `Volume_exists vol
  end

class type user_target =
  object
    method activate : unit
    method name : string
    method is_active: bool
    method id: Unique_id.t
    method render: Ketrew_target.t
    method dependencies: user_target list
    method metadata: Ketrew_artifact.Value.t
  end


let user_target_internal
  ?(active = false)
  ?(dependencies = [])
  ?(name: string option)
  ?(make: Target.build_process = Target.nop)
  ?(ready_when = `False)
  ?(metadata = Artifact.Value.unit)
  ()
  =
  let id = Unique_id.create () in
  object (self)
    val mutable active = active
    method name = 
      match name with
      | None -> id
      | Some s -> s
    method id = id
    method dependencies = dependencies
    method activate = active <- true
    method is_active = active
    method metadata = metadata
    method render =
      let creation = if active then Target.active else Target.create in
      creation ~metadata
        ~id:self#id
        ~dependencies:(List.map dependencies ~f:(fun t -> t#id))
        ~name:self#name ~condition:ready_when
        ~make ()
        
  end

let target ?active ?dependencies ?make ?ready_when ?metadata name =
  user_target_internal ?active ?dependencies ~name ?make ?metadata ?ready_when ()
let active  ?dependencies ?make ?ready_when ?metadata name =
  user_target_internal ~active:true
    ?dependencies ?metadata ~name ?make ?ready_when ()

(*
  Run a workflow:

   - make sure the run target is active,
   - get the IDs of all the depdendencies,
*)
let user_command_list t =
  t#activate;
  let rec go_through_deps t =
    t#render :: List.concat_map t#dependencies ~f:go_through_deps in
  let targets =
    go_through_deps t
    |> List.dedup ~compare:Target.(fun ta tb -> compare ta.id tb.id)
  in
  match targets with
  | first :: more -> [`Make (first, more)]
  | [] -> assert false (* there is at least the argument one *)


let run ?plugins ?override_configuration t =
  let todo_list = user_command_list t in
  match Lwt_main.run (
    Ketrew_configuration.(
      get_configuration ?override_configuration default_configuration_path)
    >>= fun configuration ->
    Ketrew_state.with_state ?plugins ~configuration (fun ~state ->
        Ketrew_user_command.run_list ~state todo_list)
  ) with
  | `Ok () -> ()
  | `Error e ->
    Log.(s "Run-error: " % s (Ketrew_error.to_string e) @ error);
    failwith (Ketrew_error.to_string e)

module Program = struct

  type t = Ketrew_program.t
  let (&&) a b = `And [a; b]
  let sh c = `Shell_command c
  let shf fmt = Printf.ksprintf sh fmt
  let exec l = `Exec l
  let chain l = `And l

  let copy_files ~source:(s_host, src) ~destination:(d_host, dest) 
      ~(f : ?host:Host.t -> t -> 'a) =
    let open Ketrew_gen_base_v0_t in
    match s_host.connection, d_host.connection with
    | `Localhost, `Localhost ->
      f ?host:None (exec ("cp" :: src @ [dest])) 
    | `Ssh ssh, `Localhost -> 
      f ?host:None (exec (Host.Ssh.scp_pull ssh ~src ~dest))
    | `Localhost, `Ssh ssh -> 
      f ?host:None (exec (Host.Ssh.scp_push ssh ~src ~dest))
    | `Ssh _, `Ssh ssh -> 
      f ~host:s_host (exec (Host.Ssh.scp_push ssh ~src ~dest))

end

let parse_host: string -> Host.t = Host.of_string

let host_cmdliner_term 
    ?(doc="URI of the host (e.g. \
           ssh://user@example.com:42/tmp/ketrewplayground).") how =
  let open Cmdliner in
  Term.(
    pure (fun s -> parse_host s)
    $ begin match how with
    | `Flag (flags) ->
      Arg.(value & opt string "/tmp/" & info flags ~doc ~docv:"URI")
    | `Required p ->
      Arg.(required & pos p (some string) None & info [] ~doc ~docv:"URI")
    end
  )

let daemonize  = Ketrew_daemonize.create

let direct_execution ?host cmd =
  `Direct_command Target.Command.(program ?host cmd)
let direct_shell_command ?host cmd =
  direct_execution ?host Program.(sh cmd)

let get_output ?host prog = `Get_output Target.Command.(program ?host prog)

let lsf = Ketrew_lsf.create
