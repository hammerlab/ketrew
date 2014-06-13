open Ketrew_pervasives


module Host = Ketrew_host
module Path = Ketrew_path
module Target = Ketrew_target
module Artifact = Ketrew_artifact

 
type host = Ketrew_host.t

class type user_artifact = object

  method artifact_type : Artifact.Type.t
  method path : string
  (** Return the path of the artifact if the artifact is a volume containing
      a single file or directory. *)

end

let unit = object
  method artifact_type = `Value `Unit
  method path = failwith "Unit has no path"
end

let file ?(host= Host.tmp_on_localhost) path  =
  let basename = Filename.basename path in
  object
    method artifact_type: Artifact.Type.t =
      `Volume Artifact.Volume.(
          create ~host
            ~root:(Path.absolute_directory_exn (Filename.dirname path))
            (file basename))
    method path = path
  end
      (* Artifact.Volume.all_paths v |> List.hd_exn |> Path.to_string in *)

class type user_target =
  object
    method activate : unit
    method name : string
    method is_active: bool
    method id: Unique_id.t
    method render: Ketrew_target.t
    method dependencies: user_target list
  end


let user_target_internal
  ?(active = false)
  ?(dependencies = [])
  ?(name: string option)
  ?(make: Target.build_process = Target.nop)
  ?(returns = unit)
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
    method render =
      let creation = if active then Target.active else Target.create in
      creation 
        ~id:self#id
        ~dependencies:(List.map dependencies ~f:(fun t -> t#id))
        ~name:self#name
        ~make returns#artifact_type
  end

let target ?active ?dependencies ?make ?returns name =
  user_target_internal ?active ?dependencies ~name ?make ?returns ()
let active  ?dependencies ?make ?returns name =
  user_target_internal ~active:true ?dependencies ~name ?make ?returns ()

type workflow = {
  mutable targets: user_target list;
}

let workflow targets = { targets }
let add_target w t = w.targets <- t :: w.targets
let make w =
  let found_one_active = ref None in
  let rec go_through_deps t =
  (* let targets = *)
    (* List.map w.targets (fun t -> *)
    if !found_one_active = None && t#is_active 
    then (
      found_one_active := Some t#render;
    );
    t#render :: List.concat_map t#dependencies ~f:go_through_deps
    |> List.dedup ~compare:Target.(fun ta tb -> compare ta.id tb.id) in
  let targets = List.concat_map w.targets ~f:go_through_deps in
  match !found_one_active with
  | Some a -> [`Make (a, targets)]
  | None -> failwith "There no active target"

let make_workflow t = workflow t |> make

let run t = t#activate; make_workflow [t]

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

let nohup_setsid ~host cmds =
  Ketrew_nohup_setsid.create ~host cmds

let direct_shell_command ?host cmd =
  `Direct_command Target.Command.(shell ?host cmd)
