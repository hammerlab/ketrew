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


module Path = Ketrew_path
module Target = Ketrew_target
module Artifact = Ketrew_artifact

 
module Host = struct
  include Ketrew_host

  let ssh
      ?add_ssh_options
      ?playground
      ?port ?user ?name str =
    let playground = Option.map ~f:Path.absolute_directory_exn playground in
    ssh ?default_shell:None
      ?execution_timeout:None
      ?add_ssh_options
      ?playground
      ?port ?user ?name str

  let parse = of_string

  let cmdliner_term 
      ?(doc="URI of the host (e.g. \
             ssh://user@example.com:42/tmp/ketrewplayground).") how =
    let open Cmdliner in
    Term.(
      pure (fun s -> parse s)
      $ begin match how with
      | `Flag (flags) ->
        Arg.(value & opt string "/tmp/" & info flags ~doc ~docv:"URI")
      | `Required p ->
        Arg.(required & pos p (some string) None & info [] ~doc ~docv:"URI")
      end
    )

end

class type user_artifact = object

  method path : string
  method exists: Target.Condition.t

  method is_bigger_than: int -> Target.Condition.t

end

let unit = object
  method path = failwith "Unit has no path"
  method exists = failwith "Unit does not “exist”"
  method is_bigger_than _ = failwith "Unit has no size"
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
    method is_bigger_than n = `Volume_size_bigger_than (vol, n)
  end

class type user_target =
  object
    method activate : unit
    method name : string
    method is_active: bool
    method id: Unique_id.t
    method render: Ketrew_target.t
    method dependencies: user_target list
    method if_fails_activate: user_target list
    method success_triggers: user_target list
    method metadata: Ketrew_artifact.Value.t
    method product: user_artifact
  end


let user_target_internal
  ?(active = false)
  ?(dependencies = [])
  ?(if_fails_activate = [])
  ?(success_triggers = [])
  ?(name: string option)
  ?(make: Target.Build_process.t = Target.Build_process.nop)
  ?done_when
  ?(metadata = Artifact.Value.unit)
  ?product
  ?equivalence
  ?tags
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
    method if_fails_activate = if_fails_activate
    method success_triggers = success_triggers
    method activate = active <- true
    method is_active = active
    method metadata = metadata
    method render =
      let creation = if active then Target.active else Target.create in
      creation ~metadata
        ~id:self#id
        ~dependencies:(List.map dependencies ~f:(fun t -> t#id))
        ~if_fails_activate:(List.map if_fails_activate ~f:(fun t -> t#id))
        ~success_triggers:(List.map success_triggers ~f:(fun t -> t#id))
        ~name:self#name ?condition:done_when
        ?equivalence ?tags
        ~make ()
    method product =
      Option.value_exn product 
        ~msg:(fmt "Target %s has no known product" self#name)
        
  end

let target ?active ?dependencies ?make ?done_when ?metadata ?product
    ?equivalence ?if_fails_activate ?success_triggers ?tags name =
  user_target_internal
    ?equivalence ?if_fails_activate ?tags ?success_triggers
    ?active ?dependencies ~name ?make ?metadata ?done_when ?product ()

let file_target 
    ?dependencies ?make ?metadata ?name ?host ?equivalence ?if_fails_activate
    ?success_triggers ?tags path =
  let product = file ?host path in
  let name = Option.value name ~default:("Make:" ^ path) in
  target ~product ?equivalence ?if_fails_activate ?tags ?success_triggers
    ~done_when:product#exists ?dependencies ?make ?metadata name

(*
  Run a workflow:

   - make sure the run target is active,
   - render all the depdendencies/fallbacks/success-triggers,
*)
let user_command_list t =
  t#activate;
  let rec go_through_deps t =
    t#render :: 
    List.concat_map t#dependencies ~f:go_through_deps
    @ List.concat_map t#if_fails_activate ~f:go_through_deps
    @ List.concat_map t#success_triggers ~f:go_through_deps
  in
  let targets =
    (go_through_deps t)
    |> List.dedup ~compare:Target.(fun ta tb -> compare ta.id tb.id)
  in
  match targets with
  | first :: more -> (first, more)
  | [] -> assert false (* there is at least the argument one *)

let run ?override_configuration t =
  let active, dependencies = user_command_list t in
  let config_path = 
    (try Sys.getenv "KETREW_CONFIGURATION" with _ -> 
       (try Sys.getenv "KETREW_CONFIG" with _ ->
          Ketrew_configuration.default_configuration_path)) in
  match Lwt_main.run (
    Ketrew_configuration.(
      get_configuration ?override_configuration config_path)
    >>= fun configuration ->
    Ketrew_client.as_client ~configuration ~f:(fun ~client ->
        Ketrew_client.add_targets client (active :: dependencies))
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
    let open Ketrew_gen_base_v0.Host in
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

module Condition = struct

  type t = Ketrew_target.Condition.t

  let (&&) a b = `And [a; b]
  let chain_and l = `And l
  let never = `False
  let program ?(returns=0) ?host p =
    `Command_returns (Ketrew_target.Command.program ?host p, returns)

end

let daemonize  = Ketrew_daemonize.create

let direct_execution ?host cmd =
  `Direct_command Target.Command.(program ?host cmd)
let direct_shell_command ?host cmd =
  direct_execution ?host Program.(sh cmd)


let lsf = Ketrew_lsf.create
let pbs = Ketrew_pbs.create
