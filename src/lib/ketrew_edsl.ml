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


module Path = Ketrew_path
module Target = Ketrew_target

 
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
      Target.Volume.(
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
    method depends_on: user_target list
    method on_failure_activate: user_target list
    method on_success_activate: user_target list
    method metadata: [`String of string ] option
    method product: user_artifact
  end


let user_target_internal
    ?(active = false)
    ?(depends_on = [])
    ?(on_failure_activate = [])
    ?(on_success_activate = [])
    ?(name: string option)
    ?(make: Target.Build_process.t = Target.Build_process.nop)
    ?done_when
    ?metadata
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
    method depends_on = depends_on
    method on_failure_activate = on_failure_activate
    method on_success_activate = on_success_activate
    method activate = active <- true
    method is_active = active
    method metadata = metadata
    method render =
      Target.create ?metadata
        ~id:self#id
        ~depends_on:(List.map depends_on ~f:(fun t -> t#id))
        ~on_failure_activate:(List.map on_failure_activate ~f:(fun t -> t#id))
        ~on_success_activate:(List.map on_success_activate ~f:(fun t -> t#id))
        ~name:self#name ?condition:done_when
        ?equivalence ?tags
        ~make ()
      |> (fun x ->
          if active then Target.activate_exn ~reason:`User x else x)
    method product =
      Option.value_exn product 
        ~msg:(fmt "Target %s has no known product" self#name)

  end

let target ?active ?depends_on ?make ?done_when ?metadata ?product
    ?equivalence ?on_failure_activate ?on_success_activate ?tags name =
  user_target_internal
    ?equivalence ?on_failure_activate ?tags ?on_success_activate
    ?active ?depends_on ~name ?make ?metadata ?done_when ?product ()

let file_target 
    ?depends_on ?make ?metadata ?name ?host ?equivalence ?on_failure_activate
    ?on_success_activate ?tags path =
  let product = file ?host path in
  let name = Option.value name ~default:("Make:" ^ path) in
  target ~product ?equivalence ?on_failure_activate ?tags ?on_success_activate
    ~done_when:product#exists ?depends_on ?make ?metadata name

module Program = struct

  type t = Ketrew_program.t
  let (&&) a b = `And [a; b]
  let sh c = `Shell_command c
  let shf fmt = Printf.ksprintf sh fmt
  let exec l = `Exec l
  let chain l = `And l

end

module Condition = struct

  type t = Ketrew_target.Condition.t

  let (&&) a b = `And [a; b]
  let chain_and l = `And l
  let never = `Never
  let program ?(returns=0) ?host p =
    `Command_returns (Ketrew_target.Command.program ?host p, returns)

end

let daemonize  = Ketrew_daemonize.create

let lsf = Ketrew_lsf.create
let pbs = Ketrew_pbs.create

let yarn_application ?host ?daemonize_using ?daemon_start_timeout program =
  Ketrew_yarn.create
    ?host ?daemonize_using ?daemon_start_timeout (`Yarn_application program)

let yarn_distributed_shell
    ?host ?daemonize_using ?daemon_start_timeout 
    ?hadoop_bin ?distributed_shell_shell_jar
    ~container_memory ~timeout ~application_name program =
  Ketrew_yarn.(
    create
      ?host ?daemonize_using ?daemon_start_timeout
      (distributed_shell_program
         ?hadoop_bin ?distributed_shell_shell_jar
         ~container_memory ~timeout ~application_name program))


