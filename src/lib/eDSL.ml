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

 
module Host = struct
  include Host

  let ssh
      ?add_ssh_options
      ?playground
      ?port ?user ?name str =
    let playground =
      Option.map ~f:Path.absolute_directory_exn playground in
    ssh ?default_shell:None
      ?execution_timeout:None
      ?add_ssh_options
      ?playground
      ?port ?user ?name str

  let parse x =
    match of_string x with
    | `Error (`Host_uri_parsing_error (uri, error)) ->
      failwith (fmt "Error while parsing URI %S: %s" uri error)
    | `Ok u -> u

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

(* The deprecated API (more dynamically typed): *)

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

let file ?(host= Ketrew_pure.Host.tmp_on_localhost) path  =
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
    method render: Target.t
    method depends_on: user_target list
    method on_failure_activate: user_target list
    method on_success_activate: user_target list
    method metadata: [`String of string ] option
    method product: user_artifact
    method add_tags: string list -> unit
    method add_recursive_tags : string list -> unit
    method get_recursive_tags : string list
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
    ?(tags = [])
    ()
  =
  let id = Unique_id.create () in
  object (self)
    val mutable active = active
    val mutable additional_tags = []
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
    method add_tags tags =
      additional_tags <- additional_tags @ tags |> List.dedup
    method render =
      Target.create ?metadata
        ~id:self#id
        ~depends_on:(List.map depends_on ~f:(fun t -> t#id))
        ~on_failure_activate:(List.map on_failure_activate ~f:(fun t -> t#id))
        ~on_success_activate:(List.map on_success_activate ~f:(fun t -> t#id))
        ~name:self#name ?condition:done_when
        ?equivalence ~tags:(tags @ additional_tags)
        ~make ()
      |> (fun x ->
          if active then Target.activate_exn ~reason:`User x else x)
    method product =
      Option.value_exn product 
        ~msg:(fmt "Target %s has no known product" self#name)
    method add_recursive_tags _ =
      failwith "user_target_internal does not support recursive tags"
    method get_recursive_tags = []

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

  type t = Program.t
  let (&&) a b = `And [a; b]
  let sh c = `Shell_command c
  let shf fmt = Printf.ksprintf sh fmt
  let exec l = `Exec l
  let chain l = `And l

end

module Condition = struct

  type t = Target.Condition.t

  let (&&) a b = `And [a; b]
  let chain_and l = `And l
  let program ?(returns=0) ?host p =
    `Command_returns (Target.Command.program ?host p, returns)

end

module Build_process = struct
  type t = Target.Build_process.t
end

let daemonize  = Daemonize.create

let lsf = Lsf.create
let pbs = Pbs.create

let yarn_application ?host ?daemonize_using ?daemon_start_timeout program =
  Yarn.create
    ?host ?daemonize_using ?daemon_start_timeout (`Yarn_application program)

let yarn_distributed_shell
    ?host ?daemonize_using ?daemon_start_timeout 
    ?hadoop_bin ?distributed_shell_shell_jar ?container_vcores
    ~container_memory ~timeout ~application_name program =
  Yarn.(
    create
      ?host ?daemonize_using ?daemon_start_timeout
      (distributed_shell_program
         ?hadoop_bin ?distributed_shell_shell_jar ?container_vcores 
         ~container_memory ~timeout ~application_name program))

(* The (chronologically) second API: *)

(* The common denominator between the legacy API and the “worfklow node” one: *)
(* type target_to_submit *)
module Internal_representation = struct
  type t =
    < name : string;
      activate : unit;
      add_tags : string list -> unit;
      add_recursive_tags : string list -> unit;
      get_recursive_tags : string list;
      id : Ketrew_pure.Internal_pervasives.Unique_id.t;
      depends_on : t list;
      on_failure_activate : t list;
      on_success_activate : t list;
      render : Ketrew_pure.Target.t;  >
end
let target_to_submit
    ?(active = false)
    ?(depends_on = [])
    ?(on_failure_activate = [])
    ?(on_success_activate = [])
    ?(name: string option)
    ?(make: Target.Build_process.t = Target.Build_process.nop)
    ?done_when
    ?metadata
    ?equivalence
    ?(tags = [])
    ()
  =
  let id = Unique_id.create () in
  object (self)
    val mutable active = active
    val mutable additional_tags = []
    val mutable recursive_tags = []
    method name = 
      match name with
      | None -> id
      | Some s -> s
    method id = id
    method depends_on = depends_on
    method on_failure_activate = on_failure_activate
    method on_success_activate = on_success_activate
    method activate = active <- true
    method add_tags tags =
      additional_tags <- additional_tags @ tags |> List.dedup
    method get_recursive_tags = recursive_tags
    method add_recursive_tags tags =
      recursive_tags <- recursive_tags @ tags |> List.dedup
    method render =
      Target.create ?metadata
        ~id:self#id
        ~depends_on:(List.map depends_on ~f:(fun t -> t#id))
        ~on_failure_activate:(List.map on_failure_activate ~f:(fun t -> t#id))
        ~on_success_activate:(List.map on_success_activate ~f:(fun t -> t#id))
        ~name:self#name ?condition:done_when
        ?equivalence ~tags:(tags @ additional_tags)
        ~make ()
      |> (fun x ->
          if active then Target.activate_exn ~reason:`User x else x)
  end

type 'a product = 'a
  constraint 'a = < is_done : Condition.t option ; .. >

(* The main building block of the worfklow graph: *)
type 'product workflow_node = <
  product : 'product product;
  render: Internal_representation.t;
>

(* We use a GADT to hide the `'product` type-parameter with an
   existential type. We can put any kind of edge in the same list this
   way: *)
type workflow_edge =
  | Depends_on: 'a workflow_node -> workflow_edge
  | On_success_activate: _ workflow_node -> workflow_edge
  | On_failure_activate: _ workflow_node -> workflow_edge

let depends_on l =  Depends_on l
let on_success_activate n = On_success_activate n
let on_failure_activate n = On_failure_activate n

let workflow_node
    ?name
    ?active
    ?make ?done_when ?metadata
    ?equivalence
    ?(tags=[]) ?(edges=[])
    (product: 'product) : 'product workflow_node =
  let target_to_submit =
    let done_when =
      match done_when with
      | Some s -> Some s
      | None -> product#is_done
    in
    let depends_on =
      List.filter_map edges ~f:(function
        | Depends_on node -> Some node#render
        | _ -> None) in
    let on_success_activate =
      List.filter_map edges ~f:(function
        | On_success_activate node -> Some node#render
        | _ -> None) in
    let on_failure_activate =
      List.filter_map edges ~f:(function
        | On_failure_activate node -> Some node#render
        | _ -> None) in
    target_to_submit ()
      ?name
      ?equivalence ~tags
      ~on_success_activate ~on_failure_activate ~depends_on
      ?active ?make ?metadata ?done_when
  in
  object
    method product = product
    method render = target_to_submit
  end

type not_already_done = < is_done : Condition.t option >
let without_product  = object method is_done = None end


type unknown_product = < is_done : Condition.t option >

type single_file = <
  exists: Ketrew_pure.Target.Condition.t;
  is_done: Ketrew_pure.Target.Condition.t option;
  path : string;
  is_bigger_than: int -> Ketrew_pure.Target.Condition.t;
> product
let single_file ?(host= Host.tmp_on_localhost) path : single_file =
  let basename = Filename.basename path in
  object
    val vol =
      Ketrew_pure.Target.Volume.(
        create ~host
          ~root:(Ketrew_pure.Path.absolute_directory_exn (Filename.dirname path))
          (file basename))
    method path = path
    method exists = `Volume_exists vol
    method is_done = Some (`Volume_exists vol)
    method is_bigger_than n = `Volume_size_bigger_than (vol, n)
  end

let forget_product (node: _ workflow_node) =
  object
    method product = object method is_done = node#product#is_done end
    method render = node#render
  end

(*
type file_workflow = single_file workflow_node
type phony_workflow = nothing workflow_node
*)
type list_of_files = <
  is_done: Ketrew_pure.Target.Condition.t option;
  paths : string list;
>
let list_of_files ?host paths =
  object
    val files = List.map paths ~f:(fun p -> single_file ?host p)
    method is_done =
      Some (`And (List.filter_map files ~f:(fun f -> f#is_done)))
    method paths = paths
  end

let to_display_string_internal ?(ansi_colors=false) ?(indentation=2) (ut : Internal_representation.t ) =
  let escape c = fmt "\027[%sm" c  in
  let color c t = if ansi_colors then escape c ^ t ^ escape "0" else t in
  let bold_red t =  color "1;31" t in
  let bold_yellow t =  color "1;33" t in
  let bold_green t =  color "1;32" t in
  (* let greyish t = color "37" t in *)
  let rec dump_workflow ?(kind=`Dep) ?(depth=0) ut =
    let sublist ~kind l =
      String.concat ~sep:""
        (List.map l ~f:(dump_workflow ~kind ~depth:(depth + 1))) in
    let line content =
      match kind with
      | `Dep -> bold_green (fmt "* %s" content)
      | `OFA -> bold_red (fmt "× %s" content)
      | `OSA -> bold_yellow (fmt "→ %s" content)
    in
    fmt "%s%s\n%s%s%s"
      (String.make (depth * indentation) ' ')
      (line ut#name)
      (sublist ut#depends_on          ~kind:`Dep)
      (sublist ut#on_failure_activate ~kind:`OFA)
      (sublist ut#on_success_activate ~kind:`OSA)
  in
  dump_workflow ut


let to_display_string ?ansi_colors ?indentation (ut : user_target) =
  to_display_string_internal ?ansi_colors ?indentation
    (ut :> Internal_representation.t)

let workflow_to_string ?ansi_colors ?indentation w =
  to_display_string_internal ?ansi_colors ?indentation (w#render :> Internal_representation.t)

let add_tags ?(recursive = false) node tags =
  node#render#add_tags tags;
  if recursive
  then (
    node#render#add_recursive_tags tags;
  );
  ()

let node_id w = w#render#id
let node_name w = w#render#name
