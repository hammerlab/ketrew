
open Ketrew_pervasives
module Path = Ketrew_path

module Host = Ketrew_host

module Artifact = Ketrew_artifact


module Command = struct

  type t = Ketrew_gen_target_v0_t.command = {
    host: Host.t;
    action: [ `Shell of string ];
  }
  let shell ?(host=Host.tmp_on_localhost) s = { host; action = `Shell s}

  let get_host t = t.host

  let to_string_hum {host; action = `Shell cmd} =
    fmt "Shell[%S] on %s" cmd (Host.to_string_hum host)

  let get_output {host; action} =
    match action with
    | `Shell cmd ->
      Host.get_shell_command_output host cmd
  let run t =
    get_output t (* TODO optimize to not record the output *)
    >>= fun (_, _) ->
    return ()


end

include Ketrew_gen_target_v0_t

let nop : build_process = `Artifact (`Value `Unit)

let create
    ?id ?name ?(persistance=`Input_data) ?(metadata=Artifact.unit)
    ?(dependencies=[]) ?(make=nop)
    result_type = 
  let history = `Created Time.(now ()) in
  let id = Option.value id ~default:(Unique_id.create ()) in
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

let set_running_exn t ~plugin_name ~run_parameters =
  match t.history with
  | `Activated _ as state -> 
    { t with
      history =
        `Running ({plugin_name; run_parameters; run_history = []}, state)}
  | _ -> invalid_argument_exn ~where:"Target" (fmt "set_running_exn")

let update_running_exn t ~run_parameters =
  match t.history with
  | `Running (bookkeeping, activation)  ->
    { t with
      history =
        `Running ({bookkeeping with 
                   run_parameters;
                   run_history = 
                     bookkeeping.run_parameters :: bookkeeping.run_history},
                  activation)}
  | _ -> invalid_argument_exn ~where:"Target" (fmt "update_running_exn")


let active ?id
    ?name ?persistance ?metadata
    ?dependencies ?make
    artifact = 
  activate_exn ~by:`User (create ?id ?name ?persistance ?metadata
                            ?dependencies ?make artifact)

let id t : Unique_id.t = t.id

let serialize t =
  Ketrew_gen_versioned_j.string_of_target (`V0 t)

let deserialize s : (t, _) Result.t =
  let open Result in
  try return (
      match Ketrew_gen_versioned_j.target_of_string s with
      | `V0 v0 -> v0
    )
  with e -> fail (`Target (`Deserilization (Printexc.to_string e)))

let log t = Log.(brakets (sf "Target: %s (%s)" t.name t.id))



