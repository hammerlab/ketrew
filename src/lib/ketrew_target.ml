
open Ketrew_pervasives
module Path = Ketrew_path

module Host = Ketrew_host

module Artifact = Ketrew_artifact

module Program = Ketrew_program

module Command = struct

  type t = Ketrew_gen_target_v0_t.command = {
    host: Host.t;
    action: Program.t;
  }
  let shell ?(host=Host.tmp_on_localhost) s = { host; action = `Shell_command s}
  let program ?(host=Host.tmp_on_localhost) action = { host; action}

  let get_host t = t.host

  let log {host; action} = 
    Log.(s "Action: " % Program.log action
         % s " on " % s (Host.to_string_hum host))

  let to_string_hum c = Log.to_long_string (log c)

  let get_output {host; action} =
    let cmd = Program.to_single_shell_command action in
    Host.get_shell_command_output host cmd

  let run t =
    get_output t (* TODO optimize to not record the output *)
    >>= fun (_, _) ->
    return ()


end

include Ketrew_gen_target_v0_t

module Condition = struct
  type t = condition
  let log =
    Log.(function
      | `True -> s "True"
      | `False -> s "False"
      | `Volume_exists v -> 
        parens (s "Volume " % Artifact.Volume.log v % s " exists"))
  let to_string_hum c = Log.to_long_string (log c)

end


let nop : build_process = `Artifact (`Value `Unit)

let create
    ?id ?name ?(persistance=`Input_data) ?(metadata=Artifact.unit)
    ?(dependencies=[]) ?(make=nop)
    ?(condition=`False)
    () = 
  let history = `Created Time.(now ()) in
  let id = Option.value id ~default:(Unique_id.create ()) in
  { id; name = Option.value name ~default:id; persistance; metadata;
    dependencies; make; condition; history }

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
    ?dependencies ?make ?condition
    () = 
  activate_exn ~by:`User (create ?id ?name ?persistance ?metadata ?condition
                            ?dependencies ?make ())

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

let should_start t =
  match t.condition with
  | `True -> return false
  | `False -> return true
  | `Volume_exists v -> 
    Artifact.Volume.exists v >>| not

let did_ensure_condition t =
  match t.condition with
  | `True -> return false
  | `False -> return true
  | `Volume_exists v -> Artifact.Volume.exists v

module Is = struct

  let created (t: t) = 
    match t.history with
    | `Created _ -> true
    | _ -> false

  let activated (t: t) = 
    match t.history with
    | `Activated _ -> true
    | _ -> false

  let running t = 
    match t.history with
    | `Running _ -> true
    | _ -> false
  let finished t =
    match t.history with
    | `Successful _ | `Dead _ -> true
    | _ -> false

  let failed t =
    match t.history with
    | `Dead _ -> true
    | _ -> false
  let successful t =
    match t.history with
    | `Successful _ -> true
    | _ -> false 
    
  let killable t =
    match t.history with
    | `Created _ | `Activated _ | `Running _ -> true
    | _ -> false


end


