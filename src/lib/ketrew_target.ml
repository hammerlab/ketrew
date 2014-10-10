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

module Host = Ketrew_host

module Artifact = Ketrew_artifact

module Program = Ketrew_program

type id = Unique_id.t

module Command = struct

  type t = Ketrew_gen_target_v0.Command.t = {
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

  let get_return_value {host; action} =
    let cmd = Program.to_single_shell_command action in
    Host.get_shell_command_return_value host cmd

  let run t =
    get_output t (* TODO optimize to not record the output *)
    >>= fun (_, _) ->
    return ()


end

module Condition = struct
  type t = Ketrew_gen_target_v0.Condition.t
  let rec log =
    Log.(function
      | `True -> s "True"
      | `False -> s "False"
      | `Volume_exists v -> 
        parens (s "Volume " % Artifact.Volume.log v % s " exists")
      | `Volume_size_bigger_than (v, sz) ->
        parens (s "Volume " % Artifact.Volume.log v % s " ≥ " 
                % i sz % nbsp % s "B")
      | `Command_returns (c, ret) ->
        parens (s "Command " % Command.log c % s " returns " % i ret)
      | `And l ->
        parens (separate (s " && ") (List.map l ~f:log))
      )
  let to_string_hum c = Log.to_long_string (log c)

end

module Equivalence = struct
  type t = Ketrew_gen_target_v0.Equivalence.t
end

module Build_process = struct
  include Ketrew_gen_target_v0.Build_process

  let nop : t = `Artifact (`Value `Unit)
end


type submitted_state = Ketrew_gen_target_v0.Submitted_state.t
type activated_state = Ketrew_gen_target_v0.Activated_state.t
type run_bookkeeping = Ketrew_gen_target_v0.Run_bookkeeping.t = {
  plugin_name : string;
  run_parameters : string;
  run_history : string list;
}
type running_state = Ketrew_gen_target_v0.Running_state.t
type death_reason = Ketrew_gen_target_v0.Death_reason.t
type finished_state = Ketrew_gen_target_v0.Finished_state.t
type workflow_state = Ketrew_gen_target_v0.Workflow_state.t

include Ketrew_gen_target_v0.Target

let create
    ?id ?name ?(persistence=`Input_data) ?(metadata=Artifact.Value.unit)
    ?(dependencies=[]) ?(if_fails_activate=[]) ?(success_triggers=[])
    ?(make=Build_process.nop)
    ?condition ?(equivalence=`Same_active_condition) ?(tags=[])
    () = 
  let history = `Created Time.(now ()) in
  let id = Option.value id ~default:(Unique_id.create ()) in
  { id; name = Option.value name ~default:id; persistence; metadata; tags; 
    log = []; dependencies; make; condition; history; equivalence;
    if_fails_activate; success_triggers; }

let is_equivalent t ext =
  match t.equivalence with
  | `None -> false
  | `Same_active_condition -> 
    begin match t.condition with
    | None -> false
    | Some other -> Some other = ext.condition
    end


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
        `Running ({Ketrew_gen_target_v0.Run_bookkeeping. 
                    plugin_name; run_parameters; run_history = []}, state)}
  | _ -> invalid_argument_exn ~where:"Target" (fmt "set_running_exn")

let update_running_exn t ~run_parameters =
  match t.history with
  | `Running (bookkeeping, activation)  ->
    { t with
      history =
        `Running Ketrew_gen_target_v0.Run_bookkeeping.(
            {bookkeeping with 
             run_parameters;
             run_history = 
               bookkeeping.run_parameters :: bookkeeping.run_history},
            activation)}
  | _ -> invalid_argument_exn ~where:"Target" (fmt "update_running_exn")



let active ?id
    ?name ?persistence ?metadata
    ?dependencies ?if_fails_activate ?success_triggers 
    ?make ?condition ?equivalence ?tags
    () = 
  activate_exn ~by:`User 
    (create ?id ?if_fails_activate ?success_triggers 
       ?name ?persistence ?metadata ?condition ?tags
       ?equivalence ?dependencies ?make ())

let reactivate 
    ?with_id ?with_name ?with_metadata t =
  activate_exn ~by:`User
    {t with
     history = `Created (Time.now ());
     id = Option.value with_id ~default:(Unique_id.create ());
     name = Option.value with_name ~default:t.name;
     metadata = Option.value with_metadata ~default:t.metadata}

let id t : Unique_id.t = t.id
let name t = t.name

include
  Json.Make_versioned_serialization
    (Ketrew_gen_target_v0.Target)
    (Ketrew_gen_versioned.Target)

let deserialize s : (t, _) Result.t =
  let open Result in
  try return (deserialize_exn s)
  with e -> fail (`Target (`Deserilization (Printexc.to_string e)))

let log t = Log.(brakets (sf "Target: %s (%s)" t.name t.id))

let rec eval_condition = 
  function
  | `True -> return true
  | `False -> return false
  | `Volume_exists v -> Artifact.Volume.exists v
  | `Volume_size_bigger_than (v, sz) ->
    Artifact.Volume.get_size v
    >>= fun size ->
    return (size >= sz)
  | `Command_returns (c, ret) ->
    Command.get_return_value c  
    >>= fun return_value ->
    return (ret = return_value)
  | `And list_of_conditions -> 
    (* Should start at the first that returns `false` *)
    let rec go = function
    | [] -> return true
    | cond :: rest ->
      eval_condition cond
      >>= function
      | true -> go rest
      | false -> return false
    in
    go list_of_conditions

let should_start t =
  match t.condition with
  | Some c -> eval_condition c >>| not
  | None -> return true

let did_ensure_condition t =
  match t.condition with
  | Some c -> eval_condition c
  | None -> return true

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

  let activated_by_user t =
    let open Ketrew_gen_target_v0 in
    let rec go_through_history (history: Workflow_state.t) =
      match history with
      | `Created _ -> false
      | `Activated (_, _, `User) -> true
      | `Activated (_, _, `Dependency) -> false
      | `Activated (_, _, `Fallback) -> false
      | `Activated (_, _, `Success_trigger) -> false
      | `Running (_, prev) -> go_through_history (prev :> Workflow_state.t)
      | `Successful (_, prev, _) -> go_through_history (prev :> Workflow_state.t)
      | `Dead (_, prev, _) -> go_through_history (prev :> Workflow_state.t)
    in
    go_through_history t.history

end


let latest_run_parameters target =
  let open Ketrew_gen_target_v0.Run_bookkeeping in
  match target.history with
  | `Running (rb, _) -> Some (rb.run_parameters)
  | `Dead (_, `Running (rb, _), _)
  | `Successful (_, `Running (rb, _), _) ->
    Some (rb.run_parameters)
  | `Created _ | `Activated _ 
  | `Successful (_, `Activated _, _)
  | `Dead (_, `Activated _, _) ->
    begin match target.make with
    | `Long_running (plugin, rp) -> Some (rp)
    | _ -> None
    end
