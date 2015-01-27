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

  let rec eval = 
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
        eval cond
        >>= function
        | true -> go rest
        | false -> return false
      in
      go list_of_conditions

end

module Equivalence = struct
  type t = Ketrew_gen_target_v0.Equivalence.t
end

module Build_process = struct
  include Ketrew_gen_target_v0.Build_process

  let nop : t = `No_operation
end

module Metadata = struct

  type t = [ `None | `String of string ]

end


let make_log ?message () = 
  let open Ketrew_gen_target_v0.Log in
  {time = Time.now (); message}
let to_history ?log previous_state =
  let open Ketrew_gen_target_v0.History in
  {log = make_log ?message:log (); previous_state}

module State = struct

  type t = Ketrew_gen_target_v0.State.t
  type history = Ketrew_gen_target_v0.State.t

  let history t = t

  let rec simplify (t: t) =
    match t with
    | `Building _
    | `Tried_to_start _
    | `Started_running _
    | `Starting _
    | `Still_building _
    | `Still_running _
    | `Ran_successfully _
    | `Successfully_did_nothing _
    | `Active _ -> `In_progress
    | `Verified_success _
    | `Already_done _ -> `Successful
    | `Dependencies_failed _
    | `Failed_running _
    | `Failed_to_kill _
    | `Failed_to_start _
    | `Killing _
    | `Tried_to_kill _
    | `Did_not_ensure_condition _
    | `Killed _ -> `Failed
    | `Finished s ->
      simplify (s.Ketrew_gen_target_v0.History.previous_state :> t)
    | `Passive _ -> `Activable

  let rec passive_time (t: t) =
    let continue history =
      passive_time (history.Ketrew_gen_target_v0.History.previous_state :> t)
    in
    match t with
    | `Building history -> continue history
    | `Tried_to_start (history, _) -> continue history
    | `Started_running (history, _) -> continue history
    | `Starting history -> continue history
    | `Still_building history -> continue history
    | `Still_running (history, _) -> continue history
    | `Ran_successfully (history, _) -> continue history
    | `Successfully_did_nothing history -> continue history
    | `Active (history, _) -> continue history
    | `Verified_success history -> continue history
    | `Already_done history -> continue history
    | `Dependencies_failed (history, _) -> continue history
    | `Failed_running (history, _, _) -> continue history
    | `Failed_to_kill history -> continue history
    | `Failed_to_start (history, _) -> continue history
    | `Killing history -> continue history
    | `Tried_to_kill history -> continue history
    | `Did_not_ensure_condition history -> continue history
    | `Killed history -> continue history
    | `Finished history -> continue history
    | `Passive log -> log.Ketrew_gen_target_v0.Log.time

  let name (t: t) =
    let open Ketrew_gen_target_v0.State in
    match t with
    | `Building _ -> "Building"
    | `Tried_to_start _ -> "Tried_to_start"
    | `Started_running _ -> "Started_running"
    | `Starting _ -> "Starting"
    | `Still_building _ -> "Still_building"
    | `Still_running _ -> "Still_running"
    | `Ran_successfully _ -> "Ran_successfully"
    | `Successfully_did_nothing _ -> "Successfully_did_nothing"
    | `Active _ -> "Active"
    | `Verified_success _ -> "Verified_success"
    | `Already_done _ -> "Already_done"
    | `Dependencies_failed _ -> "Dependencies_failed"
    | `Failed_running _ -> "Failed_running"
    | `Failed_to_kill _ -> "Failed_to_kill"
    | `Failed_to_start _ -> "Failed_to_start"
    | `Killing _ -> "Killing"
    | `Tried_to_kill _ -> "Tried_to_kill"
    | `Did_not_ensure_condition _ -> "Did_not_ensure_condition"
    | `Killed _ -> "Killed"
    | `Finished _ -> "Finished"
    | `Passive _ -> "Passive"

  let rec latest_run_bookkeeping (t: t) =
    let continue history =
      latest_run_bookkeeping
        (history.Ketrew_gen_target_v0.History.previous_state :> t)
    in
    match t with
    | `Building history -> None
    | `Tried_to_start (hist, book) -> (Some book)
    | `Started_running (hist, book) -> (Some book)
    | `Starting history -> (None)
    | `Still_building history -> (None)
    | `Still_running (hist, book) -> (Some book)
    | `Ran_successfully (hist, book) -> (Some book)
    | `Successfully_did_nothing history -> (None)
    | `Active (history, _) -> (None)
    | `Verified_success history -> continue history
    | `Already_done history -> None
    | `Dependencies_failed (history, _) -> (None)
    | `Failed_running (hist, _, book) -> (Some book)
    | `Failed_to_kill history -> continue history
    | `Failed_to_start (hist, book) -> (Some book)
    | `Killing history -> continue history
    | `Tried_to_kill history -> continue history
    | `Did_not_ensure_condition history -> continue history
    | `Killed history -> continue history
    | `Finished history -> continue history
    | `Passive log -> (None)

  let contents (t: t) =
    let some h = Some (h :> t Ketrew_gen_target_v0.History.t) in
    match t with
    | `Building history -> (some history, None)
    | `Tried_to_start (hist, book) -> (some hist, Some book)
    | `Started_running (hist, book) -> (some hist, Some book)
    | `Starting history -> (some history, None)
    | `Still_building history -> (some history, None)
    | `Still_running (hist, book) -> (some hist, Some book)
    | `Ran_successfully (hist, book) -> (some hist, Some book)
    | `Successfully_did_nothing history -> (some history, None)
    | `Active (history, _) -> (some history, None)
    | `Verified_success history -> (some history, None)
    | `Already_done history -> (some history, None)
    | `Dependencies_failed (history, _) -> (some history, None)
    | `Failed_running (hist, _, book) -> (some hist, Some book)
    | `Failed_to_kill history -> (some history, None)
    | `Failed_to_start (hist, book) -> (some hist, Some book)
    | `Killing history -> (some history, None)
    | `Tried_to_kill history -> (some history, None)
    | `Did_not_ensure_condition history -> (some history, None)
    | `Killed history -> (some history, None)
    | `Finished history -> (some history, None)
    | `Passive log -> (None, None)

  let summary t =
    let open Ketrew_gen_target_v0 in
    let rec count_start_attempts : Starting.t History.t -> int = fun h ->
      let open History in
      match h.previous_state with 
      | `Starting _ -> 1
      | `Tried_to_start (hh, _) -> 1 + (count_start_attempts hh)
    in
    let rec count_kill_attempts : Killing.t History.t -> int = fun h ->
      let open History in
      match h.previous_state with 
      | `Killing _ -> 1
      | `Tried_to_kill hh -> 1 + (count_kill_attempts hh)
    in
    let plural_of_int ?(y=false) n =
      match y, n with
      | true, 1 ->  "y"
      | true, _ -> "ies"
      | _, 1 ->  ""
      | _, _ -> "s" in
    let rec dive (t: t) =
      let continue history = dive (history.History.previous_state :> t) in
      match t with
      | `Building history -> continue history
      | `Tried_to_start (history, book) ->
        let attempts = count_start_attempts history in
        fmt " %d start-attempt%s" attempts (plural_of_int attempts)
        :: continue history
      | `Started_running (history, book) -> continue history
      | `Starting history -> continue history
      | `Still_building history -> continue history
      | `Still_running (history, book) -> continue history
      | `Ran_successfully (history, book) -> continue history
      | `Successfully_did_nothing history -> continue history
      | `Active (history, _) -> continue history
      | `Verified_success history -> continue history
      | `Already_done history ->
        "already-done" :: continue history
      | `Dependencies_failed (history, deps) ->
        let nb_deps = (List.length deps) in
        fmt "%d depependenc%s failed" nb_deps (plural_of_int ~y:true nb_deps)
        :: continue history
      | `Failed_running (history, reason, book) ->
        fmt "Reason: %S" (match reason with | `Long_running_failure s -> s)
        :: continue history
      | `Failed_to_kill history ->
        continue history
      | `Failed_to_start (history, book) ->
        continue history
      | `Killing history ->
        fmt "killed from %s" (name (history.History.previous_state :> t))
        :: continue history
      | `Tried_to_kill history ->
        fmt "%d killing-attempts" (count_kill_attempts history)
        :: continue history
      | `Did_not_ensure_condition history ->
        "Did_not_ensure_condition" :: continue history
      | `Killed history ->
        "killed" :: continue history
      | `Finished history -> continue history
      | `Passive log -> []
    in
    let history_opt, bookkeeping_opt = contents t in
    let time, message =
      Option.map history_opt ~f:(fun history ->
        let open Ketrew_gen_target_v0.History in
        let open Ketrew_gen_target_v0.Log in
        let { log = {time; message}; previous_state } = history in
        (time, message))
      |> function
      | None -> passive_time t, None
      | Some (time, m) -> time, m in
    (`Time time, `Log message, `Info (dive t))

  let rec to_flat_list (t : t) =
    let open Ketrew_gen_target_v0.History in
    let open Ketrew_gen_target_v0.Log in
    let open Ketrew_gen_target_v0.Run_bookkeeping in
    let open Ketrew_gen_target_v0.State in
    let make_item ?bookkeeping ~history name = 
        let { log; previous_state } = history in
        let bookkeeping_msg =
          Option.map bookkeeping ~f:(fun { plugin_name; run_parameters } ->
              fmt "[%s] Run-parameters: %d bytes" plugin_name
                (String.length run_parameters)) in
        (log.time, name, log.message, bookkeeping_msg)
        :: to_flat_list (previous_state :> t)
    in
    let name = name t in
    let history_opt, bookkeeping = contents t in
    match t with
    | `Passive log -> (* passive ! *)
      (log.time, name, log.message, None) :: []
    | other ->  
      let history =
        Option.value_exn history_opt ~msg:"non-passive got None history" in
      make_item ~history ?bookkeeping name

  let log ?depth t =
    to_flat_list t
    |> fun l ->
    begin match depth with
    | Some d -> List.take l d
    | None -> l
    end
    |> List.map ~f:(fun (time, name, msgopt, bookopt) ->
        Log.(s "* " % Time.log time % s ": " % s name
             % (match msgopt with None -> empty | Some m -> n % indent (s m))
             % (match bookopt with None -> empty | Some m -> n % indent (s m))))
    |> Log.(separate n)

  module Is = struct
    let building = function `Building _ -> true | _ -> false
    let tried_to_start = function `Tried_to_start _ -> true | _ -> false
    let started_running = function `Started_running _ -> true | _ -> false
    let starting = function `Starting _ -> true | _ -> false
    let still_building = function `Still_building _ -> true | _ -> false
    let still_running = function `Still_running _ -> true | _ -> false
    let ran_successfully = function `Ran_successfully _ -> true | _ -> false
    let successfully_did_nothing = function `Successfully_did_nothing _ -> true | _ -> false
    let active = function `Active _ -> true | _ -> false
    let verified_success = function `Verified_success _ -> true | _ -> false
    let already_done = function `Already_done _ -> true | _ -> false
    let dependencies_failed = function `Dependencies_failed _ -> true | _ -> false
    let failed_running = function `Failed_running _ -> true | _ -> false
    let failed_to_kill = function `Failed_to_kill _ -> true | _ -> false
    let failed_to_start = function `Failed_to_start _ -> true | _ -> false
    let killing = function `Killing _ -> true | _ -> false
    let tried_to_kill = function `Tried_to_kill _ -> true | _ -> false
    let did_not_ensure_condition = function `Did_not_ensure_condition _ -> true | _ -> false
    let killed = function `Killed _ -> true | _ -> false
    let finished = function `Finished _ -> true | _ -> false
    let passive = function `Passive _ -> true | _ -> false
  
    let killable = function
    | #Ketrew_gen_target_v0.Killable_state.t -> true
    | _ -> false
  end
end

open Ketrew_gen_target_v0.Target
type t = Ketrew_gen_target_v0.Target.t

let create
    ?id ?name ?metadata
    ?(dependencies=[]) ?(if_fails_activate=[]) ?(success_triggers=[])
    ?(make=Build_process.nop)
    ?condition ?(equivalence=`Same_active_condition) ?(tags=[])
    () = 
  let history = `Passive (make_log ()) in
  let id = Option.value id ~default:(Unique_id.create ()) in
  { id; name = Option.value name ~default:id; metadata; tags; 
    log = []; dependencies; make; condition; history; equivalence;
    if_fails_activate; success_triggers; }

let to_serializable t = t
let of_serializable t = t

let id : t -> Unique_id.t = fun t -> t.id
let name : t -> string = fun t -> t.name
let dependencies: t -> id list = fun t -> t.dependencies
let fallbacks: t -> id list = fun t -> t.if_fails_activate
let success_triggers: t -> id list = fun t -> t.success_triggers
let metadata = fun t -> t.metadata
let build_process: t -> Build_process.t = fun t -> t.make
let condition: t -> Condition.t option = fun t -> t.condition
let equivalence: t -> Equivalence.t = fun t -> t.equivalence
let additional_log: t -> (Time.t * string) list = fun t -> t.log
let tags: t -> string list = fun t -> t.tags
let state: t -> State.t = fun t -> t.history

let is_equivalent t ext =
  match t.equivalence with
  | `None -> false
  | `Same_active_condition -> 
    begin match t.condition with
    | None -> false
    | Some other -> Some other = ext.condition
    end

let with_history t h = {t with history = h}

let activate_exn ?log t ~reason =
  match t.history with 
  | `Passive _ as c ->
    with_history t (`Active (to_history ?log c, reason))
  | _ -> raise (Invalid_argument "activate_exn")

let kill ?log t =
  match state t with
  | #Ketrew_gen_target_v0.Killable_state.t as c ->
    Some (with_history t (`Killing (to_history ?log c)))
  | other ->
    None

let reactivate
    ?with_id ?with_name ?with_metadata ?log t =
  (* It's [`Passive] so there won't be any [exn]. *)
  activate_exn ~reason:`User
    {t with
     history = `Passive (make_log ?message:log ());
     id = Option.value with_id ~default:(Unique_id.create ());
     name = Option.value with_name ~default:t.name;
     metadata = Option.value with_metadata ~default:t.metadata}

module Automaton = struct

  type failure_reason = Ketrew_gen_target_v0.Process_failure_reason.t
  type progress = [ `Changed_state | `No_change ]
  type 'a transition_callback = ?log:string -> 'a -> t * progress
  type severity = [ `Try_again | `Fatal ]
  type bookkeeping = Ketrew_gen_target_v0.Run_bookkeeping.t =
    { plugin_name: string; run_parameters: string}
  type long_running_failure = severity * string * bookkeeping
  type long_running_action =  (bookkeeping, long_running_failure) Pvem.Result.t
  type process_check =
    [ `Successful of bookkeeping | `Still_running of bookkeeping ]
  type process_status_check = (process_check, long_running_failure) Pvem.Result.t
  type dependencies_status = 
    [ `All_succeeded | `At_least_one_failed of id list | `Still_processing ]
  type transition = [
    | `Do_nothing of unit transition_callback
    | `Activate of id list * unit transition_callback
    | `Check_and_activate_dependencies of dependencies_status transition_callback
    | `Start_running of bookkeeping * long_running_action transition_callback
    | `Eval_condition of Condition.t * bool transition_callback
    | `Check_process of bookkeeping * process_status_check transition_callback
    | `Kill of bookkeeping * long_running_action transition_callback
  ]

  let transition t : transition =
    let return_with_history ?(no_change=false) t h =
      with_history t h, (if no_change then `No_change else `Changed_state) in
    let activate_fallbacks c =
      `Activate (t.if_fails_activate, (fun ?log () ->
          return_with_history t (`Finished (to_history ?log c)))) in
    let activate_success_triggers c =
      `Activate (t.success_triggers, (fun ?log () ->
          return_with_history t (`Finished (to_history ?log c)))) in
    let from_killing_state killable_history current_state =
      let{ Ketrew_gen_target_v0.History. log; previous_state } =
        killable_history in
      begin match previous_state with
      | `Building _
      | `Starting _
      | `Passive _
      | `Tried_to_start _
      | `Still_building _ (* should we ask to kill the dependencies? *)
      | `Active _ ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Killed (to_history ?log current_state)))
      | `Still_running (_, bookkeeping)
      | `Started_running (_, bookkeeping) ->
        `Kill (bookkeeping, begin fun ?log -> function
          | `Ok bookkeeping -> (* loosing some bookeeping *)
            return_with_history t (`Killed (to_history ?log current_state))
          | `Error (`Try_again, reason, bookeeping) ->
            return_with_history ~no_change:true t
              (`Tried_to_kill (to_history ?log current_state))
          | `Error (`Fatal, reason, bookeeping) ->
            return_with_history t (`Failed_to_kill (to_history ?log current_state))
          end)
      end
    in
    begin match t.history with
    | `Finished _
    | `Passive _ ->
      `Do_nothing (fun ?log () -> t, `No_change)
    | `Active _ as c ->
      begin match t.condition with
      | Some cond ->
        `Eval_condition (cond, begin fun ?log -> function
          | true -> return_with_history t (`Already_done (to_history ?log c))
          | false -> return_with_history t (`Building (to_history ?log c))
          end)
      | None ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Building (to_history ?log c)))
      end      
    | `Already_done _ as c ->
      activate_success_triggers c
    | `Still_building _
    | `Building _ as c ->
      `Check_and_activate_dependencies begin fun ?log -> function
      | `All_succeeded ->
        return_with_history t (`Starting (to_history ?log c))
      | `At_least_one_failed id_list ->
        return_with_history t (`Dependencies_failed (to_history ?log c, id_list))
      | `Still_processing ->
        return_with_history ~no_change:true t
          (`Still_building (to_history ?log c))
      end
    | `Did_not_ensure_condition _
    | `Dependencies_failed _ as c -> activate_fallbacks c
    | `Starting _
    | `Tried_to_start _ as c ->
      begin match build_process t with
      | `Long_running (plugin_name, created_run_paramters) ->
        let bookeeping =
          {plugin_name; run_parameters = created_run_paramters } in
        `Start_running (bookeeping, begin fun ?log -> function
          | `Ok bookkeeping ->
            return_with_history t (`Started_running (to_history ?log c, bookkeeping))
          | `Error (`Try_again, log, bookkeeping)  ->
            return_with_history t ~no_change:true
              (`Tried_to_start (to_history ~log c, bookkeeping))
          | `Error (`Fatal, reason, bookkeeping)  ->
            return_with_history t (`Failed_to_start (to_history ?log c, bookkeeping))
          end)
      | `No_operation ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Successfully_did_nothing (to_history ?log c)))
      end
    | `Started_running (_, bookkeeping)
    | `Still_running (_, bookkeeping) as c ->
      `Check_process (bookkeeping, begin fun ?log -> function
        | `Ok (`Still_running bookkeeping) ->
          return_with_history t ~no_change:true
            (`Still_running (to_history ?log c, bookkeeping))
        | `Ok (`Successful bookkeeping) ->
          return_with_history t (`Ran_successfully (to_history ?log c, bookkeeping))
        | `Error (_, how, bookkeeping) -> 
          return_with_history t (`Failed_running (to_history ?log c,
                                                  `Long_running_failure how,
                                                  bookkeeping))
        end)
    | `Successfully_did_nothing _
    | `Ran_successfully _ as c ->
      begin match t.condition with
      | Some cond ->
        `Eval_condition (cond, begin fun ?log -> function
          | true -> return_with_history t (`Verified_success (to_history ?log c))
          | false ->
            return_with_history t (`Did_not_ensure_condition (to_history ?log c))
          end)
      | None ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Verified_success (to_history ?log c)))
      end      
    | `Verified_success _ as c ->
      activate_success_triggers c
    | `Failed_running _ as c ->
      activate_fallbacks c
    | `Tried_to_kill _ as c ->
      let killable_history =
        let rec go =
          function
          | `Killing h -> h
          | `Tried_to_kill {Ketrew_gen_target_v0.History. previous_state; _} ->
            go previous_state in
        (go c)
      in
      from_killing_state killable_history c
    | `Killing history as c ->
      from_killing_state history c
    | `Killed _
    | `Failed_to_start _
    | `Failed_to_kill _ as c ->
      (* what should we actually do? *)
      activate_fallbacks c
    end

end

module Target_pointer = struct
  type t = Ketrew_gen_target_v0.Target_pointer.t
end

module Stored_target = struct
  type target = t
  type t = Ketrew_gen_target_v0.Stored_target.t
  include
    Json.Make_versioned_serialization
      (Ketrew_gen_target_v0.Stored_target)
      (Ketrew_gen_versioned.Stored_target)
  let deserialize s : (t, _) Result.t =
    let open Result in
    try return (deserialize_exn s)
    with e -> fail (`Target (`Deserilization (Printexc.to_string e)))

  let get_target = function
  | `Target t -> `Target t
  | `Pointer { Ketrew_gen_target_v0.Target_pointer. pointer; _} -> `Pointer pointer

  let of_target t = `Target t

  let id = function
  | `Target t -> t.id
  | `Pointer { Ketrew_gen_target_v0.Target_pointer. original } -> original.id

  let make_pointer ~from ~pointing_to =
    `Pointer { Ketrew_gen_target_v0.Target_pointer.
               original = from;
               pointer = pointing_to.id }

end

  


let log t = Log.(brakets (sf "Target: %s (%s)" t.name t.id))

let should_start t =
  match t.condition with
  | Some c -> Condition.eval c >>| not
  | None -> return true

let did_ensure_condition t =
  match t.condition with
  | Some c -> Condition.eval c
  | None -> return true


let latest_run_parameters target =
  let open Ketrew_gen_target_v0.Run_bookkeeping in
  state target |> State.latest_run_bookkeeping
  |> Option.map 
    ~f:(fun rb -> rb.run_parameters)
