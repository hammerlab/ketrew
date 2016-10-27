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

open Internal_pervasives

module Volume = struct

  type structure = [
    | `File of string
    | `Directory of (string * structure list)
  ] [@@deriving yojson]

  type t = {
    host: Host.t;
    root: Path.t;
    structure: structure;
  } [@@deriving yojson]
  let create ~host ~root structure = {host; root; structure}
  let file s = `File s
  let dir name contents = `Directory (name, contents)

  let rec all_structure_paths = fun s ->
    match s with
    | `File s -> [Path.relative_file_exn s ]
    | `Directory (name, children) ->
      let children_paths = 
        List.concat_map ~f:all_structure_paths children in
      let this_one = Path.relative_directory_exn name in
      this_one :: List.map ~f:(Path.concat this_one) children_paths

  let all_paths t: Path.t list =
    List.map ~f:(Path.concat t.root) (all_structure_paths t.structure)

  let log_structure structure = 
    let all_paths = all_structure_paths structure |> List.map ~f:Path.to_string in
    let open Log in
    match all_paths with
    | [] -> s "EMPTY"
    | one :: [] -> s "Single path: " % quote one
    | more -> i (List.length more) % sp % s "paths"

  let log {host; root; structure} =
    Log.(braces (
        parens (Host.log host) % sp
        % parens (s "Root: " % s (Path.to_string root)) % sp
        % parens (s "Tree: " % log_structure structure)
      ))

  let markup {host; root; structure} =
    let open Display_markup in
    let rec markup_structure =
      function
      | `File f -> path f
      | `Directory (n, l) ->
        begin match l with
        | [] -> concat [path n; path "/";]
        | [one] -> concat [path n; path "/"; markup_structure one]
        | more ->
          concat [path n; path "/"; itemize (List.map ~f:markup_structure more)]
        end
    in
    description_list [
      "Host", Host.markup host;
      "Root", Path.to_string root |> path;
      "Structure", markup_structure structure;
    ]

  let to_string_hum v =
    Log.to_long_string (log v)

end

type id = Unique_id.t
[@@deriving yojson]

module Command = struct

  type t = {
    host: Host.t;
    action: Program.t;
  } [@@deriving yojson]
  let shell ?(host=Host.tmp_on_localhost) s = { host; action = `Shell_command s}
  let program ?(host=Host.tmp_on_localhost) action = { host; action}

  let get_host t = t.host

  let log {host; action} = 
    Log.(s "Action: " % Program.log action
         % s " on " % s (Host.to_string_hum host))

  let markup {host; action} = 
    let open Display_markup in
    description_list [
      "Host", Host.markup host;
      "Action", Program.markup action;
    ]

  let to_string_hum c = Log.to_long_string (log c)

end

module Condition = struct
  type t = [
    | `Never
    | `Volume_exists of Volume.t
    | `Volume_size_bigger_than of (Volume.t * int)
    | `Command_returns of (Command.t * int)
    | `And of t list
  ] [@@deriving yojson]
  let rec log =
    Log.(function
      | `Satisfied -> s "Satisfied"
      | `Never -> s "Never"
      | `Volume_exists v -> 
        parens (s "Volume " % Volume.log v % s " exists")
      | `Volume_size_bigger_than (v, sz) ->
        parens (s "Volume " % Volume.log v % s " ≥ " 
                % i sz % nbsp % s "B")
      | `Command_returns (c, ret) ->
        parens (s "Command " % Command.log c % s " returns " % i ret)
      | `And l ->
        parens (separate (s " && ") (List.map l ~f:log))
      )
  let to_string_hum c = Log.to_long_string (log c)

  let rec markup t =
    let open Display_markup in
    match t with
    | `Satisfied -> textf "Satisfied"
    | `Never -> textf "Never"
    | `Volume_exists v ->
      description "Volume exists" (Volume.markup v)
    | `Volume_size_bigger_than (v, i) ->
      description (fmt "Volume ≥ %d B" i) (Volume.markup v)
    | `Command_returns (c, i) ->
      description (fmt "Command returns %d" i) (Command.markup c)
    | `And tl ->
      description "All true" (itemize (List.map ~f:markup tl))

end

module Build_process = struct
  type t = [
    | `No_operation
    | `Long_running of (string * string)
  ] [@@deriving yojson]

  let nop : t = `No_operation
end


module State = struct
(**
Encoding of the state of a target:
   
- `run_bookkeeping` keeps the information for the `Long_running` plugin.
- `log` is a time stamped optional log message

Every state point to its previous state through a `'a hitory`. 

We use the subtyping of  polymorphic variants to encode the
state-machine; a given state can come only from certain previous
states, those are enforced with the type-parameter of the `history`
value.

*)
  type run_bookkeeping = 
    { plugin_name: string; run_parameters: string } [@@deriving yojson]
  type log = {
    (* time: Time.t; *)
    time: float;
    message: string option;
  } [@@deriving yojson]
  type 'a history = {
    log: log;
    previous_state: 'a;
  } [@@deriving yojson]
  type id = string
      [@@deriving yojson]
  type passive = [ `Passive of log ] [@@deriving yojson]
  type active = [
    | `Active of (passive history * [ `User | `Dependency of id ])
  ] [@@deriving yojson]
  type evaluating_condition = [
    | active
    | `Tried_to_eval_condition of evaluating_condition history
  ] [@@deriving yojson]
  type already_done = [
    | `Already_done of evaluating_condition history
  ] [@@deriving yojson]
  type building = [
    | `Building of evaluating_condition history
    | `Still_building of building history
  ] [@@deriving yojson]
  type dependency_failure = [
    | `Dependencies_failed of (building history * id list)
  ] [@@deriving yojson]
  type starting = [
    | `Starting of building history
    | `Tried_to_start of (starting history * run_bookkeeping)
  ] [@@deriving yojson]
  (* let starting_of_yojson yj : ([< starting ], _) Result.t = starting_of_yojson yj *)
  type failed_to_start = [
    | `Failed_to_eval_condition of evaluating_condition history
    | `Failed_to_start of (starting history * run_bookkeeping)
  ] [@@deriving yojson]
  type running = [
    | `Started_running of (starting history * run_bookkeeping)
    | `Still_running  of (running history * run_bookkeeping)
    | `Still_running_despite_recoverable_error of
        (string * running history * run_bookkeeping)
  ] [@@deriving yojson]
(*
Successful run is the success of the process, we still have to verify
that the potential condition has been ensured.
*)
  type successful_run = [
    | `Successfully_did_nothing of starting history
    | `Ran_successfully of (running history * run_bookkeeping)
    | `Tried_to_reeval_condition of (string * successful_run history)
  ] [@@deriving yojson]
  type process_failure_reason = [
    (* | Did_not_ensure_condition of string *)
    | `Long_running_failure of string
  ] [@@deriving yojson]
  type failed_run = [
    | `Failed_running of
        (running history * process_failure_reason * run_bookkeeping)
  ] [@@deriving yojson]
  type verified_run = [
    | `Verified_success of successful_run history
  ] [@@deriving yojson]
  type failed_to_verify_success = [
    | `Did_not_ensure_condition of successful_run history
  ] [@@deriving yojson]
  type killable_state = [
    | passive
    | evaluating_condition
    | building
    | starting
    | running
  ] [@@deriving yojson]
  type killing = [
    | `Killing of killable_state history
    | `Tried_to_kill of killing history
  ] [@@deriving yojson]
  type killed = [
    | `Killed of killing history
  ] [@@deriving yojson]
  type failed_to_kill = [
    | `Failed_to_kill of killing history
  ] [@@deriving yojson]
  type finishing_state = [
    | failed_run
    | verified_run
    | already_done
    | dependency_failure
    | failed_to_start
    | killed
    | failed_to_kill
    | failed_to_verify_success
  ] [@@deriving yojson]
  type finished = [
    | `Finished of finishing_state history
  ] [@@deriving yojson]
  type t = [
    | killing
    | killed
    | killable_state
    | successful_run
    | finishing_state
    | finished
  ] [@@deriving yojson]

  let make_log ?message () = 
    {time = Time.now (); message}
  let to_history ?log previous_state =
    {log = make_log ?message:log (); previous_state}

  type simple = [
    | `Activable
    | `In_progress
    | `Successful
    | `Failed
  ] [@@deriving yojson, show] 
  let rec simplify (t: t) : simple =
    match t with
    | `Building _
    | `Tried_to_start _
    | `Started_running _
    | `Starting _
    | `Still_building _
    | `Still_running _
    | `Still_running_despite_recoverable_error _
    | `Ran_successfully _
    | `Successfully_did_nothing _
    | `Tried_to_eval_condition _
    | `Tried_to_reeval_condition _
    | `Active _ -> `In_progress
    | `Verified_success _
    | `Already_done _ -> `Successful
    | `Dependencies_failed _
    | `Failed_running _
    | `Failed_to_kill _
    | `Failed_to_start _
    | `Failed_to_eval_condition _
    | `Killing _
    | `Tried_to_kill _
    | `Did_not_ensure_condition _
    | `Killed _ -> `Failed
    | `Finished s ->
      simplify (s.previous_state :> t)
    | `Passive _ -> `Activable

  let rec find_map (t: t) ~f =
    let continue history = find_map ~f (history.previous_state :> t) in
    begin match f t with
    | Some t -> Some t
    | None ->
      begin match t with
      | `Building history -> continue history
      | `Tried_to_start (history, _) -> continue history
      | `Started_running (history, _) -> continue history
      | `Starting history -> continue history
      | `Still_building history -> continue history
      | `Still_running (history, _) -> continue history
      | `Still_running_despite_recoverable_error (_, history, _) -> continue history
      | `Ran_successfully (history, _) -> continue history
      | `Successfully_did_nothing history -> continue history
      | `Active (history, _) -> continue history
      | `Tried_to_eval_condition history -> continue history
      | `Tried_to_reeval_condition (_, history) -> continue history
      | `Verified_success history -> continue history
      | `Already_done history -> continue history
      | `Dependencies_failed (history, _) -> continue history
      | `Failed_running (history, _, _) -> continue history
      | `Failed_to_kill history -> continue history
      | `Failed_to_eval_condition history -> continue history
      | `Failed_to_start (history, _) -> continue history
      | `Killing history -> continue history
      | `Tried_to_kill history -> continue history
      | `Did_not_ensure_condition history -> continue history
      | `Killed history -> continue history
      | `Finished history -> continue history
      | `Passive log -> None
      end
    end

  let rec passive_time (t: t) =
    find_map t ~f:(function
    | `Passive log -> Some log.time
    | other -> None)
    |> Option.value_exn ~msg:"Could not find passive-time"
      (* The exception should never happen *)

  let finished_time = function
  | `Finished {log; _} -> Some log.time
  | _ -> None

  let name (t: t) =
    match t with
    | `Building _ -> "Building"
    | `Tried_to_start _ -> "Tried_to_start"
    | `Started_running _ -> "Started_running"
    | `Starting _ -> "Starting"
    | `Still_building _ -> "Still_building"
    | `Still_running _ -> "Still_running"
    | `Still_running_despite_recoverable_error _ ->
      "Still_running_despite_recoverable_error"
    | `Ran_successfully _ -> "Ran_successfully"
    | `Successfully_did_nothing _ -> "Successfully_did_nothing"
    | `Active _ -> "Active"
    | `Tried_to_eval_condition _ -> "Tried_to_eval_condition"
    | `Tried_to_reeval_condition _ -> "Tried_to_reeval_condition"
    | `Verified_success _ -> "Verified_success"
    | `Already_done _ -> "Already_done"
    | `Dependencies_failed _ -> "Dependencies_failed"
    | `Failed_running _ -> "Failed_running"
    | `Failed_to_kill _ -> "Failed_to_kill"
    | `Failed_to_eval_condition _ -> "Failed_to_eval_condition"
    | `Failed_to_start _ -> "Failed_to_start"
    | `Killing _ -> "Killing"
    | `Tried_to_kill _ -> "Tried_to_kill"
    | `Did_not_ensure_condition _ -> "Did_not_ensure_condition"
    | `Killed _ -> "Killed"
    | `Finished _ -> "Finished"
    | `Passive _ -> "Passive"

  let rec latest_run_bookkeeping (t: t) =
    let continue history =
      latest_run_bookkeeping (history.previous_state :> t) in
    match t with
    | `Building history -> None
    | `Tried_to_start (hist, book) -> (Some book)
    | `Started_running (hist, book) -> (Some book)
    | `Starting history -> (None)
    | `Still_building history -> (None)
    | `Still_running (hist, book) -> (Some book)
    | `Still_running_despite_recoverable_error (_, hist, book) -> (Some book)
    | `Ran_successfully (hist, book) -> (Some book)
    | `Successfully_did_nothing history -> (None)
    | `Tried_to_eval_condition _ -> (None)
    | `Tried_to_reeval_condition (_, history) -> continue history
    | `Active (history, _) -> (None)
    | `Verified_success history -> continue history
    | `Already_done history -> None
    | `Dependencies_failed (history, _) -> (None)
    | `Failed_running (hist, _, book) -> (Some book)
    | `Failed_to_kill history -> continue history
    | `Failed_to_eval_condition history -> continue history
    | `Failed_to_start (hist, book) -> (Some book)
    | `Killing history -> continue history
    | `Tried_to_kill history -> continue history
    | `Did_not_ensure_condition history -> continue history
    | `Killed history -> continue history
    | `Finished history -> continue history
    | `Passive log -> (None)

  let contents (t: t) =
    let some h = Some (h :> t history) in
    match t with
    | `Building history -> (some history, None)
    | `Tried_to_start (hist, book) -> (some hist, Some book)
    | `Started_running (hist, book) -> (some hist, Some book)
    | `Starting history -> (some history, None)
    | `Still_building history -> (some history, None)
    | `Still_running (hist, book) -> (some hist, Some book)
    | `Still_running_despite_recoverable_error (_, hist, book) ->
      (some hist, Some book)
    | `Ran_successfully (hist, book) -> (some hist, Some book)
    | `Successfully_did_nothing history -> (some history, None)
    | `Active (history, _) -> (some history, None)
    | `Tried_to_eval_condition history -> (some history, None)
    | `Tried_to_reeval_condition (_, history) -> (some history, None)
    | `Verified_success history -> (some history, None)
    | `Already_done history -> (some history, None)
    | `Dependencies_failed (history, _) -> (some history, None)
    | `Failed_running (hist, _, book) -> (some hist, Some book)
    | `Failed_to_kill history -> (some history, None)
    | `Failed_to_eval_condition history -> (some history, None)
    | `Failed_to_start (hist, book) -> (some hist, Some book)
    | `Killing history -> (some history, None)
    | `Tried_to_kill history -> (some history, None)
    | `Did_not_ensure_condition history -> (some history, None)
    | `Killed history -> (some history, None)
    | `Finished history -> (some history, None)
    | `Passive log -> (None, None)

  type summary =
    [ `Time of Time.t ] * [ `Log of string option ] * [ `Info of string list ]
    [@@deriving yojson]

  let summary t =
    let rec count_start_attempts : starting history -> int = fun h ->
      match h.previous_state with 
      | `Starting _ -> 1
      | `Tried_to_start (hh, _) -> 1 + (count_start_attempts hh)
    in
    let rec count_kill_attempts : killing history -> int = fun h ->
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
    let rec dive ?(first_run = false) (t: t) =
      let continue history = dive (history.previous_state :> t) in
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
      | `Still_running_despite_recoverable_error (error, history, book) ->
        fmt "non-fatal error %S (check-running)" error :: continue history
      | `Ran_successfully (history, book) -> continue history
      | `Successfully_did_nothing history -> continue history
      | `Active (history, how) when first_run ->
        fmt "Activated by %s"
          (match how with `User -> "User" | `Dependency id -> id)
        :: continue history
      | `Active (history, _) -> continue history
      | `Tried_to_eval_condition history -> continue history
      | `Tried_to_reeval_condition (error, history) ->
        fmt "non-fatal error %S (eval-condition)" error :: continue history
      | `Verified_success history -> continue history
      | `Already_done history ->
        "already-done" :: continue history
      | `Dependencies_failed (history, deps) ->
        let nb_deps = (List.length deps) in
        fmt "%d dependenc%s failed" nb_deps (plural_of_int ~y:true nb_deps)
        :: continue history
      | `Failed_running (history, reason, book) ->
        fmt "Reason: %S" (match reason with | `Long_running_failure s -> s)
        :: continue history
      | `Failed_to_kill history -> continue history
      | `Failed_to_eval_condition history -> continue history
      | `Failed_to_start (history, book) ->
        continue history
      | `Killing history ->
        fmt "killed from %s" (name (history.previous_state :> t))
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
          let { log = {time; message}; previous_state } = history in
          (time, message))
      |> function
      | None -> passive_time t, None
      | Some (time, m) -> time, m in
    (`Time time, `Log message, `Info (dive ~first_run:true t))

  module Flat = struct
    (*
type state = [
    | `Building
    | `Tried_to_start
    | `Started_running
    | `Starting
    | `Still_building
    | `Still_running
    | `Still_running_despite_recoverable_error
    | `Ran_successfully
    | `Successfully_did_nothing
    | `Tried_to_eval_condition
    | `Tried_to_reeval_condition
    | `Active
    | `Verified_success
    | `Already_done
    | `Dependencies_failed
    | `Failed_running
    | `Failed_to_kill
    | `Failed_to_start
    | `Failed_to_eval_condition
    | `Killing
    | `Tried_to_kill
    | `Did_not_ensure_condition
    | `Killed
    | `Finished
    | `Passive
] [@@deriving yojson, show]
       *)
    type state = t
    type item = {
      time: float;
      simple: simple;
      name: string;
      message: string option;
      more_info: string list;
      finished: bool;
      depth: int;
    }
    [@@deriving yojson]

    type t = {
      history: item list;
    } [@@deriving yojson]

    let item state ~depth =
      let finished =
        match state with
        | `Finished _ -> true
        | _ -> false in
      let simple = simplify state in
      let name = name state in
      let (`Time time, `Log message, `Info too_much) = summary state in
      let more_info =
        List.remove_consecutive_duplicates too_much ~equal:((=)) in
      {time; message; name; simple; more_info; finished; depth}

    let time item = item.time
    let simple item = item.simple
    let name item = item.name
    let message item = item.message
    let more_info item = item.more_info
    let finished item = item.finished

    (*
       The invariant we keep is that the history is sorted, `List.hd`
       should give the latest piece of state.
    *)
    let empty () = {history = []}
                   
    let latest {history} = List.hd history

    let history {history} = history

    let of_state state =
      let rec dive acc t =
        let item depth = item t ~depth in
        let history_opt, bookkeeping_opt = contents t in
        let stack =
          let similar_item one_f two_f =
            let one = one_f 4242 in
            let two = two_f 4242 in (* We create fake ones for comparing *)
            one.simple = two.simple
            && one.name = two.name
            && one.message = two.message
            && one.more_info = two.more_info
          in
          match acc with
          | one :: more when similar_item one item -> item :: more
          | _ -> item :: acc
        in
        match history_opt with
        | Some history ->
          dive stack history.previous_state
        | None ->
          List.rev (List.mapi stack ~f:(fun depth item_f -> item_f depth))
      in
      let history = dive [] state in
      {history}

    let since {history} date =
      match List.filter history ~f:(fun {time; _} -> time >= date) with
      | [] -> None
      | history -> Some { history }

    let merge one two =
      let compare a b = ~- (Int.compare a.depth b.depth) in
      {history =
         List.rev_append one.history two.history
         |> List.dedup ~compare
         |> List.sort ~cmp:compare}

  end
  let rec to_flat_list (t : t) =
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
    let tried_to_eval_condition = function `Tried_to_eval_condition _ -> true | _ -> false
    let verified_success = function `Verified_success _ -> true | _ -> false
    let already_done = function `Already_done _ -> true | _ -> false
    let dependencies_failed = function `Dependencies_failed _ -> true | _ -> false
    let failed_running = function `Failed_running _ -> true | _ -> false
    let failed_to_kill = function `Failed_to_kill _ -> true | _ -> false
    let failed_to_start = function `Failed_to_start _ -> true | _ -> false
    let failed_to_eval_condition = function `Failed_to_eval_condition _ -> true | _ -> false
    let killing = function `Killing _ -> true | _ -> false
    let tried_to_kill = function `Tried_to_kill _ -> true | _ -> false
    let did_not_ensure_condition = function `Did_not_ensure_condition _ -> true | _ -> false
    let killed = function `Killed _ -> true | _ -> false
    let finished = function `Finished _ -> true | _ -> false
    let passive = function `Passive _ -> true | _ -> false
  
    let killable = function
    | #killable_state -> true
    | _ -> false

    let dependency_dead =
      function
      | `Finished {previous_state = (`Dependencies_failed _); _ } -> true
      | `Dependencies_failed _ -> true
      | other -> false

    let activated_by_user s =
      find_map s ~f:(
        function
        | `Passive _ -> Some false
        | `Active (_, `User) -> Some true
        | `Active (_, `Dependency _) -> Some false
        | other -> None)
      |> Option.value ~default:false (* actually find should always
                                        find at least `Passive *)

    let rec killed_from_passive (s : t) =
      match s with
      | `Finished { previous_state = (`Killed _ as killed); _ } ->
        killed_from_passive killed
      | `Killed {previous_state = (`Killing _ as killing); _ } ->
        killed_from_passive killing
      | `Killing {previous_state = (`Passive _); _ } -> true
      | other -> false

    let failed_from_running (s : t) =
      match s with
      | `Finished {previous_state = `Failed_running _; _} -> true
      | `Failed_running _ -> true
      | other -> false

    let failed_from_starting (s : t) =
      match s with
      | `Finished {previous_state = `Failed_to_start _; _} -> true
      | `Failed_to_start _ -> true
      | other -> false

    let failed_from_condition (s : t) =
      match s with
      | `Finished {previous_state = `Did_not_ensure_condition _; _} -> true
      | `Did_not_ensure_condition _ -> true
      | other -> false

  end

  module Count = struct
    module Latest = struct
      let make_counter ~continue t =
        let rec count v (t: t) =
          match continue t with
          | Some previous_state ->
            count (v + 1) (previous_state :> t)
          | _ -> v
        in
        count 0 t
      let tried_to_eval_condition (t: t) =
        make_counter ~continue:(function
          | `Tried_to_eval_condition { log; previous_state } -> Some previous_state
          | _ -> None) t
      let tried_to_reeval_condition (t: t) =
        make_counter ~continue:(function
          | `Tried_to_reeval_condition (_, { log; previous_state }) ->
            Some previous_state
          | _ -> None) t
      let tried_to_kill (t: t) =
        make_counter ~continue:(function
          | `Tried_to_kill { log; previous_state } -> Some previous_state
          | _ -> None) t
      let tried_to_start (t: t) =
        make_counter ~continue:(function
          | `Tried_to_start ({ log; previous_state }, _) -> Some previous_state
          | _ -> None) t
    end
    let consecutive_recent_attempts t =
      let (+-+) = max in
      let open Latest in
      tried_to_start t
      +-+ tried_to_kill t
      +-+ tried_to_eval_condition t
      +-+ tried_to_reeval_condition t
    (* let rec count v (t: t) = *)
    (*   match t with *)
    (*   | `Tried_to_eval_condition { log; previous_state } -> *)
    (*     count (v + 1) (previous_state :> t) *)
    (*   | _ -> v *)
    (* in *)
    (* count 0 t *)
  end

end
  

module Equivalence = struct
  type t = [
    | `None
    | `Same_active_condition
  ] [@@deriving yojson]
end

module Target = struct
  type t = {
    id: id;
    name: string;
    metadata: [`String of string] option;
    depends_on: id list;
    on_failure_activate: id list;
    on_success_activate: id list;
    make: Build_process.t;
    condition: Condition.t option;
    equivalence: Equivalence.t;
    history: State.t;
    log: (Time.t * string) list;
    tags: string list;
  } [@@deriving yojson]
end
include Target (* we want a module name to use for later *)

let create
    ?id ?name ?metadata
    ?(depends_on=[]) ?(on_failure_activate=[]) ?(on_success_activate=[])
    ?(make=Build_process.nop)
    ?condition ?(equivalence=`Same_active_condition) ?(tags=[])
    () = 
  let history = `Passive (State.make_log ()) in
  let id = Option.value id ~default:(Unique_id.create ()) in
  { id; name = Option.value name ~default:id; metadata; tags; 
    log = []; depends_on; make; condition; history; equivalence;
    on_failure_activate; on_success_activate; }

let to_serializable t = t
let of_serializable t = t

let id : t -> Unique_id.t = fun t -> t.id
let name : t -> string = fun t -> t.name
let depends_on: t -> id list = fun t -> t.depends_on
let on_success_activate: t -> id list = fun t -> t.on_success_activate
let on_failure_activate: t -> id list = fun t -> t.on_failure_activate
let metadata = fun t -> t.metadata
let build_process: t -> Build_process.t = fun t -> t.make
let condition: t -> Condition.t option = fun t -> t.condition
let equivalence: t -> Equivalence.t = fun t -> t.equivalence
let additional_log: t -> (Time.t * string) list = fun t -> t.log
let tags: t -> string list = fun t -> t.tags
let state: t -> State.t = fun t -> t.history

let is_equivalent t ext =
  match t.equivalence, ext.equivalence with
  | `None, _
  | _, `None -> false
  | `Same_active_condition, `Same_active_condition -> 
    begin match t.condition with
    | None -> false
    | Some other -> Some other = ext.condition
    end


let log t = Log.(brakets (sf "Target: %s (%s)" t.name t.id))

let with_history t h = {t with history = h}


let latest_run_parameters target =
  state target |> State.latest_run_bookkeeping
  |> Option.map 
    ~f:(fun rb -> rb.State.run_parameters)

let activate_exn ?log t ~reason =
  match t.history with 
  | `Passive _ as c ->
    with_history t (`Active (State.to_history ?log c, reason))
  | _ -> raise (Invalid_argument "activate_exn")

let kill ?log t =
  match state t with
  | #State.killable_state as c ->
    Some (with_history t (`Killing (State.to_history ?log c)))
  | other ->
    None

let reactivate
    ?with_id ?with_name ?with_metadata ?log t =
  (* It's [`Passive] so there won't be any [exn]. *)
  activate_exn ~reason:`User
    {t with
     history = `Passive (State.make_log ?message:log ());
     id = Option.value with_id ~default:(Unique_id.create ());
     name = Option.value with_name ~default:t.name;
     metadata = Option.value with_metadata ~default:t.metadata}


module Automaton = struct

  type failure_reason = State.process_failure_reason
  type progress = [ `Changed_state | `No_change ]
  type 'a transition_callback = ?log:string -> 'a -> t * progress
  type severity = [ `Try_again | `Fatal ]
  type bookkeeping = State.run_bookkeeping  =
    { plugin_name: string; run_parameters: string }
  type long_running_failure = severity * string * bookkeeping
  type long_running_action =  (bookkeeping, long_running_failure) Pvem.Result.t
  type process_check =
    [ `Successful of bookkeeping | `Still_running of bookkeeping ]
  type process_status_check = (process_check, long_running_failure) Pvem.Result.t
  type condition_evaluation = (bool, severity * string) Pvem.Result.t
  type dependencies_status = 
    [ `All_succeeded | `At_least_one_failed of id list | `Still_processing ]
  type transition = [
    | `Do_nothing of unit transition_callback
    | `Activate of id list * unit transition_callback
    | `Check_and_activate_dependencies of dependencies_status transition_callback
    | `Start_running of bookkeeping * long_running_action transition_callback
    | `Eval_condition of Condition.t * condition_evaluation transition_callback
    | `Check_process of bookkeeping * process_status_check transition_callback
    | `Kill of bookkeeping * long_running_action transition_callback
  ]

  open State

  let transition t : transition =
    let return_with_history ?(no_change=false) t h =
      with_history t h, (if no_change then `No_change else `Changed_state) in
    let activate_failures c =
      `Activate (t.on_failure_activate, (fun ?log () ->
          return_with_history t (`Finished (to_history ?log c)))) in
    let activate_successes c =
      `Activate (t.on_success_activate, (fun ?log () ->
          return_with_history t (`Finished (to_history ?log c)))) in
    let from_killing_state killable_history current_state =
      let{ log; previous_state } = killable_history in
      begin match previous_state with
      | `Building _
      | `Starting _
      | `Passive _
      | `Tried_to_start _
      | `Still_building _ (* should we ask to kill the dependencies? *)
      | `Tried_to_eval_condition _
      | `Tried_to_reeval_condition _
      | `Active _ ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Killed (to_history ?log current_state)))
      | `Still_running (_, bookkeeping)
      | `Still_running_despite_recoverable_error (_, _, bookkeeping)
      | `Started_running (_, bookkeeping) ->
        `Kill (bookkeeping, begin fun ?log -> function
          | `Ok bookkeeping -> (* loosing some bookeeping *)
            return_with_history t (`Killed (to_history ?log current_state))
          | `Error (`Try_again, reason, bookeeping) ->
            return_with_history ~no_change:true t
              (`Tried_to_kill (to_history ?log current_state))
          | `Error (`Fatal, log, bookeeping) ->
            return_with_history t (`Failed_to_kill (to_history ~log current_state))
          end)
      end
    in
    begin match t.history with
    | `Finished _
    | `Passive _ ->
      `Do_nothing (fun ?log () -> t, `No_change)
    | `Tried_to_eval_condition _
    | `Active _ as c ->
      begin match t.condition with
      | Some cond ->
        `Eval_condition (cond, begin fun ?log -> function
          | `Ok true -> return_with_history t (`Already_done (to_history ?log c))
          | `Ok false -> return_with_history t (`Building (to_history ?log c))
          | `Error (`Try_again, log)  ->
            return_with_history t ~no_change:true
              (`Tried_to_eval_condition (to_history ~log c))
          | `Error (`Fatal, log)  ->
            return_with_history t (`Failed_to_eval_condition (to_history ~log c))
          end)
      | None ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Building (to_history ?log c)))
      end      
    | `Already_done _ as c ->
      activate_successes c
    | `Still_building _
    | `Building _ as c ->
      `Check_and_activate_dependencies begin fun ?log -> function
      | `All_succeeded ->
        return_with_history t (`Starting (to_history ?log c))
      | `At_least_one_failed id_list ->
        return_with_history t (`Dependencies_failed (to_history ?log c, id_list))
      | `Still_processing ->
        begin match c with
        | `Still_building _ -> return_with_history ~no_change:true t c
        | other ->
          return_with_history ~no_change:true t
            (`Still_building (to_history ?log other))
        end
      end
    | `Did_not_ensure_condition _
    | `Dependencies_failed _ as c -> activate_failures c
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
          | `Error (`Fatal, log, bookkeeping)  ->
            return_with_history t (`Failed_to_start (to_history ~log c, bookkeeping))
          end)
      | `No_operation ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Successfully_did_nothing (to_history ?log c)))
      end
    | `Started_running (_, bookkeeping)
    | `Still_running_despite_recoverable_error (_, _, bookkeeping)
    | `Still_running (_, bookkeeping) as c ->
      `Check_process (bookkeeping, begin fun ?log -> function
        | `Ok (`Still_running new_bookkeeping) ->
          let new_state =
            match c with
            | `Started_running _
            | `Still_running_despite_recoverable_error _ ->
              (* Normal state transition *)
              `Still_running (to_history ?log c, new_bookkeeping)
            | `Still_running (history, old_bookkepping) ->
              (* Special case to “flatten” the history: we forget
                 previous similar states, this does not respect the
                 “pseudo-happend-only” semantics but it's a valuable
                 optimization. *)
              `Still_running (history, new_bookkeeping)
          in
          return_with_history t ~no_change:true new_state
        | `Ok (`Successful bookkeeping) ->
          return_with_history t (`Ran_successfully (to_history ?log c, bookkeeping))
        | `Error (`Try_again, how, new_bookkeeping) ->
          let new_state =
            match c with
            | `Still_running_despite_recoverable_error
                (_, history, old_bookkepping) ->
              (* Special case to “flatten” the history: see above. *)
              `Still_running_despite_recoverable_error
                (how, history, new_bookkeeping)
            | `Started_running _
            | `Still_running _ ->
              `Still_running_despite_recoverable_error
                (how, to_history ?log c, bookkeeping)
          in
          return_with_history t ~no_change:true new_state
        | `Error (`Fatal, log, bookkeeping) -> 
          return_with_history t
            (`Failed_running (to_history ~log c,
                              `Long_running_failure log, bookkeeping))
        end)
    | `Successfully_did_nothing _
    | `Tried_to_reeval_condition _
    | `Ran_successfully _ as c ->
      begin match t.condition with
      | Some cond ->
        `Eval_condition (cond, begin fun ?log -> function
          | `Ok true -> return_with_history t (`Verified_success (to_history ?log c))
          | `Ok false ->
            return_with_history t (`Did_not_ensure_condition (to_history ?log c))
          | `Error (`Try_again, how) ->
            return_with_history t ~no_change:true
              (`Tried_to_reeval_condition (how, to_history ?log c))
          | `Error (`Fatal, log)  ->
            return_with_history t (`Did_not_ensure_condition (to_history ~log c))
          end)
      | None ->
        `Do_nothing (fun ?log () ->
            return_with_history t (`Verified_success (to_history ?log c)))
      end      
    | `Verified_success _ as c ->
      activate_successes c
    | `Failed_running _ as c ->
      activate_failures c
    | `Tried_to_kill _ as c ->
      let killable_history =
        let rec go =
          function
          | `Killing h -> h
          | `Tried_to_kill {previous_state; _} ->
            go previous_state in
        (go c)
      in
      from_killing_state killable_history c
    | `Killing history as c ->
      from_killing_state history c
    | `Killed _ as cur when State.Is.killed_from_passive cur ->
      (* We consider that nodes killed just after being passive (i.e. orphans)
         should not activate their `on_failure_activate` nodes. *)
      `Do_nothing (fun ?log () ->
          return_with_history t (`Finished (to_history ?log cur)))
    | `Killed _ (* We put other killings in the class of “actual failures.” *)
    | `Failed_to_start _
    | `Failed_to_eval_condition _
    | `Failed_to_kill _ as c ->
      activate_failures c
    end

end



module Target_pointer = struct
  type target = t [@@deriving yojson]
  type t = {
    original: target;
    pointer: id;
  } [@@deriving yojson]

end

module Stored_target = struct
  type target = t [@@deriving yojson]

  module V0 = struct
    type t = [
      | `Target of target
      | `Pointer of Target_pointer.t
    ] [@@deriving yojson]
  end
  include Json.Versioned.Of_v0(V0)
  type t = V0.t

  let deserialize s : (t, _) Result.t =
    let open Result in
    begin
      try return (deserialize_exn s)
    with e -> fail (`Target (`Deserilization (Printexc.to_string e)))
    end

  let get_target = function
  | `Target t -> `Target t
  | `Pointer { Target_pointer. pointer; _} -> `Pointer pointer

  let of_target t = `Target t

  let id = function
  | `Target t -> t.id
  | `Pointer { Target_pointer. original; _ } -> original.id

  let name = function
  | `Target t -> t.name
  | `Pointer { Target_pointer. original; _ } -> original.name

  let make_pointer ~from ~pointing_to =
    `Pointer { Target_pointer.
               original = from;
               pointer = pointing_to.id }
end

module Summary = struct
  type full_target = Target.t
  type t = {
    id: id;
    name: string;
    metadata: [`String of string] option;
    depends_on: id list;
    on_failure_activate: id list;
    on_success_activate: id list;
    make: Build_process.t;
    condition: Condition.t option;
    equivalence: Equivalence.t;
    tags: string list;
  } [@@deriving yojson]
  let id : t -> Unique_id.t = fun t -> t.id
  let name : t -> string = fun t -> t.name
  let depends_on: t -> id list = fun t -> t.depends_on
  let on_success_activate: t -> id list = fun t -> t.on_success_activate
  let on_failure_activate: t -> id list = fun t -> t.on_failure_activate
  let metadata = fun t -> t.metadata
  let build_process: t -> Build_process.t = fun t -> t.make
  let condition: t -> Condition.t option = fun t -> t.condition
  let equivalence: t -> Equivalence.t = fun t -> t.equivalence
  let tags: t -> string list = fun t -> t.tags
  let create (trgt : full_target) =
    {
      id = trgt.Target.id;
      name = trgt.Target.name;
      metadata = trgt.Target.metadata;
      depends_on = trgt.Target.depends_on;
      on_failure_activate = trgt.Target.on_failure_activate;
      on_success_activate = trgt.Target.on_success_activate;
      make = trgt.Target.make;
      condition = trgt.Target.condition;
      equivalence = trgt.Target.equivalence;
      tags = trgt.Target.tags;
    } 

end
