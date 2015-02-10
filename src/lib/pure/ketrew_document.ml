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
module Target = Ketrew_target

(* Is this necesary?
let log_list ~empty l =
  let empty_log = empty in (** renaming because of {!Log.empty} *)
  let open Log in
  let if_empty = sp % empty_log in
  match l with
  | [] -> if_empty
  | more -> n % indent (separate n (List.map more ~f:(fun item -> s "- " % item)))
  *)

let build_process ?(with_details=false)  =
  let open Log in
  let open Target in
  function
  | `No_operation -> s "No-op"
  | `Long_running (name, content) ->
    s "Long-running " % parens (s name)
    % if with_details
      then s ":" % n %
           indent (concat (
            List.map (Ketrew_plugin.long_running_log name content)
              ~f:(fun (title, descr) -> s title % s ": " % descr % n)))
      else empty

let condition ?(with_details=false) =
  let open Log in
  function
  | None -> s "Always Runs"
  | Some c ->
    if with_details
    then Target.Condition.log c
    else s "Runs When “Not Done”"

let short_status t =
  let open Log in
  let state = Target.state t in
  let add_color =
    match Target.State.simplify state with
    | `In_progress -> if_color bold_yellow
    | `Failed -> if_color bold_red
    | `Activable -> if_color greyish
    | `Successful -> if_color bold_green
  in
  let (`Time time, `Log log, `Info info) = Target.State.summary state in
  add_color (s (Target.State.name state))
  %sp % braces (Time.log time
                % Option.value_map
                  ~default:empty log ~f:(fun m -> sp % parens (s m))
                % separate empty (List.map ~f:(fun m -> s ", " % s m) info))

let target_for_menu t =
  let open Log in
  if_color bold_yellow (s (Target.name t)) % n
  % if_color greyish (s (Target.id t)) % n
  % short_status t

let target ?build_process_details ?condition_details t =
  let open Log in
  let doc_build_process = build_process in
  let doc_condition = condition in
  let open Target in
  let itemize l =
    indent (concat (List.map l ~f:(fun (name, log) ->
        s "* " % s name % s ": " % log %n))) in
  s "Target " % a name t % n
  % itemize [
    "ID", a id t;
    "Dependencies", OCaml.list s (dependencies t);
    "Fallbacks", OCaml.list s (fallbacks t);
    "On Success trigger", OCaml.list s (success_triggers t);
    "Metadata", OCaml.option (function `String m -> s m) (metadata t);
    "Build-process",
    doc_build_process ?with_details:build_process_details
      (Target.build_process t);
    "Condition",
    doc_condition ?with_details:condition_details (Target.condition t);
    "Equivalence", (match equivalence t with
      | `None -> s "None"
      | `Same_active_condition -> s "Same active condition");
    "Tags", OCaml.list quote (tags t);
    "Status", short_status t;
    "Additional Log",
    OCaml.list (fun (time, msg) ->
        brakets (Time.log time) % s ": " % s msg) (additional_log t);
  ]
