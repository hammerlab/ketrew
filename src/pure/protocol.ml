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

module Server_status = struct
  type t = {
    time: float;
    read_only: bool;
    tls: [`OpenSSL | `Native | `None ];
    preemptive_bounds: int * int;
    preemptive_queue: int;
    libev: bool;
    database: string;
    host_timeout_upper_bound: float option;
    maximum_successive_attempts: int;
    concurrent_automaton_steps: int;
    gc_minor_words : float;
    gc_promoted_words : float;
    gc_major_words : float;
    gc_minor_collections : int;
    gc_major_collections : int;
    gc_heap_words : int;
    gc_heap_chunks : int;
    gc_compactions : int;
    gc_top_heap_words : int;
    gc_stack_size : int;
  } [@@deriving yojson]
  let create
      ~database
      ~host_timeout_upper_bound
      ~maximum_successive_attempts
      ~concurrent_automaton_steps
      ~time ~read_only ~tls ~preemptive_bounds ~preemptive_queue ~libev ~gc =
    {time; read_only; tls; preemptive_bounds; preemptive_queue; libev;
     database;
     host_timeout_upper_bound;
     maximum_successive_attempts;
     concurrent_automaton_steps;
     gc_minor_words = gc.Gc.minor_words;
     gc_promoted_words = gc.Gc.promoted_words;
     gc_major_words = gc.Gc.major_words;
     gc_minor_collections = gc.Gc.minor_collections;
     gc_major_collections = gc.Gc.major_collections;
     gc_heap_words = gc.Gc.heap_words;
     gc_heap_chunks = gc.Gc.heap_chunks;
     gc_compactions = gc.Gc.compactions;
     gc_top_heap_words = gc.Gc.top_heap_words;
     gc_stack_size = gc.Gc.stack_size;
    }
end


module Down_message = struct
  module V0 = struct
    type t = [
      | `List_of_targets of Target.t list
      | `List_of_target_summaries of (string (* ID *) * Target.Summary.t) list
      | `List_of_target_flat_states of
          (string (* ID *) * Target.State.Flat.t) list * float (* server-time *)
      | `List_of_target_ids of string list * float (* IDs × server-time *)
      | `Deferred_list_of_target_ids of string * int (* id × total-length *)
      | `List_of_query_descriptions of (string * string) list
      | `Query_result of string
      | `Query_error of string
      | `Server_status of Server_status.t
      | `Ok
      | `Missing_deferred
      | `Notifications of (float * string) list
    ] [@@deriving yojson]
  end
  include Json.Versioned.Of_v0(V0)
  type t = V0.t

end

module Up_message = struct
  module V0 = struct
    type time_constraint = [
      | `All
      | `Not_finished_before of float
      | `Created_after of float
      | `Status_changed_since of float
    ] [@@deriving yojson,show]
    type string_predicate = [`Equals of string | `Matches of string]
        [@@deriving yojson,show]
    type filter = [
      | `True
      | `False
      | `And of filter list
      | `Or of filter list
      | `Not of filter
      | `Status of [
          | `Simple of Target.State.simple
          | `Really_running
          | `Killable
          | `Dead_because_of_dependencies
          | `Activated_by_user
          | `Killed_from_passive
          | `Failed_from_running
          | `Failed_from_starting
          | `Failed_from_condition
        ]
      | `Has_tag of string_predicate
      | `Name of string_predicate
      | `Id of string_predicate
    ] [@@deriving yojson,show]
    type target_query = {
      time_constraint : time_constraint;
      filter : filter;
    } [@@deriving yojson,show]
    type query_option = [
      | `Block_if_empty_at_most of float
    ] [@@deriving yojson,show]
    type t = [
      | `Get_targets of string list (* List of Ids, empty means “all” *)
      | `Get_target_summaries of string list (* List of Ids, empty means “all” *)
      | `Get_target_flat_states of
          [`All | `Since of float] * string list * (query_option list)
      (* List of Ids, empty means “all” *)
      | `Get_available_queries of string (* Id of the target *)
      | `Call_query of (string * string) (* target-id × query-name *)
      | `Submit_targets of Target.t list
      | `Kill_targets of string list (* List of Ids *)
      | `Restart_targets of string list (* List of Ids *)
      | `Get_target_ids of target_query * (query_option list)
      | `Get_server_status
      | `Get_deferred of string * int * int (* id × index × length *)
      | `Get_notifications of float option
    ] [@@deriving yojson]
  end
  include Json.Versioned.Of_v0(V0)
  include V0

  let target_query_markup {time_constraint; filter }  =
    let open Display_markup in
    let string_predicate =
      function
      | `Equals s -> fmt "(equals %S)" s
      | `Matches s -> fmt "(matches %S" s
    in
    let rec markup_filter =
      let func n l =
        concat [textf "(%s " n;
                concat ~sep:(text " ") (List.map l ~f:markup_filter);
                text ")"]
      in
      function
      | `True -> text "True"
      | `False -> text "False"
      | `And l -> func "And" l
      | `Or l -> func "Or" l
      | `Not f -> func "Not" [f]
      | `Status  s ->
        concat [
          text "(Status-is ";
          text
            begin match s with
            | `Simple `Activable -> "activable"
            | `Simple `In_progress ->  "in-progress"
            | `Simple `Successful -> "successful"
            | `Simple `Failed -> "failed"
            | `Really_running -> "really-running"
            | `Killable -> "killable"
            | `Dead_because_of_dependencies -> "dead-because-of-dependencies"
            | `Activated_by_user -> "activated-by-user"
            | `Killed_from_passive -> "killed-from-passive"
            | `Failed_from_running -> "failed-from-running"
            | `Failed_from_starting -> "failed-from-starting"
            | `Failed_from_condition -> "failed-from-condition"
            end;
          text ")";
        ]
      | `Has_tag that  ->
        textf "(Has-tag-that %s)" (string_predicate that)
      | `Name that  ->
        textf "(Name %s)" (string_predicate that)
      | `Id that  ->
        textf "(Id %s)" (string_predicate that)
    in
    description_list [
      "Time-constraint",
      begin match time_constraint with
      | `All -> text "All"
      | `Not_finished_before t -> description "Not-finished-before" (date t)
      | `Created_after t -> description "Created-after" (date t)
      | `Status_changed_since t -> description "Status-change-since" (date t)
      end;
      "Filter",  markup_filter filter
    ]
end

