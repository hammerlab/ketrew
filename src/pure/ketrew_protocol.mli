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

module Down_message : sig

  type t = [
    | `List_of_targets of Ketrew_target.t list
    | `List_of_target_ids of string list
    | `List_of_query_descriptions of (string * string) list
    | `Query_result of string
    | `Ok
  ]
  include Json.Versioned.WITH_VERSIONED_SERIALIZATION with type t := t

end

module Up_message : sig
  type target_query = [
    | `All
    | `Not_finished_before of float
    | `Created_after of float
  ]
  type t = [
    | `Get_targets of string list (* List of Ids, empty means “all” *)
    | `Get_available_queries of string (* Id of the target *)
    | `Call_query of (string * string) (* target-id × query-name *)
    | `Submit_targets of Ketrew_target.t list
    | `Kill_targets of string list (* List of Ids *)
    | `Restart_targets of string list (* List of Ids *)
    | `Get_target_ids of target_query
  ]
  include Json.Versioned.WITH_VERSIONED_SERIALIZATION with type t := t

end
