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
  } [@@deriving yojson]
  let create ~time () = {time}
  let time t = t.time
end

module Down_message = struct
  module V0 = struct
    type t = [
      | `List_of_targets of Target.t list
      | `List_of_target_summaries of Target.Summary.t list
      | `List_of_target_flat_states of (string (* ID *) * Target.State.Flat.t) list
      | `List_of_target_ids of string list
      | `List_of_query_descriptions of (string * string) list
      | `Query_result of string
      | `Server_status of Server_status.t
      | `Ok
    ] [@@deriving yojson]
  end
  include Json.Versioned.Of_v0(V0)
  type t = V0.t

end

module Up_message = struct
  module V0 = struct
    type target_query = [
      | `All
      | `Not_finished_before of float
      | `Created_after of float
    ] [@@deriving yojson]
    type t = [
      | `Get_targets of string list (* List of Ids, empty means “all” *)
      | `Get_target_summaries of string list (* List of Ids, empty means “all” *)
      | `Get_target_flat_states of
          [`All | `Since of float] * string list (* List of Ids, empty means “all” *)
      | `Get_available_queries of string (* Id of the target *)
      | `Call_query of (string * string) (* target-id × query-name *)
      | `Submit_targets of Target.t list
      | `Kill_targets of string list (* List of Ids *)
      | `Restart_targets of string list (* List of Ids *)
      | `Get_target_ids of target_query
      | `Get_server_status
    ] [@@deriving yojson]
  end
  include Json.Versioned.Of_v0(V0)
  include V0

end

