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

type t = [
  | `Fail of Log.t
  | `Make of Ketrew_target.t * Ketrew_target.t list
]

let log = function
| `Fail l -> Log.(s "Fail with: " % brakets l)
| `Make (act, deps) -> 
  Log.(s "Make target :" % Ketrew_target.log act
       % (match deps with
         | [] -> empty
         | more ->
           parens (s "with " % separate (s ", ") 
                     (List.map ~f:Ketrew_target.log more))))

let run_list ~state todo_list =
  Deferred_list.while_sequential todo_list (function
    | `Make (active, dependencies) ->
      begin 
        Deferred_list.while_sequential (active :: dependencies) (fun t ->
            Ketrew_state.add_target state t)
        >>= fun (_ : unit list) ->
        return ()
      end
    | `Fail l ->
      Log.(s "Fail: " % l @ error);
      fail (`Failure (Log.to_long_string l)))
  >>= fun (_ : unit list) ->
  return ()
