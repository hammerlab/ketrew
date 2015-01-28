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

(** Deal with error values common across the library. *)

open Ketrew_pervasives

let log_client_error error_value = 
  let open Log in
  let log_action =
    function
    | `Call (meth, the_uri) ->
      s (Cohttp.Code.string_of_method meth) % sp % uri the_uri
    | `Targets -> s "Getting targets"
    | `Kill_targets ids -> s "Killing targets" % sp % OCaml.list quote ids
    | `Archive_targets ids ->
      s "Archiving targets" % sp % OCaml.list quote ids
    | `Restart_targets ids ->
      s "Restarting targets" % sp % OCaml.list quote ids
    | `Target_query (id, query) ->
      s "Calling " % quote query % s " on " % quote id
    | `Cleanable_targets _ ->
      s "Querying cleanable targets"
  in
  match error_value with
  | `Server_error_response (action, error_string) ->
    s "Server replied: " % s error_string
  | `Http (action, error) ->
    let act = log_action action in
    let error_log = 
      match error with
      | `Exn e -> s "Exn:" % sp % exn e
      | `Wrong_response (http_resp, body) ->
        s "Returned:" % n % 
        indent (s "Response: "
                % sexp Cohttp_lwt_unix.Client.Response.sexp_of_t http_resp)
        % n
        % indent (s "Body: " % quote body)
      | `Json_parsing (j, `Exn e) ->
        s "Json parse error: " % exn e
        % indent (string j)
      | `Wrong_json j ->
        s "Wrong Json: " % indent (Json.log j)
      | `Unexpected_message m ->
        s "Wrong Json: " % indent (Ketrew_protocol.Down_message.log m)
    in
    s "HTTP Call" % sp % parens (act % s " → " % error_log)

let to_string = function
| `Wrong_command_line sl ->
  fmt "Wrong command line: %s" 
    (String.concat ~sep:", " (List.map sl (fmt "%S")))
| `IO _ as io -> IO.error_to_string io
| `System _ as s -> System.error_to_string s
| `Configuration (`Parsing e) ->
  fmt "Parsing error in config-file: %S" e
| `Wrong_configuration (`Found f, got) ->
  fmt "Wrong configuration: %S → %s" f
    (match got with
     | `Expected s -> fmt "expected %s" s
     | `Exn e -> fmt "exception: %S" (Printexc.to_string e))
| `Database e -> (Trakeva.Error.to_string e)
| `Host e ->
  fmt "Host: %s" (Ketrew_host.Error.log e |> Log.to_long_string)
| `Persistent_state (`Deserilization s) ->
  fmt "Persistent_state-Deserilization: %S" s
| `Target (`Deserilization s) -> fmt "target-deserialization: %s" s
| `Database_unavailable s -> fmt "DB %s" s
| `Not_implemented s -> fmt "Not-impl %S" s
| `Missing_data p -> fmt "missing data at id: %s" p
| `Failed_to_kill msg -> fmt "Failed to kill target: %S" msg
| `Long_running_failed_to_start (id, msg) ->
  fmt "Long running %s failed to start: %s" id msg
| `Failure msg -> fmt "Failure: %S" msg
| `Process _ as pe -> Ketrew_unix_process.error_to_string pe
| `Shell _ as se -> System.error_to_string se
| `Volume (`No_size l) ->
  fmt "Did not get the size of the volume: %s" (Log.to_long_string l)
| `Deserialization (except, str) ->
  fmt "Deserialization: %s (%S)" (Printexc.to_string except) str
| `Start_server_error e -> fmt "Error starting the server: %s" e
| `Stop_server_error e -> fmt "Error stopping the server: %s" e
| `Server_status_error e -> fmt "Error while getting the server's status: %s" e
| `Wrong_http_request (short, long) ->
  fmt "Wrong HTTP Request: %s → %s" short long
| `Client (client_error) ->
  fmt "Client: %s" (log_client_error client_error |> Log.to_long_string) 
| `Dyn_plugin e ->
  begin match e with
  | `Dynlink_error e ->
    fmt "Dynamic plugin linking error: %s" (Dynlink.error_message e)
  | `Findlib e ->
    fmt "Dynamic plugin findlib error: %s" (Printexc.to_string e)
  end
