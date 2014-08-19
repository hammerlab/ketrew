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


module Cohttp_server_core = Cohttp_lwt.Make_server
    (Cohttp_lwt_unix_io)(Cohttp_lwt_unix.Request)(Cohttp_lwt_unix.Response)(Cohttp_lwt_unix_net)

type answer = [
  | `Unit
  | `Json of string
  | `Wrong_request of string
]

let handle_request ~state ~body req : (answer, _) Deferred_result.t =
  match Uri.path (Cohttp_server_core.Request.uri req) with
  | "/targets" ->
    let target_ids =
      Uri.get_query_param' (Cohttp_server_core.Request.uri req) "id"
      |> Option.value ~default:[] in
    begin match target_ids  with
    | [] ->
      Ketrew_state.current_targets state
      >>= fun trgt_list ->
      let json =
        Yojson.Basic.pretty_to_string ~std:true
          (`List (List.map trgt_list ~f:(fun t -> `String (Ketrew_target.id t))))
      in
      Log.(s "Replying: " % s json @ very_verbose);
      return (`Json json)
    | more ->
      Deferred_list.while_sequential more ~f:(fun id ->
          Ketrew_state.get_target state id
          >>< function
          | `Ok t -> return (Ketrew_target.serialize t)
          | `Error e -> 
            Log.(s "Error while getting the target " % s id % s ": "
                 % s (Ketrew_error.to_string e) @ error);
            return "Not_found")
      >>= fun jsons ->
      let json = fmt "[%s]" (String.concat ~sep:",\n" jsons) in
      return (`Json json)
    end
  | other ->
    return (`Wrong_request (fmt "path: %S" other))

let start ?(return_error_messages=true) ~state how =
  let kstate = state in
  begin match how with
  | `Tls (certfile, keyfile, port) ->
    Deferred_result.wrap_deferred
      ~on_exn:(function
        | e -> `Start_server_error (Printexc.to_string e))
      Lwt.(fun () ->
          let mode =
            `SSL (
              `Crt_file_path certfile,
              `Key_file_path keyfile) in
              (* `No_password, `Port port) in *)
          let sockaddr = Lwt_unix.(ADDR_INET (Unix.inet_addr_any, port)) in
          let callback conn_id req body =
            Log.(s "HTTP callback" @ verbose);
            handle_request ~state:kstate ~body req 
            >>= function
            | `Ok `Unit ->
              Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body:"" ()
            | `Ok (`Json body) ->
              Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body ()
            | `Ok (`Wrong_request body) ->
              Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body ()
            | `Error e ->
              Log.(s "Error while handling the request: "
                   % s (Ketrew_error.to_string e) @ error);
              let body =
                if return_error_messages
                then "Error: " ^ (Ketrew_error.to_string e)
                else "Undisclosed server error" in
              Cohttp_lwt_unix.Server.respond_string ~status:`Not_found  ~body ()
          in
          let conn_closed conn_id () =
            Log.(sf "conn %S closed" (Cohttp.Connection.to_string conn_id) 
                 @ verbose);
          in
          let config = 
            { Cohttp_lwt_unix.Server.callback = callback; conn_closed } in
          let handler_http = Cohttp_server_core.(callback config) in
          Lwt_unix_conduit.serve ~mode ~sockaddr handler_http)
  end

