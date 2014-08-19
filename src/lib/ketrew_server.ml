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

let start how =
  begin match how with
  | `Tls (certfile, keyfile, port) ->
    Deferred_result.wrap_deferred
      ~on_exn:(function
        | e -> `Connection_error (Printexc.to_string e)
        )
      Lwt.(fun () ->
          let mode =
            `SSL (
              `Crt_file_path certfile,
              `Key_file_path keyfile) in
              (* `No_password, `Port port) in *)
          let sockaddr = Lwt_unix.(ADDR_INET (Unix.inet_addr_any, port)) in
          let callback conn_id req body =
            (* handle_request ~body req *) 
            Log.(s "HTTP callback" @ verbose);
            Cohttp_lwt_unix.Server.respond_string 
              ~status:`Not_found  ~body:"Server error" ()
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
  >>< function
  | `Ok () -> return ()
  | `Error e ->
    begin match e with
    | `Connection_error e ->
      Log.(s "Connection error: " % s e @ error);
      fail 1
    end

