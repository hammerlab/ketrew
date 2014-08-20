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

let wrong_request short long = fail (`Wrong_http_request (short, long))

module Authentication = struct
  type token = {name: string; value: string; comments : string list}
  type t = { valid_tokens: token list }

  let load_file file =
    IO.read_file file
    >>= fun content ->
    let valid_tokens =
      String.split content ~on:(`Character '\n')
      |> List.filter_map ~f:(fun line ->
          match String.split line ~on:(`Character ' ')
                |> List.map ~f:(fun t -> String.strip ~on:`Both t)
                |> List.filter ~f:(fun s -> s <> "") with
          | comment :: more when String.get comment ~index:1 = Some '#' -> None
          | name :: value :: comments -> Some {name; value; comments}
          | [] -> None
          | other ->
            Log.(s "Ignoring line: " % OCaml.string line % s " of file "
                 % OCaml.string file @ warning);
            None)
    in
    Log.(s "Loaded auth from " % OCaml.string file
         % OCaml.list (fun t ->
             OCaml.list OCaml.string [t.name; t.value;
                                      String.concat ~sep:" "  t.comments])
           valid_tokens
         @ verbose);
    return {valid_tokens}

  let can t ?token do_stuff =
    let token_is_valid tok =
      List.exists t.valid_tokens ~f:(fun x -> x.value = tok) in
    begin match token, do_stuff with
    | Some tok, `See_targets -> return (token_is_valid tok)
    | None, _ -> return false
    end

  let ensure_can t ?token do_stuff =
    can t ?token do_stuff
    >>= function
    | true -> return ()
    | false -> wrong_request "Authentication" "Insufficient credentials"

end

module Server_state = struct

  type t = {
    state: Ketrew_state.t;
    authentication: Authentication.t;
  }

  let create ~state ~authentication = {state; authentication}

end
open Server_state

module Cohttp_server_core = Cohttp_lwt.Make_server
    (Cohttp_lwt_unix_io)(Cohttp_lwt_unix.Request)(Cohttp_lwt_unix.Response)(Cohttp_lwt_unix_net)

type answer = [
  | `Unit
  | `Json of string
]

let token_parameter req =
  Uri.get_query_param (Cohttp_server_core.Request.uri req) "token"

let format_parameter req =
  match Uri.get_query_param (Cohttp_server_core.Request.uri req) "format" with
  | Some "json" -> return `Json
  | Some other ->
    wrong_request "format-parameter" (fmt "I can't handle %S" other)
  | None -> wrong_request "format-parameter" "Missing parameter"

let handle_request ~server_state ~body req : (answer, _) Deferred_result.t =
  match Uri.path (Cohttp_server_core.Request.uri req) with
  | "/targets" ->
    begin
      let token = token_parameter req in
      Authentication.ensure_can server_state.authentication ?token `See_targets
      >>= fun () ->
      let target_ids =
        Uri.get_query_param' (Cohttp_server_core.Request.uri req) "id"
        |> Option.value ~default:[] in
      format_parameter req
      >>= fun `Json ->
      begin match target_ids  with
      | [] ->
        Ketrew_state.current_targets server_state.state
        >>= fun trgt_list ->
        let json =
          Yojson.Basic.pretty_to_string ~std:true
            (`List (List.map trgt_list ~f:(fun t -> `String (Ketrew_target.id t))))
        in
        Log.(s "Replying: " % s json @ very_verbose);
        return (`Json json)
      | more ->
        Deferred_list.while_sequential more ~f:(fun id ->
            Ketrew_state.get_target server_state.state id
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
    end
  | other ->
    wrong_request "Wrong path" other

let start ?(return_error_messages=true) ~state ~authentication_file how =
  begin match how with
  | `Tls (certfile, keyfile, port) ->
    Authentication.load_file authentication_file
    >>= fun authentication ->
    let server_state = Server_state.create ~authentication ~state in
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
            handle_request ~server_state ~body req 
            >>= function
            | `Ok `Unit ->
              Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body:"" ()
            | `Ok (`Json body) ->
              Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body ()
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

