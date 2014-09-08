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
    | Some tok, `See_targets
    | Some tok, `Query_targets -> return (token_is_valid tok)
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
    server_configuration: Ketrew_configuration.server;
    authentication_file: string;
    mutable authentication: Authentication.t;
  }

  let create ~state ~authentication ~authentication_file server_configuration =
    {state; authentication; authentication_file; server_configuration}

end
open Server_state

module Cohttp_server_core = Cohttp_lwt.Make_server
    (Cohttp_lwt_unix_io)(Cohttp_lwt_unix.Request)(Cohttp_lwt_unix.Response)(Cohttp_lwt_unix_net)

type answer = [
  | `Unit
  | `Json of string
]

let token_parameter req =
  let token =
    Uri.get_query_param (Cohttp_server_core.Request.uri req) "token" in
  Log.(s "Got token: " % OCaml.option quote token @ very_verbose);
  token


let mandatory_parameter req ~name =
  match Uri.get_query_param (Cohttp_server_core.Request.uri req) name with
  | Some v ->
    Log.(s "Got " % quote name % s ": " % quote v @ very_verbose);
    return v
  | None ->
    wrong_request (fmt "%s-mandatory-parameter" name) (fmt "Missing mandatory parameter: %S" name)

let format_parameter req =
  mandatory_parameter req ~name:"format"
  >>= function
  | "json" -> return `Json
  | other ->
    wrong_request "unknown-format-parameter" (fmt "I can't handle %S" other)

let check_that_it_is_a_get request =
  begin match Cohttp_server_core.Request.meth request with
  | `GET ->
    Log.(s "It is a GET request" @ very_verbose);
    return ()
  | other -> wrong_request "wrong method" (Cohttp.Code.string_of_method other)
  end

let get_post_body request ~body =
  begin match Cohttp_server_core.Request.meth request with
  | `POST ->
    Log.(s "It is a GET request" @ very_verbose);
    begin match body with
    | `Empty -> wrong_request "empty body" ""
    | `String s -> return s
    | `Stream lwt_stream ->
      wrap_deferred ~on_exn:(fun e -> `Failure (Printexc.to_string e))
        Lwt.(fun () ->
            Lwt_stream.to_list lwt_stream
            >>= fun l ->
            return (String.concat ~sep:"" l))
    end
  | other ->
    wrong_request "wrong method" (Cohttp.Code.string_of_method other)
  end

let handle_request ~server_state ~body req : (answer, _) Deferred_result.t =
  match Uri.path (Cohttp_server_core.Request.uri req) with
  | "/hello" -> return `Unit
  | "/targets" ->
    begin
      check_that_it_is_a_get req >>= fun () ->
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
        >>= fun current_targets ->
        begin
          if Uri.get_query_param (Cohttp_server_core.Request.uri req) "archived"
             = Some "true"
          then
            Ketrew_state.archived_targets server_state.state
            >>= fun archived ->
            return (current_targets @ archived)
          else return current_targets
        end
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
  | "/target-available-queries" ->
    check_that_it_is_a_get req >>= fun () ->
    let token = token_parameter req in
    Authentication.ensure_can server_state.authentication ?token `Query_targets
    >>= fun () ->
    mandatory_parameter req ~name:"id"
    >>= fun target_id ->
    format_parameter req
    >>= fun `Json ->
    Ketrew_state.get_target server_state.state target_id
    >>= fun target ->
    let json =
      Ketrew_state.additional_queries ~state:server_state.state target
      |> List.map ~f:(fun (name, descr) ->
          (`List [`String name; `String (Log.to_long_string descr)]))
      |> (fun l ->
          Yojson.Basic.pretty_to_string ~std:true (`List l)) in
    Log.(s "Replying: " % s json @ very_verbose);
    return (`Json json)
  | "/target-call-query" ->
    check_that_it_is_a_get req >>= fun () ->
    let token = token_parameter req in
    Authentication.ensure_can server_state.authentication ?token `Query_targets
    >>= fun () ->
    mandatory_parameter req ~name:"id"
    >>= fun target_id ->
    format_parameter req
    >>= fun `Json ->
    mandatory_parameter req ~name:"query"
    >>= fun query_name ->
    Ketrew_state.get_target server_state.state target_id
    >>= fun target ->
    Log.(s "Calling query " % quote query_name % s " on "
         % Ketrew_target.log target @ very_verbose);
    begin
      Ketrew_state.call_query ~state:server_state.state ~target query_name
      >>< function
      | `Ok string -> 
        let json = 
          Yojson.Basic.pretty_to_string ~std:true (`List [`String string]) in
        Log.(s "Replying: " % s json @ very_verbose);
        return (`Json json)
      | `Error error_log ->
        wrong_request "Failed Query" (Log.to_long_string error_log)
    end
  | "/add-targets" ->
    get_post_body req ~body 
    >>= fun body ->
    wrap_preemptively ~on_exn:(fun e -> `Failure (Printexc.to_string e))
      (fun () ->
         let parsed = Yojson.Basic.from_string body in
         match parsed with
         | `List json_targets ->
           List.map json_targets ~f:(fun jt ->
               match Ketrew_target.deserialize (Yojson.Basic.to_string jt)  with
               | `Ok o -> o
               | `Error e -> failwith (Ketrew_error.to_string e)) 
         | other -> 
           failwith "wrong-format: expecting Json list")
    >>= fun targets ->
    Log.(s "Adding " % i (List.length targets) % s " targets" @ normal);
    Deferred_list.while_sequential targets ~f:(fun t ->
        Ketrew_state.add_target server_state.state t
        >>= fun () ->
        let original_id = Ketrew_target.id t in
        Ketrew_state.get_target server_state.state original_id
        >>= fun freshen ->
        return (`List [`String original_id; `String (Ketrew_target.id freshen)])
      )
    >>= fun ids ->
    return (`Json (`List ids |> Yojson.Basic.to_string))
  | other ->
    wrong_request "Wrong path" other


let mandatory_for_starting opt ~msg =
  Deferred_result.some opt ~or_fail:(`Start_server_error msg)

let die_command = "die"
let reload_authorized_tokens = "reload-auth"

let reload_authentication_file ~server_state =
  Authentication.load_file server_state.authentication_file
  >>= fun authentication ->
  server_state.authentication <- authentication;
  return ()

let start_listening_on_command_pipe ~server_state =
  let conf = server_state.server_configuration in
  match Ketrew_configuration.command_pipe conf with
  | Some file_path ->
    System.remove file_path >>= fun () ->
    wrap_deferred 
      ~on_exn:(fun e -> `Start_server_error (Printexc.to_string e))
      (fun () -> Lwt_unix.mkfifo file_path 0o600)
    >>= fun () ->
    wrap_deferred
      ~on_exn:(fun e -> `Start_server_error (Printexc.to_string e))
      (fun () ->
         Lwt_io.open_file ~buffer_size:16
           ~flags:[Unix.O_RDWR; Unix.O_NONBLOCK; Unix.O_APPEND] ~perm:0o660
           ~mode:Lwt_io.input file_path)
    >>= fun pipe ->
    begin
      let open Lwt in
      let rec read_loop ~error_count () =
        Log.(s "Listening on " % OCaml.string file_path @ verbose);
        Lwt.catch (fun () ->
            Lwt_io.read_line pipe
            >>= function
            |  die when die = die_command ->
              Log.(s "Server killed by “die” command " 
                   % parens (OCaml.string file_path)
                   @ normal);
              exit 0
            | reload_auth when reload_auth = reload_authorized_tokens ->
              begin reload_authentication_file ~server_state
                >>= function
                | `Ok () -> return ()
                | `Error e ->
                  Log.(s "Could not reload " 
                       % quote server_state.authentication_file
                       % s": " % s (Ketrew_error.to_string e) @ error);
                  return ()
              end
              >>= fun () ->
              read_loop ~error_count ()
            |  other ->
              Log.(s "Cannot understand command: " % OCaml.string other @ error);
              read_loop ~error_count ())
          (fun e ->
             let error_count = error_count + 1 in
             Log.(s "Exn while reading command pipe: " % exn e 
                  % sp % parens (i error_count % s "-th error") @ error);
             if error_count >= 5 then
               return ()
             else
               read_loop ~error_count ())
      in
      Lwt.ignore_result (read_loop ~error_count:0 ())
    end;
    return ()
  | None -> 
    return ()


let start ~state  =
  let config =  Ketrew_state.configuration state in
  Log.(s "Starting server!" @ very_verbose);
  mandatory_for_starting
    (Ketrew_configuration.server_configuration config)
    ~msg:"Server not configured"
  >>= fun server_config ->
  mandatory_for_starting
    (Ketrew_configuration.authorized_tokens_path server_config)
    ~msg:"Authentication-less server not implemented"
  >>= fun authentication_file ->
  let return_error_messages, how =
    Ketrew_configuration.return_error_messages server_config,
    Ketrew_configuration.listen_to server_config in
  begin match how with
  | `Tls (certfile, keyfile, port) ->
    Authentication.load_file authentication_file
    >>= fun authentication ->
    let server_state =
      Server_state.create ~authentication ~state
        ~authentication_file server_config
    in
    start_listening_on_command_pipe ~server_state
    >>= fun () ->
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

let stop ~state =
  let config =  Ketrew_state.configuration state in
  Deferred_result.some ~or_fail:(`Stop_server_error "No server configured")
    (Ketrew_configuration.server_configuration config)
  >>= fun server_config ->
  Deferred_result.some ~or_fail:(`Stop_server_error "No command-pipe configured")
    (Ketrew_configuration.command_pipe server_config)
  >>= fun file_path ->
  System.file_info ~follow_symlink:true file_path
  >>= function
  | `Fifo ->
    begin
    System.with_timeout 2. (fun () ->
        IO.with_out_channel (`Append_to_file file_path) ~buffer_size:16 ~f:(fun oc ->
            IO.write oc die_command
            >>= fun () ->
            IO.write oc "\n"))
    >>< function
    | `Ok () -> return `Done
    | `Error (`Timeout _) -> return `Timeout
    | `Error (`IO _ as e) -> fail e
    | `Error (`System _) -> fail (`Stop_server_error "System.timeout failed!")
  end
  | other -> 
    fail (`Stop_server_error (fmt "%S is not a named-pipe (%s)"
                                file_path (System.file_info_to_string other)))

