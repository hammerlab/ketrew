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


open Ketrew_pure
open Internal_pervasives
open Unix_io


(** A common error that simply means “invalid argument”. *)
let wrong_request short long = fail (`Wrong_http_request (short, long))

(** Module dealing with access tokens and access rights. There are no
    “sessions” here; just a file that looks like SSH's `authorized_keys`, and a
    function: token × capability → bool.

    Capabilities are defined with polymorphic variants.
*)
module Authentication = struct

  type token = {name: string; value: string; comments : string list}
  type t = {
    valid_tokens: token list;
    authentication_input: [`Path of string | `Inline of (string * string)] list;
  }

  let log {valid_tokens; authentication_input} =
    let paths =
      List.filter_map authentication_input ~f:(function
        | `Path p -> Some p | `Inline _ -> None) in
    let inlines =
      List.filter_map authentication_input ~f:(function
        | `Path _ -> None | `Inline (name, _) -> Some name) in
    Log.(
      s "From paths: " % OCaml.list quote paths
      % s "; inline: " % OCaml.list string inlines)

  let load_file file =
    Log.(s "Authentication: loading " % quote file @ verbose);
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
    return valid_tokens

  let load meta_tokens =
    Deferred_list.while_sequential meta_tokens ~f:(function
      | `Path p -> load_file p
      | `Inline (name, value) -> return [{name; value; comments = []}])
    >>| List.concat
    >>= fun valid_tokens ->
    return {valid_tokens; authentication_input = meta_tokens}

  let reload {authentication_input; _} = load authentication_input

  let can t ?token do_stuff =
    let token_is_valid tok =
      List.exists t.valid_tokens ~f:(fun x -> x.value = tok) in
    begin match token, do_stuff with
    | Some tok, `Browse_gui
    | Some tok, `See_server_status
    | Some tok, `See_targets
    | Some tok, `Query_targets
    | Some tok, `Kill_targets
    | Some tok, `Restart_targets
    | Some tok, `Submit_targets -> return (token_is_valid tok)
    | None, _ -> return false
    end

  let ensure_can t ?token do_stuff =
    can t ?token do_stuff
    >>= function
    | true -> return ()
    | false -> wrong_request "Authentication" "Insufficient credentials"

end

(** The state maintained by the HTTP server. *)
module Server_state = struct

  type t = {
    state: Engine.t;
    server_configuration: Configuration.server;
    mutable authentication: Authentication.t;
    loop_traffic_light: Light.t;
  }
(*M
The `loop_traffic_light` is “red” for the server  by default but some services
can set it to `Green to wake-up it earlier and do things.

For example, after adding targets, the server will be woken-up to start running
the targets and not wait for the next “loop timeout.”
M*)
  let create ~state ~authentication server_configuration =
    let loop_traffic_light = Light.create () in
    {state; authentication; server_configuration; loop_traffic_light;}


end
open Server_state

type answer = [
  | `Unit
  | `Message of [ `Json ] * Protocol.Down_message.t
  | `Page of string
]
(** A service can replay one of those cases; or an error. *)

type 'error service =
  server_state:Server_state.t ->
  body:Cohttp_lwt_body.t ->
  Cohttp.Request.t ->
  (answer, 'error) Deferred_result.t
(** A service is something that replies an [answer] on a ["/<path>"] URL. *)

(** Get the ["token"] parameter from an URI. *)
let token_parameter req =
  let token =
    Uri.get_query_param (Cohttp.Request.uri req) "token" in
  Log.(s "Got token: " % OCaml.option quote token @ very_verbose);
  token

(** Get a parameter or fail. *)
let mandatory_parameter req ~name =
  match Uri.get_query_param (Cohttp.Request.uri req) name with
  | Some v ->
    Log.(s "Got " % quote name % s ": " % quote v @ very_verbose);
    return v
  | None ->
    wrong_request (fmt "%s-mandatory-parameter" name) (fmt "Missing mandatory parameter: %S" name)

(** Get the ["format"] parameter from an URI. *)
let format_parameter req =
  mandatory_parameter req ~name:"format"
  >>= function
  | "json" -> return `Json
  | other ->
    wrong_request "unknown-format-parameter" (fmt "I can't handle %S" other)

(** Fail if the request is not a [`GET]. *)
let check_that_it_is_a_get request =
  begin match Cohttp.Request.meth request with
  | `GET ->
    Log.(s "It is a GET request" @ very_verbose);
    return ()
  | other -> wrong_request "wrong method" (Cohttp.Code.string_of_method other)
  end

(** Check that it is a [`POST], get the {i non-empty} body; or fail. *)
let get_post_body request ~body =
  begin match Cohttp.Request.meth request with
  | `POST ->
    Log.(s "It is a GET request" @ very_verbose);
    wrap_deferred ~on_exn:(fun e -> `IO (`Exn e))
      (fun () -> Cohttp_lwt_body.to_string  body)
  | other ->
    wrong_request "wrong method" (Cohttp.Code.string_of_method other)
  end

(** Grab and deserialize a POST body into an up_message. *)
let message_of_body ~body =
  wrap_preemptively ~on_exn:(fun e -> `Failure (Printexc.to_string e))
    (fun () -> Protocol.Up_message.deserialize_exn body)

(** {2 Services; Answering Requests} *)

let get_targets_from_ids ~server_state target_ids =
  begin match target_ids  with
  | [] ->
    Engine.all_targets server_state.state
  | more ->
    Deferred_list.while_sequential more ~f:(fun id ->
        Engine.get_target server_state.state id
        >>< function
        | `Ok t -> return (Some t)
        | `Error e -> 
          Log.(s "Error while getting the target " % s id % s ": "
               % s (Error.to_string e) @ error);
          return None)
    >>| List.filter_opt
  end

let answer_get_targets ?(summaries=false) ~server_state target_ids =
  get_targets_from_ids ~server_state target_ids
  >>= fun targets ->
  begin match summaries with
  | false -> return (`List_of_targets targets)
  | true ->
    return (`List_of_target_summaries
                      (List.map targets ~f:Target.Summary.create))
  end

let answer_get_target_available_queries ~server_state target_id =
  Engine.get_target server_state.state target_id
  >>= fun target ->
  let msg =
    `List_of_query_descriptions (
      Plugin.additional_queries target
      |> List.map ~f:(fun (a, l) -> a, Log.to_long_string l)
    ) in
  return (msg)


let answer_call_query ~server_state ~target_id ~query =
  Engine.get_target server_state.state target_id
  >>= fun target ->
  Log.(s "Calling query " % quote query % s " on "
       % Target.log target @ very_verbose);
  begin
    Plugin.call_query ~target query
    >>< function
    | `Ok string -> 
      return (`Query_result string)
    | `Error error_log ->
      wrong_request "Failed Query" (Log.to_long_string error_log)
  end


let answer_add_targets ~server_state ~targets =
  Log.(s "Adding " % i (List.length targets) % s " targets" @ normal);
  Engine.add_targets server_state.state targets
  >>= fun () ->
  Light.green server_state.loop_traffic_light;
  return (`Ok)

let do_action_on_ids ~server_state ~ids (what_to_do: [`Kill  | `Restart]) =
  Deferred_list.while_sequential ids (fun id ->
      begin match what_to_do with
      | `Kill -> Engine.kill server_state.state id
      | `Restart ->
        Engine.restart_target server_state.state id
        >>= fun (_ : Target.id) ->
        return ()
      end)
  >>= fun (_ : unit list) ->
  Light.green server_state.loop_traffic_light;
  return (`Ok)

let answer_get_target_ids ~server_state query =
  Engine.get_list_of_target_ids server_state.state query
  >>= fun list_of_ids ->
  return (`List_of_target_ids list_of_ids)

let answer_get_server_status ~server_state =
  return (`Server_status (Protocol.Server_status.create ~time:Time.(now ()) ()))

let answer_get_target_flat_states ~server_state (time_constrain, target_ids)  =
  get_targets_from_ids ~server_state target_ids
  >>= fun targets ->
  Log.(s "answer_get_target_flat_states computing states " @ verbose);
  let states =
    List.filter_map targets ~f:(fun trgt ->
        let flat_state = Target.State.Flat.of_state (Target.state trgt) in
        match time_constrain with
        | `All -> Some (Target.id trgt, flat_state)
        | `Since ti ->
          Target.State.Flat.since flat_state ti
          |> Option.map ~f:(fun st -> (Target.id trgt, st))
      )
  in
  let total_items =
    (List.fold states ~init:0 ~f:(fun prev (_, flat) ->
         prev + (List.length flat.Target.State.Flat.history)))
  in
  Log.(s "answer_get_target_flat_states" % n
       % s "States: " % i (List.length states) % n
       % s "Total items: " % i  total_items @ normal);
  return (`List_of_target_flat_states states)


let answer_message ~server_state ?token msg =
  let with_capability cap =
    Authentication.ensure_can server_state.authentication ?token cap
  in
  match msg with
  | `Get_targets l ->
    with_capability `See_targets
    >>= fun () ->
    answer_get_targets ~server_state l
  | `Get_target_summaries l ->
    with_capability `See_targets
    >>= fun () ->
    answer_get_targets ~summaries:true ~server_state l
  | `Get_available_queries target_id ->
    with_capability `Query_targets
    >>= fun () ->
    answer_get_target_available_queries ~server_state target_id
  | `Call_query (target_id, query) ->
    with_capability `Query_targets
    >>= fun () ->
    answer_call_query ~server_state ~target_id ~query
  | `Submit_targets targets ->
    with_capability `Submit_targets
    >>= fun () ->
    answer_add_targets ~server_state ~targets
  | `Kill_targets ids ->
    with_capability `Kill_targets
    >>= fun () ->
    do_action_on_ids ~server_state ~ids `Kill
  | `Restart_targets ids ->
    with_capability `Restart_targets
    >>= fun () ->
    do_action_on_ids ~server_state ~ids `Restart
  | `Get_target_ids query ->
    with_capability `See_targets
    >>= fun () ->
    answer_get_target_ids ~server_state query
  | `Get_server_status ->
    with_capability `See_server_status
    >>= fun () ->
    answer_get_server_status ~server_state
  | `Get_target_flat_states query ->
    with_capability `See_targets
    >>= fun () ->
    answer_get_target_flat_states ~server_state query

let api_service ~server_state ~body req =
  get_post_body req ~body
  >>= fun body ->
    let token = token_parameter req in
  message_of_body ~body 
  >>= answer_message ~server_state ?token
  >>= fun msg ->
  return (`Message (`Json, msg))

let html_page () = Client_html.gui_page

let gui_service ~server_state ~body req =
  let token = token_parameter req in
  Authentication.ensure_can server_state.authentication ?token `Browse_gui
  >>= fun () ->
  return (`Page (html_page ()))

(** {2 Dispatcher} *)

let handle_request ~server_state ~body req : (answer, _) Deferred_result.t =
  Log.(s "Request-in: " % sexp Cohttp.Request.sexp_of_t req
       @ verbose);
  match Uri.path (Cohttp.Request.uri req) with
  | "/hello" -> return `Unit
  | "/api" -> api_service ~server_state ~body req
  | "/apijsonp" ->
    (* api_service ~server_state ~body req *)
    let token =
      Uri.get_query_param (Cohttp.Request.uri req) "token" in
    let body = 
      Uri.get_query_param (Cohttp.Request.uri req) "message" in
    begin match body with
    | Some s -> return s
    | None -> wrong_request "missing jsonp-message" ""
    end
    >>= fun body ->
    message_of_body body
    >>= fun up_msg ->
    answer_message ~server_state ?token up_msg
    >>= fun down_msg ->
    let callback =
      Uri.get_query_param (Cohttp.Request.uri req) "callback" in
    let page =
      fmt "window.%s({ \"message\" : %S })"
        Option.(value callback ~default:"missing_callback")
        (Protocol.Down_message.serialize down_msg |> Uri.pct_encode)
    in
    Log.(s "Returning "
         % i (String.length page) %s " bytes"
         %sp % parens (s "Callback: " % OCaml.option quote callback)
         @ verbose);
    return (`Page page)
  | "/gui" -> gui_service ~server_state ~body req
  | other ->
    wrong_request "Wrong path" other


(** {2 Start/Stop The Server} *)

let mandatory_for_starting opt ~msg =
  Deferred_result.some opt ~or_fail:(`Start_server_error msg)

let die_command = "die"
let reload_authorized_tokens = "reload-auth"

let reload_authentication ~server_state =
  Authentication.reload server_state.authentication
  >>= fun authentication ->
  server_state.authentication <- authentication;
  return ()

let start_listening_on_command_pipe ~server_state =
  let conf = server_state.server_configuration in
  match Configuration.command_pipe conf with
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
              begin Engine.unload server_state.state
                >>= function
                | `Ok () -> exit 0
                | `Error e ->
                  Log.(s "Could not unload engine:"  % sp
                       % s (Error.to_string e) @ error);
                  exit 10
              end
            | reload_auth when reload_auth = reload_authorized_tokens ->
              begin reload_authentication ~server_state
                >>= function
                | `Ok () -> return ()
                | `Error e ->
                  Log.(s "Could not reload Authentication:" 
                       % Authentication.log server_state.authentication
                       % s": " % s (Error.to_string e) @ error);
                  return ()
              end
              >>= fun () ->
              read_loop ~error_count ()
            | tag when String.sub tag ~index:0  ~length:3 = Some "tag" ->
              let length = String.length tag - 3 in
              Engine.Measure.tag server_state.state
                (String.sub_exn tag ~index:3 ~length);
              read_loop ~error_count ()
            | "flush-measurements" ->
              Engine.Measurements.flush server_state.state
              >>= fun result ->
              begin match result with
              | `Ok () -> read_loop ~error_count ()
              | `Error e ->
                Log.(s "Could not flush the measurements: " 
                     % s (Error.to_string e) @ error);
                return ()
              end
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

let start_engine_loop ~server_state =
  let time_step = 1. in
  let time_factor = 2. in
  let max_sleep = 120. in
  let rec loop previous_sleep =
    begin
      Engine.Run_automaton.fix_point server_state.state
      >>< function
      | `Ok (`Steps step_count) ->
        let seconds =
          if step_count = 1 then
            min (previous_sleep *. time_factor) max_sleep
          else
            time_step
        in
        Log.(s "Successful fix-point: "
             % parens (i step_count % s " steps") %n
             % s "Sleeping " % f seconds % s " s" @ verbose);
        return seconds
      | `Error e ->
        Log.(s "Errorneous fix-point: "
             % s (Error.to_string e) %n
             % s "Sleeping " % f time_step % s " s" @ verbose);
        return time_step
    end
    >>= fun seconds ->
    Deferred_list.pick_and_cancel [
      System.sleep seconds;
      begin
        Light.try_to_pass server_state.loop_traffic_light
        >>= fun () ->
        server_state.loop_traffic_light.Light.color <- `Red;
        return ()
      end;
    ]
    >>= fun () ->
    loop seconds 
  in
  Lwt.ignore_result (loop time_step)

let start_listening_on_connections ~server_state =
  let return_error_messages, how =
    Configuration.return_error_messages server_state.server_configuration,
    Configuration.listen_to server_state.server_configuration in
  begin match how with
  | `Tls (certfile, keyfile, port) ->
    Deferred_result.wrap_deferred
      ~on_exn:(function
        | e -> `Start_server_error (Printexc.to_string e))
      Lwt.(fun () ->
          let mode =
            `OpenSSL (
              `Crt_file_path certfile,
              `Key_file_path keyfile,
              `No_password, `Port port) in
          (* let sockaddr = Lwt_unix.(ADDR_INET (Unix.inet_addr_any, port)) in *)
          let request_callback _ request body =
            let connection_id = Unique_id.create () in
            Engine.Measure.incomming_request
              server_state.state ~connection_id ~request;
            handle_request ~server_state ~body request 
            >>= fun high_level_answer ->
            let respond_string ?headers ~status ~body () =
              (* Cohttp's `respond_string` function does not flush in `0.12.0`
                 so here it is pasted and fixed.
                 See <https://github.com/mirage/ocaml-cohttp/issues/205>.  *)
              let res = Cohttp.Response.make ~status ~flush:true
                  ~encoding:(Cohttp.Transfer.Fixed (Int64.of_int (String.length body)))
                  ?headers () in
              let body = Cohttp_lwt_body.of_string body in
              return (res,body)
            in
            begin match high_level_answer with
            | `Ok `Unit ->
              respond_string ~status:`OK  ~body:"" ()
            | `Ok (`Message (`Json, msg)) ->
              let body = Protocol.Down_message.serialize msg in
              respond_string ~status:`OK  ~body ()
            | `Ok (`Page body) ->
              respond_string ~status:`OK  ~body ()
            | `Error e ->
              Log.(s "Error while handling the request: "
                   % s (Error.to_string e) @ error);
              let body =
                if return_error_messages
                then "Error: " ^ (Error.to_string e)
                else "Undisclosed server error" in
              respond_string ~status:`Not_found ~body ()
            end
            >>= fun ((response, body) as cohttp_answer) ->
            let response_log =
              Cohttp.Response.sexp_of_t response
              |> Sexplib.Sexp.to_string_hum ~indent:2 in
            let body_length =
              match body with
              | `Stream s -> -1
              | `String s -> String.length s
              | `Strings l -> 
                List.fold ~init:0 ~f:(fun a b -> a + String.length b) l
              | `Empty -> 0
            in
            Engine.Measure.end_of_request server_state.state
              ~connection_id ~request ~response_log ~body_length;
            return cohttp_answer
          in
          let conn_closed (_, conn_id) =
            Log.(sf "conn %S closed" (Cohttp.Connection.to_string conn_id) 
                 @ verbose);
          in
          Cohttp_lwt_unix.Server.(
            create ~mode (make ~callback:request_callback ~conn_closed ()))
        )
  end

let stop ~configuration =
  Deferred_result.some ~or_fail:(`Stop_server_error "No command-pipe configured")
    (Configuration.command_pipe configuration)
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

let status ~configuration =
  let local_server_uri =
    match Configuration.listen_to configuration with
    | `Tls (_, _, port) ->
      Uri.make ~scheme:"https" ~host:"127.0.0.1" ~path:"/hello" () ~port in
  Log.(s "Trying GET on " % uri local_server_uri @ verbose);
  begin
    System.with_timeout 5. ~f:(fun () ->
        wrap_deferred
          ~on_exn:(fun e -> `Get_exn e) (fun () ->
              Cohttp_lwt_unix.Client.call `GET local_server_uri)
      ) 
    >>< function
    | `Ok (response, body) ->
      Log.(s "Response: " 
           % sexp Cohttp.Response.sexp_of_t response @ verbose);
      begin match Cohttp.Response.status response with
      | `OK -> return `Running
      | other -> return (`Wrong_response response)
      end
    | `Error (`Get_exn
                (Unix.Unix_error (Unix.ECONNREFUSED, "connect", ""))) ->
      return (`Not_responding "connection refused")
    | `Error (`System (`With_timeout _, `Exn e)) ->
      fail (`Failure (Printexc.to_string e))
    | `Error (`Timeout _) ->
      return (`Not_responding "connection timeouted")
    |  `Error (`Get_exn other_exn) ->
      fail (`Server_status_error (Printexc.to_string other_exn))
  end


let start ~configuration  =
  Log.(s "Start-Server: Checking status" @ verbose);
  begin
    status ~configuration
    >>= function
    | `Running -> fail (`Start_server_error "Server seems to be already running")
    | `Wrong_response c ->
      fail (`Start_server_error
              (fmt "An unrecognized HTTPS Server seems to be already there; \
                    response: %s"
                 (Cohttp.Response.sexp_of_t c |> Sexplib.Sexp.to_string_hum)))
    | `Not_responding _ ->
      Log.(s "Status answers `Not_responding`" @ verbose);
      return ()
  end
  >>= fun () ->
  Log.(s "Start-Server: Loading the Engine" @ verbose);
  Engine.load (Configuration.server_engine configuration) 
  >>= fun engine ->
  Log.(s "Start-Server: Loading authentication config" @ verbose);
  Authentication.load (Configuration.authorized_tokens configuration)
  >>= fun authentication ->
  let server_state =
    Server_state.create ~authentication ~state:engine configuration in
  Log.(s "Start-Server: Starting the Engine loop" @ verbose);
  start_engine_loop ~server_state;
  Log.(s "Start-Server: Starting listening on command-pipe" @ verbose);
  start_listening_on_command_pipe ~server_state
  >>= fun () ->
  Log.(s "Start-Server: Starting listening on connections" @ verbose);
  start_listening_on_connections ~server_state
