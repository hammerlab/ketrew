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



(** A common error that simply means “invalid argument”. *)
let wrong_request short long = fail (`Wrong_http_request (short, long))

(** Module dealing with access tokens and access rights. There are no
    “sessions” here; just a file that looks like SSH's `authorized_keys`, and a
    function: token × capability → bool.

    Capabilities are defined with polymorphic variants.
*)
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

(** The state maintained by the HTTP server. *)
module Server_state = struct

  type t = {
    state: Ketrew_engine.t;
    server_configuration: Ketrew_configuration.server;
    authentication_file: string;
    mutable authentication: Authentication.t;
    loop_traffic_light: Light.t;
  }
(*M
The `loop_traffic_light` is “red” for the server  by default but some services
can set it to `Green to wake-up it earlier and do things.

For example, after adding targets, the server will be woken-up to start running
the targets and not wait for the next “loop timeout.”
M*)
  let create ~state ~authentication ~authentication_file server_configuration =
    let loop_traffic_light = Light.create () in
    {state; authentication; authentication_file;
     server_configuration; loop_traffic_light;}


end
open Server_state

type answer = [
  | `Unit
  | `Message of [ `Json ] * Ketrew_protocol.Down_message.t
]
(** A service can replay one of those cases; or an error. *)

type 'error service =
  server_state:Server_state.t ->
  body:Cohttp_lwt_body.t ->
  Cohttp_lwt_unix.Server.Request.t ->
  (answer, 'error) Deferred_result.t
(** A service is something that replies an [answer] on a ["/<path>"] URL. *)

(** Get the ["token"] parameter from an URI. *)
let token_parameter req =
  let token =
    Uri.get_query_param (Cohttp_lwt_unix.Server.Request.uri req) "token" in
  Log.(s "Got token: " % OCaml.option quote token @ very_verbose);
  token

(** Get a parameter or fail. *)
let mandatory_parameter req ~name =
  match Uri.get_query_param (Cohttp_lwt_unix.Server.Request.uri req) name with
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
  begin match Cohttp_lwt_unix.Server.Request.meth request with
  | `GET ->
    Log.(s "It is a GET request" @ very_verbose);
    return ()
  | other -> wrong_request "wrong method" (Cohttp.Code.string_of_method other)
  end

(** Check that it is a [`POST], get the {i non-empty} body; or fail. *)
let get_post_body request ~body =
  begin match Cohttp_lwt_unix.Server.Request.meth request with
  | `POST ->
    Log.(s "It is a GET request" @ very_verbose);
    wrap_deferred ~on_exn:(fun e -> `IO (`Exn e))
      (fun () -> Cohttp_lwt_body.to_string  body)
  | other ->
    wrong_request "wrong method" (Cohttp.Code.string_of_method other)
  end

(** {2 Services} *)

let targets_service: _ service = fun ~server_state ~body req ->
  check_that_it_is_a_get req >>= fun () ->
  let token = token_parameter req in
  Authentication.ensure_can server_state.authentication ?token `See_targets
  >>= fun () ->
  let target_ids =
    Uri.get_query_param' (Cohttp_lwt_unix.Server.Request.uri req) "id"
    |> Option.value ~default:[] in
  format_parameter req
  >>= fun response_format ->
  begin match target_ids  with
  | [] ->
    Ketrew_engine.current_targets server_state.state
  | more ->
    Deferred_list.while_sequential more ~f:(fun id ->
        Ketrew_engine.get_target server_state.state id
        >>< function
        | `Ok t -> return (Some t)
        | `Error e -> 
          Log.(s "Error while getting the target " % s id % s ": "
               % s (Ketrew_error.to_string e) @ error);
          return None)
    >>| List.filter_opt
  end
  >>= fun targets ->
  return (`Message
            (response_format,
             `List_of_targets
               (List.map ~f:Ketrew_target.to_serializable targets)))

let target_available_queries_service ~server_state ~body req =
  check_that_it_is_a_get req >>= fun () ->
  let token = token_parameter req in
  Authentication.ensure_can server_state.authentication ?token `Query_targets
  >>= fun () ->
  mandatory_parameter req ~name:"id"
  >>= fun target_id ->
  format_parameter req
  >>= fun response_format ->
  Ketrew_engine.get_target server_state.state target_id
  >>= fun target ->
  let msg =
    `List_of_query_descriptions (
      Ketrew_plugin.additional_queries target
      |> List.map ~f:(fun (a, l) -> a, Log.to_long_string l)
    ) in
  return (`Message (response_format, msg))

let target_call_query_service ~server_state ~body req =
  check_that_it_is_a_get req >>= fun () ->
  let token = token_parameter req in
  Authentication.ensure_can server_state.authentication ?token `Query_targets
  >>= fun () ->
  mandatory_parameter req ~name:"id"
  >>= fun target_id ->
  format_parameter req
  >>= fun response_format ->
  mandatory_parameter req ~name:"query"
  >>= fun query_name ->
  Ketrew_engine.get_target server_state.state target_id
  >>= fun target ->
  Log.(s "Calling query " % quote query_name % s " on "
       % Ketrew_target.log target @ very_verbose);
  begin
    Ketrew_plugin.call_query ~target query_name
    >>< function
    | `Ok string -> 
      return (`Message (response_format, `Query_result string))
    | `Error error_log ->
      wrong_request "Failed Query" (Log.to_long_string error_log)
  end

let message_of_body ~body ~select =
  wrap_preemptively ~on_exn:(fun e -> `Failure (Printexc.to_string e))
    (fun () -> Ketrew_protocol.Post_message.deserialize_exn body)
  >>= fun message ->
  begin match select message with
  | Some t -> return t
  | None ->
    fail (`Failure 
            (fmt "wrong post-message: %s"
               (Ketrew_protocol.Post_message.to_string_hum message)))
  end

let add_targets_service  ~server_state ~body req =
  get_post_body req ~body 
  >>= fun body ->
  message_of_body ~body ~select:(function
    | `List_of_targets t -> Some (List.map ~f:Ketrew_target.of_serializable t)
    | _ -> None)
  >>= fun targets ->
  Log.(s "Adding " % i (List.length targets) % s " targets" @ normal);
  Ketrew_engine.add_targets server_state.state targets
  >>= fun () ->
  (*
    Deferred_list.while_sequential targets ~f:(fun t ->
      let original_id = Ketrew_target.id t in
      Ketrew_engine.get_target server_state.state original_id
      >>= fun freshen ->
      return (Ketrew_protocol.Down_message.added_target
                ~original_id ~fresh_id:(Ketrew_target.id freshen))
    )
  >>= fun added_targets ->
 *)
  Light.green server_state.loop_traffic_light;
  return (`Message (`Json, `Ok))

let action_on_ids_service: [`Kill  | `Restart] -> _ service = 
  fun what_to_do ~server_state ~body req ->
    get_post_body req ~body 
    >>= fun body ->
    message_of_body ~body ~select:(function
      | `List_of_target_ids t -> Some t
      | _ -> None)
    >>= fun target_ids ->
    Deferred_list.while_sequential target_ids (fun id ->
        begin match what_to_do with
        | `Kill -> Ketrew_engine.kill server_state.state id
        | `Restart ->
          Ketrew_engine.restart_target server_state.state id
          >>= fun (_ : Ketrew_target.id) ->
          return ()
        end)
    >>= fun (_ : unit list) ->
    Light.green server_state.loop_traffic_light;
    return (`Message (`Json, `Ok))

let list_cleanable_targets ~server_state ~body req =
  check_that_it_is_a_get req >>= fun () ->
  mandatory_parameter req ~name:"howmuch"
  >>= fun how_much_str ->
  begin match how_much_str with
  | "soft" -> return `Soft
  | "hard" -> return `Hard
  | other ->
    failwith (fmt "wrong-parameter: %S expecting 'soft' or 'hard'" other)
  end
  >>= fun how_much ->
  Log.(s "list_cleanable_targets: getting graph" @ verbose);
  Ketrew_engine.Target_graph.(
    get_current server_state.state
    >>= fun graph ->
    let  to_kill = targets_to_clean_up graph how_much in
    return (`Message (`Json, `List_of_target_ids to_kill))
  )

(** {2 Dispatcher} *)

let handle_request ~server_state ~body req : (answer, _) Deferred_result.t =
  Log.(s "Request-in: " % sexp Cohttp_lwt_unix.Server.Request.sexp_of_t req
       @ verbose);
  match Uri.path (Cohttp_lwt_unix.Server.Request.uri req) with
  | "/hello" -> return `Unit
  | "/targets" -> targets_service ~server_state ~body req
  | "/target-available-queries" ->
    target_available_queries_service ~server_state ~body req
  | "/target-call-query" -> target_call_query_service ~server_state ~body req
  | "/add-targets" -> add_targets_service  ~server_state ~body req
  | "/kill-targets" ->
    action_on_ids_service `Kill  ~server_state ~body req
  | "/restart-targets" ->
    action_on_ids_service `Restart  ~server_state ~body req
  | "/cleanable-targets" ->
    list_cleanable_targets ~server_state ~body req
  | other ->
    wrong_request "Wrong path" other


(** {2 Start/Stop The Server} *)

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
              begin Ketrew_engine.unload server_state.state
                >>= function
                | `Ok () -> exit 0
                | `Error e ->
                  Log.(s "Could not unload engine:"  % sp
                       % s (Ketrew_error.to_string e) @ error);
                  exit 10
              end
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
            | tag when String.sub tag ~index:0  ~length:3 = Some "tag" ->
              let length = String.length tag - 3 in
              Ketrew_engine.Measure.tag server_state.state
                (String.sub_exn tag ~index:3 ~length);
              read_loop ~error_count ()
            | "flush-measurements" ->
              Ketrew_engine.Measurements.flush server_state.state
              >>= fun result ->
              begin match result with
              | `Ok () -> read_loop ~error_count ()
              | `Error e ->
                Log.(s "Could not flush the measurements: " 
                     % s (Ketrew_error.to_string e) @ error);
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
      Ketrew_engine.Run_automaton.fix_point server_state.state
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
             % s (Ketrew_error.to_string e) %n
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


let start ~configuration  =
  Log.(s "Starting server!" @ very_verbose);
  mandatory_for_starting
    (Ketrew_configuration.authorized_tokens_path configuration)
    ~msg:"Authentication-less server not implemented"
  >>= fun authentication_file ->
  let return_error_messages, how =
    Ketrew_configuration.return_error_messages configuration,
    Ketrew_configuration.listen_to configuration in
  begin match how with
  | `Tls (certfile, keyfile, port) ->
    Authentication.load_file authentication_file
    >>= fun authentication ->
    Ketrew_engine.load (Ketrew_configuration.server_engine configuration) 
    >>= fun engine ->
    Log.(s "Getting database" @ very_verbose);
    Ketrew_engine.database engine
    >>= fun _ ->
    Log.(s "Got database" @ very_verbose);
    let server_state =
      Server_state.create ~authentication ~state:engine
        ~authentication_file configuration
    in
    start_engine_loop ~server_state;
    start_listening_on_command_pipe ~server_state
    >>= fun () ->
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
            Ketrew_engine.Measure.incomming_request
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
              let body = Ketrew_protocol.Down_message.serialize msg in
              respond_string ~status:`OK  ~body ()
            | `Error e ->
              Log.(s "Error while handling the request: "
                   % s (Ketrew_error.to_string e) @ error);
              let body =
                if return_error_messages
                then "Error: " ^ (Ketrew_error.to_string e)
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
            Ketrew_engine.Measure.end_of_request server_state.state
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
    (Ketrew_configuration.command_pipe configuration)
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
    match Ketrew_configuration.listen_to configuration with
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
