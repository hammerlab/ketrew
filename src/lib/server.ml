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

open Logging.Global
include Make_module_error_and_info(struct
    let module_name = "Server"
  end)

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

  let valid {value; _} =
    let valid_chars = B64.uri_safe_alphabet ^ "=" in
    let invalid_char = fun c -> not (String.exists valid_chars ((=) c)) in
    match String.find ~f:invalid_char value with
    | Some _ -> false
    | None -> true

  let split_and_trim line =
    String.split line ~on:(`Character ' ')
    |> List.map ~f:(fun t -> String.strip ~on:`Both t)
    |> List.filter ~f:(fun s -> s <> "")

  let load_file file =
    Log.(s "Authentication: loading " % quote file @ verbose);
    IO.read_file file
    >>= fun content ->
    let valid_tokens =
      String.split content ~on:(`Character '\n')
      |> List.filter_map ~f:(fun line ->
          match split_and_trim line with
          | comment :: _ when String.get comment ~index:1 = Some '#' -> None
          | name :: value :: comments ->
             let token =  {name; value; comments} in
             begin match valid token with
             | true -> Some token
             | false ->  Log.(s "Invalid character(s) in token: " % OCaml.string value % s " in file "
                              % OCaml.string file @ warning);
                         None
             end
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
      | `Inline (name, value) ->
         let token = {name; value; comments = []} in
         let tokens =
           match valid token with
           | true -> [token]
           | false ->
              Log.(s "Invalid character(s) in token: " % OCaml.string value @ warning);
              [] in
         return tokens)
    >>| List.concat
    >>= fun valid_tokens ->
    return {valid_tokens; authentication_input = meta_tokens}

  let reload {authentication_input; _} = load authentication_input

  let able_to_do ~read_only_mode = function
    | `Browse_gui
    | `See_server_status
    | `See_targets
    | `Query_targets  -> true
    | `Kill_targets
    | `Restart_targets
    | `Play_with_process_holder
    | `Submit_targets -> not read_only_mode

  let can t ~read_only_mode ?token stuff =
    let token_is_valid tok =
      List.exists t.valid_tokens ~f:(fun x -> x.value = tok) in
    let valid_token =
      Option.value ~default:false (Option.map ~f:token_is_valid token) in
    valid_token && able_to_do stuff ~read_only_mode

  let ensure_can t ~read_only_mode ?token do_stuff =
    if can t ?token do_stuff ~read_only_mode then
      return ()
    else
      wrong_request "Authentication" "Insufficient credentials"

end (* Authentication *)

(** The state maintained by the HTTP server. *)
module Server_state = struct
  module Deferred_queries = struct
    type query_content =
      | List_of_ids of string list
    type query = {
      id: string;
      content: query_content;
      birthdate: Time.t;
    }
    type t = {
      table: (string, query) Hashtbl.t
    }

    let create () = {table = Hashtbl.create 42}

    let add_list_of_ids t ids =
      let id = Unique_id.create () in
      let content = List_of_ids ids in
      let birthdate = Time.now () in
      Hashtbl.add t.table id {id; content; birthdate};
      id

    let make_message t ~id ~index ~length : Protocol.Down_message.t =
      match Hashtbl.find t.table id with
      | { content = List_of_ids l; _ } ->
        `List_of_target_ids (List.take (List.drop l index) length)
      | exception _ ->
        `Missing_deferred

    let garbage_collection t =
      let now = Time.now () in
      let to_remove =
        Hashtbl.fold
          (fun id {birthdate; _} prev  -> 
             if birthdate +. 60. *. 60. *. 2. > now
             then id :: prev
             else prev)
          t.table [] in
      List.iter to_remove ~f:(Hashtbl.remove t.table);
      ()

    let markup t =
      let open Display_markup in
      description_list [
        "Query Number", textf "%d" (Hashtbl.length t.table);
      ]


  end (* Deferred_queries *)

  type t = {
    state: Engine.t;
    server_configuration: Configuration.server;
    mutable authentication: Authentication.t;
    loop_traffic_light: Light.t;
    all_connections:
      (string,
       [`Open of Time.t | `Request of Time.t | `Closed of Time.t] list)
        Hashtbl.t;
    deferred_queries: Deferred_queries.t;
    process_holder: Process_holder.t;
  }
(*M
The `loop_traffic_light` is “red” for the server  by default but some services
can set it to `Green to wake it up earlier and do things.

For example, after adding targets, the server will be woken-up to start running
the targets and not wait for the next “loop timeout.”
See [Pvem_lwt_unix.LIGHT](http://seb.mondet.org/software/pvem_lwt_unix/api/Pvem_lwt_unix.LIGHT.html)
for more details.
M*)
  let create ~state ~process_holder ~authentication server_configuration =
    let loop_traffic_light = Light.create () in
    {state; authentication; server_configuration; loop_traffic_light;
     all_connections = Hashtbl.create 42; process_holder;
     deferred_queries = Deferred_queries.create ()}

  let markup t =
    let open Display_markup in
    description_list [
      "Connections", textf "%d" (Hashtbl.length t.all_connections);
      "Deferred_queries", Deferred_queries.markup t.deferred_queries;
    ]

end (* Server_state *)
open Server_state

type answer = [
  | `Unit
  | `Message of [ `Json ] * Protocol.Down_message.t
  | `Page of string
]
(** A service can reply to one of these cases; or error. *)

type 'error service =
  server_state:Server_state.t ->
  body:Cohttp_lwt_body.t ->
  Cohttp.Request.t ->
  (answer, 'error) Deferred_result.t
(** A service is something that replies an [answer] on a ["/<path>"] URL. *)

(** Get the ["token"] parameter from an URI. *)
let token_parameter req =
  Uri.get_query_param (Cohttp.Request.uri req) "token"

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
  | `GET -> return ()
  | other ->
    wrong_request "wrong method, wanted get" (Cohttp.Code.string_of_method other)
  end

(** Check that it is a [`POST], get the {i non-empty} body; or fail. *)
let get_post_body request ~body =
  begin match Cohttp.Request.meth request with
  | `POST ->
    wrap_deferred ~on_exn:(fun e -> `IO (`Exn e))
      (fun () -> Cohttp_lwt_body.to_string  body)
  | other ->
    wrong_request "wrong method, wanted post" (Cohttp.Code.string_of_method other)
  end

(** Grab and deserialize a POST body into an up_message. *)
let message_of_body ~body =
  wrap_preemptively ~on_exn:(fun e -> `Failure (Printexc.to_string e))
    (fun () -> Protocol.Up_message.deserialize_exn body)

(** {2 Services: Answering Requests} *)

let get_targets_from_ids ~server_state target_ids =
  begin match target_ids  with
  | [] ->
    Engine.all_targets server_state.state
    >>| List.map ~f:(fun trt -> (Target.id trt, trt))
  | more ->
    Deferred_list.while_sequential more ~f:(fun id ->
        Engine.get_target server_state.state id
        >>< function
        | `Ok t -> return (Some (id, t))
        | `Error e -> 
          log_error e Log.(s "Error while getting the target " % s id);
          return None)
    >>| List.filter_opt
  end

let answer_get_targets ?(summaries=false) ~server_state target_ids =
  get_targets_from_ids ~server_state target_ids
  >>= fun targets ->
  begin match summaries with
  | false ->
    return (`List_of_targets (List.map ~f:snd targets))
  | true ->
    return (`List_of_target_summaries
              (List.map targets ~f:(fun (id, t) ->
                 (id, Target.Summary.create t))))
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
  log_info
    Log.(s "Calling query " % quote query % s " on " % Target.log target);
  begin
    Plugin.call_query ~target query
    >>< function
    | `Ok string -> 
      return (`Query_result string)
    | `Error error_log ->
      return (`Query_error (Log.to_long_string error_log))
  end


let answer_add_targets ~server_state ~targets =
  log_info
    Log.(s "Adding " % i (List.length targets) % s " targets");
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


let block_if_empty_at_most ~server_state
    ~get_values
    ~should_send
    ~send
    options =
  begin match
    List.find options ~f:(function `Block_if_empty_at_most _ -> true)
  with
  | None ->
    get_values ()
    >>= fun values ->
    send values
  | Some (`Block_if_empty_at_most req_block_time) ->
    let start_time = Time.now () in
    let sleep_time =
      Configuration.block_step_time server_state.server_configuration in
    let block_time =
      let max_block_time =
        Configuration.max_blocking_time server_state.server_configuration in
      if req_block_time > max_block_time then (
        log_info
          Log.(s "requested block-time: " % f req_block_time %n
               % s "Using max instead: " % f max_block_time);
        max_block_time
      ) else
        req_block_time in
    let rec loop () =
      let now =  Time.now () in
      get_values ()
      >>= fun values ->
      match start_time +. block_time < now with
      | true ->
        log_info
          Log.(s "block_if_empty_at_most + blocking → returns " % n
               % s "start_time: " % Time.log start_time % n
               % s "block_time: " % f block_time % n
               % s "req_block_time: " % f req_block_time % n
               % s "now: " % Time.log now);
        send values
      | false ->
        begin match should_send values with
        | false ->
          (System.sleep sleep_time >>< fun _ -> return ())
          >>= fun () ->
          loop ()
        | true -> send values
        end
    in
    loop ()
  end

let answer_get_target_ids ~server_state (query, options) =
  block_if_empty_at_most ~server_state options
    ~get_values:(fun () ->
        Engine.get_list_of_target_ids server_state.state query)
    ~should_send:(function [] -> false | _ -> true)
    ~send:begin fun v ->
      match List.length v with
      | small when small < 1001 ->
        return (`List_of_target_ids v)
      | big ->
        let answer_id = 
          Deferred_queries.add_list_of_ids server_state.deferred_queries v in
        let msg = `Deferred_list_of_target_ids (answer_id, big) in
        log_info Log.(s "answer_get_target_ids -> Deferred_list_of_target_ids "
                      % s answer_id % s ", " % i big);
        return msg
    end

let answer_get_server_status ~server_state =
  let tls =
    match !Conduit_lwt_unix.tls_library with
    | Conduit_lwt_unix.OpenSSL  -> `OpenSSL
    | Conduit_lwt_unix.Native  -> `Native
    | Conduit_lwt_unix.No_tls -> `None
  in
  let database =
    let configuration = server_state.server_configuration in
    Configuration.database_parameters (Configuration.server_engine configuration)
  in
  let status =
    Protocol.Server_status.create ~database ~tls
      ~time:Time.(now ())
      ~read_only:(Configuration.read_only_mode server_state.server_configuration)
      ~preemptive_bounds:(Lwt_preemptive.get_bounds ()) 
      ~preemptive_queue:(Lwt_preemptive.get_max_number_of_threads_queued ()) 
      ~libev:(Lwt_sys.have `libev)
      ~gc:(Gc.quick_stat ())
  in
  return (`Server_status status)

let answer_get_target_flat_states ~server_state
    (time_constrain, target_ids, options) =
  block_if_empty_at_most ~server_state options
    ~get_values:(fun () ->
        get_targets_from_ids ~server_state target_ids
        >>= fun targets ->
        log_info
          Log.(s "answer_get_target_flat_states computing states for "
               % OCaml.list s target_ids);
        let states =
          List.filter_map targets ~f:(fun (id, trgt) ->
              let flat_state = Target.State.Flat.of_state (Target.state trgt) in
              match time_constrain with
              | `All -> Some (id, flat_state)
              | `Since ti ->
                Target.State.Flat.since flat_state ti
                |> Option.map ~f:(fun st -> (id, st))
            )
        in
        return states)
    ~should_send:(fun states ->
        let total_items =
          (List.fold states ~init:0 ~f:(fun prev (_, flat) ->
               prev + (List.length flat.Target.State.Flat.history)))
        in
        log_info
          Log.(s "answer_get_target_flat_states" % n
               % s "States: " % i (List.length states) % n
               % s "Total items: " % i  total_items);
        total_items <> 0)
    ~send:(fun states ->
        return (`List_of_target_flat_states states))


let answer_message ~server_state ?token msg =
  let with_capability cap =
    let read_only_mode =
      Configuration.read_only_mode server_state.server_configuration in
    Authentication.ensure_can server_state.authentication ?token cap
      ~read_only_mode
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
  | `Get_deferred (id, index, length) ->
    with_capability `See_targets
    >>= fun () ->
    let msg =
      Deferred_queries.make_message server_state.deferred_queries
        ~id ~index ~length in
    return msg
  | `Process p_msg ->
    with_capability `Play_with_process_holder
    >>= fun () ->
    Process_holder.answer_message server_state.process_holder p_msg
    >>= fun p_down ->
    return (`Process p_down)

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
    ~read_only_mode:(Configuration.read_only_mode
                       server_state.server_configuration)
  >>= fun () ->
  return (`Page (html_page ()))

(** {2 Dispatcher} *)

let handle_request ~server_state ~body req : (answer, _) Deferred_result.t =
  log_info Log.(s "Request-in: " % sexp Cohttp.Request.sexp_of_t req);
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

let execute_deferred server_state f =
  Lwt.(
    f ()
    >>= function
    | `Ok () -> return ()
    | `Error e ->
      log_error e Log.(s "execute_deferred");
      return ()
  )

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
              log_info
                Log.(s "Server killed by “die” command " 
                     % parens (OCaml.string file_path));
              begin Engine.unload server_state.state
                >>= function
                | `Ok () -> exit 0
                | `Error e ->
                  log_error e Log.(s "Could not unload engine");
                  exit 10
              end
            | reload_auth when reload_auth = reload_authorized_tokens ->
              begin reload_authentication ~server_state
                >>= function
                | `Ok () -> return ()
                | `Error e ->
                  log_error e 
                    Log.(s "Could not reload Authentication: " 
                         % Authentication.log server_state.authentication);
                  return ()
              end
              >>= fun () ->
              read_loop ~error_count ()
            | "dump-all-connections" ->
              let uniq = Unique_id.create () in
              Deferred_result.(
                IO.with_out_channel
                  (`Overwrite_file ("all-connections-" ^ uniq))
                  ~f:(fun o ->
                      Hashtbl.fold (fun id status prev ->
                          prev >>= fun () ->
                          IO.write o (fmt "%s:\n" id)
                          >>= fun () ->
                          List.fold (List.rev status) ~init:(return ())
                            ~f:(fun prev status ->
                                prev >>= fun () ->
                                let ti = Time.to_filename in
                                IO.write o
                                  (fmt "    %s\n"
                                     begin match status with
                                     | `Open t -> fmt "Open %s" (ti t)
                                     | `Request t -> fmt "Request %s" (ti t)
                                     | `Closed t -> fmt "Closed %s" (ti t)
                                     end))
                        )
                        server_state.all_connections
                        (return ()))
                >>< function
                | `Ok () -> return ()
                | `Error e ->
                  log_error e Log.(s "dump-all-connections");
                  return ()
              )
              >>= fun _ ->
              read_loop ~error_count ()
            | get_log when String.sub get_log 0 (8) = Some "get-log:" ->
              execute_deferred server_state Deferred_result.(fun () ->
                  begin match String.split get_log ~on:(`Character ':') with
                  | "get-log" :: path :: format :: [] ->
                    begin match format with
                    | "json" -> return `Json
                    | "txt" -> return `Txt
                    | _ -> fail (`Failure (fmt "wrong format: %S" format))
                    end
                    >>= fun format ->
                    Log.(s "Append logs to " %quote path @ verbose);
                    Logging.Global.append_to_file ~path ~format
                  | _ ->
                    log_info Log.(s "Wrong get-log: " % quote get_log);
                    return ()
                  end
                )
              >>= fun () ->
              read_loop ~error_count ()
            |  other ->
              log_info
                Log.(s "Cannot understand command: " % OCaml.string other);
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
        let markup =
          Display_markup.(
            description_list [
              "Steps", textf "%d" step_count;
              "Sleep", time_span seconds;
            ]) in
        return (markup, seconds)
      | `Error e ->
        let markup =
          Display_markup.(
            description_list [
              "Error", Error.to_string e |> text;
              "Sleep", time_span time_step;
            ]) in
        return (markup, time_step)
    end
    >>= fun (fix_point_markup, seconds) ->
    begin match Configuration.log_path server_state.server_configuration with
    | None  -> return ()
    | Some path ->
      System.ensure_directory_path path
      >>= fun () ->
      let new_file = path // Unique_id.create () ^ ".json" in
      Logging.Global.append_to_file ~path:new_file ~format:`Json
      >>= fun () ->
      Logging.Global.clear ()
    end
    >>= fun () ->
    Deferred_queries.garbage_collection server_state.deferred_queries;
    Deferred_list.pick_and_cancel [
      begin System.sleep seconds >>= fun () -> return `Slept end;
      begin
        Light.try_to_pass server_state.loop_traffic_light
        >>= fun () ->
        server_state.loop_traffic_light.Light.color <- `Red;
        return `Woken_up
      end;
    ]
    >>= fun how_awake ->
    Logger.(
      log
        Display_markup.(
          description_list [
            "End-of-engine-loop",
            text (match how_awake with `Slept -> "Slept" | `Woken_up -> "Woken-up");
            "State", Server_state.markup server_state;
            "Fix-point", fix_point_markup;
          ]));
    loop seconds 
  in
  Lwt.ignore_result (loop time_step)

let start_listening_on_connections ~server_state =
  let return_error_messages, how =
    Configuration.return_error_messages server_state.server_configuration,
    Configuration.listen_to server_state.server_configuration in
  Deferred_result.wrap_deferred
    ~on_exn:(function
      | e -> `Start_server_error (Printexc.to_string e))
    Lwt.(fun () ->
        let mode =
          (* Convert to Conduit_lwt_unix.server *)
          match how with
          | `Tls (certfile, keyfile, port) ->
            `TLS (
              (* `TLS means that conduit will do:
                 match Sys.getenv "CONDUIT_TLS" with
                 | "native" | "Native" | "NATIVE" -> Native
                 | _ -> OpenSSL *)
              `Crt_file_path certfile,
              `Key_file_path keyfile,
              `No_password, `Port port)
          | `Tcp port -> `TCP (`Port port)
        in
        let request_callback (_, conn_id) request body =
          let id = Cohttp.Connection.to_string conn_id in
          begin match Hashtbl.find server_state.all_connections id with
          | some ->
            Hashtbl.replace server_state.all_connections
              id (`Request Time.(now ()) :: some);
          | exception _ ->
            Hashtbl.replace server_state.all_connections 
              id (`Request Time.(now ()) :: []);
          end;
          handle_request ~server_state ~body request 
          >>= fun high_level_answer ->
          begin match high_level_answer with
          | `Ok `Unit ->
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body:"" ()
          | `Ok (`Message (`Json, msg)) ->
            let body = Protocol.Down_message.serialize msg in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body ()
          | `Ok (`Page body) ->
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body ()
          | `Error e ->
            log_error e
              Log.(s "Error while handling the request: conn_id: "
                   % s id % s ", request: "
                   % sexp Cohttp.Request.sexp_of_t request);
            let body =
              if return_error_messages
              then "Error: " ^ (Error.to_string e)
              else "Undisclosed server error" in
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body ()
          end
          >>= fun ((response, body) as cohttp_answer) ->
          return cohttp_answer
        in
        let conn_closed (_, conn_id) =
          let id = Cohttp.Connection.to_string conn_id in
          begin match Hashtbl.find server_state.all_connections id with
          | some ->
            Hashtbl.replace server_state.all_connections
              id (`Open Time.(now ()) :: some);
          | exception _ ->
            Hashtbl.replace server_state.all_connections 
              id (`Open Time.(now ()) :: []);
          end;
          Log.(sf "conn %S closed" (Cohttp.Connection.to_string conn_id) 
               @ verbose);
        in
        Cohttp_lwt_unix.Server.(
          create ~mode (make ~callback:request_callback ~conn_closed ()))
      )

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
      Uri.make ~scheme:"https" ~host:"127.0.0.1" ~path:"/hello" () ~port
    | `Tcp (port) ->
      Uri.make ~scheme:"http" ~host:"127.0.0.1" ~path:"/hello" () ~port
  in
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
  Log.(s "Set preemptive bounds: 10, 52" @ verbose);
  Lwt_preemptive.init 10 52 (fun str ->
      Log.(s " Lwt_preemptive error: " % s str @ error);
    );
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
  Process_holder.load ()
  >>= fun process_holder ->
  Log.(s "Start-Server: Loading the Engine" @ verbose);
  Engine.load (Configuration.server_engine configuration) 
  >>= fun engine ->
  Log.(s "Start-Server: Loading authentication config" @ verbose);
  Authentication.load (Configuration.authorized_tokens configuration)
  >>= fun authentication ->
  let server_state =
    Server_state.create
      ~process_holder ~authentication ~state:engine configuration in
  log_info Log.(s "Start-Server: Starting the Engine loop");
  start_engine_loop ~server_state;
  log_info Log.(s "Start-Server: Starting listening on command-pipe");
  start_listening_on_command_pipe ~server_state
  >>= fun () ->
  Log.(s "Start-Server: Starting listening on connections" @ verbose);
  log_info Log.(s "Start-Server: Starting listening on connections");
  start_listening_on_connections ~server_state
