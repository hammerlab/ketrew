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

let ensure_can t ~read_only_mode ?token do_stuff =
  if Authentication.can t ?token do_stuff ~read_only_mode then
    return ()
  else
    wrong_request "Authentication" "Insufficient credentials"

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
      | { content = List_of_ids l; birthdate; _ } ->
        `List_of_target_ids (List.take (List.drop l index) length, birthdate)
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
      List.iter to_remove ~f:(Hashtbl.remove t.table)

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
    deferred_queries: Deferred_queries.t;
    mutable status : [ `On | `Off ]; (* For now the status just tracks
                                        whetther `shut_down` has
                                        occured or not *)
  }

(*M
The `loop_traffic_light` is “red” for the server  by default but some services
can set it to `Green to wake it up earlier and do things.

For example, after adding targets, the server will be woken-up to start running
the targets and not wait for the next “loop timeout.”
See [Pvem_lwt_unix.LIGHT](http://www.hammerlab.org/docs/pvem_lwt_unix/master/api/Pvem_lwt_unix.LIGHT.html)
for more details.
M*)
  let create ~state ~authentication server_configuration =
    let loop_traffic_light = Light.create () in
    {state; authentication; server_configuration; loop_traffic_light;
     deferred_queries = Deferred_queries.create (); status = `On}

  let markup t =
    let open Display_markup in
    description_list [
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
  body:Cohttp_lwt.Body.t ->
  Cohttp.Request.t ->
  (answer, 'error) Deferred_result.t
(** A service is something that replies an [answer] on a ["/<path>"] URL. *)

(** Extract information from HTTP requests. *)
module Request = struct

  type t = {
    id: string;
    cohttp_id: string;
    body: Cohttp_lwt.Body.t;
    req: Cohttp.Request.t;
    mutable log: (string * Display_markup.t) list;
  }
  let create ~body ~cohttp_id req =
    let id = Unique_id.create () in
    {
      id; cohttp_id;
      body; req; log = Display_markup.[
          "module", text "Server";
          "function", text "create-request";
          "creation", date_now ();
          "id", text id;
          "cohttp_id", text cohttp_id;
          "request", of_pp Cohttp.Request.pp_hum req;
        ];
    }

  let add_log req l = req.log <- req.log @ l

  let finalize req =
    Logger.log Display_markup.(
        description_list (
          req.log
          @ [
            "finalize", date_now ();
          ]
        )
      )

  let parameter {req; _} =
    Uri.get_query_param (Cohttp.Request.uri req)

  let path {req; _} =
    Uri.path (Cohttp.Request.uri req)

  let log {id; req; _} =
    Log.(braces (
        s "Id"% s id % sp %s "Content: " % sexp Cohttp.Request.sexp_of_t req
      ))

  (** Check that it is a [`POST], get the {i non-empty} body; or fail. *)
  let get_post_body ({req; body; _} as request) =
    begin match Cohttp.Request.meth req with
    | `POST ->
      wrap_deferred ~on_exn:(fun e -> `IO (`Exn e))
        (fun () -> Cohttp_lwt.Body.to_string body)
      >>= fun str ->
      add_log request Display_markup.[
          "body", big_string str;
        ];
      return str
    | other ->
      wrong_request "wrong method, wanted post" (Cohttp.Code.string_of_method other)
    end

end (* Request *)

let block_if_empty_at_most
    ~server_state
    ~debug_info
    ~get_values
    ~should_send
    ~send
    options =
  begin match
    List.find options ~f:(function `Block_if_empty_at_most _ -> true)
  with
  | None ->
    get_values () >>= send
  | Some (`Block_if_empty_at_most req_block_time) ->
    let start_time = Time.now () in
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
    let rec loop ~why_in_loop () =
      let do_send ~why values =
        log_markup Display_markup.[
            "Function", text "block_if_empty_at_most";
            "Info", text "Sending";
            "start_time", date start_time;
            "block_time", time_span block_time;
            "req_block_time", time_span req_block_time;
            "now", date (Time.now ());
            "debug-info", text debug_info;
            "why-in-loop", text why_in_loop;
            "why-send", text why;
          ];
        send values in
      let now =  Time.now () in
      match start_time +. block_time < now with
      | true ->
        get_values () >>= do_send ~why:"block-time maxed-out"
      | false ->
        get_values ()
        >>= fun ( values) ->
        if should_send values then (
          do_send values ~why:"should_send → true"
        ) else (
          Deferred_list.pick_and_cancel [
            begin Engine.next_changes server_state.state
              >>= fun changes ->
              let why_in_loop = fmt "Got %d changes" (List.length changes) in
              loop ~why_in_loop ()
            end;
            begin System.sleep 120. (* This should only be an emergency break *)
              >>< function
              | `Ok () ->
                loop ~why_in_loop:"Slept to maximum" ()
              | `Error (`System (_, `Exn exn)) ->
                begin match exn with
                | Lwt.Canceled -> failwith "thread canceled"
                | other ->
                  Log.(s "System.sleep in block_if_empty_at_most \
                          waken-up by exn: " % exn other % n %
                       s "debug_info: " % s debug_info % n %
                       s "why_in_loop: " % s why_in_loop @ warning);
                  failwith "thread canceled"
                end
            end;
          ]
        )
    in
    loop ~why_in_loop:"first time" ()
  end

(** {2 Services: Answering Requests} *)

(* Tell the engine to do stuff; unless otherwise specified the operations
   relate to targets. *)
module Engine_instructions = struct

  let from_ids ~server_state l =
    Deferred_list.while_sequential l ~f:(fun id ->
        Engine.get_target server_state.state id
        >>< function
        | `Ok t -> return (Some (id, t))
        | `Error e ->
          log_error Error.to_string e
            Log.(s "Error while getting the target " % s id);
          return None)
    >>| List.filter_opt

  let get ~server_state target_ids =
    from_ids ~server_state target_ids
    >>= fun targets ->
    return (`List_of_targets (List.map ~f:snd targets))

  let summaries ~server_state target_ids =
    from_ids ~server_state target_ids
    >>= fun targets ->
    return (`List_of_target_summaries
      (List.map targets ~f:(fun (id, t) ->
                  (id, Target.Summary.create t))))

  let available_queries ~server_state target_id =
    Engine.get_target server_state.state target_id
    >>= fun target ->
    return (
      `List_of_query_descriptions (
        Plugin.additional_queries target
        |> List.map ~f:(fun (a, l) -> a, Log.to_long_string l)))

  let call_query ~server_state ~target_id ~query =
    Engine.get_target server_state.state target_id
    >>= fun target ->
    log_info
      Log.(s "Calling query " % quote query % s " on " % Target.log target);
    begin
      let host_io = Engine.host_io server_state.state in
      Plugin.call_query ~host_io ~target query
      >>< function
      | `Ok string ->
        return (`Query_result string)
      | `Error error_log ->
        return (`Query_error (Log.to_long_string error_log))
    end

  let go_green server_state _ =
    Light.green server_state.loop_traffic_light;
    return `Ok

  let submit ~server_state ~targets =
    log_info Log.(s "Adding " % i (List.length targets) % s " targets");
    Engine.add_targets server_state.state targets
    >>= fun () ->
    go_green server_state ()

  let kill ~server_state ids =
    Deferred_list.while_sequential ids (fun id -> Engine.kill server_state.state ~id)
    >>= go_green server_state

  let restart ~server_state ids =
    Deferred_list.while_sequential ids (Engine.restart_target server_state.state)
    >>= go_green server_state

  let get_flat_states ~server_state
      (time_constrain, target_ids, options) =
    let debug_info =
      fmt "get_flat_states:%d-ids-%s" (List.length target_ids)
        begin match time_constrain with
        | `All -> "ALL"
        | `Since t -> fmt "since-%s" (Time.to_string_hum t)
        end
    in
    block_if_empty_at_most ~server_state options ~debug_info
      ~get_values:(fun () ->
          let now = Time.now () in
          from_ids ~server_state target_ids
          >>= fun targets ->
          log_info
            Log.(s "get_flat_states computing states for "
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
          return (states, now))
      ~should_send:(fun states ->
          let total_items =
            List.fold (fst states) ~init:0 ~f:(fun prev (_, flat) ->
                prev + (List.length flat.Target.State.Flat.history)) in
          log_info
            Log.(s "get_flat_states" % n
                 % s "States: " % i (List.length (fst states)) % n
                 % s "Total items: " % i  total_items);
          total_items <> 0)
      ~send:(fun states ->
          return (`List_of_target_flat_states states))

  let get_ids ~server_state (query, options) =
    block_if_empty_at_most ~server_state options
      ~debug_info:(fmt "get_ids:%s" (Protocol.Up_message.show_target_query query))
      ~get_values:(fun () ->
          let date = Time.now () in
          Engine.get_list_of_target_ids server_state.state query
          >>= fun l ->
          return (l, date)
        )
      ~should_send:(function ([], _) -> false | _ -> true)
      ~send:begin fun (v, birthdate) ->
        match List.length v with
        | small when small < 1001 ->
          return (`List_of_target_ids (v, birthdate))
        | big ->
          let answer_id =
            Deferred_queries.add_list_of_ids server_state.deferred_queries v in
          let msg = `Deferred_list_of_target_ids (answer_id, big) in
          log_info Log.(s "answer_get_target_ids -> Deferred_list_of_target_ids "
                        % s answer_id % s ", " % i big);
          return msg
      end

end (* Engine_instructions *)

let answer_get_server_status ~server_state =
  let tls =
    match !Conduit_lwt_unix.tls_library with
    | Conduit_lwt_unix.OpenSSL  -> `OpenSSL
    | Conduit_lwt_unix.Native  -> `Native
    | Conduit_lwt_unix.No_tls -> `None
  in
  let module C = Configuration in
  let engine = Configuration.server_engine server_state.server_configuration in
  let database = C.database_parameters engine in
  let host_timeout_upper_bound = C.host_timeout_upper_bound engine in
  let maximum_successive_attempts = C.maximum_successive_attempts engine in
  let concurrent_automaton_steps = C.concurrent_automaton_steps engine in
  let status =
    Protocol.Server_status.create ~database ~tls
      ~host_timeout_upper_bound
      ~maximum_successive_attempts
      ~concurrent_automaton_steps
      ~time:Time.(now ())
      ~read_only:(Configuration.read_only_mode server_state.server_configuration)
      ~preemptive_bounds:(Lwt_preemptive.get_bounds ())
      ~preemptive_queue:(Lwt_preemptive.get_max_number_of_threads_queued ())
      ~libev:(Lwt_sys.have `libev)
      ~gc:(Gc.quick_stat ())
  in
  return (`Server_status status)

let answer_message ~server_state ?token msg =
  let read_only_mode =
    Configuration.read_only_mode server_state.server_configuration in
  let with_capability cap f =
    ensure_can server_state.authentication ?token cap ~read_only_mode
    >>= fun () -> f ~server_state
  in
  match msg with
  | `Get_targets l ->
    with_capability `See_targets (Engine_instructions.get l)
  | `Get_target_summaries l ->
    with_capability `See_targets (Engine_instructions.summaries l)
  | `Get_available_queries target_id ->
    with_capability `Query_targets (Engine_instructions.available_queries target_id)
  | `Call_query (target_id, query) ->
    with_capability `Query_targets (Engine_instructions.call_query ~target_id ~query)
  | `Submit_targets targets ->
    with_capability `Submit_targets (Engine_instructions.submit ~targets)
  | `Kill_targets ids ->
    with_capability `Kill_targets (Engine_instructions.kill ids)
  | `Restart_targets ids ->
    with_capability `Restart_targets (Engine_instructions.restart ids)
  | `Get_target_ids query ->
    with_capability `See_targets (Engine_instructions.get_ids query)
  | `Get_server_status ->
    with_capability `See_server_status answer_get_server_status
  | `Get_target_flat_states query ->
    with_capability `See_targets (Engine_instructions.get_flat_states query)
  | `Get_deferred (id, index, length) ->
    with_capability `See_targets (fun ~server_state ->
        let msg =
          Deferred_queries.make_message server_state.deferred_queries
            ~id ~index ~length
        in
        return msg)
  | `Get_notifications query ->
    with_capability `See_server_status (fun ~server_state ->
        Logging.User_level_events.get_notifications_or_block ~query
        >>= fun notifications ->
        return (`Notifications notifications)
      )
      (*
  | `Process p_msg ->
    with_capability `Play_with_process_holder (fun ~server_state ->
        let host_io = Engine.host_io server_state.state in
        Process_holder.answer_message ~host_io
          server_state.process_holder p_msg)
    >>= fun p_down ->
    return (`Process p_down)
         *)

(** Grab and deserialize a body (either in POST or in the "message" of a URI)
    into an up_message. *)
let message_of_body body =
  wrap_preemptively ~on_exn:(fun e -> `Failure (Printexc.to_string e))
    (fun () -> Protocol.Up_message.deserialize_exn body)

let api_service ~server_state req =
  let token = Request.parameter req "token" in
  Request.add_log req Display_markup.[
      "service", text "api";
      "token", option ~f:text token;
    ];
  Request.get_post_body req
  >>= fun body ->
  message_of_body body
  >>= fun up_message ->
  answer_message ~server_state ?token up_message
  >>= fun msg ->
  return (`Message (`Json, msg))

(* Handle requests by messaging to the appropriate JSONP callback. *)
let apijsonp_service ~server_state req =
  let token = Request.parameter req "token" in
  let body = Request.parameter req "message" in
  let callback = Request.parameter req "callback" in
  Request.add_log req Display_markup.[
      "service", text "api-jsonp";
      "token", option ~f:text token;
      "message", option ~f:big_string body;
      "callback", option ~f:text callback;
    ];
  begin match body with
  | None -> wrong_request "missing jsonp-message" ""
  | Some b -> begin match callback with
              | None -> wrong_request "missing jsonp-callback" ""
              | Some c -> return (b, c)
              end
  end
  (* the 'message' here is of Up_message.t *)
  >>= fun (body, callback) -> message_of_body body
  >>= answer_message ~server_state ?token
  >>= fun down_msg ->
  let page =
    fmt "window.%s({ \"message\" : %S })" callback
      (Protocol.Down_message.serialize down_msg |> Uri.pct_encode)
  in
  Log.(s "Returning "
        % i (String.length page) %s " bytes"
        %sp % parens (s "Callback: " % quote callback)
        @ verbose);
  Request.add_log req Display_markup.[
      "return", big_string page;
    ];
  return (`Page page)

let html_page () = Client_html.gui_page

let gui_service ~server_state req =
  let token = Request.parameter req "token" in
  ensure_can server_state.authentication ?token `Browse_gui
    ~read_only_mode:(Configuration.read_only_mode
                       server_state.server_configuration)
  >>= fun () ->
  Request.add_log req Display_markup.[
      "service", text "GUI-html-page";
    ];
  return (`Page (html_page ()))

(** {2 Dispatcher} *)

let handle_request ~server_state req : (answer, _) Deferred_result.t =
  log_info Log.(s "Request-in: " % Request.log req);
  match Request.path req with
  | "/hello"    -> return `Unit
  | "/api"      -> api_service ~server_state req
  | "/apijsonp" -> apijsonp_service ~server_state req
  | "/gui"      -> gui_service ~server_state req
  | other       -> wrong_request "Wrong path" other

(** {2 Start/Stop The Server} *)


(* Server's graceful sefl-shut-down. *)
let shut_down server_state : ([ `Never_returns ], 'a) Deferred_result.t =
  begin match server_state.status with
  | `Off -> exit 0
  | `On ->
    Logging.User_level_events.server_shut_down ();
    Engine.unload server_state.state
    >>< fun engine_unload_result ->
    let display_errors_and_exit e =
      log_error Error.to_string e
        Log.(s "Could not shut down the server properly");
      Log.(s "Errors while shutting down the server:" %n
           % a Error.to_string e @ error);
      exit 10 in
    server_state.status <- `Off;
    begin match engine_unload_result with
    | `Ok () -> exit 0
    | `Error e -> display_errors_and_exit e
    end
  end

(* Text commands that we can send to the server via the pipe *)
module Commands = struct

  let die_command = "die"

  let parse = function
    | "die"                   -> `Die
    | "reload-auth"           -> `ReloadAuth
    | is_get_log ->
        if (String.sub is_get_log 0 8) = Some "get-log:" then
          match String.split is_get_log ~on:(`Character ':') with
          | "get-log" :: path :: format :: [] ->
            if format = "json" then `GetLog (`Json, path)
            else if format = "txt" then `GetLog (`Txt, path)
            else `UnrecognizedFormat format
          | _ -> `WrongGetLog is_get_log
        else
          `Other is_get_log

  let die server_state file_path =
    log_markup Display_markup.[
        "Submodule", text "Commands";
        "Function", text "die";
        "Info", textf "Server killed by “die” command (%s)" file_path;
      ];
    shut_down server_state
    >>= fun `Never_returns ->
    return ()

  let reload_authentication ~server_state =
    Authentication.reload server_state.authentication
    >>< function
      | `Ok authentication ->
        server_state.authentication <- authentication;
        return ()
      | `Error e ->
        log_error Error.to_string e Log.(s "Could not reload Authentication: "
          % Authentication.log server_state.authentication);
        return ()

  let get_log format path =
    Log.(s "Append logs to " %quote path @ verbose);
    Lwt.bind (Logging.Global.append_to_file ~path ~format)
      begin function
      | `Ok () -> return ()
      | `Error e ->
        log_error Error.to_string e Log.(s "get_log");
        return ()
      end

end (* Commands *)

let read_loop ~server_state ~file_path pipe =
  log_markup Display_markup.[
      "Function", text "read_loop";
      "Info", textf "Starting read_loop: %s" file_path;
    ];
  let rec loop ~error_count =
    if error_count >= 5 then begin
      Log.(s "Encountered 5 errors, stop reading from pipe." @ verbose);
      return ()
    end else begin
      Log.(s "Listening on " % OCaml.string file_path @ verbose);
      begin
        wrap_deferred (fun () -> Lwt_io.read_line pipe)
          ~on_exn:(fun e ->
              let error_count = error_count + 1 in
              Log.(s "Exn while reading command pipe: " % exn e
                   % sp % parens (i error_count % s "-th error") @ error);
              `Count error_count)
        >>= fun command_string ->
        log_markup Display_markup.[
            "Function", text "read_loop";
            "Command", text command_string;
            "Error-count", textf "%d" error_count;
          ];
        begin match Commands.parse command_string with
        | `Die ->
          Commands.die server_state file_path
        | `ReloadAuth ->
          Commands.reload_authentication ~server_state
        | `GetLog (format, path) ->
          Commands.get_log format path
        | `UnrecognizedFormat format ->
          log_error Error.to_string (`Failure (fmt "wrong format: %S" format))
            Log.(s "get_log:unrecognized format");
          return ()
        | `WrongGetLog msg ->
          log_info Log.(s "Wrong get-log: " % quote msg);
          return ()
        | `Other other ->
          log_info Log.(s "Cannot understand command: " % OCaml.string other);
          return ()
        end
      end
      >>< function
      | `Ok () -> loop ~error_count
      | `Error (`Count error_count) -> loop ~error_count
    end
  in
  Lwt.ignore_result
    (loop ~error_count:0
     >>< function | `Ok () | `Error (`Count _) -> return ());
  return ()

let start_listening_on_command_pipe ~server_state =
  let conf = server_state.server_configuration in
  match Configuration.command_pipe conf with
  | None -> return ()
  | Some file_path ->
    System.remove file_path
    >>= fun () ->
    wrap_deferred
      ~on_exn:(fun e -> `Start_server_error (Printexc.to_string e))
      (fun () -> Lwt_unix.mkfifo file_path 0o600)
    >>= fun () ->
    wrap_deferred
      ~on_exn:(fun e -> `Start_server_error (Printexc.to_string e))
      (fun () ->
         Lwt_io.open_file
           ~flags:[Unix.O_RDWR; Unix.O_NONBLOCK; Unix.O_APPEND] ~perm:0o660
           ~mode:Lwt_io.input file_path)
    >>= read_loop ~server_state ~file_path

let start_engine_loop ~server_state =
  let time_step = 1. in
  let time_factor = 2. in
  let max_sleep = 120. in
  let error_time_factor = 4. in
  let error_max_sleep = 600. in
  let errors_before_fixing = 5 in
  let errors = Hashtbl.create 4 in
  let add_error e =
    let now = Time.now () in
    match Hashtbl.find errors e with
    | (nb, dates) -> Hashtbl.replace errors e (nb + 1, now :: dates)
    | exception _ -> Hashtbl.add errors e (1, [now])
  in
  let rec loop previous_sleep =
    begin
      let to_fix =
        Hashtbl.fold (fun err (nb, dates) prev ->
            Printf.eprintf "E: %s -> %d\n%!"
              (Error.to_string err) nb;
            if nb >= errors_before_fixing
            then (err, nb, dates) :: prev else prev) errors []
      in
      Deferred_list.for_concurrent to_fix  ~f:(fun (err, nb, dates) ->
          let info =
            fmt "happened %d times between %s and %s"
              nb
              (List.fold dates ~f:min ~init:infinity |> Time.to_string_hum)
              (List.fold dates ~f:max ~init:0. |> Time.to_string_hum)
          in
          Engine.Run_automaton.try_to_fix_step_error server_state.state
            ~info err
          >>< function
          | `Ok () ->
            Hashtbl.remove errors err;
            Logging.User_level_events.tried_to_fix_engine_error
              ~info (Error.to_string err) `Ok;
            return ()
          | `Error (`Not_fixable ne) ->
            Logging.User_level_events.tried_to_fix_engine_error
              ~info (Error.to_string err) `Not_fixable;
            return ()
          | `Error ((`Database _ | `Database_unavailable _) as ne) ->
            Logging.User_level_events.tried_to_fix_engine_error
              ~info (Error.to_string err) (`Error_again (Error.to_string ne));
            return ()
        )
      >>= fun ((_ : unit list), (_ : unit list)) ->
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
        add_error e;
        let sleep = min (previous_sleep *. error_time_factor) error_max_sleep in
        Logging.User_level_events.engine_fatal_error (Error.to_string e) sleep;
        return (markup, sleep)
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

let debug_make_server_slow () =
  try
    let spec = Sys.getenv "KETREW_DEBUG_SLOW_SERVER" in
    let random, minimum =
      match String.split ~on:(`Character ',') spec with
      | [] -> 2.0, 1.0
      | [one] -> float_of_string one, 1.
      | one :: two :: _ -> float_of_string one, float_of_string two
    in
    System.sleep (Random.float random +. minimum)
  with _ -> return ()

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
          let req = Request.create ~body ~cohttp_id:id request in
          handle_request ~server_state req
          >>= fun high_level_answer ->
          (debug_make_server_slow () >>= fun _ -> return ())
          >>= fun () ->
          begin match high_level_answer with
          | `Ok `Unit ->
            Request.add_log req Display_markup.[
                "respond-unit", text ""
              ];
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body:"" ()
          | `Ok (`Message (`Json, msg)) ->
            let body = Protocol.Down_message.serialize msg in
            Request.add_log req Display_markup.[
                "respond-message", big_string body;
              ];
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body ()
          | `Ok (`Page body) ->
            Request.add_log req Display_markup.[
                "respond-page", big_string body;
              ];
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body ()
          | `Error e ->
            log_error Error.to_string e
              Log.(s "Error while handling the request: conn_id: "
                   % s id % s ", request: "
                   % Request.log req);
            Request.add_log req Display_markup.[
                "error", text (Error.to_string e);
              ];
            let body =
              if return_error_messages
              then "Error: " ^ (Error.to_string e)
              else "Undisclosed server error" in
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body ()
          end
          >>= fun ((response, body) as cohttp_answer) ->
          Request.add_log req Display_markup.[
              "response", of_pp Cohttp.Response.pp_hum response;
            ];
          Request.finalize req;
          return cohttp_answer
        in
        let conn_closed (_, conn_id) =
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
            IO.write oc Commands.die_command
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
    | `Error (`Get_exn other_exn) ->
      fail (`Server_status_error (Printexc.to_string other_exn))
  end

(** This function starts running a query on the engine asynchronously so the
    cache starts getting filled before the user does the first query from a UI.
*)

let start ~just_before_listening ~configuration  =
  Log.(s "Set preemptive bounds: 10, 52" @ verbose);
  Lwt_preemptive.init 10 52 (fun str ->
      Log.(s " Lwt_preemptive error: " % s str @ error);
    );
  Lwt.async_exception_hook := begin fun e ->
    Log.(s " Lwt async error: " % exn e @ error);
    let backtrace = Printexc.get_backtrace () in
    let exn = Printexc.to_string e in
    Printf.eprintf "Lwt-async-exn: %s\nBacktrace:\n%s\n%!" exn backtrace;
    Logger.(
      log
        Display_markup.(
          description_list [
            "Location", text "Lwt.async_exception_hook";
            "Exception", text exn;
            "Backtrace", code_block backtrace;
          ]));
    Logging.User_level_events.async_error ~exn ~backtrace
  end;
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
  log_info Log.(s "Start-Server: Setup “At-Exit” hooks");
  (* Set at-exit hooks. *)
  Lwt_main.at_exit Lwt.(fun () ->
      shut_down server_state
      >>= fun _ ->
      return ());
  let (_ : Lwt_unix.signal_handler_id) =
    Lwt_unix.on_signal Sys.sigint (fun id ->
        Log.(sf "Catching SIGINT (handler id: %d)" id @verbose);
        exit 0) in
  log_info Log.(s "Start-Server: Starting the Engine loop");
  start_engine_loop ~server_state;
  log_info Log.(s "Start-Server: Starting listening on command-pipe");
  start_listening_on_command_pipe ~server_state
  >>= fun () ->
  Log.(s "Start-Server: Starting listening on connections" @ verbose);
  log_info Log.(s "Start-Server: Starting listening on connections");
  just_before_listening ()
  >>= fun () ->
  start_listening_on_connections ~server_state
