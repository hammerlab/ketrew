
open Ketrew_pure
open Internal_pervasives
open Pvem_js
open Reactive_html5

open Local_cache


type status = Time.t * [
    | `Unknown
    | `Ok of Protocol.Server_status.t
    | `Problem of string
  ]

module Tab = struct
  type t = [
    | `Status
    | `Target_table
    | `Processes_ui
    | `Target_page of Target_page.t
  ]
  let is_target_page ~id =
    function
    | `Target_page t -> (String.compare (Target_page.target_id t) id = 0)
    | `Status | `Target_table | `Processes_ui -> false
  let eq a b =
    match a, b with
    | `Target_page ta, `Target_page tb -> Target_page.eq ta tb
    | _, _ -> a = b
end

module Error_log = struct

  type item = {
    timestamp: Time.t;
    content: [
      | `Async_error of string * Display_markup.t
    ];
  }
  type t = item list Reactive.Source.t
  let create () = Reactive.Source.create []

  let append t v =
    let open Reactive in
    Source.modify t (fun current -> current @ [v])

  let append_async_error t (n, e) =
    let item = {
      timestamp = Time.now ();
      content =
        `Async_error (n,
          let open Display_markup in
          match e with
          | `Exn e -> textf "Exception: %s" (Printexc.to_string e)
          | `Protocol_client pc ->
            textf "Protocol_client: %s" (Protocol_client.Error.to_string pc)
          | `Wrong_down_message d -> text "Wrong down-message"
        )
    } in
    append t item

  let markup_signal t =
    let open Reactive in
    Source.signal t |> Signal.map ~f:(function
      | [] -> None
      | more ->
        Some Display_markup.(
            List.map more ~f:(fun {timestamp; content} ->
                "Async-error",
                begin match content with
                | `Async_error (n, m) ->
                  description_list [
                    "Timestamp", Date timestamp;
                    "Name", text n;
                    "Content", m
                  ]
                end
              ) |> description_list
          )
      )


end

module Async_task_log = struct

  type item = {
    uid: string;
    name: Display_markup.t;
    start: Time.t;
    finish: Time.t option;
  }

  type t = item list Reactive.Source.t

  let create () : t = Reactive.Source.create []

  let add t name =
    let uid = Unique_id.create () in
    Reactive.Source.modify t (fun current ->
        let item = {uid; name; start = Time.now (); finish = None} in
        current @ [item]);
    uid

  let declare_finished t uid =
    Reactive.Source.modify t (fun current ->
        List.map current ~f:(fun i ->
            if i.uid = uid
            then { i with finish = Some (Time.now ())}
            else i
          )
      )

  let markup_signal ?(only_non_finished = true) t =
    let open Reactive in
    let open Display_markup in
    Source.signal t |> Signal.map ~f:(fun items ->
        description_list (List.filter_map items ~f:(fun {uid; name; start; finish} ->
            match finish with
            | Some _ when only_non_finished -> None
            | _ ->
              Some (
                (match finish with None -> "Active-task" | Some _ -> "Finished-Task"),
                description_list [
                  "ID", command uid;
                  "Name", name;
                  "Started-on", date start;
                  "Finished",
                  begin match finish with
                  | None -> text "Not yet"
                  | Some d -> date d
                  end;
                ]
              ))))




end



type t = {
  protocol_client: Protocol_client.t;
  target_cache: Target_cache.t;

  status: status Reactive.Source.t;
  tabs: Tab.t list Reactive.Source.t;
  current_tab: Tab.t Reactive.Source.t;
  block_time_request: float;
  default_protocol_client_timeout: float;
  wait_before_retry_asynchronous_loop: float;
  reload_status_condition: unit Lwt_condition.t;
  target_table: Target_table.t;

  processes_ui : Processes_ui.t;
  should_show_processes_ui: bool Reactive.Source.t;

  error_log: Error_log.t;

  async_task_log: Async_task_log.t;

  list_of_ids_log: Display_markup.t list Reactive.Source.t;
}


let create ~protocol_client () =
  let status = Reactive.Source.create (Time.now (), `Unknown) in
  let current_tab = Reactive.Source.create ~eq:Tab.eq `Target_table in
  let target_table = Target_table.create () in
  let tabs =
    Reactive.Source.create
      ~eq:(fun la lb -> try List.for_all2 ~f:Tab.eq la lb with _ -> false)
      [ `Status; `Target_table ] in
  let processes_ui = Processes_ui.create protocol_client in
  let should_show_processes_ui = Reactive.Source.create false in
  let (_ : unit React.E.t) =
    Reactive.Source.signal should_show_processes_ui
    |> React.S.changes
    |> React.E.map (function
      | true ->
        Reactive.Source.modify tabs ~f:(fun l ->
            if List.mem `Processes_ui ~set:l then l else
              begin match l with (* Insert it “sorted” after the Status one *)
              | `Status :: t -> `Status :: `Processes_ui :: t
              | other -> `Processes_ui :: other
              end)
      | false -> 
        Reactive.Source.modify tabs ~f:(fun l ->
            List.filter l ~f:(fun x -> not (Tab.eq x `Processes_ui)))
      )
  in
  {
    protocol_client;
    target_cache = Target_cache.create ();
    processes_ui; should_show_processes_ui;
    status;
    tabs;
    current_tab;
    target_table;
    block_time_request = 255.; (* the server will cut at 300. anyway *)
    default_protocol_client_timeout = 20.;
    wait_before_retry_asynchronous_loop =
      (if !global_debug_level > 0 then 120. else 30.);
    reload_status_condition = Lwt_condition.create ();
    error_log = Error_log.create ();
    async_task_log = Async_task_log.create ();
    list_of_ids_log = Reactive.Source.create [];
  }

let log t =
  Log.(s "Protocol-client: "
       % Protocol_client.log t.protocol_client)



let name {protocol_client; _ } = Protocol_client.name protocol_client

let interesting_target_ids t =
  Reactive.(
    Signal.tuple_2
      (Target_table.visible_target_ids t.target_table
       |> Signal.map ~f:(Option.value ~default:[]))
      (Source.signal t.tabs
       |> Signal.map ~f:(List.filter_map ~f:(function
         | `Target_page tp -> Some (Target_page.target_id tp)
         | `Processes_ui
         | `Status
         | `Target_table -> None)))
    |> Signal.map  ~f:(fun (from_table, from_pages) ->
        let with_deps id =
          match
            Target_cache.get_target_summary_signal t.target_cache ~id
            |> Signal.value 
          with
          | `None -> [id]
          | `Pointer (_, summary)
          | `Summary summary ->
            id :: Target.Summary.(depends_on summary
                                  @ on_success_activate summary
                                  @ on_failure_activate summary)
        in
        let transitively_from_pages = List.concat_map from_pages ~f:with_deps in
        List.dedup (List.rev_append transitively_from_pages from_table))
  )

let error_markup e =
  let open Display_markup in
  match e with
  | `Exn e -> description "Exception" (text (Printexc.to_string e))
  | `Protocol_client pc ->
    description "Protocol-client" (text (Protocol_client.Error.to_string pc))
  | `Wrong_down_message d ->
    description "Wrong-down-message"
      (Protocol.Down_message.serialize d |> text)

let asynchronous_loop ?wake_up ?more_info t ~name loop =
  let uid =
    let big_name =
      let open Display_markup in
      match more_info with
      | Some m -> description name m
      | None -> text name in
    Async_task_log.add t.async_task_log big_name in
  Lwt.(
    async begin fun () ->
      let rec meta_loop () =
        loop () >>= function
        | `Ok () ->
          Log.(s "Loop ended with OK: " % quote name @ normal);
          Async_task_log.declare_finished t.async_task_log uid;
          return ()
        | `Error e ->
          let problem =
            let open Log in
            match e with
            | `Exn e -> exn e
            | `Protocol_client pc ->
              Protocol_client.Error.to_string pc |> s
            | `Wrong_down_message d -> s "Wrong down-message"
          in
          Error_log.append_async_error t.error_log (name, e);
          Log.(s "Error in loop " % quote name % s ": " % problem @ error);
          Reactive.Source.set t.status
            (Time.now (), `Problem (problem |> Log.to_long_string));
          let to_pick =
            begin match wake_up with
            | Some (name, (cond : unit Lwt_condition.t)) ->
              [
                begin
                  Log.(s "asynchronous_loop waiting on " % quote name @ verbose);
                  Lwt_condition.wait cond
                  >>= fun () ->
                  Log.(s "asynchronous_loop woken-up by " % quote name @ verbose);
                  return ()
                end
              ]
            | None -> []
            end
            @ [
              Lwt_js.sleep t.wait_before_retry_asynchronous_loop
            ] in
          pick to_pick
          >>= fun () ->
          meta_loop ()
      in
      meta_loop ()
    end
  )

let preemptible_asynchronous_loop t ~name ~body =
  let condition = Lwt_condition.create () in
  let loop_log = Reactive.Source.create [] in
  let add_log the_log =
    Reactive.Source.modify loop_log (fun current ->
        let with_ts =
          Display_markup.(concat [command name;
                                  date Time.(now ());
                                  text ": "; the_log]) in
        (with_ts :: List.take current 41)
      ) in
  let woken_up = ref false in
  let rec loop () =
    Lwt.pick [
      begin try
        begin body () end
        >>< function
        | `Ok () ->
          add_log Display_markup.(textf "ends ok");
          return ()
        | `Error e ->
          add_log Display_markup.(
              concat [
                textf "ends with error: ";
                error_markup e;
                textf " woken-up: "; command (string_of_bool !woken_up);
              ]
            );
          (if !woken_up then
             (woken_up := false;
              return ())
           else
             (sleep t.wait_before_retry_asynchronous_loop
              >>< fun _ -> return ()))
      with
      | e ->
        add_log Display_markup.(
            textf "exception: %s" (Printexc.to_string e));
        Log.(s "exception in preemptible_asynchronous_loop" % sp
             % quote name % s ": " % exn e @ error);
        return ()
      end;
      Lwt.(Lwt_condition.wait condition
           >>= fun () ->
           add_log Display_markup.(textf "Loop waken-up");
           return (`Ok ()));
    ]
    >>< function
    | `Ok () -> loop ()
    | `Error () -> (* making sure all errors were treated above *) loop ()
  in
  asynchronous_loop t ~name loop ~wake_up:(name, condition);
  (object
    method wake_up = 
      woken_up := true;
      Lwt_condition.broadcast condition ()
    method log = loop_log
  end)

let start_server_status_loop t =
  let rec update_server_status () =
    Protocol_client.call
      ~timeout:t.default_protocol_client_timeout
      t.protocol_client `Get_server_status
    >>= begin function
    | `Server_status status ->
      (* let _, previous_status = *)
      (*   Reactive.(Source.signal t.status |> Signal.value) in *)
      Reactive.Source.set t.status (Time.now (), `Ok status);
      Reactive.Source.set t.should_show_processes_ui
        status.Protocol.Server_status.enable_ssh_ui;
      sleep 30.
      >>= fun () ->
      update_server_status ()
    | other ->
      fail (`Wrong_down_message other)
    end
  in
  asynchronous_loop t ~name:"update-server-status" update_server_status
    ~wake_up:("Reload-status-condition", t.reload_status_condition);
  ()


let start_list_of_ids_loop t =
  let add_log the_log =
    Reactive.Source.modify t.list_of_ids_log (fun current ->
        let with_ts =
          Display_markup.(concat [date Time.(now ()); text ": "; the_log]) in
        (with_ts :: List.take current 41)
      ) in
  let update_list_of_ids query ~and_block =
    let timeout, options =
      if and_block then
        let blocking_time = t.block_time_request in
        (blocking_time +. t.default_protocol_client_timeout,
         [`Block_if_empty_at_most blocking_time])
      else
        (t.default_protocol_client_timeout, [])
    in
    add_log Display_markup.(
        concat [
          textf "Calling server: ";
          description_list [
            "Query", Protocol.Up_message.target_query_markup query;
            "Options",
            concat ~sep:(text ", ")
              (List.map options ~f:(function
                 | `Block_if_empty_at_most t ->
                   concat [text "[block-if-empty-at-most "; time_span t; text "]"]
                 ));
          ];
        ]);
    let rec get_ids first_time message =
      Protocol_client.call ~timeout t.protocol_client message
      >>= begin function
      | `List_of_target_ids l ->
        let server_time =
          match snd Reactive.(Source.signal t.status |> Signal.value) with
          | `Ok s -> Some s.Protocol.Server_status.time
          | _ -> None
        in
        add_log Display_markup.(
            concat [
              textf "Got %d ids, server_time: " (List.length l);
              option server_time date;
            ]
          );
        begin if first_time && not and_block then
            Target_table.modify_filter_results_number t.target_table
              (fun _ -> List.length l)
        end;
        Target_table.add_target_ids ?server_time t.target_table l;
        return ()
      | `Deferred_list_of_target_ids (answer_id, big) ->
        begin if first_time && not and_block then
            Target_table.modify_filter_results_number t.target_table
              (fun _ -> big);
        end;
        get_ids false (`Get_deferred (answer_id, 0, min 100 big))
      | other ->
        fail (`Wrong_down_message other)
      end
    in
    get_ids true (`Get_target_ids (query, options))
  in
  let loop_handle =
    preemptible_asynchronous_loop t ~name:"List-of-IDS" ~body:(fun () ->
        let query, and_block =
          Reactive.(
            let last_updated =
              Target_table.target_ids_last_updated t.target_table
              |> Signal.value in
            let filter = Target_table.filter t.target_table |> Signal.value in
            (Target_table.Filter.target_query ?last_updated filter,
             (last_updated <> None))
          ) in
        update_list_of_ids query ~and_block)
  in
  let (_ : unit React.E.t) =
    let event = (Target_table.filter t.target_table) |> React.S.changes in
    React.E.map (fun fil ->
        add_log Display_markup.(
            concat [
              textf "Filter changes, waking-up";
              command (Target_table.Filter.to_lisp fil)
            ]
          );
        Target_table.reset_target_ids_last_updated t.target_table;
        loop_handle#wake_up
      ) event
  in
  ()

let start_getting_flat_statuses t =
  let update_flat_states target_ids =
    Log.(s "get_all_missing_states TRIGGERED !" %n
         % s "targets_ids has " % i (List.length target_ids)
         % s " elements" @ verbose);
    let at_once = 200 in
    (* if the number of targets goes above 200, we have the problem
       that 100 first ones have to go through before the next ones are
       explored. *)
    let sleep_time = 0.3 in
    let rec batch_fetching ids =
      match ids with
      | [] -> return ()
      | more ->
        let now, later = List.split_n more at_once in
        let status_query =
          (* if we find a `None` then we ask for `` `All``, if not
             we ask for `` `Since minimal_date``: *)
          List.fold now ~init:(Some (Time.now ())) ~f:(fun prev id ->
              match
                prev,
                Target_cache.get_target_flat_status_last_retrieved_time
                  t.target_cache ~id
              with
              | None, _ -> None
              | Some pr, None -> None
              | Some pr, Some nw -> Some (min pr nw))
          |> function
          | Some t -> `Since (t -. 1.)
          | None -> `All in
        Protocol_client.call t.protocol_client
          ~timeout:(t.block_time_request
                    +. t.default_protocol_client_timeout)
          (`Get_target_flat_states (status_query, now, 
                                    [`Block_if_empty_at_most
                                       t.block_time_request]))
        >>= fun msg_down ->
        begin match msg_down with
        | `List_of_target_flat_states l ->
          (* Log.(s "fill_cache_loop got " % i (List.length l) % s " flat-states" *)
          (*      @ verbose); *)
          List.iter l ~f:begin fun (id, value) ->
            Target_cache.update_flat_state t.target_cache ~id value
          end;
          sleep sleep_time
          >>= fun () ->
          batch_fetching later
        | other ->
          fail (`Wrong_down_message other)
        end
    in
    batch_fetching target_ids
  in
  (* We use this reference because 
     `interesting_target_ids t |> Reactive.Signal.value` was throwing
     React exceptions. The reference gets the “current” value from 
     the `React.event` below. *)
  let ref_to_changed_id_list = ref [] in
  let body () =
    let to_fetch =
      let tids = !ref_to_changed_id_list in
      List.filter tids ~f:(fun id ->
          let signal =
            Target_cache.get_target_flat_status_signal t.target_cache id in
          let latest =
            try
              Reactive.Signal.value signal |> Target.State.Flat.latest
            with e ->
              Log.(s "Reactive.Signal.value flat-status -> " % exn e @ error);
              raise e
          in
          let not_finished =
            not (Option.value_map ~default:false
                   ~f:Target.State.Flat.finished latest) in
          match Option.map ~f:Target.State.Flat.simple latest with
          | None
          | Some `In_progress
          | Some `Activable -> true
          | Some `Successful
          | Some `Failed -> not_finished)
    in
    match to_fetch with
    | [] -> sleep 42. (* We wait to be interupted *)
    | more  ->
      Log.(s "Batch fetching state for "
           % (match List.length to_fetch > 14 with
             | true -> i (List.length to_fetch) % s " targets"
             | false -> OCaml.list quote to_fetch)
           @ verbose);
      update_flat_states to_fetch
  in
  let loop_handle =
    preemptible_asynchronous_loop t ~name:"Flat-statuses" ~body in
  let (_ : unit React.E.t) =
    let event = interesting_target_ids t |> React.S.changes in
    React.E.map (fun ids ->
        Log.(s "waking up falt-statuses" @ verbose);
        ref_to_changed_id_list := ids;
        loop_handle#wake_up
      ) event
  in
  ()

let start_getting_summaries t =
  let get_all_missing_summaries targets_ids =
    Log.(s "get_all_missing_summaries TRIGGERED !" %n
         % s "targets_ids has " % i (List.length targets_ids)
         % s " elements" @ verbose);
    let at_once = 20 in
    let sleep_time = 0.01 in
    let rec fetch_summaries ids =
      match ids with
      | [] ->
        (* Log.(s "fill_cache_loop.fetch_summaries nothing left to do" @ verbose); *)
        sleep 42.
      | more ->
        let now, later = List.split_n more at_once in
        Protocol_client.call
          ~timeout:t.default_protocol_client_timeout
          t.protocol_client (`Get_target_summaries now)
        >>= fun msg_down ->
        begin match msg_down with
        | `List_of_target_summaries l ->
          Log.(s "fill_cache_loop got " % i (List.length l) % s " targets" @ verbose);
          List.iter l ~f:(fun (id, value) ->
              begin match Target.Summary.id value with
              | i when id = i ->
                Target_cache.update_target t.target_cache ~id (`Summary value)
              | p ->
                Target_cache.update_target t.target_cache ~id (`Pointer (p, value))
              end
            );
          sleep sleep_time
          >>= fun () ->
          fetch_summaries later
        | other ->
          fail (`Wrong_down_message other)
        end
    in
    let missing_ids =
      List.filter targets_ids ~f:(fun id ->
          match
            Target_cache.get_target_summary_signal t.target_cache id
            |> Reactive.Signal.value
          with
          | `None -> true
          | _ -> false)
    in
    fetch_summaries missing_ids
  in
  let ref_to_changed_id_list = ref [] in
  let loop_handle =
    preemptible_asynchronous_loop t ~name:"Target-summaries" ~body:(fun () ->
        get_all_missing_summaries !ref_to_changed_id_list)
  in
  let (_ : unit React.E.t) =
    let event = interesting_target_ids t |> React.S.changes in
    React.E.map (fun ids ->
        Log.(s "waking up target-summaries" @ verbose);
        ref_to_changed_id_list := ids;
        loop_handle#wake_up
      ) event
  in
  ()


let start_updating t =
  start_server_status_loop t;
  start_list_of_ids_loop t;
  start_getting_flat_statuses t;
  start_getting_summaries t;
  ()

let fetch_available_queries t ~id =
  let go () =
    Protocol_client.call
      ~timeout:t.default_protocol_client_timeout
      t.protocol_client (`Get_available_queries id)
    >>= begin function
    | `List_of_query_descriptions qds ->
      Target_cache.update_target_query_descriptions
        t.target_cache ~id (`Descriptions qds);
      return ()
    | other ->
      fail (`Wrong_down_message other)
    end
  in
  asynchronous_loop t ~name:(fmt "queries-for-%s" id) go

let get_available_queries t ~id =
  let signal = Target_cache.get_target_query_descriptions t.target_cache ~id in
  begin match Reactive.Signal.value signal with
  | `None -> fetch_available_queries t ~id
  | `Descriptions _ -> ()
  end;
  signal

let reload_available_queries t ~id =
  fetch_available_queries t ~id


let get_query_result t ~id ~query =
  let go () =
    Protocol_client.call
      ~timeout:t.default_protocol_client_timeout
      t.protocol_client (`Call_query (id, query))
    >>= begin function
    | `Query_result result ->
      Target_cache.update_target_query_result t.target_cache ~id ~query
        (`String (Time.now (), result));
      return ()
    | `Query_error error ->
      Target_cache.update_target_query_result t.target_cache ~id ~query
        (`Error error);
      return ()
    | other ->
      fail (`Wrong_down_message other)
    end
  in
  let signal = Target_cache.get_target_query_result t.target_cache ~id ~query in
  let event = React.S.changes signal in
  let update_if_none =
    function
    | `None -> asynchronous_loop t ~name:(fmt "queries-for-%s" id) go
    |  _ -> () in
  let (_ : unit React.E.t) = React.E.map update_if_none event in
  update_if_none (Reactive.Signal.value signal);
  signal

let reload_query_result t ~id ~query =
  Target_cache.update_target_query_result t.target_cache ~id ~query `None;
  ()

let call_unit_message ~name ~message t ~on_result ~id =
  let go () =
    Protocol_client.call ~timeout:t.default_protocol_client_timeout
      t.protocol_client message
    >>< begin function
    | `Ok `Ok ->
      on_result (`Ok ());
      return ()
    | `Ok other ->
      on_result (`Error (Protocol.Down_message.serialize other));
      return ()
    | `Error (`Protocol_client e) ->
      on_result (`Error (Protocol_client.Error.to_string e));
      return ()
    end
  in
  asynchronous_loop t ~name:(fmt "%s-%s" name id) go;
  ()

let restart_target t ~id ~on_result =
  call_unit_message t ~id ~on_result
    ~name:"restart" ~message:(`Restart_targets [id])

let kill_targets t ~ids ~on_result =
  call_unit_message t ~id:(String.concat ~sep:"-" ids) ~on_result
    ~name:"kill" ~message:(`Kill_targets ids)
let kill_target t ~id ~on_result =
  kill_targets t ~ids:[id] ~on_result

module Html = struct

  let status_icon t =
    let open H5 in
    let display (date, status) =
      match status with
      | `Ok status ->
        Bootstrap.icon_success
          ~title:(fmt "OK (%s)"
                    (status.Protocol.Server_status.time |> Time.to_filename))
      | `Unknown ->
        Bootstrap.icon_unknown ~title:"Status Unknown"
      | `Problem problem ->
        Bootstrap.icon_wrong ~title:(fmt "Problem: %s" problem)
    in
    Reactive_node.span
      Reactive.(
        Source.signal t.status
        |> Signal.map ~f:display
        |> Signal.singleton
      )

  let server_status t status =
    let open Display_markup in
    let {
      Protocol.Server_status.
      time (*  float *);
      read_only (* bool *);
      tls (*  [`OpenSSL | `Native | `None ] *);
      preemptive_bounds (*  int * int *);
      preemptive_queue (*  int *);
      libev (*  bool *);
      database;
      host_timeout_upper_bound (* float *);
      maximum_successive_attempts (* int *);
      concurrent_automaton_steps (* int *);
      gc_minor_words  (*  float *);
      gc_promoted_words  (*  float *);
      gc_major_words  (*  float *);
      gc_minor_collections  (*  int *);
      gc_major_collections  (*  int *);
      gc_heap_words  (*  int *);
      gc_heap_chunks  (*  int *);
      gc_compactions  (*  int *);
      gc_top_heap_words  (*  int *);
      gc_stack_size  (*  int *);
      enable_ssh_ui (* bool *);
    } = status in
    let int64 i =
      let open Int64 in
      let (/) = Int64.div in
      let (mod) = Int64.rem in
      if i < 1000L then textf "%Ld" i
      else if i < 1_000_000L
      then textf "%Ld %03Ld" (i / 1000L) (i mod 1000L)
      else if i < 1_000_000_000L
      then textf "%Ld %03Ld %03Ld"
          (i / 1_000_000L) ((i / 1000L) mod 1_000L) (i mod 1000L)
      else
        textf "%Ld %03Ld %03Ld %03Ld"
          (i / 1_000_000_000L)
          ((i / 1_000_000L) / 1_000_000L)
          ((i / 1000L) mod 1_000L) (i mod 1000L)
    in
    let float f =
      let dec, i = modf f in
      concat [
        int64 (Int64.of_float i);
        if abs_float dec > 0.001 then
          textf ".%d" (1000. *. dec |> int_of_float)
        else text ""
      ]
    in
    let int i = Int64.of_int i |> int64 in
    description_list [
      "Server-Time", date time;
      "Access", (if read_only then text "Read-only" else text "Read-write");
      "TLS",
      begin match tls with
      | `OpenSSL -> text "OpenSSL"
      | `Native -> text "OCaml-SSL"
      | `None -> text "None"
      end;
      begin
        let m,n = preemptive_bounds in
        "Preemptive → bounds", textf "[%d, %d]" m n
      end;
      "Preemptive → size of the waiting queue",  textf "%d" preemptive_queue;
      "LibEV", (if libev then text "Yes" else text "No");
      "Database", text database;
      "host_timeout_upper_bound",
      (match host_timeout_upper_bound with
      | None -> text "None" | Some t -> time_span t);
      "maximum_successive_attempts", int maximum_successive_attempts;
      "concurrent_automaton_steps", int concurrent_automaton_steps;
      "enable_ssh_ui", (if enable_ssh_ui then text "Yes" else text "No");
      "GC", description_list [
        "minor_words", float gc_minor_words (*  float *);
        "promoted_words", float gc_promoted_words (*  float *);
        "major_words", float gc_major_words (*  float *);
        "minor_collections", int gc_minor_collections (*  int *);
        "major_collections", int gc_major_collections (*  int *);
        "heap_words", int gc_heap_words (*  int *);
        "heap_chunks", int gc_heap_chunks (*  int *);
        "compactions", int gc_compactions (*  int *);
        "top_heap_words", int gc_top_heap_words (*  int *);
        "stack_size", int gc_stack_size (*  int *);
      ]
    ]
    |> H5.Markup.to_html

  let status t =
    let open H5 in
    Bootstrap.panel ~body:[
      Reactive_node.div Reactive.(
          Source.signal t.status
          |> Signal.map ~f:begin fun (date, status) ->
            [
              h4 [
                pcdata (fmt "Server Status");
              ];
              pcdata (fmt "Last updated: %s."
                        (Markup.date_to_string date));
              begin match status with
              | `Ok server ->
                Bootstrap.success_box [
                  span ~a:[a_style "color: green"] [pcdata "OK"];
                  server_status t server;
                ]
              | `Unknown ->
                Bootstrap.warning_box
                  [span ~a:[a_style "color: orange"] [pcdata "Unkown ???"]]
              | `Problem problem ->
                Bootstrap.error_box [
                  strong [pcdata "Problem "];
                  a ~a:[
                    a_onclick (fun _ ->
                        Lwt_condition.signal t.reload_status_condition ();
                        false);
                    a_class ["label"; "label-default"];
                  ] [
                    Bootstrap.reload_icon ()
                  ];
                  br ();
                  code [pcdata problem];
                  br ();
                  pcdata "Cannot connect, and cannot get a decent \
                          error message from the browser.";
                  br ();
                  pcdata "You should try \
                          to open the following link in a new tab, and \
                          investigate the problem ";
                  i [pcdata
                       "(the most common issue being \
                        self-signed TLS certificates, by accepting it \
                        in the other tab \
                        you may fix the problem for the current session)"];
                  pcdata ":";
                  br ();
                  a ~a:[
                    a_href (Protocol_client.base_url t.protocol_client)
                  ] [
                    pcdata (Protocol_client.base_url t.protocol_client)
                  ];
                ]
              end
            ]
          end
          |> Signal.list
        );
      h4 [pcdata "Protocol Client"];
      div [
        Markup.to_html (Protocol_client.markup t.protocol_client);
      ];
      h4 [pcdata "Cache"];
      div [
        Markup.to_html (Target_cache.markup_counts t.target_cache);
      ];
      h4 [pcdata "Settings"];
      Markup.to_html
        Display_markup.(description_list [
            "Block-time-request", time_span t.block_time_request;
            "Default-protocol-timeout",
            time_span t.default_protocol_client_timeout;
            "Asynchronous-retry-wait",
            time_span t.wait_before_retry_asynchronous_loop;
          ]);
      h4 [pcdata "Currently Interesting Targets"];
      Reactive_node.div Reactive.(
          (interesting_target_ids t)
          |> Signal.map ~f:(fun l ->
              Bootstrap.collapsable_ul ~ul_kind:`None ~maximum_items:10
                (List.map l ~f:(fun id -> code [pcdata id]))
            ) |> Signal.singleton);
      h4 [pcdata "List-of-IDs Loop Log"];
      Reactive_node.div Reactive.(
          Source.signal t.list_of_ids_log
          |> Signal.map ~f:(fun l ->
              Bootstrap.collapsable_ul ~ul_kind:`None ~maximum_items:5
                (List.map l ~f:(fun log -> Markup.to_html log))
            ) |> Signal.singleton);
      h4 [pcdata "Asynchronous Tasks"];
      Reactive_node.div Reactive.(
          Async_task_log.markup_signal t.async_task_log
          |> Signal.map ~f:Markup.to_html
          |> Signal.singleton
        );
      h4 [pcdata "Errors"];
      Reactive_node.div Reactive.(
          Error_log.markup_signal t.error_log
          |> Signal.map ~f:(function
            | None -> Bootstrap.success_box [pcdata "All good!"]
            | Some m ->
              Bootstrap.error_box [
                strong [pcdata "Errors"];
                Markup.to_html m;
              ]
            )
          |> Signal.singleton
        );
    ]


  let rec target_link_on_click_handler t ~id =
    let open Reactive in
    let current = Source.value t.tabs in
    begin match List.find current ~f:(Tab.is_target_page ~id) with
    | Some tp -> Source.set t.current_tab tp
    | None ->
      let new_tp =
        Target_page.create ~id ~target_cache:t.target_cache
          ~restart_target:(restart_target t ~id)
          ~kill_target:(kill_target t ~id)
          ~target_link_on_click_handler:(target_link_on_click_handler t)
          ~reload_query_result:(reload_query_result t ~id)
          ~reload_available_queries:(fun () -> reload_available_queries t ~id)
          ~available_queries:(get_available_queries t ~id)
          ~get_query_result:(get_query_result t ~id)
      in
      Log.(s "Created TP for " % quote id @ verbose);
      Source.set t.tabs (current @ [`Target_page new_tp])
    end;
    Log.(s "end of target_link_on_click_handler " % quote id @ verbose);
    ()



  let render client =
    let open H5 in
    let current_tab = client.current_tab in
    let tabs =
      Reactive.Source.signal client.tabs
      |> React.S.map ~eq:(fun _ _ -> false) (fun tabs ->
          List.map tabs ~f:begin function
          | `Target_table ->
            Bootstrap.tab_item
              ~active:Reactive.(
                  Source.signal current_tab
                  |> Signal.map ~f:(function `Target_table -> true | _ -> false))
              ~on_click:(fun _ -> Reactive.Source.set current_tab `Target_table; false)
              [Target_table.Html.title client.target_table]
          | `Status ->
            Bootstrap.tab_item
              ~active:Reactive.(
                  Source.signal current_tab
                  |> Signal.map ~f:(function `Status -> true | _ -> false)
                )
              ~on_click:(fun _ -> Reactive.Source.set current_tab `Status; false)
              [pcdata "Internal Information"]
          | `Processes_ui ->
            Bootstrap.tab_item
              ~active:Reactive.(
                  Source.signal current_tab
                  |> Signal.map ~f:(function `Processes_ui -> true | _ -> false))
              ~on_click:(fun _ -> Reactive.Source.set current_tab `Processes_ui; false)
              [Processes_ui.Html.title client.processes_ui]
          | `Target_page tp ->
            let id = Target_page.target_id tp in
            Bootstrap.tab_item
              ~active:Reactive.(
                  Source.signal current_tab
                  |> Signal.map
                    ~f:(function
                      | `Target_page tp ->
                        Target_page.target_id tp = id
                      | _ -> false)
                )
              ~on_click:Reactive.(fun _ ->
                  (* We need to check that the tabs is still in the
                     list of tabs, if it is not, it means that the user
                     just clicked on the `×`. This is due to bootstrap's
                     way of creating tabs, I don't know how to prevent the
                     event from being passed here. *)
                  let current_tabs =
                    Source.signal client.tabs |> Signal.value in
                  begin match
                    List.find current_tabs ~f:(Tab.eq (`Target_page tp))
                  with
                  | Some _ -> Source.set current_tab (`Target_page tp);
                  | None -> ()
                  end;
                  false)
              [
                Target_page.Html.title tp;
                pcdata " ";
                span ~a:[ a_class ["label"; "label-default"];
                          a_title (fmt "Close: %s" id);
                          a_onclick Reactive.(fun _ ->
                              let current =
                                Source.signal client.tabs |> Signal.value in
                              let visible =
                                Source.signal client.current_tab |> Signal.value in
                              begin match Tab.eq visible (`Target_page tp) with
                              | true ->
                                Source.set client.current_tab `Target_table
                              | _ -> ()
                              end;
                              Source.set client.tabs
                                (List.filter current
                                   ~f:(fun t -> not (Tab.eq t (`Target_page tp))));
                              false)
                        ] [pcdata "✖"];
              ]
          end
        )
    in
    (* div ~a:[ a_class ["container-fluid"]] [ *)
    Bootstrap.panel ~body:[
      Bootstrap.with_tab_bar ~tabs
        ~content:(
          Reactive_node.div
            Reactive.(Source.signal current_tab
                      |> Signal.map ~f:(function
                        | `Target_table ->
                          Target_table.Html.render client.target_table
                            ~kill_targets:(kill_targets client)
                            ~get_target:(fun id ->
                                Target_cache.get_target_summary_signal
                                  client.target_cache ~id)
                            ~target_link_on_click:(fun id ->
                                target_link_on_click_handler client ~id)
                            ~get_target_status:(fun id ->
                                Target_cache.get_target_flat_status_signal
                                  client.target_cache ~id)
                        | `Status -> status client
                        | `Processes_ui ->
                          Processes_ui.Html.render client.processes_ui
                        | `Target_page tp -> Target_page.Html.render tp)
                      |> Signal.singleton)
        );
    ]
end
