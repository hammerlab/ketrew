
open Ketrew_pure
open Internal_pervasives
open Pvem_js
open Reactive_html5

open Local_cache

module Markup_queries = struct

  let discriminate query =
    begin match String.split ~on:(`Character '/') query with
    | "ketrew-markup" :: more -> Some (String.concat ~sep:"/" more)
    | other -> None
    end

  let render content =
    let open H5 in
    begin try
      let ast = Display_markup.deserialize_exn content in
      Markup.to_html ast
    with
    | e ->
      let title =
        pcdata
          (fmt "Error parsing query-result: %s" (Printexc.to_string e)) in
      Bootstrap.error_box_pre ~title content
    end

end



type status = Time.t * [
  | `Unknown
  | `Ok of Protocol.Server_status.t
  | `Problem of string
]

type column = [
  | `Controls
  | `Arbitrary_index
  | `Name
  | `Id
  | `Backend
  | `Tags
  | `Status
]
let all_columns = [
  `Controls;
  `Arbitrary_index;
  `Name;
  `Id;
  `Backend;
  `Tags;
  `Status;
]
let default_columns = all_columns
let column_name : column -> _ =
  let open H5 in
  function
  | `Controls -> Bootstrap.wrench_icon ()
  | `Arbitrary_index -> span [pcdata "Index"]
  | `Name -> span [pcdata "Name"]
  | `Id -> span [pcdata "Unique Id"]
  | `Backend -> span [pcdata "Backend"]
  | `Tags -> span [pcdata "Tags"]
  | `Status -> span [pcdata "Status"]

let insert_column columns col =
  List.filter all_columns
    (fun c -> c = col || List.mem c columns)


module Target_id_set = struct
  include Set.Make(struct
      type t = string
      let compare a b = String.compare b a
    end)
  let add_list t list =
    List.fold ~init:t list ~f:(fun set elt ->
        add elt set)
  (* TODO: check whether (union (of_list list) t) is faster. *)
  let length = cardinal
  let to_list = elements
end

module Target_table = struct

  module Filter = struct

    type time_span = [
      | `Hours of float
      | `Days of float
      | `Weeks of float
    ]
    type ast = [
      | `All
      | `Created_in_the_past of time_span
      | `And of ast list
      | `Or of ast list
      | `Not of ast
      | `Status of [
          | `Simple of Target.State.simple
          | `Really_running
          | `Killable
          | `Dead_because_of_dependencies
        ]
      | `Has_tags of string list
    ]

    type t = {
      ast: ast;
    }
    let create () = {
      ast =
        (if !global_debug_level > 0 then `All
         else `Created_in_the_past (`Weeks 2.));
    }

    let examples = [
      { ast = `All }, "Get all the targets known to the server.";
      { ast = `Created_in_the_past (`Hours 5.) },
      "Get all the targets created in the past 5 hours.";
      { ast = `Created_in_the_past (`Days 0.5) },
      "Get all the targets created in the past half day.";
      { ast = `Created_in_the_past (`Weeks 2.5) },
      "Get all the targets created in the past 2.5 weeks.";
      { ast = `And [
            `Created_in_the_past (`Weeks 5.);
            `Or [
              `Status (`Simple `In_progress);
              `Status (`Simple `Successful);
            ];
          ] },
      "Get all the targets created in the past 5 weeks and \
       either successful or still in progress.";
      { ast = `And [
            `Created_in_the_past (`Weeks 5.);
            `Has_tags ["workflow-examples"];
          ] },
      "Get all the targets created in the past 5 weeks that \
       have the \"workflow-examples\" tag.";
      { ast = `And [
            `Created_in_the_past (`Weeks 4.2);
            `Status (`Simple `Failed);
            `Not (`Status `Dead_because_of_dependencies);
          ] },
      "Get all the targets created in the past 4.2 weeks that \
       died but not because of some their dependencies dying.";
      { ast = `Status `Killable },
      "Get all the targets that can be killed.";
    ]

    let to_server_query ast =
      let to_seconds =
        function
        | `Hours f -> f *. 60. *. 60.
        | `Days f -> f *. 60. *. 60. *. 24.
        | `Weeks f -> f *. 60. *. 60. *. 24. *. 7.
      in
      let rec to_filter =
        function
        | `All -> None
        | `Created_in_the_past time ->
          None
        | `And l -> Some (`And (List.filter_map l ~f:to_filter))
        | `Or l -> Some (`Or (List.filter_map l ~f:to_filter))
        | `Status s -> Some (`Status s)
        | `Has_tags sl ->
          Some (`And (List.map sl ~f:(fun s -> `Has_tag (`Equals s))))
        | `Not s ->
          Option.(to_filter s >>= fun n -> return (`Not n))
      in
      let rec to_time =
        function
        | `All -> Some `All
        | `Has_tags _
        | `Status _ -> None
        | `Created_in_the_past time ->
           Some (`Created_after (Time.now () -. (to_seconds time)))
        | `And l ->
          List.fold l ~init:None ~f:(fun prev v ->
              match prev, to_time v with
              | old, None -> old
              | None, new_one -> new_one
              | Some `All, Some new_one -> Some new_one
              | Some old, Some `All -> Some old
              | Some (`Created_after t1), Some (`Created_after t2) ->
                Some (`Created_after (max t1 t2)))
        | `Or l ->
          List.fold l ~init:None ~f:(fun prev v ->
              match prev, to_time v with
              | old, None -> old
              | None, new_one -> new_one
              | Some `All, Some new_one -> Some `All
              | Some old, Some `All -> Some `All
              | Some (`Created_after t1), Some (`Created_after t2) ->
                Some (`Created_after (min t1 t2)))
        | `Not _ ->
          None (* we reach the limits of this weird logic … *)
      in
      let time_constraint :> Protocol.Up_message.time_constraint =
        Option.value (to_time ast) ~default:(`Created_after 42.) in
      let filter :> Protocol.Up_message.filter =
        to_filter ast |> Option.value ~default:`True in
      { Protocol.Up_message. time_constraint ; filter }

    let target_query ?last_updated filter =
      let query = to_server_query filter.ast in
      match last_updated with
      | Some t ->
        (* Those 5 seconds actually generate traffic, but for know, who cares … *)
        { query with
          Protocol.Up_message.time_constraint = `Created_after (t -. 5.)}
      | None -> query

    let to_lisp { ast } =
      let time_span =
        function
        | `Hours h -> fmt "(hours %g)" h
        | `Days h -> fmt "(days %g)" h
        | `Weeks h -> fmt "(weeks %g)" h
      in
      let rec ast_to_lisp =
        function
        | `All -> "(all)"
        | `Created_in_the_past time ->
          fmt "(created-in-the-past %s)" (time_span time)
        | `And l ->
          fmt "(and %s)" (List.map ~f:ast_to_lisp l |> String.concat ~sep:" ")
        | `Or l ->
          fmt "(or %s)" (List.map ~f:ast_to_lisp l |> String.concat ~sep:" ")
        | `Has_tags sl ->
          fmt "(tags %s)" (List.map ~f:(fmt "%S") sl |> String.concat ~sep:" ")
        | `Not l -> fmt "(not %s)" (ast_to_lisp l)
        | `Status s ->
          begin match s with
          | `Simple `Activable -> "(is-activable)"
          | `Simple `In_progress -> "(is-in-progress)"
          | `Simple `Successful -> "(is-successful)"
          | `Simple `Failed -> "(is-failed)"
          | `Really_running -> "(is-really-running)"
          | `Killable -> "(is-killable)"
          | `Dead_because_of_dependencies -> "(is-dependency-dead)"
          end
      in
      ast_to_lisp ast

    exception Syntax_error of string
    let of_lisp v =
      begin try
        let fail ?sexp ffmt =
          Printf.ksprintf (fun s ->
              failwith (fmt "%s%s" s
                          (match sexp with
                          | Some sx ->
                            fmt "\nOn: %s" (Sexplib.Sexp.to_string_hum sx)
                          | None -> ""))
            ) ffmt in
        let rec parse_sexp sexp =
          let open Sexplib.Sexp in
          let time_span =
            function
            | List [Atom "hours"; Atom f] -> `Hours (float_of_string f)
            | List [Atom "days"; Atom f] -> `Days (float_of_string f)
            | List [Atom "weeks"; Atom f] -> `Weeks (float_of_string f)
            | sexp ->
              fail ~sexp "Syntax error while parsing time-span"
          in
          match sexp with
          | List [List _ as l] -> parse_sexp l
          | List [Atom "all"] -> `All
          | List [Atom "is-activable"] -> `Status (`Simple `Activable)
          | List [Atom "is-in-progress"] -> `Status (`Simple `In_progress)
          | List [Atom "is-successful"] -> `Status (`Simple `Successful)
          | List [Atom "is-failed"] -> `Status (`Simple `Failed)
          | List [Atom "is-really-running"] -> `Status `Really_running
          | List [Atom "is-killable"] -> `Status `Killable
          | List [Atom "is-dependency-dead"] ->
            `Status `Dead_because_of_dependencies
          | List [Atom "created-in-the-past"; time] ->
            `Created_in_the_past (time_span time)
          | List (Atom "or" :: tl) -> `Or (List.map tl ~f:parse_sexp)
          | List (Atom "and" :: tl) -> `And (List.map tl ~f:parse_sexp)
          | List [Atom "not"; tl] -> `Not (parse_sexp tl)
          | List (Atom "tags" :: tl) ->
            `Has_tags (List.map tl ~f:(function
              | Atom l -> l
              | List [Atom l] -> l
              | sexp ->
                fail ~sexp "syntax error while parsing tags"))
          | other ->
            fail ~sexp "Syntax error while parsing top-level expression"
        in
        let sexp = Sexplib.Sexp.of_string ("(" ^ v ^ ")") in
        let ast = parse_sexp sexp in
        `Ok {ast}
      with
      | Syntax_error s -> `Error s
      | Failure s -> `Error s
      | e -> 
        (`Error (Printexc.to_string e))
      end

  end

  type t = {
    target_ids: Target_id_set.t option Reactive.Source.t;
    target_ids_last_updated: Time.t option Reactive.Source.t; (* server-time *) 
    showing: (int * int) Reactive.Source.t;
    columns: column list Reactive.Source.t;
    filter_interface_visible: bool Reactive.Source.t;
    filter_interface_showing_help: bool Reactive.Source.t;
    filter: Filter.t Reactive.Source.t;
  }

  let create () =
    let target_ids = Reactive.Source.create None in
    let showing = Reactive.Source.create (0, 10) in
    let columns = Reactive.Source.create default_columns in
    let filter_interface_visible = Reactive.Source.create false in
    let filter = Filter.create () |> Reactive.Source.create in
    let target_ids_last_updated = Reactive.Source.create None in
    let filter_interface_showing_help = Reactive.Source.create false in
    let (_ : unit React.E.t) =
      let event = Reactive.Source.signal filter |> React.S.changes in
      React.E.map (fun _ ->
          Reactive.Source.set target_ids_last_updated None;
          Reactive.Source.set target_ids None;
          ())
        event
    in
    {target_ids;
     target_ids_last_updated;
     filter_interface_visible;
     filter_interface_showing_help;
     showing; columns; filter}


  let add_target_ids t ?server_time l =
    let current =
      Reactive.(Source.signal t.target_ids |> Signal.value)
      |> Option.value ~default:Target_id_set.empty
    in
    begin match server_time with
    | Some s -> Reactive.Source.set t.target_ids_last_updated (Some s)
    | None -> ()
    end;
    Reactive.Source.set t.target_ids
      (Some (Target_id_set.add_list current l));
    ()

end

module Target_page = struct
  type t = {
    target_id: string;
    showing_on_the_right: [`Flat_status | `Build_process_details ] Reactive.Source.t;
    build_process_on_display:
      [ `Nothing | `Result_of of string | `Raw_json ] Reactive.Source.t;
  }
  let create target_id = {
    target_id;
    showing_on_the_right = Reactive.Source.create `Flat_status;
    build_process_on_display = Reactive.Source.create `Nothing;
  }
  let eq a b =
    a.target_id = b.target_id
end
module Tab = struct
  type t = [
    | `Status
    | `Target_table
    | `Target_page of Target_page.t
  ]
  let is_target_page ~id =
    function
    | `Target_page t -> (String.compare t.Target_page.target_id id = 0)
    | `Status | `Target_table -> false
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
    let current = Source.signal t |> Signal.value in
    Source.set t (current @ [v])

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
    let open Reactive in
    let current = Source.signal t |> Signal.value in
    let item = {uid; name; start = Time.now (); finish = None} in
    Source.set t (current @ [item]);
    uid

  let declare_finished t uid =
    let open Reactive in
    let current = Source.signal t |> Signal.value in
    Source.set t
      (List.map current ~f:(fun i ->
           if i.uid = uid
           then { i with finish = Some (Time.now ())}
           else i))

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


  interesting_targets: Target_id_set.t Reactive.Source.t;
  (* TODO split into priority and prefetching *)

  status: status Reactive.Source.t;
  tabs: Tab.t list Reactive.Source.t;
  current_tab: Tab.t Reactive.Source.t;
  block_time_request: float;
  default_protocol_client_timeout: float;
  wait_before_retry_asynchronous_loop: float;
  reload_status_condition: unit Lwt_condition.t;
  target_table: Target_table.t;

  error_log: Error_log.t;

  async_task_log: Async_task_log.t;

  list_of_ids_log: Display_markup.t list Reactive.Source.t;
}


let create ~protocol_client () =
  let interesting_targets = Reactive.Source.create Target_id_set.empty in
  let status = Reactive.Source.create (Time.now (), `Unknown) in
  let current_tab = Reactive.Source.create ~eq:Tab.eq `Target_table in
  let target_table = Target_table.create () in
  let tabs =
    Reactive.Source.create
      ~eq:(fun la lb -> try List.for_all2 ~f:Tab.eq la lb with _ -> false)
      [ `Status; `Target_table ] in
  {
    protocol_client;
    target_cache = Target_cache.create ();
    interesting_targets;
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


let add_interesting_targets t l =
  let current =
    Reactive.Source.signal t.interesting_targets |> Reactive.Signal.value in
  Reactive.Source.set t.interesting_targets (Target_id_set.add_list current l)

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
    Reactive.(
      let current = Source.signal t.list_of_ids_log |> Signal.value in
      let with_ts =
        Display_markup.(concat [date Time.(now ()); text ": "; the_log]) in
      Source.set t.list_of_ids_log (with_ts :: current)
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
    Protocol_client.call ~timeout
      t.protocol_client (`Get_target_ids (query, options))
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
      Target_table.add_target_ids ?server_time t.target_table l;
      return ()
    | other ->
      fail (`Wrong_down_message other)
    end
  in
  let condition = Lwt_condition.create () in
  let woken_up = ref false in
  let rec loop () =
    (* add_log Display_markup.(text "Loop starts"); *)
    let query, and_block =
      Reactive.(
        let open Target_table in
        let tab = t.target_table in
        let last_updated_signal =
          Reactive.Source.signal tab.Target_table.target_ids_last_updated in
        let filter = Source.signal tab.filter |> Signal.value in
        let last_updated = Signal.value last_updated_signal in
        (Filter.target_query ?last_updated filter, (last_updated <> None))
      ) in
    Lwt.pick [
      begin try
        begin
          update_list_of_ids query ~and_block
        end
        >>< function
        | `Ok () ->
          (* add_log Display_markup.(textf "update_list_of_ids ends ok"); *)
          return ()
        | `Error e ->
          add_log Display_markup.(
              concat [
                textf "update_list_of_ids ends with error: ";
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
            textf "update_list_of_ids exception: %s" (Printexc.to_string e));
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
  asynchronous_loop t ~name:"list-of-ids" loop
    ~wake_up:("query-changes", condition);
  let (_ : unit React.E.t) =
    let event =
      Reactive.Source.signal t.target_table.Target_table.filter
      |> React.S.changes in
    React.E.map (fun fil ->
        add_log Display_markup.(
            concat [
              textf "Filter changes, waking-up";
              command (Target_table.Filter.to_lisp fil)
            ]
          );
        woken_up := true;
        Reactive.Source.set
          t.target_table.Target_table.target_ids_last_updated None;
        Lwt_condition.broadcast condition ()
      ) event
  in
  ()



let start_updating t =
  start_server_status_loop t;
  start_list_of_ids_loop t;
  let (_ : unit React.E.t) =
    let get_all_missing_summaries targets_ids =
      Log.(s "get_all_missing_summaries TRIGGERED !" %n
           % s "targets_ids has " % i (List.length targets_ids)
           % s " elements" @ verbose);
      let at_once = 50 in
      let sleep_time = 0.1 in
      let rec fetch_summaries ids =
        match ids with
        | [] ->
          (* Log.(s "fill_cache_loop.fetch_summaries nothing left to do" @ verbose); *)
          return ()
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
                  add_interesting_targets t [p];
                  Target_cache.update_target t.target_cache ~id (`Pointer p)
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
      asynchronous_loop t ~name:"fetch-summaries" (fun () ->
          fetch_summaries missing_ids)
    in
    let event =
      Reactive.Source.signal t.interesting_targets |> React.S.changes in
    React.E.map
      (fun set -> get_all_missing_summaries (Target_id_set.to_list set))
      event
  in
  let (_ : unit React.E.t) =
    let update_flat_states targets_ids =
      Log.(s "get_all_missing_states TRIGGERED !" %n
           % s "targets_ids has " % i (List.length targets_ids)
           % s " elements" @ verbose);
      let at_once = 10 in
      let sleep_time = 0.3 in
      let rec batch_fetching ids =
        match ids with
        | [] ->
          (* Log.(s "fill_cache_loop.fetch_summaries nothing left to do" *)
          (*      @ verbose); *)
          return ()
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
      let rec keep_fetching_for_active_targets tids =
        let to_fetch, some_in_progress =
          (* we want to fetch all the ones that can still change,
             if there are some active targets that “can” make them
             change. *)
          let in_progress = ref false in
          let filtered =
            List.filter tids ~f:(fun id ->
                let signal =
                  Target_cache.get_target_flat_status_signal t.target_cache id in
                let latest =
                  Reactive.Signal.value signal |> Target.State.Flat.latest in
                let not_finished =
                  not (Option.value_map ~default:false
                         ~f:Target.State.Flat.finished latest) in
                match Option.map ~f:Target.State.Flat.simple latest with
                | None
                | Some `In_progress -> in_progress := true; true
                | Some `Activable -> true
                | Some `Successful
                | Some `Failed -> not_finished)
          in
          (filtered, !in_progress)
        in
        match to_fetch with
        | [] -> return ()
        | some when not some_in_progress -> return ()
        | more  ->
          Log.(s "Batch fetching state for "
               % (match List.length to_fetch > 14 with
                 | true -> i (List.length to_fetch) % s " targets"
                 | false -> OCaml.list quote to_fetch)
               @ verbose);
          batch_fetching to_fetch
          >>= fun () ->
          sleep 3.
          >>= fun () ->
          keep_fetching_for_active_targets to_fetch
      in
      asynchronous_loop t ~name:"fetch-summaries" (fun () ->
          keep_fetching_for_active_targets targets_ids)
    in
    let event =
      Reactive.Source.signal t.interesting_targets
      |> React.S.diff (fun set1 set2 -> Target_id_set.diff set1 set2)
    in
    React.E.map
      (fun set -> update_flat_states (Target_id_set.to_list set))
      event
  in
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
        (`String result);
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

let kill_target t ~id ~on_result =
  call_unit_message t ~id ~on_result
    ~name:"kill" ~message:(`Kill_targets [id])

module Html = struct

  let status_icon t =
    let open H5 in
    let display (date, status) =
      match status with
      | `Ok status ->
        span ~a:[ a_class ["label"; "label-success"];
                  a_title (fmt "OK (%s)"
                             (status.Protocol.Server_status.time |> Time.to_filename));
                ] [pcdata "✔"]
      | `Unknown ->
        span ~a:[ a_class ["label"; "label-warning"];
                  a_title "Unknown";
                ] [pcdata "?"]
      | `Problem problem ->
        span ~a:[ a_class ["label"; "label-danger"];
                  a_title (fmt "Problem: %s" problem);
                ] [pcdata "✖"]
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
      tls (*  [`OpenSSL | `Native | `None ] *);
      preemptive_bounds (*  int * int *);
      preemptive_queue (*  int *);
      libev (*  bool *);
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
      h3 [pcdata "Client"];
      Reactive_node.div Reactive.(
          Source.signal t.status
          |> Signal.map ~f:begin fun (date, status) ->
            [
              h4 [
                pcdata (fmt "Status");
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
        Reactive_node.span Reactive.(
            Source.signal t.target_table.Target_table.target_ids
            |> Signal.map ~f:(function
              | Some s ->
                [pcdata (fmt "%d target-IDs in the table"
                           (Target_id_set.length s))]
              | None -> [pcdata "Fetching fresh target-IDs"]
              )
            |> Signal.list
          );
        pcdata ", ";
        Reactive_node.span Reactive.(
            Source.signal t.interesting_targets
            |> Signal.map ~f:(fun s ->
                [pcdata (fmt "%d “interesting” targets" (Target_id_set.length s))]
              )
            |> Signal.list
          );
        Markup.to_html
          (Target_cache.markup_counts t.target_cache);
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
      h4 [pcdata "List-of-IDs Loop"];
      Reactive_node.ul Reactive.(
          Source.signal t.list_of_ids_log
          |> Signal.map ~f:(List.map ~f:(fun log -> li [Markup.to_html log]))
          |> Signal.list
        );
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

  let target_status_badge ?(tiny = false) t ~id =
    let open H5 in
    let signal =
      Target_cache.get_target_flat_status_signal
        t.target_cache ~id in
    let content =
      Reactive.Signal.(
        map signal ~f:Target.State.Flat.latest
        |> map ~f:(function
          | None ->
            span ~a:[a_class ["label"; "label-warning"]]
              [pcdata "Unknown … yet"]
          | Some item ->
            let text_of_item item =
              fmt "%s%s%s"
                (Target.State.Flat.name item)
                (Target.State.Flat.message item
                 |> Option.value_map ~default:""
                   ~f:(fmt " (%s)"))
                (Target.State.Flat.more_info item
                 |> function
                 | [] -> ""
                 | more -> ": " ^ String.concat ~sep:", " more)
            in
            let label =
              match Target.State.Flat.simple item with
              | `Activable ->  "label-default"
              | `In_progress -> "label-info"
              | `Successful -> "label-success"
              | `Failed -> "label-danger"
            in
            let additional_info =
              List.take (
                Reactive.Signal.value signal |> Target.State.Flat.history
              ) 10
              |> List.map ~f:(fun item ->
                  div [
                    code [pcdata
                            (Target.State.Flat.time item |> Time.to_filename)];
                    br ();
                    pcdata (text_of_item item);
                  ])
              |> fun l ->
              if List.length l > 10 then
                l @ [div [code [pcdata "..."]]]
              else
                l
            in
            let visible_popover = Reactive.Source.create None in
            let popover =
              Reactive.(
                Source.signal visible_popover
                |> Signal.map ~f:(function
                  | Some (x,y) ->
                    let width = 500 in
                    div ~a:[
                      a_class ["popover"; "fade"; "left"; "in"];
                      a_style
                        (fmt "left: %dpx; top: 10px; position: fixed;  \
                              max-width: %dpx; width: %dpx; display: block"
                           (x - width - 100) width width);
                    ] [
                      h3 ~a:[a_class ["popover-title"]] [pcdata "State History"];
                      div ~a:[a_class ["popover-content"]] additional_info;
                    ]
                  | None -> div [])
                |> Signal.singleton
              ) in
            (* let span_id = Unique_id.create () in *)
            div [
              span ~a:[
                (* a_id span_id; *)
                a_class ["label"; label];
                a_onmouseover (fun ev ->
                    let mx, my =
                      Js.Optdef.case ev##.toElement
                        (fun () ->
                           Log.(s "toElement undefined !!" @ error);
                           (200, 200))
                        (fun eltopt ->
                           Js.Opt.case eltopt
                             (fun () ->
                                Log.(s "toElement defined but null!!" @ error);
                                (200, 200))
                             (fun elt ->
                                let rect = elt##getBoundingClientRect in
                                (int_of_float rect##.left,
                                 int_of_float rect##.top)))
                    in
                    Log.(s "Mouseover: " % parens (i mx % s ", " % i my)
                         @ verbose);
                    Reactive.Source.set visible_popover (Some (mx, my));
                    false);
                a_onmouseout (fun _ ->
                    Reactive.Source.set visible_popover None;
                    false);
              ] [pcdata (if tiny then " " else Target.State.Flat.name item)];
              Reactive_node.div popover;
            ]
          )
        |> singleton) in
    Reactive_node.div content

  let target_link_on_click_handler t ~id =
    let open Reactive in
    let current = Source.signal t.tabs |> Signal.value in
    begin match List.find current ~f:(Tab.is_target_page ~id) with
    | Some tp -> Source.set t.current_tab tp
    | None ->
      let new_tp = Target_page.create id in
      Log.(s "Created TP for " % quote id @ verbose);
      Source.set t.tabs (current @ [`Target_page new_tp])
    end;
    Log.(s "end of target_link_on_click_handler " % quote id @ verbose);
    ()

  let display_list_of_tags client tags =
    let open H5 in
    Bootstrap.collapsable_ul
      (List.map tags ~f:(fun tag ->
           small ~a:[
             a_class ["text-info"]
           ] [pcdata tag]))


  let filter_ui t =
    let open H5 in
    hide_show_div
      ~signal:(Reactive.Source.signal
                 t.target_table.Target_table.filter_interface_visible) [
      Reactive_node.div Reactive.(
          (Source.signal t.target_table.Target_table.filter)
          |> Signal.map ~f:(fun filter ->
              let status = Reactive.Source.create (`Ok filter) in
              div [
                div ~a:[a_class ["input-group"]] [
                  div ~a:[a_class ["input-group-addon"]] [
                    pcdata "Write your filtering query ";
                    local_anchor
                      ~on_click:(fun _ ->
                          Reactive.(
                            let src =
                              t.target_table.Target_table.
                                filter_interface_showing_help in
                            let current = Source.signal src |> Signal.value in
                            Source.set src (not current);
                            false))
                      [
                        span ~a:[
                          a_class ["label"; "label-default"]
                        ] [
                          pcdata "?"
                        ];
                      ];
                    pcdata ": ";
                  ];
                  input () ~a:[
                    a_class ["form-control"];
                    a_input_type `Text;
                    (* a_size 100; *)
                    a_autocomplete `Off;
                    a_value (Target_table.Filter.to_lisp filter);
                    a_oninput (fun ev ->
                        Js.Opt.iter ev##.target (fun input ->
                            Js.Opt.iter (Dom_html.CoerceTo.input input) (fun input ->
                                let v = input##.value |> Js.to_string in
                                Log.(s "input inputs: " % s v @ verbose);
                                Reactive.Source.set status
                                  (Target_table.Filter.of_lisp v)
                              );
                          );
                        false);
                  ];
                  Reactive_node.div ~a:[a_class ["input-group-btn"]]
                    Reactive.(
                      Source.signal status
                      |> Signal.map ~f:(function
                        | `Ok v ->
                          Bootstrap.button
                            ~enabled:(v <> filter)
                            ~on_click:(fun _ ->
                                Reactive.Source.set
                                  t.target_table.Target_table.filter v;
                                false)
                            [pcdata "Submit"]
                        | `Error e -> div []
                        )
                      |> Signal.singleton);
                ];
                Reactive_node.div Reactive.(
                    Source.signal status
                    |> Signal.map ~f:(
                      function
                      | `Ok _ -> div []
                      | `Error e ->
                        Bootstrap.error_box_pre ~title:(pcdata "Error") e
                    )
                    |> Signal.singleton
                  );
                let signal =
                  Source.signal
                    t.target_table.Target_table.filter_interface_showing_help in
                let current_filter = filter in
                hide_show_div ~signal [
                  div ~a:[a_class ["alert"; "alert-info"]] [
                    h3 [pcdata "Help"];
                    p [
                      pcdata "The language is based on S-Expressions \
                              (Like Lisp or Scheme), but you can omit the \
                              outermost parentheses.";
                    ];
                    p [pcdata "Here are some examples:"];
                    ul (List.map Target_table.Filter.examples
                          ~f:(fun (filter, description) ->
                              li [
                                code [pcdata (Target_table.Filter.to_lisp filter)];
                                strong [pcdata " → "];
                                span [pcdata description];
                                pcdata " ";
                                begin match current_filter = filter with
                                | true ->
                                  pcdata "It's the current one."
                                | false ->
                                  local_anchor
                                    ~on_click:(fun _ ->
                                        Reactive.Source.set
                                          t.target_table.Target_table.filter filter;
                                        false)
                                    [pcdata "Try it now!"]
                                end;
                              ]));
                  ];
                ];
              ])
          |> Signal.singleton
        );
    ]

  let target_table t =
    let open H5 in
    let showing = t.target_table.Target_table.showing in
    let controls =
      Reactive_node.div Reactive.(
          Signal.tuple_3
            (Source.signal showing)
            (Source.signal t.target_table.Target_table.target_ids)
            (Source.signal t.target_table.Target_table.filter_interface_visible)
          |> Signal.map ~f:(fun ((n_from, n_count), ids_option, filters_visible) ->
              let ids =
                Nonstd.Option.value ids_option ~default:Target_id_set.empty in
              let total = Target_id_set.length ids in
              let enable_if enabled on_click content =
                Bootstrap.button ~enabled ~on_click content in
              Bootstrap.button_group [
                Bootstrap.dropdown_button
                  ~content:[
                    pcdata (fmt "Showing %d per page" n_count)
                  ]
                  (List.map [10; 25; 50] ~f:(fun new_count ->
                       let content = [pcdata (fmt "Show %d" new_count)] in
                       if new_count = n_count
                       then `Disabled content
                       else
                         `Close (
                           (fun _ ->
                              Source.set showing (n_from, new_count);
                              false), content)
                     ));
                Bootstrap.button ~enabled:true
                  ~on_click:(fun _ ->
                      Source.set
                        t.target_table.Target_table.filter_interface_visible
                        (not filters_visible);
                      false)
                  (if filters_visible
                   then [pcdata "Hide filters"]
                   else [pcdata "Show filters"]);
                Bootstrap.dropdown_button
                  ~content:[
                    pcdata (fmt "Columns")
                  ]
                  (`Close ((fun _ ->
                       Source.set t.target_table.Target_table.columns all_columns;
                       false), [pcdata "ALL"])
                   :: List.map all_columns ~f:(fun col ->
                       let content = column_name col in
                       let signal =
                         Source.signal t.target_table.Target_table.columns 
                         |> Signal.map ~f:(fun current ->
                             List.mem ~set:current col)
                       in
                       let on_click _ =
                         let current = 
                           Source.signal t.target_table.Target_table.columns
                           |> Signal.value in
                         Source.set t.target_table.Target_table.columns
                           (if List.mem ~set:current col
                            then List.filter current ((<>) col)
                            else insert_column current col);
                         false in
                       `Checkbox (signal, on_click, content)
                     ));
                enable_if (n_from > 0)
                  (fun _ -> Source.set showing (0, n_count); false)
                  [pcdata (fmt "Start [1, %d]" n_count)];
                enable_if (n_from > 0)
                  (fun _ ->
                     Source.set showing
                       (n_from - (min n_count n_from), n_count);
                     false)
                  [pcdata (fmt "Previous %d" n_count)];
                enable_if  (n_from + n_count < total)
                  (fun _ ->
                     let incr = min (total - n_count - n_from) n_count in
                     Source.set showing (n_from + incr, n_count);
                     false)
                  [pcdata (fmt "Next %d" n_count)];
                enable_if (n_from + n_count < total
                           || (total - n_count + 1 < n_from
                               && total - n_count + 1 > 0))
                  (fun _ ->
                     Source.set showing (total - n_count, n_count);
                     false)
                  [pcdata (fmt "End [%d, %d]"
                             (max 0 (total - n_count + 1))
                             total)];
              ];
            )
          |> Signal.singleton)
    in
    let target_table =
      let row_of_id columns index id =
        let target_signal =
          Target_cache.get_target_summary_signal t.target_cache ~id in
        Reactive_node.tr Reactive.Signal.(
            map target_signal ~f:(function
              | `None ->
                [
                  td ~a:[
                    a_colspan (List.length columns);
                  ] [Bootstrap.muted_text (pcdata (fmt "Still fetching %s " id));
                     Bootstrap.loader_gif ();];
                ]
              | `Pointer p ->
                [ (* This should not show, as the table is supposed to
                     avoid target-pointers *)
                  td ~a:[
                    a_colspan (List.length columns);
                  ] [pcdata (fmt "Pointer to %s" p)]
                ]
              | `Summary trgt ->
                List.map columns ~f:(function
                  | `Controls ->
                    td [
                      local_anchor ~on_click:Reactive.(fun _ ->
                          target_link_on_click_handler t ~id;
                          false) [
                        Bootstrap.north_east_arrow_label ();
                      ]
                    ]
                  | `Arbitrary_index -> td [pcdata (fmt "%d" (index + 1))]
                  | `Name -> td [pcdata (Target.Summary.name trgt)]
                  | `Id -> td [pcdata (Target.Summary.id trgt)]
                  | `Backend ->
                    begin match Target.Summary.build_process trgt with
                    | `No_operation -> td []
                    | `Long_running (name, _) -> td [code [pcdata name]]
                    end
                  | `Tags ->
                    td [
                      display_list_of_tags t (Target.Summary.tags trgt);
                    ]
                  | `Status ->
                    td [target_status_badge t ~id;]
                  ))
            |> list)
      in
      let table_head columns =
        thead [tr (List.map columns ~f:(fun col -> th [column_name col]))] in
      Reactive_node.div
        Reactive.(
          Signal.tuple_3 
            (Source.signal t.target_table.Target_table.target_ids)
            (* |> Signal.map ~f:Target_id_set.to_list) *)
            (Source.signal showing)
            (Source.signal t.target_table.Target_table.columns)
          |> Signal.map ~f:begin fun (target_ids_opt, (index, count), columns) ->
            begin match target_ids_opt with
            | Some tids ->
              let target_ids = Target_id_set.to_list tids in
              let ids = List.take (List.drop target_ids index) count in
              add_interesting_targets t
                (let greedy_index = max 0 (index - count) in
                 let greedy_count = count + count + (index - greedy_index) in
                 List.take (List.drop target_ids greedy_index) greedy_count);
              Bootstrap.table_responsive
                ~head:(table_head columns)
                ~body:(List.mapi ids
                         ~f:(fun ind id -> row_of_id columns (index + ind) id))
            | None ->
              div ~a:[a_class ["alert"; "alert-warning"]] [
                strong [pcdata "Fetching targets "];
                Bootstrap.loader_gif ();
              ]
            end
          end
          |> Signal.singleton
        )
    in
    (* div ~a:[a_class ["container"]] [ *)
    Bootstrap.panel ~body:[
      controls;
      filter_ui t;
      target_table
    ]

  let summarize_id id =
    String.sub id ~index:10 ~length:(String.length id - 10)
    |> Option.value_map ~default:id ~f:(fmt "…%s")

  let target_page_tab_title client ~id =
    let open H5 in
    let colorize_classes =
      Reactive.Signal.(
        Target_cache.get_target_flat_status_signal client.target_cache ~id
        |> map ~f:Target.State.Flat.latest
        |> map ~f:(function
          | None -> ["text-warning"]
          | Some item ->
            let text_class =
              match Target.State.Flat.simple item with
              | `Failed -> "text-danger"
              | `In_progress -> "text-info"
              | `Activable -> "text-muted"
              | `Successful -> "text-success"
            in
            [text_class]
          )
      ) in
    let rec text id =
      let open Reactive.Signal in
      Target_cache.get_target_summary_signal client.target_cache ~id
      |> map ~f:(function
        | `None ->
          span ~a:[a_title "Not yet fetched"] [pcdata (summarize_id id)]
        | `Pointer id ->
          Reactive_node.span ~a:[a_title "Pointer"] (text id)
        | `Summary summary ->
          span ~a:[
            a_title id;
          ] [
            pcdata (Target.Summary.name summary);
          ])
      |> singleton in
    span ~a:[
      Reactive_node.a_class colorize_classes;
    ] [
      Reactive_node.span (text id);
    ]

  let target_controls client ~id =
    let open H5 in
    let restarting = Reactive.Source.create `None in
    let killing = Reactive.Source.create `None in
    Reactive_node.div Reactive.(
        Signal.tuple_2
          (Source.signal restarting)
          (Source.signal killing)
        |> Signal.map ~f:(fun (rest, kill) ->
            [
              Bootstrap.button_group ~justified:false [
                Bootstrap.button 
                  ~enabled:(rest <> `In_progress)
                  ~on_click:(fun _ ->
                      Log.(s "Restart Target" @ verbose);
                      Reactive.Source.set restarting `In_progress;
                      restart_target client ~id
                        ~on_result:(fun r ->
                            Reactive.Source.set restarting (`Result r);
                          );
                      false)
                  (match rest with
                  | `None | `Result _ -> [pcdata "Restart"]
                  | `In_progress ->
                    [pcdata "Restarting "; Bootstrap.loader_gif ()]);
                Bootstrap.button 
                  ~enabled:(kill <> `In_progress)
                  ~on_click:(fun _ ->
                      Log.(s "Kill Target" @ verbose);
                      Reactive.Source.set killing `In_progress;
                      kill_target client ~id
                        ~on_result:(fun r ->
                            Reactive.Source.set killing (`Result r);
                          );
                      (* Reactive.Source.set showing_on_the_right `Flat_status; *)
                      false)
                  [pcdata "Kill"];
              ];
              div (
                match rest with
                | `None | `In_progress -> []
                | `Result (`Ok ()) ->
                  [Bootstrap.success_box [pcdata "Restarted OK"]]
                | `Result (`Error e) ->
                  [Bootstrap.error_box_pre ~title:(pcdata "Restarting error") e]
              );
              div (
                match kill with
                | `None | `In_progress -> []
                | `Result (`Ok ()) ->
                  [Bootstrap.success_box [pcdata "Killing in progress"]]
                | `Result (`Error e) ->
                  [Bootstrap.error_box_pre ~title:(pcdata "Killing error") e]
              );
            ])
        |> Signal.list
      )

  let target_summary_panel client ~id =
    let open H5 in
    let rec make_body id =
      Reactive_node.div Reactive.Signal.(
          Target_cache.get_target_summary_signal client.target_cache ~id
          |> map ~f:(function
            | `None ->
              div ~a:[a_title "Not yet fetched"] [
                pcdata "Still fetching summary for ";
                pcdata (summarize_id id)
              ]
            | `Pointer pid ->
              div ~a:[a_title (fmt "Pointer %s → %s" id pid)]
                [make_body pid]
            | `Summary summary ->
              let row head content =
                tr [
                  th head;
                  td content;
                ] in
              let simple_row head content =
                tr [
                  th [pcdata head];
                  td content;
                ] in
              let code_row n v = simple_row n [code [pcdata v]] in
              let list_of_ids_row name ids =
                simple_row name [
                  ul
                    (List.map ids ~f:(fun id ->
                         li [
                           local_anchor
                             ~on_click:Reactive.(fun _ ->
                                 target_link_on_click_handler client ~id;
                                 false)
                             [target_page_tab_title client ~id]
                         ]))
                ]
              in
              add_interesting_targets client
                Target.Summary.(depends_on summary
                                @ on_success_activate summary
                                @ on_failure_activate summary);
              Bootstrap.table_responsive
                ~head:(thead [])
                ~body:[
                  simple_row "Name" [target_page_tab_title client ~id];
                  code_row "ID" (Target.Summary.id summary);
                  simple_row "Controls" [target_controls client ~id];
                  begin
                    let potential_button, content =
                      Target.Summary.metadata summary
                      |> Option.value_map
                        ~default:(None, pcdata "None")
                        ~f:(fun (`String s) -> Bootstrap.collapsable_pre s) in
                    let head =
                      pcdata "Metadata"
                      ::
                      (match potential_button with
                      | None -> []
                      | Some b -> [pcdata " ";b]) in
                    row head [content]
                  end;
                  row
                    [pcdata "Tags"]
                    [display_list_of_tags client
                       (Target.Summary.tags summary)];
                  list_of_ids_row "Depends on"
                    (Target.Summary.depends_on summary);
                  list_of_ids_row "On failure activates"
                    (Target.Summary.on_failure_activate summary);
                  list_of_ids_row "On success activates"
                    (Target.Summary.on_success_activate summary);
                  code_row "Equivalence"
                    (Target.Summary.equivalence summary
                     |> function
                     | `None -> "None"
                     | `Same_active_condition -> "Same-active-condition");
                  simple_row "Condition"
                    [Target.Summary.condition summary
                     |> Option.value_map ~default:(pcdata "") ~f:(fun c ->
                         Target.Condition.markup c
                         |> Markup.to_html
                           ~collapse_descriptions:["Host", "Name"])];
                  code_row "Build-process"
                    (Target.Summary.build_process summary
                     |> function
                     | `No_operation -> "No-op"
                     | `Long_running (name, init) -> "Backend: " ^ name);
                ]
            )
          |> singleton)
        in
    Bootstrap.panel ~body:[make_body id]

  let flat_status_display_div client ~id =
    let open H5 in
    Reactive_node.div (
      Target_cache.get_target_flat_status_signal client.target_cache ~id
      |> Reactive.Signal.map ~f:(fun state ->
          let text_of_item item =
            fmt "%s%s%s"
              (Target.State.Flat.name item)
              (Target.State.Flat.message item
               |> Option.value_map ~default:""
                 ~f:(fmt " (%s)"))
              (Target.State.Flat.more_info item
               |> function
               | [] -> ""
               | more -> ": " ^ String.concat ~sep:", " more)
          in
          let additional_info =
            (state |> Target.State.Flat.history)
            |> List.map ~f:(fun item ->
                div [
                  code [pcdata
                          (Target.State.Flat.time item |> Time.to_filename)];
                  br ();
                  pcdata (text_of_item item);
                ])
          in
          div additional_info
        )
      |> Reactive.Signal.singleton)

  let build_process_display_div client tp =
    let open H5 in
    let id = tp.Target_page.target_id in
    let on_display_right_now = tp.Target_page.build_process_on_display in
    let control ~content ~help ~self current =
      Bootstrap.button
        ~enabled:(current <> self)
        ~on_click:(fun _ -> Reactive.Source.set on_display_right_now self; false)
        [span ~a:[a_title help] content]
    in
    let raw_json_control current =
      control ~content:[pcdata "Raw JSON"] ~self:`Raw_json current
        ~help:"Display the JSON defined by the backend pluging" in
    let query_additional_controls current = [
      Bootstrap.button
        ~on_click:(fun _ ->
            reload_available_queries client ~id;
            begin match current with
            | `Nothing | `Raw_json -> ()
            | `Result_of query -> reload_query_result client ~id ~query;
            end;
            false)
        [Bootstrap.reload_icon ()];
    ]
    in
    let make_toolbar list_of_lists =
      div  ~a:[a_class ["btn-toolbar"]]
        (List.filter_map list_of_lists ~f:(function
           | [] -> None
           | more -> Some (Bootstrap.button_group ~justified:false more))) in
    let rec make_div id =
      Reactive_node.div Reactive.Signal.(
          Target_cache.get_target_summary_signal client.target_cache ~id
          |> map ~f:(function
            | `None ->
              div ~a:[a_title "Not yet fetched"] [
                pcdata "Still fetching summary for ";
                pcdata (summarize_id id)
              ]
            | `Pointer pid ->
              div ~a:[a_title "Pointer"] [make_div pid]
            | `Summary summary ->
              begin match Target.Summary.build_process summary with
              | `No_operation -> div [h3 [pcdata "No-operation"]]
              | `Long_running (name, init) ->
                div [
                  h3 [pcdata (fmt "Using %s" name)];
                  Reactive_node.div Reactive.(
                      Signal.tuple_2
                        (Source.signal on_display_right_now)
                        (get_available_queries client ~id)
                      |> Signal.map
                        ~f:(fun (current, query_descriptions) ->
                            match query_descriptions with
                            | `None ->
                              make_toolbar [
                                [raw_json_control current];
                                [
                                  control
                                    ~content:[pcdata "Loading"; Bootstrap.loader_gif ()]
                                    ~help:"Fetching query descriptions …"
                                    ~self:`Nothing `Nothing
                                ]
                              ]
                            | `Descriptions qds ->
                              make_toolbar [
                                [raw_json_control current];
                                List.map qds ~f:(fun (qname, help) ->
                                    match Markup_queries.discriminate qname with
                                    | Some subname ->
                                      control
                                        ~content:[pcdata subname]
                                        ~help
                                        ~self:(`Result_of qname) current
                                    | None ->
                                      control
                                        ~content:[pcdata (fmt "%s:%s" name qname)]
                                        ~help
                                        ~self:(`Result_of qname) current);
                                query_additional_controls current;
                              ]
                          )
                      |> Signal.singleton);
                  Reactive_node.div Reactive.(
                      Source.signal on_display_right_now
                      |> Signal.map ~f: begin function
                      | `Nothing -> pcdata "Nothing here"
                      | `Raw_json ->
                        pre [
                          pcdata (
                            let pretty_json =
                              Yojson.Safe.(from_string init
                                           |> pretty_to_string ~std:true) in
                            pretty_json
                          );
                        ]
                      | `Result_of query ->
                        Reactive_node.div Reactive.(
                            get_query_result client ~id ~query
                            |> Signal.map ~f:(function
                              | `None -> [pcdata (fmt "Calling “%s”" query);
                                          Bootstrap.loader_gif ()]
                              | `String r ->
                                begin match Markup_queries.discriminate query with
                                | Some _ ->
                                  [Markup_queries.render r]
                                | None -> [pre [pcdata r]]
                                end
                              | `Error e ->
                                let title =
                                  pcdata
                                    (fmt "Error while calling %s:" query) in
                                [Bootstrap.error_box_pre e ~title]
                              )
                            |> Signal.list
                          )

                    end
                    |> Signal.singleton);
              ]
            end)
        |> Reactive.Signal.singleton)
    in
    make_div id

  let target_page client tp =
    let id = tp.Target_page.target_id in
    let showing_on_the_right = tp.Target_page.showing_on_the_right in
    add_interesting_targets client [id];
    let open H5 in
    let two_columns ~left ~right =
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["col-md-4"]] left;
        (* div ~a:[a_class ["col-md-4"]] middle; *)
        div ~a:[a_class ["col-md-8"]] right;
      ] in
    let target_sumary = target_summary_panel client ~id in
    let target_status =
      Bootstrap.panel ~body:[
        Reactive_node.div (
          Reactive.Source.signal showing_on_the_right
          |> Reactive.Signal.map ~f:(fun showing ->
              Bootstrap.button_group ~justified:true [
                Bootstrap.button 
                  ~enabled:(showing <> `Flat_status)
                  ~on_click:(fun _ ->
                      Reactive.Source.set showing_on_the_right `Flat_status;
                      false)
                  [pcdata "Status history"];
                Bootstrap.button 
                  ~enabled:(showing <> `Build_process_details)
                  ~on_click:(fun _ ->
                      Reactive.Source.set showing_on_the_right `Build_process_details;
                      false)
                  [pcdata "Backend details/queries"];
              ])
          |> Reactive.Signal.singleton);
        Reactive_node.div (
          Reactive.Source.signal showing_on_the_right
          |> Reactive.Signal.map ~f:(fun showing ->
              begin match showing with
              | `Flat_status -> flat_status_display_div client ~id
              | `Build_process_details -> build_process_display_div client tp
              end)
          |> Reactive.Signal.singleton)
      ]
    in
    two_columns
      ~left:[target_sumary]
      ~right:[target_status]

  let render client =
    let open H5 in
    let current_tab = client.current_tab in
    let tabs =
      Reactive.Source.signal client.tabs
      |> Reactive.Signal.map ~f:(fun tabs ->
          List.map tabs ~f:begin function
          | `Target_table ->
            Bootstrap.tab_item
              ~active:Reactive.(
                  Source.signal current_tab
                  |> Signal.map ~f:(function `Target_table -> true | _ -> false))
              ~on_click:(fun _ -> Reactive.Source.set current_tab `Target_table; false)
              [Reactive_node.pcdata
                 Reactive.(
                   Signal.tuple_2
                     (Source.signal client.target_table.Target_table.showing)
                     (Source.signal client.target_table.Target_table.target_ids)
                   |> Signal.map ~f:(fun ((n_from, n_count), target_ids) ->
                       match target_ids with
                       | None -> "Fetching targets"
                       | Some tids ->
                         let total = Target_id_set.length tids in
                         (fmt "Target-table ([%d, %d] of %d)"
                            (min total (n_from + 1))
                            (min (n_from + n_count) total)
                            total)))]
          | `Status ->
            Bootstrap.tab_item
              ~active:Reactive.(
                  Source.signal current_tab
                  |> Signal.map ~f:(function `Status -> true | _ -> false)
                )
              ~on_click:(fun _ -> Reactive.Source.set current_tab `Status; false)
              [pcdata "Status"]
          | `Target_page tp ->
            let id = tp.Target_page.target_id in
            Bootstrap.tab_item
              ~active:Reactive.(
                  Source.signal current_tab
                  |> Signal.map
                    ~f:(function
                      | `Target_page {Target_page.target_id; _} ->
                        target_id = id
                      | _ -> false)
                )
              ~on_click:Reactive.(fun _ ->
                  (* We need to check that the tabs is still in the
                     list of tabs, if it is not, it means that the user
                     just clicked on the `×`. This is due to bootstrap's
                     way of creating tabs, I don't know how to avoid the
                     event to be passed here. *)
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
                target_page_tab_title client ~id;
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
                        | `Target_table -> target_table client
                        | `Status -> status client
                        | `Target_page tp -> target_page client tp)
                      |> Signal.singleton)
        );
    ]
end
