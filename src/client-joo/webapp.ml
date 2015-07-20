open Ketrew_pure
open Internal_pervasives

module Reactive = struct
  (* Trying to wrap React and ReactiveData stuff *)

  module Source = struct
    type 'a t = {
      signal: 'a React.S.t;
      set: 'a -> unit;
    }

    let create v =
      let signal, set = React.S.create v in
      {signal; set}

    let set t v = t.set v

    let signal t = t.signal

  end

  module Signal = struct

    type 'a t = 'a React.S.t
  
    let map s ~f = React.S.map f s
    let value s = React.S.value s

    let singleton t =
      let open ReactiveData.RList in
      make_from
        [React.S.value t]
        (React.E.map (fun e -> Set [e]) (React.S.changes t))

    let list t =
      let open ReactiveData.RList in
      make_from
        (React.S.value t)
        (React.E.map (fun e -> Set e) (React.S.changes t))

    let tuple_2 a b =
      React.S.l2 (fun a b -> (a, b)) a b

    let tuple_3 a b c =
      React.S.l3 (fun a b c -> (a, b, c)) a b c
  end

  module Option = struct
    type 'a t =  'a option Source.t 
    open Source
    let create () = create None
    let switch t v =
      match signal t |> React.S.value with
      | None -> t.set (Some v)
      | Some v -> t.set None
    let singleton_or_empty (t : _ t) =
      let open ReactiveData.RList in
      let s = signal t in
      make_from
        (match React.S.value s with None -> [] | Some e -> [e])
        (React.E.map (function
           | Some e -> Set [e]
           | None -> Set []
           ) (React.S.changes s))
  end

end

module H5 = struct
  include Tyxml_js.Html5
  module Reactive_node = Tyxml_js.R.Html5

  module Bootstrap = struct

    let loader_gif () =
      let src =
        "https://cdn.rawgit.com/hammerlab/cycledash/\
         99431cf62210523d352b37c01f6b0dea8fa921f4/\
         cycledash/static/img/loader.gif" in
      img ~alt:"Spining loader from Cycledash" ~src ()

    let muted_text content =
      span ~a:[a_class ["text-muted"];] [content]
        
    let wrench_icon () =
      (* span ~a:[a_class ["glyphicon"; "glyphicon-wrench"]] [] *)
      span
        ~a:[a_style "font-weight: normal"]
        [pcdata "🔧"]

    let tab_item ~active ~on_click content = (active, on_click, content)
    let with_tab_bar ~tabs ~content =
      nav ~a:[a_class ["navbar"; "no-navbar-static-top"]] [
        div  [
          Reactive_node.ul ~a:[a_class ["nav"; "nav-tabs"]] (
            Reactive.Signal.map tabs ~f:(fun tablist ->
                List.map tablist ~f:(fun (active_signal, on_click, content_list) ->
                    let active_class =
                      Reactive.Signal.map
                        ~f:(function | true -> ["active"] | false -> [])
                        active_signal in
                    li ~a:[ Reactive_node.a_class active_class ] [
                      a ~a:[ (* The `a` must be directly under the `li`. *)
                        a_href "#"; (* The `href` transforms the mouse like a link. *)
                        a_onclick on_click;
                      ] content_list;
                    ]
                  )
              )
            |> Reactive.Signal.list);
          content;
        ]
      ]

    let disabled_li content =
      li ~a:[a_class ["disabled"]] [a content]

    let dropdown_button ~content items =
      let visible = Reactive.Source.create false in
      let toggle _ =
        Reactive.(
          Source.set visible
            (Source.signal visible |> Signal.value |> not));
        false in
      let menu =
        ul ~a:[a_class ["dropdown-menu"]]
          (List.map items ~f:(function
             | `Disabled content -> disabled_li content
             | `Close (on_click, content) ->
               let on_click ev =
                 toggle ev |> ignore;
                 on_click ev in
               li [a ~a:[a_onclick on_click] content]
             | `Checkbox (status_signal, on_click, content) ->
               let tick_or_cross =
                 status_signal
                 |> Reactive.Signal.map ~f:(function
                   | true -> " ✔"
                   | false -> " ✖") in
               li [a ~a:[a_onclick on_click;]
                     [content; Reactive_node.pcdata tick_or_cross]]
             )) in
      let classes =
        Reactive.Source.signal visible
        |> Reactive.Signal.map ~f:(function
          | true -> ["btn-group"; "open"]
          | false -> ["btn-group"])
      in
      div ~a:[Reactive_node.a_class classes] [
        button ~a:[
          a_class ["btn"; "btn-default"; "dropdown-toggle"];
          a_onclick toggle
        ] (content @ [pcdata " "; span ~a:[a_class ["caret"]] []]);
        menu;
      ]

    let button_group ?(justified=true) content =
      div content
        ~a:[a_class ["btn-group";
                     if justified then "btn-group-justified" else "";]]

    let button =
      let in_group b = button_group ~justified:false [b] in
      (* buttons must be in a group for justification to work *)
      function
      | `Disabled content ->
        button ~a:[ a_class ["btn"; "btn-default"; "disabled"]] content
        |> in_group
      | `Enabled (on_click, content) ->
        button content
          ~a:[ a_class ["btn"; "btn-default"; ];
               a_onclick on_click;
             ]
        |> in_group


    let pagination items =
      nav [
        ul ~a:[a_class ["pagination"]]
          (List.map items ~f:(function
             | `Disabled content ->
               disabled_li content
             | `Enabled (on_click, content) ->
               li [a ~a:[a_onclick on_click] content]
             ))
      ]

    let panel ~body =
      div ~a:[ a_class ["not-container-fluid"]] [
        div ~a:[ a_class ["panel"; "panel-default"]] [
          div ~a:[ a_class ["panel-body"; ]] body
        ];
      ]

    let table_responsive ~head ~body =
      div ~a:[a_class ["table-responsive"]] [
        tablex
          ~thead:head
          ~a:[a_class ["table"; "table-condensed";
                       "table-bordered"; "table-hover"]] [
          tbody body
        ]
      ]

    let collapsable_pre ?(first_line_limit = 30) string =
      match String.find string ~f:((=) '\n') with
      | None -> (None, pre [pcdata string])
      | Some end_of_first_line ->
        let expanded = Reactive.Source.create false in
        let content_signal =
          Reactive.Source.signal expanded
          |> Reactive.Signal.map ~f:(function
            | true -> string
            | false ->
              (String.sub_exn string ~index:0
                 ~length:(min end_of_first_line first_line_limit)
               ^ " [...]"))
        in
        let expand_button =
          Reactive.Source.signal expanded
          |> Reactive.Signal.map ~f:(fun expandedness ->
              a ~a:[
                a_onclick (fun _ ->
                    Reactive.Source.set expanded (not expandedness);
                    false);
              ] [
                pcdata (if expandedness then "⊖" else "⊕")
              ]
            )
          |> Reactive.Signal.singleton
        in
        (Some (Reactive_node.span expand_button),
         pre [
           Reactive_node.pcdata content_signal;
         ])

  end
end

module Lwt_result = struct
  (* Similar to `Pvem_lwt_unix`, we embed the error monad into `Lwt_js`.  *)
  include Pvem.With_deferred(Lwt)
  let sleep f =
    wrap_deferred ~on_exn:(fun e -> `Exn e) begin fun () ->
      Lwt_js.sleep f
    end
end
open Lwt_result

module Protocol_client = struct

  type connection =
    | JSONP of string

        (* (Protocol.Up_message.t -> string (\* callback for json functions *\) -> string) *)

  type t = {
    name: string;
    connection: connection;
  }

  let create ~name connection = {name; connection}

  let name {name; _} = name
    
  let jsonp_of_raw_url ~name url = create ~name (JSONP url)

  let jsonp_call {connection = JSONP url; _} =
    (fun msg callback_name ->
       fmt "%s&callback=%s&message=%s"
         url
         callback_name
         (Protocol.Up_message.serialize msg |> Url.urlencode))

  let jsonp_of_url ~protocol ~host ?port ~token () =
    let url =
      Printf.sprintf  "%s//%s%s/apijsonp?token=%s"
        protocol host
        (Option.value_map port ~f:(fmt ":%d") ~default:"")
        token
    in
    jsonp_of_raw_url url

  let log {connection} =
    Log.(braces
           (match connection with
           | JSONP url -> s "JSONP→" % s url))


  let of_current () : t option =
    Log.(s "Protocol_client.of_current"
         % s "Arguments: "
         % OCaml.list (fun  (x, y) -> sf "(%s, %s)" x y) Url.Current.arguments
         % n
         % s "Protocol: " % s Url.Current.protocol
         @ verbose);
    let token_opt =
      List.find_map Url.Current.arguments  ~f:(function
        | ("?token", t) ->  Some t
        (* this weird case bypasses https://github.com/ocsigen/js_of_ocaml/issues/272 *)
        | ("token", t) ->  Some t
        | _ -> None)
    in
    match token_opt, Url.Current.protocol with
    | Some token, not_file when not_file <> "file:" ->
      Some (jsonp_of_url ~name:"Main"
              ~protocol:Url.Current.protocol
              ~host:Url.Current.host
              ?port:Url.Current.port
              ~token ())
    | Some tok, prot ->
      Log.(s "There is a token (" % quote tok % s ") but protocol is "
           % quote prot @ warning);
      None
    | None, _ -> None

  let base_url {connection; _} =
    match connection with
    | JSONP url -> url


  let of_window_object () =
    let base_urls =
      try
        Js.Unsafe.get Dom_html.window (Js.string "ketrew_connections")
        |> Js.to_array
        |> Array.map ~f:(fun array ->
            Js.to_array array |> Array.map ~f:Js.to_string |> Array.to_list)
        |> Array.to_list
      with e ->
        Log.(s "getting window.ketrew_connections: "
             % exn e @ warning);
        []
    in
    List.filter_map base_urls ~f:(function
      | [ name; url ] -> Some (jsonp_of_raw_url ~name url)
      | other ->
        Log.(s "wrong specification of clients: "
             % OCaml.list quote other @ error);
        None)


  let call ?(timeout = 20.) t msg =
    match t.connection with
    | JSONP url ->
      wrap_deferred
        ~on_exn:(fun e -> `Protocol_client (`JSONP (`Exn e)))
        Lwt.(fun () ->
            pick [
              begin
                Jsonp.call_custom_url (jsonp_call t msg)
                >>= fun msg ->
                return (`String msg)
              end;
              begin
                Lwt_js.sleep timeout
                >>= fun () ->
                return (`Timeout)
              end;
            ])
      >>= fun result ->
      begin match result with
      | `String content ->
        let content = (Js.to_string (content##message)) in
        (* Log.(s "Received string: " % big_byte_sequence content @ verbose); *)
        (* used the example in toplevel.ml:
           https://github.com/ocsigen/js_of_ocaml/commit/65fcc49cfe9d4df8fd193eb5330953923a001618 *)
        let got = content  |> Url.urldecode in
        (* debug "Got content %S" got; *)
        Log.(s "Decoded string: "
             % big_byte_sequence ~max_length:100 got @ verbose);
        begin try
          return (Protocol.Down_message.deserialize_exn got)
        with e -> 
          Log.(s "Deserializing message "
               % big_byte_sequence ~max_length:200 got
               %sp % s "Exn: " % exn e
               @ verbose);
          fail (`Protocol_client (`JSONP (`Parsing_message (got, e))))
        end
      | `Timeout ->
        fail (`Protocol_client (`JSONP `Timeout))
      end

  module Error = struct
    let to_string = function
    | `JSONP (`Timeout) -> fmt "JSONP Timeout"
    | `JSONP (`Exn e) -> fmt "JSONP Exception: %s" (Printexc.to_string e)
    | `JSONP (`Parsing_message (_, e)) ->
      fmt "JSONP Parsing Exception: %s" (Printexc.to_string e)
  end
end




module Target_cache  = struct
  type target_knowledge = {
    summary: Target.Summary.t option;
    full: Target.t option;
  }
  type t = {
    targets: (Target.id, target_knowledge option Reactive.Source.t) Hashtbl.t;
    flat_statuses: (Target.id, Target.State.Flat.t Reactive.Source.t) Hashtbl.t; 
  }

  let create () = {
    targets = Hashtbl.create 42;
    flat_statuses = Hashtbl.create 42;
  }

  let _get_target_knowledge {targets} ~id =
    try (Hashtbl.find targets id)
    with _ ->
      (* Log.(s "Target-cache: miss on " % s id @ verbose); *)
      let signal = Reactive.Source.create None in
      (* Log.(s "Created `None` target signal for " % s id @ verbose); *)
      Hashtbl.replace targets id signal;
      signal

  let _get_target_flat_status {flat_statuses; _} ~id =
    try (Hashtbl.find flat_statuses id)
    with _ ->
      let signal = Reactive.Source.create (Target.State.Flat.empty ()) in
      Hashtbl.replace flat_statuses id signal;
      signal

  let summary knowledge =
    match knowledge with
    | Some {summary = Some s; _} -> Some s
    | Some {full = Some f; _} -> Some (Target.Summary.create f)
    | _ -> None

  let get_target_summary_signal t ~id =
    _get_target_knowledge t ~id
    |> Reactive.Source.signal |> Reactive.Signal.map ~f:summary

  let get_target_flat_status_signal t ~id =
    _get_target_flat_status t ~id
    |> Reactive.Source.signal

  let update_target t ~id what =
    let signal = _get_target_knowledge t id in
    let current = Reactive.(Source.signal signal |> Signal.value) in
    let new_value =
      match current, what with
      | None, `Summary sum -> { summary = Some sum; full = None }
      | None, `Full f -> { summary = None; full = Some f }
      | Some {summary; full}, `Full f -> { summary; full = Some f }
      | Some {summary; full}, `Summary s -> { summary = Some s; full }
    in
    Reactive.Source.set signal (Some new_value);
    (* Hashtbl.replace targets id signal; *)
    ()

  let update_flat_state t ~id more_state =
    let source = _get_target_flat_status t ~id in
    let current_value = Reactive.(Source.signal source |> Signal.value) in
    Reactive.Source.set source
      (Target.State.Flat.merge current_value more_state);
    ()

  let clear {targets; flat_statuses} =
    Hashtbl.clear targets;
    Hashtbl.clear flat_statuses;
    ()


end

module Single_client = struct

  type status = [
    | `Unknown
    | `Ok of Protocol.Server_status.t
    | `Problem of string
  ]

  type column = [
    | `Arbitrary_index
    | `Name
    | `Id
    | `Tags
    | `Status
  ]
  let all_columns = [
    `Arbitrary_index;
    `Name;
    `Id;
    `Tags;
    `Status;
  ]
  let default_columns = all_columns
  let column_name : column -> _ =
    let open H5 in
    function
    | `Arbitrary_index -> span [pcdata "Index"]
    | `Name -> span [pcdata "Name"]
    | `Id -> span [pcdata "Unique Id"]
    | `Tags -> span [pcdata "Tags"]
    | `Status -> span [pcdata "Status"]

  let insert_column columns col =
    List.filter all_columns
      (fun c -> c = col || List.mem c columns)

  type tab = [
    | `Status
    | `Target_table
    | `Target_page of string
  ]

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

  type t = {
    protocol_client: Protocol_client.t;
    target_cache: Target_cache.t;
    target_ids: Target_id_set.t Reactive.Source.t;
    interesting_targets: Target_id_set.t Reactive.Source.t;
    status: status Reactive.Source.t;
    tabs: tab list Reactive.Source.t;
    current_tab: tab Reactive.Source.t;
    table_showing: (int * int) Reactive.Source.t;
    table_columns: column list Reactive.Source.t;
    default_target_query: Protocol.Up_message.target_query;
    block_time_request: float;
    default_protocol_client_timeout: float;
    wait_before_retry_asynchronous_loop: float;
  }


  let create ~protocol_client () =
    let target_ids = Reactive.Source.create Target_id_set.empty in
    let interesting_targets = Reactive.Source.create Target_id_set.empty in
    let status = Reactive.Source.create `Unknown in
    let current_tab = Reactive.Source.create `Target_table in
    let table_showing = Reactive.Source.create (0, 10) in
    let table_columns = Reactive.Source.create default_columns in
    let tabs = Reactive.Source.create [ `Status; `Target_table ] in
    let default_target_query =
      if !global_debug_level > 0 then
        `All
      else
        (* Two weeks *)
        `Created_after (Time.now () -. (60. *. 60. *. 24. *. 15.))
    in
    {
      protocol_client;
      target_cache = Target_cache.create ();
      target_ids; 
      interesting_targets;
      status;
      tabs;
      current_tab;
      table_showing;
      table_columns;
      default_target_query;
      block_time_request = 255.; (* the server will cut at 300. anyway *)
      default_protocol_client_timeout = 20.;
      wait_before_retry_asynchronous_loop =
        (if !global_debug_level > 0 then 120. else 30.);
    }

  let log t =
    Log.(s "Protocol-client: "
         % Protocol_client.log t.protocol_client)

  let name {protocol_client; _ } = Protocol_client.name protocol_client

  let target_query_of_status client = function
  | `Problem _
  | `Unknown -> client.default_target_query
  | `Ok status ->
    (* Those 5 seconds actually generate traffic, but for know, who cares … *)
    `Created_after (Protocol.Server_status.time status -. 5.)

  let add_interesting_targets t l =
    let current =
      Reactive.Source.signal t.interesting_targets |> Reactive.Signal.value in
    Reactive.Source.set t.interesting_targets (Target_id_set.add_list current l)

  let add_target_ids t l =
    let current = Reactive.(Source.signal t.target_ids |> Signal.value) in
    Reactive.Source.set t.target_ids
      (Target_id_set.add_list current l);
    ()

  let asynchronous_loop t ~name loop =
    Lwt.(
      async begin fun () ->
        let rec meta_loop () =
          loop () >>= function
          | `Ok () ->
            Log.(s "Loop ended with OK: " % quote name @ normal);
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
            Log.(s "Error in loop " % quote name % s ": " % problem @ error);
            Reactive.Source.set t.status
              (`Problem (problem |> Log.to_long_string));
            Lwt_js.sleep t.wait_before_retry_asynchronous_loop
            >>= fun () ->
            meta_loop ()
        in
        meta_loop ()
      end
    )

  let start_updating t =
    let rec list_of_ids_loop () =
      let update_server_status () =
        Protocol_client.call
          ~timeout:t.default_protocol_client_timeout
          t.protocol_client `Get_server_status
        >>= begin function
        | `Server_status status ->
          let previous_status =
            Reactive.(Source.signal t.status |> Signal.value) in
          Reactive.Source.set t.status (`Ok status);
          return (target_query_of_status t previous_status)
        | other ->
          fail (`Wrong_down_message other)
        end
      in
      let update_list_of_ids query =
        let blocking_time = t.block_time_request in
        Protocol_client.call
          ~timeout:(blocking_time +. t.default_protocol_client_timeout)
          t.protocol_client (`Get_target_ids (query,
                                              [`Block_if_empty blocking_time]))
        >>= begin function
        | `List_of_target_ids l ->
          (*
          Log.(log t % s " got " % i (List.length l) % s " new IDs at "
               % Time.(log (now ())) @ verbose); *)
          add_target_ids t l;
          return ()
        | other ->
          fail (`Wrong_down_message other)
        end
      in
      update_server_status ()
      >>= fun query ->
      update_list_of_ids query
      >>= fun () ->
      list_of_ids_loop ()
    in
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
                  (* We add all the dependencies to make sur we get values
                     for all the pointers. *)
                  add_target_ids t
                    Target.Summary.(depends_on value
                                    @ on_success_activate value
                                    @ on_failure_activate value);
                  Target_cache.update_target t.target_cache ~id (`Summary value)
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
              | Some _ -> false
              | None -> true)
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
            Protocol_client.call
              ~timeout:t.default_protocol_client_timeout
              t.protocol_client (`Get_target_flat_states (`All, now)) (*TODO*)
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
    asynchronous_loop t ~name:"list-of-ids" list_of_ids_loop;
    ()

  module Html = struct

    let status_icon t =
      let open H5 in
      let display = function
      | `Ok status ->
        span ~a:[ a_class ["label"; "label-success"];
                  a_title (fmt "OK (%s)"
                             (Protocol.Server_status.time status |> Time.to_filename));
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

    let status t =
      let open H5 in
      let display_status =
        function
        | `Ok status ->
          span ~a:[a_style "color: green"] [
            pcdata (fmt "OK (%s)"
                      (Protocol.Server_status.time status |> Time.to_filename))
          ]
        | `Unknown -> span ~a:[a_style "color: orange"] [pcdata "???"]
        | `Problem problem ->
          let help_message = Reactive.Option.create () in
          span [
            span ~a:[a_style "color: red"] [pcdata problem];
            pcdata " ";
            button ~a:[
              a_onclick (fun ev ->
                  Reactive.Option.switch help_message
                    (span [
                        pcdata "Cannot connect, and cannot get decent \
                                error message from the browser. You should try \
                                to open the following link in a new tab, and \
                                see the problem, the most common one being \
                                self-signed TLS certificates, by accepting it \
                                you may fix it for the current session: ";
                        a ~a:[
                          a_href (Protocol_client.base_url t.protocol_client)
                        ] [
                          pcdata (Protocol_client.base_url t.protocol_client)
                        ];
                      ]);
                    false)
            ] [pcdata "Investigate"];
            Reactive_node.span Reactive.Option.(
                singleton_or_empty help_message
              )
          ]
      in
      div [
        span ~a:[] [pcdata "Client: "];
        code [pcdata (Protocol_client.log t.protocol_client
                      |> Log.to_long_string)];
        br ();
        Reactive_node.span
          Reactive.(
            Source.signal t.status
            |> Signal.map ~f:display_status
            |> Signal.singleton
          )
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
                        Js.Optdef.case (ev##toElement)
                          (fun () ->
                             Log.(s "toElement undefined !!" @ error);
                             (200, 200))
                          (fun eltopt ->
                             Js.Opt.case eltopt
                               (fun () ->
                                  Log.(s "toElement defined but null!!" @ error);
                                  (200, 200))
                               (fun elt ->
                                  let rect = elt##getBoundingClientRect() in
                                  (int_of_float rect##left,
                                   int_of_float rect##top)))
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
      begin match List.exists current ~f:((=) (`Target_page id)) with
      | true -> Source.set t.current_tab (`Target_page id)
      | false -> Source.set t.tabs (current @ [`Target_page id])
      end;
      ()

    let target_table t =
      let open H5 in
      let showing = t.table_showing in
      let controls =
        Reactive_node.div Reactive.(
            Signal.tuple_2 (Source.signal showing) (Source.signal t.target_ids)
            |> Signal.map ~f:(fun ((n_from, n_count), ids) ->
                let total = Target_id_set.length ids in
                let enable_if cond on_click content =
                  if cond
                  then Bootstrap.button (`Enabled (on_click, content))
                  else Bootstrap.button (`Disabled content)
                in
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
                  Bootstrap.dropdown_button
                    ~content:[
                      pcdata (fmt "Columns")
                    ]
                    (`Close ((fun _ ->
                         Source.set t.table_columns all_columns;
                         false), [pcdata "ALL"])
                     :: List.map all_columns ~f:(fun col ->
                         let content = column_name col in
                         let signal =
                           Source.signal t.table_columns 
                           |> Signal.map ~f:(fun current ->
                               List.mem ~set:current col)
                         in
                         let on_click _ =
                           let current = 
                             Source.signal t.table_columns |> Signal.value in
                           Source.set t.table_columns
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
          Reactive_node.tr
            ~a:[a_onclick Reactive.(fun _ ->
                target_link_on_click_handler t ~id;
                false)]
            Reactive.Signal.(
              map target_signal ~f:(function
                | None ->
                  [
                    td [pcdata (fmt "%d" (index + 1))];
                    td ~a:[
                      a_colspan (List.length columns - 1);
                    ] [Bootstrap.muted_text (pcdata (fmt "Still fetching %s " id));
                       Bootstrap.loader_gif ();];
                  ]
                | Some trgt ->
                  List.map columns ~f:(function
                    | `Arbitrary_index -> td [pcdata (fmt "%d" (index + 1))]
                    | `Name -> td [pcdata (Target.Summary.name trgt)]
                    | `Id -> td [pcdata (Target.Summary.id trgt)]
                    | `Tags ->
                      td [pcdata (Target.Summary.tags trgt
                                  |> String.concat ~sep:", ")]
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
              (Source.signal t.target_ids
               |> Signal.map ~f:Target_id_set.to_list)
              (Source.signal showing)
              (Source.signal t.table_columns)
            |> Signal.map ~f:begin fun (target_ids, (index, count), columns) ->
              let ids = List.take (List.drop target_ids index) count in
              add_interesting_targets t
                (let greedy_index = max 0 (index - 25) in
                 let greedy_count = count + 25 + (index - greedy_index) in
                  List.take (List.drop target_ids greedy_index) greedy_count);
              Bootstrap.table_responsive
                ~head:(table_head columns)
                ~body:(List.mapi ids
                         ~f:(fun ind id -> row_of_id columns (index + ind) id))
            end
            |> Signal.singleton
          )
      in
      (* div ~a:[a_class ["container"]] [ *)
      Bootstrap.panel ~body:[
        controls;
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
      let text =
        let open Reactive.Signal in
        Target_cache.get_target_summary_signal client.target_cache ~id
        |> map ~f:(function
          | None ->
            span ~a:[a_title "Not yet fetched"] [pcdata (summarize_id id)]
          | Some summary ->
            span ~a:[
              a_title id;
            ] [
              pcdata (Target.Summary.name summary);
            ])
        |> singleton in
      span ~a:[
        Reactive_node.a_class colorize_classes;
      ] [
        Reactive_node.span text;
      ]

    let target_summary_panel client ~id =
      let open H5 in
      Bootstrap.panel ~body:[
        Reactive_node.div Reactive.Signal.(
            Target_cache.get_target_summary_signal client.target_cache ~id
            |> map ~f:(function
              | None ->
                div ~a:[a_title "Not yet fetched"] [
                  pcdata "Still fetching summary for ";
                  pcdata (summarize_id id)
                ]
              | Some summary ->
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
                           li ~a:[
                             a_onclick Reactive.(fun _ ->
                                 target_link_on_click_handler client ~id;
                                 false)
                           ] [
                             target_page_tab_title client ~id
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
                    code_row "Tags"
                      (Target.Summary.tags summary
                       |> String.concat ~sep:", ");
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
                    code_row "Condition"
                      (Target.Summary.condition summary
                       |> Option.value_map ~default:"None"
                         ~f:Target.Condition.to_string_hum);
                    code_row "Build-process"
                      (Target.Summary.build_process summary
                       |> function
                       | `No_operation -> "No-op"
                       | `Long_running (name, init) -> "Backend: " ^ name);
                  ]
              )
            |> singleton)
      ]

    let build_process_display_div client ~id =
      let open H5 in
      Reactive_node.div Reactive.Signal.(
          Target_cache.get_target_summary_signal client.target_cache ~id
          |> map ~f:(function
            | None ->
              div ~a:[a_title "Not yet fetched"] [
                pcdata "Still fetching summary for ";
                pcdata (summarize_id id)
              ]
            | Some summary ->
              begin match Target.Summary.build_process summary with
              | `No_operation -> div [h3 [pcdata "No-operation"]]
              | `Long_running (name, init) ->
                div [
                  h3 [pcdata (fmt "Using %s" name)];
                  pre [
                    pcdata (
                      let pretty_json =
                        Yojson.Safe.(from_string init
                                     |> pretty_to_string ~std:true) in
                      pretty_json
                    );
                  ]
                ]
              end)
          |> Reactive.Signal.singleton)

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

    let target_page client ~id =
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
        let showing_on_the_right = Reactive.Source.create `Flat_status in
        Bootstrap.panel ~body:[
          Reactive_node.div (
            Reactive.Source.signal showing_on_the_right
            |> Reactive.Signal.map ~f:(fun showing ->
                let enable_if_not_shown thing ~on_click content =
                  if showing = thing
                  then `Disabled content
                  else `Enabled (on_click, content) in
                Bootstrap.button_group ~justified:true [
                  Bootstrap.button 
                    (enable_if_not_shown `Flat_status
                       ~on_click:(fun _ ->
                           Reactive.Source.set showing_on_the_right `Flat_status;
                           false)
                       [pcdata "Status history"]);
                  Bootstrap.button 
                    (enable_if_not_shown `Build_process_details
                       ~on_click:(fun _ ->
                           Reactive.Source.set showing_on_the_right `Build_process_details;
                           false)
                       [pcdata "Build-process details"]);
                ])
            |> Reactive.Signal.singleton);
          Reactive_node.div (
            Reactive.Source.signal showing_on_the_right
            |> Reactive.Signal.map ~f:(fun showing ->
                begin match showing with
                | `Flat_status -> flat_status_display_div client ~id
                | `Build_process_details -> build_process_display_div client ~id
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
            List.map tabs ~f:(function
              | `Target_table ->
                Bootstrap.tab_item
                  ~active:Reactive.(
                      Source.signal current_tab
                      |> Signal.map ~f:(function `Target_table -> true | _ -> false))
                  ~on_click:(fun _ -> Reactive.Source.set current_tab `Target_table; false)
                  [Reactive_node.pcdata
                     Reactive.(
                       Signal.tuple_2
                         (Source.signal client.table_showing)
                         (Source.signal client.target_ids
                          |> Signal.map ~f:Target_id_set.length)
                       |> Signal.map ~f:(fun ((n_from, n_count), total) ->
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
              | `Target_page id ->
                Bootstrap.tab_item
                  ~active:Reactive.(
                      Source.signal current_tab
                      |> Signal.map ~f:(function `Target_page i -> i = id | _ -> false)
                    )
                  ~on_click:Reactive.(fun _ ->
                      let current_tabs =
                        Source.signal client.tabs |> Signal.value in
                      if List.mem ~set:current_tabs (`Target_page id)
                      then (Source.set current_tab (`Target_page id););
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
                                    Source.signal client.current_tab
                                    |> Signal.value in
                                  begin match visible = `Target_page id with
                                  | true ->
                                    Source.set client.current_tab `Target_table
                                  | _ -> ()
                                  end;
                                  Source.set client.tabs
                                    (List.filter current
                                       ~f:(fun t -> t <> `Target_page id));
                                  false)
                            ] [pcdata "✖"];
                  ]
              )
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
                          | `Target_page id -> target_page client ~id)
                        |> Signal.singleton)
          );
      ]
  end

end

module Application_state = struct

  type t = {
    clients: Single_client.t list;
  }

  let create () =
    let current_client = Protocol_client.of_current () in
    Log.(s "Protocol_client.of_current → " %
         OCaml.option Protocol_client.log current_client
         @ verbose);
    let window_clients =
      Protocol_client.of_window_object ()
    in
    Log.(s "Protocol_client.of_window_object → " %
         OCaml.list Protocol_client.log window_clients
         @ verbose);
    let clients =
      (match current_client with
      | None  -> fun e -> e
      | Some one ->
        fun l -> Single_client.create one () :: l)
        (List.mapi ~f:(fun i c -> Single_client.create c ())
           window_clients)
    in
    {clients}

  let start_background_processing t =
    List.iter t.clients ~f:Single_client.start_updating

  let to_html t =
    let visible_tab =
      Reactive.Source.create
        (List.hd t.clients
         |> Option.value_map ~f:(fun c ->
             `Client (Single_client.name c, c)) ~default:`About) in
    let open H5 in
    let navigation =
      let tab_list =
        (List.map t.clients ~f:(fun client ->
             let name = Single_client.name client in
             let active =
               Reactive.(
                 Source.signal visible_tab
                 |> Signal.map ~f:(function
                   | `Client (c, _) when c = name -> true | _ -> false)) in
             Bootstrap.tab_item
               ~active
               ~on_click:(fun _ ->
                   Reactive.Source.set visible_tab (`Client (name, client));
                   false)
               [pcdata (fmt "Client %s " name);
                Single_client.Html.status_icon client]
           ))
        @ [
          let active =
            Reactive.(
              Source.signal visible_tab
              |> Signal.map ~f:(function `About -> true | _ -> false)) in
          Bootstrap.tab_item
            ~active
            ~on_click:(fun _ -> Reactive.Source.set visible_tab `About; false)
            [pcdata "About"]
        ]
      in
      Bootstrap.with_tab_bar ~tabs:Reactive.Source.(create tab_list |> signal)
    in
    (* div ~a:[a_class ["container-fluid"]] [ *)
    (* Bootstrap.panel ~body:[ *)
    div [
      navigation
        ~content:(
          Reactive_node.div Reactive.(
              Source.signal visible_tab
              |> Signal.map ~f:(function
                | `Client (_, client) ->
                  Single_client.Html.render client
                | `About ->
                  Bootstrap.panel ~body:[
                    pcdata "This is Ketrew's GUI."; br ();
                    pcdata "See the ";
                    a ~a:[
                      a_href "http://seb.mondet.org/software/ketrew/";
                    ] [pcdata "documentation"];
                    pcdata " for Ketrew."; br ();
                    pcdata "Report issues and ask questions on the Github ";
                    a ~a:[
                      a_href "https://github.com/hammerlab/ketrew";
                    ] [pcdata "repository"];
                    pcdata " page.";
                  ]
                )
              |> Signal.singleton
            ));
    ]

end


let attach_to_page gui =
  let base_div =
    Dom_html.getElementById "ketrew-gui" in
  base_div##appendChild (Tyxml_js.To_dom.of_node gui) |> ignore;
  Lwt.return ()

let go _ =
  ignore Lwt.(
      catch begin fun () ->
        Log.(s "—→ START !!!" @ verbose);
        let application_state = Application_state.create () in
        Application_state.start_background_processing application_state;
        attach_to_page H5.(
            div [
              Application_state.to_html application_state;
            ]
          )
        >>= fun () ->
        Log.(s "→ END !!!" @ verbose);
        return ()
      end (fun exn ->
          Printf.ksprintf
            (fun s -> Firebug.console##error (Js.string s); failwith s)
            "Uncaught Exception: %s" (Printexc.to_string exn)));
  Js._true

let _ =
  let debug_level =
    try
      Js.Unsafe.get Dom_html.window (Js.string "ketrew_debug_level")
      |> Js.to_string
      |> int_of_string
    with e ->
      Log.(s "getting window.ketrew_debug_level: "
           % exn e @ warning);
      0
  in
  global_debug_level := debug_level;
  let debug f =
    Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f in
  global_log_print_string := (debug "%s%!");
  global_with_color := false;
  Dom_html.window##onload <- Dom_html.handler go
