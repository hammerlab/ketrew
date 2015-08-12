
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



type t = {
  target_id: string;
  showing_on_the_right: [`Flat_status | `Build_process_details ] Reactive.Source.t;
  build_process_on_display:
    [ `Nothing | `Result_of of string | `Raw_json ] Reactive.Source.t;
  target_cache: Local_cache.Target_cache.t;
  restart_target:
    on_result:([ `Error of string | `Ok of unit ] -> unit) -> unit;
  kill_target:
    on_result:([ `Error of string | `Ok of unit ] -> unit) -> unit;
  target_link_on_click_handler: id:string -> unit;
  available_queries:
    Local_cache.Target_cache.query_description Reactive.Signal.t;
  reload_query_result: query:string -> unit;
  reload_available_queries: unit -> unit;
  get_query_result: query:string ->
    [ `Error of bytes | `None | `String of (Time.t * bytes) ] Reactive.Signal.t;
}
let create ~target_cache ~id
    ~restart_target ~kill_target ~target_link_on_click_handler
    ~reload_available_queries
    ~reload_query_result
    ~available_queries
    ~get_query_result
  = {
    target_id = id;
    target_cache; restart_target; kill_target;
    target_link_on_click_handler;
    reload_available_queries;
    reload_query_result;
    available_queries;
    get_query_result;
    showing_on_the_right = Reactive.Source.create `Flat_status;
    build_process_on_display = Reactive.Source.create `Nothing;
  }
let eq a b =
  a.target_id = b.target_id

let target_id t = t.target_id

module Html = struct

  let link_title ?id t =
    let open H5 in
    let id = Option.value ~default:t.target_id id in
    let colorize_classes =
      Reactive.Signal.(
        Target_cache.get_target_flat_status_signal t.target_cache ~id
        |> map ~f:Target.State.Flat.latest
        |> map ~f:(function
          | None -> ["text-warning"]
          | Some item ->
            let text_class =
              Target.State.Flat.simple item |>
              Custom_data.class_of_simple_status in
            [text_class]
          )
      ) in
    let text =
      let open Reactive.Signal in
      Target_cache.get_target_summary_signal t.target_cache ~id
      |> map ~f:(function
        | `None ->
          span ~a:[a_title "Not yet fetched"]
            [pcdata (Custom_data.summarize_id id)]
        | `Pointer (_, summary)
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
      Reactive_node.span text
    ]

  let title t = link_title t (* with the default argument *)

  
  let target_controls t =
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
                      t.restart_target
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
                      t.kill_target
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

  let target_summary_panel t =
    let open H5 in
    let body =
      let id = t.target_id in
      Reactive_node.div Reactive.Signal.(
          Target_cache.get_target_summary_signal t.target_cache ~id
          |> map ~f:(function
            | `None ->
              div ~a:[a_title "Not yet fetched"] [
                pcdata "Still fetching summary for ";
                pcdata (Custom_data.summarize_id id)
              ]
            | `Pointer (_, summary)
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
                                 t.target_link_on_click_handler ~id;
                                 false)
                             [link_title t ~id]
                         ]))
                ]
              in
              Bootstrap.table_responsive
                ~head:(thead [])
                ~body:[
                  simple_row "Name" [link_title t];
                  code_row "ID" (Target.Summary.id summary);
                  simple_row "Controls" [target_controls t];
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
                    [Custom_data.display_list_of_tags (Target.Summary.tags summary)];
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
    Bootstrap.panel ~body:[body]

  let flat_status_display_div t =
    let open H5 in
    Reactive_node.div (
      Target_cache.get_target_flat_status_signal t.target_cache ~id:t.target_id
      |> Reactive.Signal.map ~f:(fun state ->
          div [
            h3 [pcdata "History"];
            Custom_data.full_flat_state_ul state;
          ]
        )
      |> Reactive.Signal.singleton)

  let build_process_display_div t =
    let open H5 in
    let id = t.target_id in
    let on_display_right_now = t.build_process_on_display in
    let control ~content ~help ~self current =
      Bootstrap.button
        ~enabled:(current <> self)
        ~on_click:(fun _ -> Reactive.Source.set on_display_right_now self; false)
        [span ~a:[a_title help] content]
    in
    let raw_json_control current =
      control ~content:[pcdata "Initial Raw JSON"] ~self:`Raw_json current
        ~help:"Display the JSON defined by the backend pluging" in
    let query_additional_controls current = [
      Bootstrap.button
        ~on_click:(fun _ ->
            t.reload_available_queries ();
            begin match current with
            | `Nothing | `Raw_json -> ()
            | `Result_of query -> t.reload_query_result ~query;
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
    Reactive_node.div Reactive.Signal.(
        Target_cache.get_target_summary_signal t.target_cache ~id
        |> map ~f:(function
          | `None ->
            div ~a:[a_title "Not yet fetched"] [
              pcdata "Still fetching summary for ";
              pcdata (Custom_data.summarize_id id)
            ]
          | `Pointer (_, summary)
          | `Summary summary ->
            begin match Target.Summary.build_process summary with
            | `No_operation -> div [h3 [pcdata "No-operation"]]
            | `Long_running (name, init) ->
              div [
                h3 [pcdata (fmt "Using %s" name)];
                Reactive_node.div Reactive.(
                    Signal.tuple_2
                      (Source.signal on_display_right_now)
                      (t.available_queries)
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
                    | `Nothing -> span []
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
                          t.get_query_result ~query
                          |> Signal.map ~f:(function
                            | `None -> [pcdata (fmt "Calling “%s”" query);
                                        Bootstrap.loader_gif ()]
                            | `String (date, r) ->
                              [
                                Bootstrap.success_box [
                                  strong [pcdata "Result of "; code [pcdata query]];
                                  pcdata (fmt " fetched on %s:" (Markup.date_to_string date));
                                  div 
                                    begin match Markup_queries.discriminate query with
                                    | Some _ ->
                                      [Markup_queries.render r]
                                    | None -> [pre [pcdata r]]
                                    end;
                                ]
                              ]
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

  let render t =
    let showing_on_the_right = t.showing_on_the_right in
    let open H5 in
    let two_columns ~left ~right =
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["col-md-4"]] left;
        (* div ~a:[a_class ["col-md-4"]] middle; *)
        div ~a:[a_class ["col-md-8"]] right;
      ] in
    let target_sumary = target_summary_panel t in
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
              | `Flat_status -> flat_status_display_div t
              | `Build_process_details -> build_process_display_div t
              end)
          |> Reactive.Signal.singleton)
      ]
    in
    two_columns
      ~left:[target_sumary]
      ~right:[target_status]
end
