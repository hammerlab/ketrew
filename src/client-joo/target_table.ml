
open Ketrew_pure
open Internal_pervasives
open Pvem_js
open Reactive_html5


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
    | `Has_tags of Protocol.Up_message.string_predicate list
    | `Name of  Protocol.Up_message.string_predicate
    | `Id of  Protocol.Up_message.string_predicate
  ]

  type t = {
    ast: ast;
  }


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
        let string_predicate =
          function
          | Atom l
          | List [Atom "equals"; Atom l]
          | List [Atom l] -> `Equals l
          | List [Atom "re"; Atom l]
          | List [Atom "matches"; Atom l] as sexp ->
            let _ =
              try Re_posix.compile_pat l
              with e ->
                fail ~sexp "Trouble with Posix regular expression: %s"
                  (Printexc.to_string e)
            in
            `Matches l
          | sexp ->
            fail ~sexp "syntax error while parsing tags"
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
        | List (Atom "tags" :: tl) -> `Has_tags (List.map tl ~f:string_predicate)
        | List (Atom "name" :: pred :: []) -> `Name (string_predicate pred)
        | List (Atom "id" :: pred :: []) -> `Id (string_predicate pred)
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

  let create () =
    let default () =
      let ast =
        (if !global_debug_level > 0 then `All
         else `Created_in_the_past (`Weeks 2.))
      in
      {ast} in
    List.find_map Url.Current.arguments  ~f:(function
      | ("?filter", t)
      (* this weird case bypasses https://github.com/ocsigen/js_of_ocaml/issues/272 *)
      | ("filter", t) ->  Some t
      | _ -> None)
    |> function
    | Some t ->
      begin match of_lisp t with
      | `Ok t -> t
      | `Error e ->
        Log.(s "Found filter but could not load it: " % s e @ error);
        default ()
      end
    | None ->
      default ()

  let examples = [
    { ast = `All }, "Get all the targets known to the server.";
    { ast = `Created_in_the_past (`Days 0.5) },
    "Get all the targets created in the past half day.";
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
          `Has_tags [`Equals "workflow-examples"];
        ] },
    "Get all the targets created in the past 5 weeks that \
     have the \"workflow-examples\" tag.";
    { ast = 
        `Has_tags [
          (* `Equals "workflow-examples"; *)
          `Matches "^in[0-9]*tegr[a-z]tion$";
        ]
         },
    "Get all the targets that have tag matching the given 
     regular expression (POSIX syntax).";
    { ast = `And [
          `Created_in_the_past (`Weeks 4.2);
          `Status (`Simple `Failed);
          `Not (`Status `Dead_because_of_dependencies);
        ] },
    "Get all the targets created in the past 4.2 weeks that \
     died but not because of some their dependencies dying.";
    { ast = `Status `Killable },
    "Get all the targets that can be killed.";
    { ast = `And [
          `Created_in_the_past (`Days 1.);
          `Status (`Really_running);
        ] },
    "Get all the targets created in the past day that \
     are in-progress and not waiting for a dependency."
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
        Some (`And (List.map sl ~f:(fun s -> `Has_tag s)))
      | `Not s ->
        Option.(to_filter s >>= fun n -> return (`Not n))
      | `Name _ | `Id _ as name_or_id ->
        Some name_or_id
    in
    let rec to_time =
      function
      | `All -> Some `All
      | `Has_tags _
      | `Name _ | `Id _
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
    let pred = (function
      | `Equals s -> fmt "%S" s
      | `Matches s -> fmt "(re %S)" s) in
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
        fmt "(tags %s)" (List.map ~f:pred sl |> String.concat ~sep:" ")
      | `Name p -> fmt "(name %s)" (pred p)
      | `Id p -> fmt "(id %s)" (pred p)
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

  let lisp_help () =
    let open H5 in
    let describe_function name blob =
      li [
        code [pcdata (fmt "(%s)" name)]; pcdata ": "; pcdata blob; pcdata "."
      ] in
    div [
      p [
        pcdata "The language is based on S-Expressions \
                (Like Lisp or Scheme), but you can omit the \
                outermost parentheses.";
      ];
      p [pcdata "You may use the following ";
         code [pcdata "filter"];
         pcdata " “boolean functions:”"];
      ul [
        describe_function "all" "All the known targets.";
        describe_function "is-activable" "The “passive” targets.";
        describe_function "is-in-progress" "The activated or running targets.";
        describe_function "is-successful" "The finished and successful targets.";
        describe_function "is-failed" "The finished and failed targets.";
        describe_function "is-really-running" "The targets that are running \
                                               but not waiting on some \
                                               dependency.";
        describe_function "is-killable" "The targets that can be killed.";
        describe_function "is-dependency-dead" "The targets that failed \
                                                because some of  their \
                                                dependencies died.";
        describe_function "created-in-the-past <time-span>"
          "The targets that were created between now and “time-span ago.”";
        describe_function "or <...filters...>"
          "Logical “or” of a list of expressions.";
        describe_function "and <...filters...>"
          "Logical “and” of a list of expressions.";
        describe_function "not <filter>" "Logical “not” of an expression.";
        describe_function "name <string-matching-predicate>"
          "The targets whose name satisfies the condition";
        describe_function "id <string-matching-predicate>"
          "The targets whose id satisfies the condition";
        describe_function "tags <...string-matching-predicates...>"
          "Give list of conditions that the tags of a target should match \
           (it's an “and”.)"
      ];
      p [pcdata "Where a "; code [pcdata "time-span"]; pcdata " is:"];
      ul [
        describe_function "hours <float>" "A given number of hours.";
        describe_function "days <float>" "A given number of days.";
        describe_function "weeks <float>" "A given number of weeks.";
      ];
      p [pcdata "And a "; code [pcdata "string-matching-predicate"]; pcdata " is:"];
      ul [
        describe_function "equals <string-literal>"
          "Exact string equality (using just the string-literal is a \
           valid alias).";
        describe_function "re <regular-expression>"
          "Match a POSIX regular expression \
           (the function `matches` is a valid alias); partial matches are \
           allowed use \"^...$\" to force the match of the full string.";
      ];
    ]


end

type t = {
  target_ids: Target_id_set.t option Reactive.Source.t;
  target_ids_last_updated: Time.t option Reactive.Source.t; (* server-time *) 
  showing: (int * int) Reactive.Source.t;
  columns: column list Reactive.Source.t;
  filter_interface_visible: bool Reactive.Source.t;
  filter_interface_showing_help: bool Reactive.Source.t;
  filter: Filter.t Reactive.Source.t;
  saved_filters: Filter.t list Reactive.Source.t;
}

let create () =
  let target_ids = Reactive.Source.create None in
  let showing = Reactive.Source.create (0, 10) in
  let columns = Reactive.Source.create default_columns in
  let filter_interface_visible = Reactive.Source.create false in
  let filter = Filter.create () |> Reactive.Source.create in
  let target_ids_last_updated = Reactive.Source.create None in
  let filter_interface_showing_help = Reactive.Source.create false in
  let saved_filters = Reactive.Source.create [] in
  let (_ : unit React.E.t) =
    let event = Reactive.Source.signal filter |> React.S.changes in
    React.E.map (fun _ ->
        Reactive.Source.set target_ids_last_updated None;
        Reactive.Source.set target_ids None;
        Reactive.Source.modify showing (fun (_, c) -> (0, c));
        ())
      event
  in
  {target_ids;
   target_ids_last_updated;
   filter_interface_visible;
   filter_interface_showing_help;
   showing; columns; filter; saved_filters}

let target_ids_last_updated t = Reactive.Source.signal t.target_ids_last_updated
let filter t =  Reactive.Source.signal t.filter

let reset_target_ids_last_updated t =
  Reactive.Source.set t.target_ids_last_updated None;
  ()
  
let visible_target_ids t =
  Reactive.(
    Signal.tuple_2
      (Source.signal t.target_ids)
      (Source.signal t.showing)
    |> Signal.map  ~f:(fun (ids, (index, count)) ->
        match ids with
        | None -> None
        | Some tids ->
          let target_ids = Target_id_set.to_list tids in
          let ids = List.take (List.drop target_ids index) count in
          Some ids)
  )

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


module Html = struct

  let title t =
    let open H5 in
    span [Reactive_node.pcdata
            Reactive.(
              Signal.tuple_2
                (Source.signal t.showing) (Source.signal t.target_ids)
              |> Signal.map ~f:(fun ((n_from, n_count), target_ids) ->
                  match target_ids with
                  | None -> "Fetching targets …"
                  | Some tids ->
                    let total = Target_id_set.length tids in
                    begin match total with
                    | 0 -> "Target-table (empty)"
                    | other ->
                      (fmt "Target-table ([%d, %d] of %d)"
                         (min total (n_from + 1))
                         (min (n_from + n_count) total)
                         total)
                    end))]

  let target_status_badge target_status_signal  =
    let open H5 in
    let content =
      Reactive.Signal.(
        map target_status_signal ~f:Target.State.Flat.latest
        |> map ~f:(function
          | None ->
            span ~a:[a_class ["label"; "label-warning"]]
              [pcdata "Unknown … yet"]
          | Some item ->
            let label =
              match Target.State.Flat.simple item with
              | `Activable ->  "label-default"
              | `In_progress -> "label-info"
              | `Successful -> "label-success"
              | `Failed -> "label-danger"
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
                      div ~a:[a_class ["popover-content"]] [
                        Custom_data.full_flat_state_ul ~max_items:10 
                          (Reactive.Signal.value target_status_signal)
                      ]
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
              ] [pcdata (Target.State.Flat.name item)];
              Reactive_node.div popover;
            ]
          )
        |> singleton) in
    Reactive_node.div content


  
  let filter_ui target_table =
    let open H5 in
    hide_show_div
      ~signal:(Reactive.Source.signal
                 target_table.filter_interface_visible) [
      Reactive_node.div Reactive.(
          (Source.signal target_table.filter)
          |> Signal.map ~f:(fun filter ->
              let status = Reactive.Source.create (`Ok filter) in
              let url_box = Reactive.Source.create None in
              div [
                div ~a:[a_class ["input-group"]] [
                  div ~a:[a_class ["input-group-addon"]] [
                    pcdata "Write your filtering query ";
                    local_anchor
                      ~on_click:(fun _ ->
                          Reactive.(
                            Source.modify ~f:not
                              target_table.
                                filter_interface_showing_help;
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
                    a_value (Filter.to_lisp filter);
                    a_oninput (fun ev ->
                        Js.Opt.iter ev##.target (fun input ->
                            Js.Opt.iter (Dom_html.CoerceTo.input input) (fun input ->
                                let v = input##.value |> Js.to_string in
                                Log.(s "input inputs: " % s v @ verbose);
                                Reactive.Source.set status
                                  (Filter.of_lisp v)
                              );
                          );
                        false);
                  ];
                  Reactive_node.div ~a:[a_class ["input-group-btn"]]
                    Reactive.(
                      Source.signal status
                      |> Signal.map ~f:(function
                        | `Ok v -> [
                            Bootstrap.button
                              ~enabled:(v <> filter)
                              ~on_click:(fun _ ->
                                  Reactive.Source.set
                                    target_table.filter v;
                                  false)
                              [pcdata "Submit"];
                            Bootstrap.button [pcdata "Save for later"]
                              ~on_click:(fun _ ->
                                  Reactive.Source.modify
                                    target_table.saved_filters
                                    (fun l -> v :: l);
                                  false);
                            Bootstrap.button [pcdata "Make URL"]
                              ~on_click:(fun _ ->
                                  Reactive.Source.set url_box (Some v);
                                  false);
                          ]
                        | `Error e -> []
                        )
                      |> Signal.list);
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
                Reactive_node.div Reactive.(
                    Source.map_signal url_box ~f:(function
                      | Some v ->
                        begin match Url.Current.get () with
                        | Some url ->
                          let new_one =
                            let new_arg = "filter", Filter.to_lisp  v in
                            let change_arg l =
                              new_arg
                              :: List.filter l ~f:(fun (arg, _) ->
                                  arg <> "filter" && arg <> "?filter")
                            in
                            let open Url in
                            begin match url with
                            | Https u ->
                              Https { u with
                                      hu_arguments = change_arg u.hu_arguments }
                            | Http u ->
                              Http { u with
                                      hu_arguments = change_arg u.hu_arguments }
                            | File u ->
                              File { u with
                                     fu_arguments = change_arg u.fu_arguments}
                            end
                            |> string_of_url
                          in
                          Bootstrap.success_box [
                            pcdata "→ ";
                            a ~a:[a_href new_one] [pcdata new_one];
                          ]
                        | None ->
                          Bootstrap.error_box [
                            pcdata "Can't get the current URL"
                          ]
                        end
                      | None -> div []
                      )
                    |> Signal.singleton
                  );
                Reactive_node.div Reactive.(
                    Source.map_signal target_table.saved_filters
                      ~f:(function
                        | [] -> div []
                        | more ->
                          div ~a:[a_class ["alert"; "alert-success"]] [
                            h3 [pcdata "Saved Filters"];
                            ul (List.map more ~f:(fun fil ->
                                li [
                                  code [pcdata
                                          (Filter.to_lisp fil)];
                                  pcdata ": ";
                                  begin match filter = fil with
                                  | true ->
                                    pcdata "It's the current one"
                                  | false ->
                                    local_anchor
                                      ~on_click:(fun _ ->
                                          Reactive.Source.set
                                            target_table.filter
                                            fil;
                                          false)
                                      [pcdata "Load"]
                                  end;
                                  pcdata ", ";
                                  local_anchor
                                    ~on_click:(fun _ ->
                                        Reactive.Source.modify
                                          target_table.saved_filters
                                          (List.filter ~f:((<>) fil));
                                        false)
                                    [pcdata "Remove"];
                                  pcdata "."
                                ]
                              ))
                          ]
                        )
                    |> Signal.singleton
                  );
                let signal =
                  Source.signal
                    target_table.filter_interface_showing_help in
                let current_filter = filter in
                hide_show_div ~signal [
                  div ~a:[a_class ["alert"; "alert-info"]] [
                    h3 [pcdata "Help"];
                    Filter.lisp_help ();
                    p [pcdata "Here are some examples:"];
                    ul (List.map Filter.examples
                          ~f:(fun (filter, description) ->
                              li [
                                code [pcdata (Filter.to_lisp filter)];
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
                                          target_table.filter filter;
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

  let render
      ~get_target ~target_link_on_click ~get_target_status
      target_table =
    let open H5 in
    let showing = target_table.showing in
    let controls =
      Reactive_node.div Reactive.(
          Signal.tuple_3
            (Source.signal showing)
            (Source.signal target_table.target_ids)
            (Source.signal target_table.filter_interface_visible)
          |> Signal.map ~f:(fun ((n_from, n_count), ids_option, filters_visible) ->
              let ids =
                Option.value ids_option ~default:Target_id_set.empty in
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
                        target_table.filter_interface_visible
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
                       Source.set target_table.columns
                         all_columns;
                       false), [pcdata "ALL"])
                   :: List.map all_columns ~f:(fun col ->
                       let content = column_name col in
                       let signal =
                         Source.signal target_table.columns 
                         |> Signal.map ~f:(fun current ->
                             List.mem ~set:current col)
                       in
                       let on_click _ =
                         Source.modify  target_table.columns
                           (fun current -> 
                              if List.mem ~set:current col
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
    let the_table =
      let row_of_id columns index id =
        let target_signal = get_target id in
        Reactive_node.tr Reactive.Signal.(
            map target_signal ~f:(function
              | `None ->
                [
                  td ~a:[
                    a_colspan (List.length columns);
                  ] [Bootstrap.muted_text (pcdata (fmt "Still fetching %s " id));
                     Bootstrap.loader_gif ();];
                ]
              | `Pointer (_, trgt)
              | `Summary trgt ->
                List.map columns ~f:(function
                  | `Controls ->
                    td [
                      local_anchor ~on_click:Reactive.(fun _ ->
                          target_link_on_click id;
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
                      Custom_data.display_list_of_tags
                        (Target.Summary.tags trgt);
                    ]
                  | `Status ->
                    td [target_status_badge (get_target_status id)]
                  ))
            |> list)
      in
      let table_head columns =
        thead [tr (List.map columns
                     ~f:(fun col -> th [column_name col]))] in
      Reactive_node.div
        Reactive.(
          Signal.tuple_3 
            (Source.signal target_table.target_ids)
            (* |> Signal.map ~f:Target_id_set.to_list) *)
            (Source.signal showing)
            (Source.signal target_table.columns)
          |> Signal.map ~f:begin fun (target_ids_opt, (index, count), columns) ->
            begin match target_ids_opt with
            | Some tids ->
              let target_ids = Target_id_set.to_list tids in
              let ids = List.take (List.drop target_ids index) count in
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
      filter_ui target_table;
      the_table
    ]




end
