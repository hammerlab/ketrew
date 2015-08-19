open Ketrew_pure.Internal_pervasives


module H5 = struct
  include Tyxml_js.Html5
  module Reactive_node = Tyxml_js.R.Html5

  let to_dom e = Tyxml_js.To_dom.of_node e

  let local_anchor ~on_click ?(a=[]) content =
    Tyxml_js.Html5.a ~a:(
      [
        a_href "javascript:;";
        (* The `href` transforms the mouse like a link.
           http://stackoverflow.com/questions/5637969/is-an-empty-href-valid *)
        a_onclick on_click;
      ]
      @ a)
      content

  let hide_show_div ?(a=[]) ~signal content =
    div ~a:[
      Reactive_node.a_style Reactive.(
          signal
          |> Signal.map ~f:(function
            | true -> "display: block"
            | false -> "display: none")
        );
    ] content

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
      span ~a:[
        a_style "font-weight: normal";
        a_class ["label"; "label-default"];
      ] [pcdata "🔧"]

    let label_default ?(a=[]) c =
      span ~a:(a_class ["label"; "label-default"] :: a) c

    let north_east_arrow_label () =
      label_default [pcdata "➚"]

    let reload_icon () =
      span ~a:[a_title "Reload"] [pcdata "↻"]

    type tab_item =
      bool React.signal * Xml.mouse_event_handler *
      Html5_types.flow5_without_interactive elt list_wrap

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
                      (* The `a` must be directly under the `li`. *)
                      local_anchor ~on_click content_list;
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

    let button ?on_click ?(enabled = true) content =
      let in_group b = button_group ~justified:false [b] in
      (* buttons must be in a group for justification to work *)
      let a =
        (Option.value_map ~default:[] ~f:(fun c ->
             if enabled then [a_onclick c] else []) on_click)
        @ [
          a_class
            ((if enabled then [] else ["disabled"])
             @ ["btn"; "btn-default"; ]);
        ] in
      button ~a content |> in_group


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

    let collapsable_ul
        ?(ul_kind = `Inline) ?(maximum_items = 4) items =
      let make_ul_content items =
        List.map items ~f:(fun s -> li [s]) in
      let list_style =
        match ul_kind with
        | `None -> []
        | `Inline -> ["list-inline"; "inline-items-separated"]
      in
      match List.length items with
      | n when n <= maximum_items ->
        ul ~a:[a_class list_style] (make_ul_content items)
      | n ->
        let expanded = Reactive.Source.create false in
        Reactive_node.ul
          ~a:[a_class list_style]
          Reactive.(
            Source.signal expanded
            |> Signal.map ~f:(fun expandedness ->
                let button =
                  a ~a:[
                    a_onclick (fun _ ->
                        Reactive.Source.set expanded (not expandedness);
                        false);
                  ] [
                    pcdata (if expandedness then "⊖" else "⊕")
                  ] in
                match expandedness with
                | true -> (make_ul_content (items @ [button]))
                | false ->
                  let shown_items = List.take items maximum_items @ [button] in
                  (make_ul_content shown_items)
              )
            |> Signal.list
          )


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

    let error_box content =
      div ~a:[
        a_class ["alert"; "alert-danger"];
      ]  content

    let error_box_pre ~title content =
      error_box [
        strong [title];
        pre [pcdata content];
      ]

    let success_box content =
      div ~a:[
        a_class ["alert"; "alert-success"];
      ] content

    let warning_box content =
      div ~a:[
        a_class ["alert"; "alert-warning"];
      ] content

  end

  module Markup = struct


    let date_to_string ?(style = `UTC) fl =
      let obj = new%js Js.date_fromTimeValue (1000. *. fl) in
      Js.to_string
        begin match style with
        | `ISO -> obj##toISOString
        | `Javascript -> obj##toString
        | `Locale -> obj##toLocaleString
        | `UTC -> obj##toUTCString
        end

    let time_span_to_string fl =
      let subsecond, seconds_f = modf fl in
      let seconds = int_of_float seconds_f in
      let seconds, minutes = seconds mod 60, seconds / 60 in
      let minutes, hours = minutes mod 60, minutes / 60 in
      fmt "%s%s%d%s s"
        (if hours <> 0 then fmt "%d h " hours  else "")
        (if minutes <> 0 then fmt "%d m " minutes else
           (if hours = 0 then "" else "00 m "))
        seconds
        (subsecond *. 1000. |> int_of_float
         |> function
         | 0 -> ""
         | n -> "." ^ string_of_int n)

    let rec to_html ?(collapse_descriptions = []) ast =
      let open Display_markup in
      let continue ast = to_html ~collapse_descriptions ast in
      let inline l = div ~a:[a_style "display: inline"] l in
      let catches_description name =
        List.exists collapse_descriptions ~f:(fun (n, _) -> n = name) in
      let rec find_subcontent name ast =
        match ast with
        | Description (n, c) when n = name -> Some c
        | Description (_, c) -> find_subcontent name c
        | Itemize l
        | Concat (_, l) ->
          List.find_map ~f:(find_subcontent name) l
        | _ -> None
      in
      match ast with
      | Date fl -> pcdata (date_to_string fl)
      | Time_span s -> pcdata (time_span_to_string s)
      | Text s -> pcdata s
      | Path p
      | Command p -> code [pcdata p]
      | Uri u ->
        a ~a:[
          a_href u
        ] [code [pcdata u]]
      | Concat (None, p) ->
        inline (List.map ~f:continue p)
      | Concat (Some sep, p) ->
        let rec interleave =
          function
          | [] -> []
          | [one] -> [continue one]
          | one :: more -> continue one :: continue sep :: interleave more
        in
        inline (interleave p)
      | Description (name, t) when catches_description name ->
        let expanded = Reactive.Source.create false in
        let button expandedness =
          a ~a:[
            a_onclick (fun _ ->
                Reactive.Source.set expanded (not expandedness);
                false);
          ] [
            pcdata (if expandedness then "⊖" else "⊕")
          ] in
        inline [
          Reactive_node.div Reactive.(
              Source.signal expanded
              |> Signal.map
                ~f:begin function
                | true ->
                  [strong [pcdata name; pcdata ": "];
                   button true; continue t]
                | false ->
                  let d = ref [] in
                  let summary =
                    Nonstd.Option.(
                      begin
                        List.find collapse_descriptions ~f:(fun (n, _) ->
                            d := fmt "trying %S Vs %S, " n name :: !d;
                            n = name)
                        >>= fun (_, to_find) ->
                        d := fmt "to_find : %s" to_find :: !d;
                        find_subcontent to_find t
                      end
                      |> map ~f:continue
                      |> value ~default:(pcdata " ")
                      (* ~default:(pcdata (fmt "??? -> %s" (String.concat ~sep:", " !d))) *)
                    )
                  in
                  [strong [pcdata name; pcdata ": "]; summary; button false]
                end
              |> Signal.list
            );
        ]
      | Description (name, t) ->
        inline [strong [pcdata (fmt "%s: " name)]; continue t]
      | Itemize ts ->
        ul (List.map ~f:(fun ast -> li [continue ast]) ts)

  end

  module Custom_data = struct

    open Ketrew_pure

    let display_list_of_tags tags =
      Bootstrap.collapsable_ul
        (List.map tags ~f:(fun tag ->
             small ~a:[
               a_class ["text-info"]
             ] [pcdata tag]))

    let summarize_id id =
      String.sub id ~index:10 ~length:(String.length id - 10)
      |> Option.value_map ~default:id ~f:(fmt "…%s")

    let class_of_simple_status =
      function
      | `Failed -> "text-danger"
      | `In_progress -> "text-info"
      | `Activable -> "text-muted"
      | `Successful -> "text-success"

    let full_flat_state_ul ?(max_items=1000) state =
      let history = state |> Target.State.Flat.history in
      let li_list =
        List.take history max_items
        |> List.map ~f:(fun item ->
            li [
              strong [
                pcdata (Target.State.Flat.time item
                        |> Markup.date_to_string);
                pcdata ": "];
              span  ~a:[
                a_class [Target.State.Flat.simple item
                         |> class_of_simple_status]
              ] [pcdata (Target.State.Flat.name item)];
              begin match Target.State.Flat.more_info item with
              | [] -> span []
              | more ->
                span [
                  br ();
                  pcdata (String.concat ~sep:", " more);
                ]
              end;
              begin match Target.State.Flat.message item with
              | None -> span []
              | Some m ->
                span [
                  br ();
                  pcdata m
                ]
              end;
            ])
      in
      ul (
        li_list
        @ (if List.length history > max_items
           then [li [code [pcdata "..."]]]
           else [])
      )


  end


  
end
