open Ketrew_pure.Internal_pervasives

module Reactive = struct
  (* Trying to wrap React and ReactiveData stuff *)

  type 'a signal = 'a React.S.t
  type 'a signal_list_wrap = 'a ReactiveData.RList.t

  module Source = struct
    type 'a t = {
      signal: 'a React.S.t;
      set: 'a -> unit;
    }

    let create ?eq v =
      let signal, set = React.S.create ?eq v in
      {signal; set}

    let set t v = t.set v

    let signal t = t.signal

  end

  module Signal = struct

    type 'a t = 'a React.S.t
  
    let map s ~f = React.S.map f s
    let value s = React.S.value s

    let bind s ~f = React.S.bind s f
    let constant s = React.S.const s

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
      content;

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

    let north_east_arrow_label () =
      span ~a:[
        a_class ["label"; "label-default"]
      ] [
        pcdata "➚"
      ]

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
        ?(ul_kind = `Inline) ?(maxium_items = 4) items =
      let make_ul_content items =
        List.map items ~f:(fun s -> li [s]) in
      let list_style =
        match ul_kind with
        | `None -> []
        | `Inline -> ["list-inline"; "inline-items-separated"]
      in
      match List.length items with
      | n when n <= maxium_items ->
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
                  let shown_items = List.take items maxium_items @ [button] in
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
end
