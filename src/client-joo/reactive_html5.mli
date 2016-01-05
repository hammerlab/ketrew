(**
Wrappers and additions around [ReactiveData], and [Tyxml_js]. 

*)


module H5: sig
  (* include module type of Tyxml_js.Html5 *)
  open Tyxml_js
  include Html5_sigs.Make(Xml)(Svg).T
  module Reactive_node:
    (* module type of Tyxml_js.R.Html5 *)
    Html5_sigs.MakeWrapped(Xml_wrap)(Xml)(Svg).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib

  val to_dom: 'a elt -> Dom.node Js.t

  val local_anchor :
    on_click:Xml.mouse_event_handler ->
    ?a:[< Html5_types.a_attrib > `Href `OnClick ] attrib list_wrap ->
    'a elt list_wrap -> [> `A of 'a ] elt

  val hide_show_div:
    ?a:'a list ->
    signal:bool Reactive.Signal.t ->
    [< Html5_types.div_content_fun ] elt list_wrap -> [> Html5_types.div ] elt

  val a_inline : unit -> [> `Style_Attr ] attrib

  module Bootstrap: sig

    val loader_gif : unit -> [> Html5_types.img ] elt

    val muted_text :
      [< Html5_types.span_content_fun ] elt -> [> Html5_types.span ] elt

    val wrench_icon : unit -> [> Html5_types.span ] elt

    val label_default :
      ?a:[< Html5_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html5_types.span_content_fun ] elt list_wrap ->
      [> Html5_types.span ] elt
    val label_success :
      ?a:[< Html5_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html5_types.span_content_fun ] elt list_wrap ->
      [> Html5_types.span ] elt
    val label_danger :
      ?a:[< Html5_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html5_types.span_content_fun ] elt list_wrap ->
      [> Html5_types.span ] elt
    val label_warning :
      ?a:[< Html5_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html5_types.span_content_fun ] elt list_wrap ->
      [> Html5_types.span ] elt

    val icon_success: title: string -> [> Html5_types.span ] elt
    val icon_unknown: title: string -> [> Html5_types.span ] elt
    val icon_wrong: title: string -> [> Html5_types.span ] elt


    val north_east_arrow_label: unit -> [> Html5_types.span ] elt

    val reload_icon: ?tooltip: string -> unit -> [> Html5_types.span ] elt

    type tab_item

    val tab_item :
      active: bool React.signal ->
      on_click: Xml.mouse_event_handler ->
      Html5_types.flow5_without_interactive elt list_wrap -> tab_item

    val with_tab_bar :
      tabs: tab_item list Reactive.Signal.t ->
      content:[< Html5_types.div_content_fun > `Ul ] elt ->
      [> Html5_types.nav ] elt

    val disabled_li :
      Html5_types.flow5_without_interactive elt list_wrap ->
      [> Html5_types.li ] elt

    val dropdown_button :
      content:[< Html5_types.button_content_fun > `PCDATA `Span ] elt list ->
      [ `Checkbox of
           bool React.signal * Xml.mouse_event_handler *
           Html5_types.flow5_without_interactive elt
      | `Close of
           (Dom_html.mouseEvent Js.t -> bool) *
           Html5_types.flow5_without_interactive elt list_wrap
      | `Disabled of Html5_types.flow5_without_interactive elt list_wrap ]
        list ->
      [> Html5_types.div ] elt

    val button_group :
      ?justified:bool ->
      [< Html5_types.div_content_fun ] elt list_wrap ->
      [> Html5_types.div ] elt

    val button :
      ?on_click:Xml.mouse_event_handler ->
      ?enabled:bool ->
      [< Html5_types.button_content_fun ] elt list_wrap ->
      [> Html5_types.div ] elt

    val pagination :
      [ `Disabled of Html5_types.flow5_without_interactive elt list_wrap
      | `Enabled of
          Xml.mouse_event_handler *
          Html5_types.flow5_without_interactive elt list_wrap ]
      list -> [> Html5_types.nav ] elt

    val panel :
      body:[< Html5_types.div_content_fun ] elt list_wrap ->
      [> Html5_types.div ] elt

    val table_responsive :
      head:[< Html5_types.thead ] elt wrap ->
      body:[< Html5_types.tbody_content_fun ] elt list_wrap ->
      [> Html5_types.div ] elt

    val collapsable_ul :
      ?ul_kind:[ `Inline | `None ] ->
      ?maximum_items:int ->
      [< Html5_types.li_content_fun > `A ] elt list -> [> Html5_types.ul ] elt

    val collapsable_pre :
      ?first_line_limit:int ->
      Ketrew_pure.Internal_pervasives.String.t ->
      [> Html5_types.span ] Reactive_node.elt option * [> Html5_types.pre ] elt

    module Input_group : sig
      type item
      val make :
        item list -> [> Html5_types.div ] elt

      val addon:  [< Html5_types.div_content_fun ] elt list -> item
      val button_group: [< Html5_types.div_content_fun ] elt list -> item
      val text_input :
        ?value: string ->
        on_input:(string -> unit) ->
        on_keypress:(int -> unit) ->
        [`Text | `Password ] ->
        item
  end


  val success_box:
    [< Html5_types.div_content_fun ] elt list_wrap -> [> Html5_types.div ] elt
  val warning_box:
    [< Html5_types.div_content_fun ] elt list_wrap -> [> Html5_types.div ] elt
  val error_box:
    [< Html5_types.div_content_fun ] elt list_wrap -> [> Html5_types.div ] elt

  val error_box_pre : 
    title:[< Html5_types.strong_content_fun ] elt ->
    string -> [> Html5_types.div ] elt

  end

  module Markup: sig
    val date_to_string :
      ?style:[ `ISO | `Javascript | `Locale | `UTC ] -> float -> string
    val time_span_to_string : float -> string
    val to_html :
      ?collapse_descriptions:(string * string) list ->
      Ketrew_pure.Internal_pervasives.Display_markup.t ->
      [< Html5_types.div_content_fun
           > `A `Code `Div `PCDATA `Strong `Ul ]
        elt
  end

  module Custom_data : sig
    open Ketrew_pure
        
    val display_list_of_tags : string list -> [> Html5_types.ul ] elt
    val summarize_id : string -> string

    val class_of_simple_status: Target.State.simple -> string
    val full_flat_state_ul:
      ?max_items:int -> Target.State.Flat.t -> [> Html5_types.ul ] elt
  end

end
