(**
Wrappers and additions around [ReactiveData], and [Tyxml_js]. 

*)

open Ketrew_pure

module H5: sig
  (* include module type of Tyxml_js.Html *)
  open Tyxml_js
  include Html_sigs.Make(Xml)(Svg).T
  module Reactive_node:
    Html_sigs.Make(Tyxml_js.R.Xml)(Tyxml_js.R.Svg).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib

  val to_dom: 'a elt -> Dom.node Js.t

  val local_anchor :
    on_click:Xml.mouse_event_handler ->
    ?a:[< Html_types.a_attrib > `Href `OnClick ] attrib list_wrap ->
    'a elt list_wrap -> [> `A of 'a ] elt

  val hide_show_div:
    ?a:'a list ->
    signal:bool Reactive.Signal.t ->
    [< Html_types.div_content_fun ] elt list_wrap -> [> Html_types.div ] elt

  val a_inline : unit -> [> `Style_Attr ] attrib

  module Bootstrap: sig

    val loader_gif : unit -> [> Html_types.img ] elt

    val muted_text :
      [< Html_types.span_content_fun ] elt -> [> Html_types.span ] elt

    val wrench_icon : unit -> [> Html_types.span ] elt

    val label_default :
      ?a:[< Html_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html_types.span_content_fun ] elt list_wrap ->
      [> Html_types.span ] elt
    val label_success :
      ?a:[< Html_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html_types.span_content_fun ] elt list_wrap ->
      [> Html_types.span ] elt
    val label_danger :
      ?a:[< Html_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html_types.span_content_fun ] elt list_wrap ->
      [> Html_types.span ] elt
    val label_warning :
      ?a:[< Html_types.span_attrib > `Class ] attrib list_wrap ->
      [< Html_types.span_content_fun ] elt list_wrap ->
      [> Html_types.span ] elt

    val icon_success: title: string -> [> Html_types.span ] elt
    val icon_unknown: title: string -> [> Html_types.span ] elt
    val icon_wrong: title: string -> [> Html_types.span ] elt


    val north_east_arrow_label: unit -> [> Html_types.span ] elt

    val reload_icon: ?tooltip: string -> unit -> [> Html_types.span ] elt

    type tab_item

    val tab_item :
      active: bool React.signal ->
      on_click: Xml.mouse_event_handler ->
      Html_types.flow5_without_interactive elt list_wrap -> tab_item

    val with_tab_bar :
      tabs: tab_item list Reactive.Signal.t ->
      content:[< Html_types.div_content_fun > `Ul ] elt ->
      [> Html_types.nav ] elt

    val disabled_li :
      Html_types.flow5_without_interactive elt list_wrap ->
      [> Html_types.li ] elt

    val dropdown_button :
      content:[< Html_types.button_content_fun > `PCDATA `Span ] elt list ->
      [ `Checkbox of
           bool React.signal * Xml.mouse_event_handler *
           Html_types.flow5_without_interactive elt
      | `Close of
           (Dom_html.mouseEvent Js.t -> bool) *
           Html_types.flow5_without_interactive elt list_wrap
      | `Disabled of Html_types.flow5_without_interactive elt list_wrap ]
        list ->
      [> Html_types.div ] elt

    val button_group :
      ?justified:bool ->
      [< Html_types.div_content_fun ] elt list_wrap ->
      [> Html_types.div ] elt

    val button :
      ?on_click:Xml.mouse_event_handler ->
      ?enabled:bool ->
      [< Html_types.button_content_fun ] elt list_wrap ->
      [> Html_types.div ] elt

    val pagination :
      [ `Disabled of Html_types.flow5_without_interactive elt list_wrap
      | `Enabled of
          Xml.mouse_event_handler *
          Html_types.flow5_without_interactive elt list_wrap ]
      list -> [> Html_types.nav ] elt

    val panel :
      body:[< Html_types.div_content_fun ] elt list_wrap ->
      [> Html_types.div ] elt

    val table_responsive :
      head:[< Html_types.thead ] elt wrap ->
      body:[< Html_types.tbody_content_fun ] elt list_wrap ->
      [> Html_types.div ] elt

    val collapsable_ul :
      ?ul_kind:[ `Inline | `None ] ->
      ?maximum_items:int ->
      [< Html_types.li_content_fun > `A ] elt list -> [> Html_types.ul ] elt

    val collapsable_pre :
      ?first_line_limit:int ->
      Ketrew_pure.Internal_pervasives.String.t ->
      [> Html_types.span ] Reactive_node.elt option * [> Html_types.pre ] elt

    val pageable_code_block : uri -> [> Html_types.div ] elt

    module Input_group : sig
      type item
      val make :
        item list -> [> Html_types.div ] elt

      val addon:  [< Html_types.div_content_fun ] elt list -> item
      val button_group: [< Html_types.div_content_fun ] elt list -> item
      val text_input :
        ?value: string ->
        on_input:(string -> unit) ->
        on_keypress:(int -> unit) ->
        [`Text | `Password ] ->
        item
  end


  val success_box:
    [< Html_types.div_content_fun ] elt list_wrap -> [> Html_types.div ] elt
  val warning_box:
    [< Html_types.div_content_fun ] elt list_wrap -> [> Html_types.div ] elt
  val error_box:
    [< Html_types.div_content_fun ] elt list_wrap -> [> Html_types.div ] elt

  val error_box_pre : 
    title:[< Html_types.strong_content_fun ] elt ->
    string -> [> Html_types.div ] elt

  end

  module Markup: sig
    val date_to_string :
      ?style:[ `ISO | `Javascript | `Locale | `UTC ] -> float -> string
    val time_span_to_string : float -> string
    val to_html :
      ?collapse_descriptions:(string * string) list ->
      Ketrew_pure.Internal_pervasives.Display_markup.t ->
      [< Html_types.div_content_fun
           > `A `Code `Div `PCDATA `Pre `Strong `Ul ]
        elt
  end

  module Custom_data : sig
    open Ketrew_pure
        
    val display_list_of_tags : string list -> [> Html_types.ul ] elt
    val summarize_id : string -> string

    val class_of_simple_status: Target.State.simple -> string
    val full_flat_state_ul:
      ?max_items:int -> Target.State.Flat.t -> [> Html_types.ul ] elt
  end

end
