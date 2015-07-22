(**
Wrappers and additions around [React], [ReactiveData], and [Tyxml_js]. 

*)

(**
Convenient wrapper around [React] and [ReactiveData] modules.
*)
module Reactive: sig

  type 'a signal = 'a React.S.t

  type 'a signal_list_wrap = 'a ReactiveData.RList.t

  module Source: sig
    type 'a t
    val create: ?eq:('a -> 'a -> bool) -> 'a -> 'a t
    val set: 'a t -> 'a -> unit
    val signal: 'a t -> 'a signal
  end
  module Signal: sig
    type 'a t = 'a signal
    val map: 'a t -> f:('a -> 'b) -> 'b t
    val bind: 'a t -> f:('a -> 'b t) -> 'b t
    val constant: 'a -> 'a t
    val value: 'a t -> 'a
    val singleton: 'a t -> 'a signal_list_wrap
    val list: 'a list t -> 'a signal_list_wrap
    val tuple_2: 'a t -> 'b t -> ('a * 'b) t
    val tuple_3: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  end
  module Option: sig
    type 'a t = 'a option Source.t
    val create : unit -> 'a t
    val switch : 'a t -> 'a -> unit
    val singleton_or_empty : 'a t -> 'a signal_list_wrap
  end

end

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

  module Bootstrap: sig

    val loader_gif : unit -> [> Html5_types.img ] elt

    val muted_text :
      [< Html5_types.span_content_fun ] elt -> [> Html5_types.span ] elt

    val wrench_icon : unit -> [> Html5_types.span ] elt

    val north_east_arrow_label: unit -> [> Html5_types.span ] elt

    val reload_icon: unit -> [> Html5_types.span ] elt

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
    ?maxium_items:int ->
    [< Html5_types.li_content_fun > `A ] elt list -> [> Html5_types.ul ] elt

  val collapsable_pre :
    ?first_line_limit:int ->
    Ketrew_pure.Internal_pervasives.String.t ->
    [> Html5_types.span ] Reactive_node.elt option * [> Html5_types.pre ] elt
    
  val error_box : 
    title:[< Html5_types.strong_content_fun ] elt ->
    string -> [> Html5_types.div ] elt

  val success_box:
    [< Html5_types.div_content_fun ] elt list_wrap -> [> Html5_types.div ] elt
  end
end
