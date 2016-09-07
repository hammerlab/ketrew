
    
module Filter: sig

  type t

  val target_query :
    ?last_updated:float -> t -> Ketrew_pure.Protocol.Up_message.target_query

  val to_lisp: ?match_aliases : bool -> t -> string

end

type t

val create: unit -> t

val add_target_ids:
  t ->
  ?server_time:Ketrew_pure.Internal_pervasives.Time.t ->
  string list -> unit

val modify_filter_results_number: t -> (int -> int) -> unit

val visible_target_ids: t -> string list option Reactive.Signal.t

val target_ids_last_updated: t -> float option Reactive.Signal.t
val reset_target_ids_last_updated: t -> unit

val filter: t -> Filter.t Reactive.Signal.t

module Html: sig


  val title: t -> [> Html_types.span ] Reactive_html5.H5.elt

  val render :
    kill_targets:(
      ids: string list ->
      on_result:([ `Error of string | `Ok of unit ] -> unit) ->
      unit) ->
    get_target:(string ->
                Local_cache.Target_cache.target_knowledge
                  Reactive.signal) ->
    target_link_on_click:(string -> unit) ->
    get_target_status:(string ->
                       Ketrew_pure.Target.State.Flat.t Reactive.signal) ->
    t -> [> Html_types.div ] Reactive_html5.H5.elt
end
