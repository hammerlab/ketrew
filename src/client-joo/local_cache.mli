open Ketrew_pure
open Internal_pervasives

module Target_cache: sig
  type target_knowledge = [
    | `None
    | `Summary of Target.Summary.t
    | `Pointer of Target.id * Target.Summary.t
  ]
  type flat_state = {
    retrieved: Time.t option;
    value: Target.State.Flat.t;
  }
  type query_description = [
    | `None
    | `Descriptions of (string * string) list
  ]
  type query_result = [
    | `None
    | `String of (Time.t * string)
    | `Error of string
  ]
  type t

  val create: unit -> t

  val markup_counts : t -> Ketrew_pure.Internal_pervasives.Display_markup.t
                             
  val get_target_summary_signal :
    t -> id:Ketrew_pure.Target.id -> target_knowledge Reactive.signal

  val get_target_flat_status_signal :
    t ->
    id:Ketrew_pure.Target.id ->
    Ketrew_pure.Target.State.Flat.t Reactive.Signal.t

  val get_target_query_descriptions :
    t -> id:Ketrew_pure.Target.id -> query_description Reactive.signal

  val get_target_query_result :
    t ->
    id:Ketrew_pure.Target.id -> query:string -> query_result Reactive.signal

  val get_target_flat_status_last_retrieved_time :
    t ->
    id:Ketrew_pure.Target.id -> Ketrew_pure.Internal_pervasives.Time.t option

  val update_target :
    t -> id:Ketrew_pure.Target.id -> target_knowledge -> unit

  val update_flat_state :
    t -> id:Ketrew_pure.Target.id -> Ketrew_pure.Target.State.Flat.t -> unit

  val update_target_query_descriptions :
    t -> id:Ketrew_pure.Target.id -> query_description -> unit

  val update_target_query_result :
    t -> id:Ketrew_pure.Target.id -> query:string -> query_result -> unit

  val clear : t -> unit
end
