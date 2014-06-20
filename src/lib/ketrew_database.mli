(** A key-value database API with basic transactions. *)

open Ketrew_pervasives

type action
(** An action is a transaction that (attempts) to modify the database. *)

val set : key:string -> string -> action
(** Create a “set” action: [set ~key v] will add or set the value [v] for the
    key [key]. *)

val seq : action list -> action
(** Put a sequence of actions into a transaction. *)

val contains : key:string -> string -> action
(** An action that checks that the [key] is set in the DB and has the given
    value. *)

val is_not_set : string -> action
(** An action that checks that the [key] is not set in the DB. *)

val unset: string -> action
(** An actions that removes a value from the DB. *)

type t
(** The handle to the database. *)

val load :
  string ->
  (t, [> `Database of [> `Load of string ] * string ]) Deferred_result.t
(** Load a handle from the given database parameters. *)

val close: t ->
  (unit, [> `Database of [> `Close ] * string ]) Deferred_result.t
(** Close the DB. *)

val get : t -> key:string ->
    (string option, [> `Database of [> `Get of string ] * string ])
      Deferred_result.t
(** Get a value in the DB. *)

val act :
  t ->
  action:action ->
  ([ `Done | `Not_done ],
   [> `Database of [> `Act of action ] * string ]) Deferred_result.t
(** Process a transaction, which can be [`Done] is successful or [`Not_done]
    if one of the checks in the [action] failed. *)

val log_action: action -> Log.t
(** Get a {!Log.t} document representing the {!action}. *)

type error =
  [ `Act of action | `Get of string | `Load of string | `Close ] * string
(** Merge of the possible errors. *)

val log_error: [< `Database of error ] -> Log.t
(** Get a {!Log.t} document describing an error. *)


val global_debug_level: int ref
(** Debug-logging level used in the module (default: 4). *)

