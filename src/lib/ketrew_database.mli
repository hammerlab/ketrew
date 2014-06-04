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

type t
(** The handle to the database. *)

val load :
  string ->
  (t,
   [> `Database of [> `Load ] * string
   | `IO of
        [> `Read_file_exn of string * exn
        | `Write_file_exn of string * exn ]
   | `System of [> `File_info of string ] * [> `Exn of exn ] ])
    Deferred_result.t
(** Load a handle from the given database parameters. *)

val get : t -> key:string -> (string option, 'a) Deferred_result.t
(** Get a value in the DB. *)

val act :
  t ->
  action:action ->
  ([> `Done | `Not_done ],
   [> `IO of [> `Write_file_exn of string * exn ] ])
  Deferred_result.t
(** Process a transaction, which can be [`Done] is successful or [`Not_done]
    if one of the checks in the [action] failed. *)

