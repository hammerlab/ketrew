open Ketrew_pervasives
type action =
    Set of string * string
  | Sequence of action list
  | Check of string * string option
val set : key:string -> string -> action
val seq : action list -> action
val contains : key:string -> string -> action
val is_not_set : string -> action
type t

val load :
  string ->
  (t,
   [> `Database of [> `Load ] * string
   | `IO of
        [> `Read_file_exn of string * exn
        | `Write_file_exn of string * exn ]
   | `System of [> `File_info of string ] * [> `Exn of exn ] ])
    Deferred_result.t

val get : t -> key:string -> (string option, 'a) Deferred_result.t

val act :
  t ->
  action:action ->
  ([> `Done | `Not_done ],
   [> `IO of [> `Write_file_exn of string * exn ] ])
  Deferred_result.t
  
