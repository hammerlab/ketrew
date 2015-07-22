(**
Low-level client for the Ketrew protocol.
*)

type t

val of_current: unit -> t option

val of_window_object: unit -> t list

val log: t -> Ketrew_pure.Internal_pervasives.Log.t
val markup: t -> Ketrew_pure.Internal_pervasives.Display_markup.t

val name: t -> string

val base_url: t -> string

val call :
  ?timeout:float ->
  t ->
  Ketrew_pure.Protocol.Up_message.t ->
  (Ketrew_pure.Protocol.Down_message.t,
   [> `Protocol_client of
        [> `JSONP of
             [> `Exn of exn
             | `Parsing_message of bytes * exn
             | `Timeout ] ] ]) Pvem_js.t


module Error: sig
  val to_string :
    [< `JSONP of
         [< `Exn of exn | `Parsing_message of 'a * exn | `Timeout ] ] ->
    string
end
