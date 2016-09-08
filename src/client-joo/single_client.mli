(** A JOO-client for a signle Ketrew server. *)

type t
(**
The container for a client.
*)


val create: protocol_client:Protocol_client.t -> unit -> t
(** Create a client from a lower level protocol client. *)

val name: t -> string
(** Get the name of the client. *)

val start_updating: t -> unit
(** Start the asynchronous update loop(s). *)

(** Generate HTML elements from a client. *)
module Html: sig
  val status_icon: t ->
    Html_types.flow5_without_interactive Reactive_html5.H5.elt
  val render: t -> Html_types.div_content_fun  Reactive_html5.H5.elt
end
