

type t

val create : Protocol_client.t -> t

module Html: sig

  val title: t -> [> Html_types.span ] Reactive_html5.H5.elt
  val render : t -> [> Html_types.div ] Reactive_html5.H5.elt
end
