

type t

val create : Protocol_client.t -> t

module Html: sig

  val title: t -> [> Html5_types.span ] Reactive_html5.H5.elt
  val render : t -> [> Html5_types.div ] Reactive_html5.H5.elt
end
