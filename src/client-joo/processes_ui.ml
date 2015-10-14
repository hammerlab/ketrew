open Ketrew_pure
open Internal_pervasives
open Pvem_js
open Reactive_html5

type t = {
  client: Protocol_client.t;
}
let create client =
  {client}

module Html = struct

  let title _ = H5.(span [pcdata "Processes"])

  let render _ = H5.(div [pcdata "TODO"])
end
