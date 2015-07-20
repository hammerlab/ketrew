open Ketrew_pure
open Internal_pervasives
open Pvem_js

type connection =
  | JSONP of string

(* (Protocol.Up_message.t -> string (\* callback for json functions *\) -> string) *)

type t = {
  name: string;
  connection: connection;
}

let create ~name connection = {name; connection}

let name {name; _} = name

let jsonp_of_raw_url ~name url = create ~name (JSONP url)

let jsonp_call {connection = JSONP url; _} =
  (fun msg callback_name ->
     fmt "%s&callback=%s&message=%s"
       url
       callback_name
       (Protocol.Up_message.serialize msg |> Url.urlencode))

let jsonp_of_url ~protocol ~host ?port ~token () =
  let url =
    Printf.sprintf  "%s//%s%s/apijsonp?token=%s"
      protocol host
      (Option.value_map port ~f:(fmt ":%d") ~default:"")
      token
  in
  jsonp_of_raw_url url

let log {connection} =
  Log.(braces
         (match connection with
         | JSONP url -> s "JSONP→" % s url))


let of_current () : t option =
  Log.(s "Protocol_client.of_current"
       % s "Arguments: "
       % OCaml.list (fun  (x, y) -> sf "(%s, %s)" x y) Url.Current.arguments
       % n
       % s "Protocol: " % s Url.Current.protocol
       @ verbose);
  let token_opt =
    List.find_map Url.Current.arguments  ~f:(function
      | ("?token", t) ->  Some t
      (* this weird case bypasses https://github.com/ocsigen/js_of_ocaml/issues/272 *)
      | ("token", t) ->  Some t
      | _ -> None)
  in
  match token_opt, Url.Current.protocol with
  | Some token, not_file when not_file <> "file:" ->
    Some (jsonp_of_url ~name:"Main"
            ~protocol:Url.Current.protocol
            ~host:Url.Current.host
            ?port:Url.Current.port
            ~token ())
  | Some tok, prot ->
    Log.(s "There is a token (" % quote tok % s ") but protocol is "
         % quote prot @ warning);
    None
  | None, _ -> None

let base_url {connection; _} =
  match connection with
  | JSONP url -> url


let of_window_object () =
  let base_urls =
    try
      Js.Unsafe.get Dom_html.window (Js.string "ketrew_connections")
      |> Js.to_array
      |> Array.map ~f:(fun array ->
          Js.to_array array |> Array.map ~f:Js.to_string |> Array.to_list)
      |> Array.to_list
    with e ->
      Log.(s "getting window.ketrew_connections: "
           % exn e @ warning);
      []
  in
  List.filter_map base_urls ~f:(function
    | [ name; url ] -> Some (jsonp_of_raw_url ~name url)
    | other ->
      Log.(s "wrong specification of clients: "
           % OCaml.list quote other @ error);
      None)


let call ?(timeout = 20.) t msg =
  match t.connection with
  | JSONP url ->
    wrap_deferred
      ~on_exn:(fun e -> `Protocol_client (`JSONP (`Exn e)))
      Lwt.(fun () ->
          pick [
            begin
              Jsonp.call_custom_url (jsonp_call t msg)
              >>= fun msg ->
              return (`String msg)
            end;
            begin
              Lwt_js.sleep timeout
              >>= fun () ->
              return (`Timeout)
            end;
          ])
    >>= fun result ->
    begin match result with
    | `String content ->
      let content = (Js.to_string (content##message)) in
      (* Log.(s "Received string: " % big_byte_sequence content @ verbose); *)
      (* used the example in toplevel.ml:
         https://github.com/ocsigen/js_of_ocaml/commit/65fcc49cfe9d4df8fd193eb5330953923a001618 *)
      let got = content  |> Url.urldecode in
      (* debug "Got content %S" got; *)
      Log.(s "Decoded string: "
           % big_byte_sequence ~max_length:100 got @ verbose);
      begin try
        return (Protocol.Down_message.deserialize_exn got)
      with e -> 
        Log.(s "Deserializing message "
             % big_byte_sequence ~max_length:200 got
             %sp % s "Exn: " % exn e
             @ verbose);
        fail (`Protocol_client (`JSONP (`Parsing_message (got, e))))
      end
    | `Timeout ->
      fail (`Protocol_client (`JSONP `Timeout))
    end

module Error = struct
  let to_string = function
  | `JSONP (`Timeout) -> fmt "JSONP Timeout"
  | `JSONP (`Exn e) -> fmt "JSONP Exception: %s" (Printexc.to_string e)
  | `JSONP (`Parsing_message (_, e)) ->
    fmt "JSONP Parsing Exception: %s" (Printexc.to_string e)
end
