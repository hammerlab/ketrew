open Ketrew_pure
open Internal_pervasives
open Pvem_js

module Custom_xml_http_request = struct

  let post url ~body =
    let task, condition = Lwt.task () in
    let open XmlHttpRequest in
    let xhr = create () in
    xhr##_open
      (Js.string "POST") (Js.string url) (Js._true);
    Log.(s "Custom_xml_http_request.post " % s url @ verbose);
    xhr##.onreadystatechange :=  Js.wrap_callback
        (fun _ ->
           begin match xhr##.readyState with
           | DONE ->
             (*
             Log.(s "READY !! status: " % i xhr##.status % n
                  % s " status text: " % s (xhr##.statusText |> Js.to_string) % n
                  % s " resp text: " % s (xhr##.responseText |> Js.to_string) % n
                  @ normal);
                *)
             begin match xhr##.status with
             | 200 ->
               Lwt.wakeup condition (`Ok (Js.to_string xhr##.responseText))
             | other ->
             Log.(s "READY !! status: " % i xhr##.status % n
                  % s " status text: " % s (xhr##.statusText |> Js.to_string) % n
                  % s " resp text: " % s (xhr##.responseText |> Js.to_string) % n
                  @ normal);
               Lwt.wakeup condition
                 (`Error (`Protocol_client
                            (`Wrong_xhr_status
                               (other, Js.to_string xhr##.statusText))))
             end
           | other -> Log.(s "READY other !! " @ normal);
           end);
    xhr##.ontimeout := Dom.handler (fun _ ->
        Lwt.wakeup condition (`Error (`Protocol_client (`Xhr_timeout)));
        Js._false);
    xhr##send (Js.Opt.return (Js.string body));
    Lwt.on_cancel task (fun () -> xhr##abort);
    task

end


type connection =
  | JSONP of string
  | XHR of [`Token of string]

(* (Protocol.Up_message.t -> string (\* callback for json functions *\) -> string) *)

type t = {
  name: string;
  connection: connection;
}

let create ~name connection = {name; connection}

let name {name; _} = name

let jsonp_of_raw_url ~name url = create ~name (JSONP url)

let encode_uri_value = Uri.pct_encode ~component:`Query_key

let jsonp_call url =
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
      (encode_uri_value token)
  in
  jsonp_of_raw_url url

let markup {name; connection} =
  let open Display_markup in
  description_list [
    "Name", textf "%s" name;
    "Connection",
    begin match connection with
    | JSONP url ->
      description "JSONP" (path url)
    | XHR (`Token tok) ->
      description "XHR" (description "Token" (command tok))
    end; 
  ]

let log {connection; _} =
  Log.(braces
         (match connection with
         | JSONP url -> s "JSONP→" % s url
         | XHR (`Token tok) -> s "XHR (token: " % s tok % s ")"
         ))


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
    (* Some (jsonp_of_url ~name:"Main" *)
    (*         ~protocol:Url.Current.protocol *)
    (*         ~host:Url.Current.host *)
    (*         ?port:Url.Current.port *)
    (*         ~token ()) *)
    Some {name = "XHR"; connection = XHR (`Token token)}
  | Some tok, prot ->
    Log.(s "There is a token (" % quote tok % s ") but protocol is "
         % quote prot @ warning);
    None
  | None, _ -> None

let base_url {connection; _} =
  match connection with
  | JSONP url -> url
  | XHR (`Token tok)  -> fmt "/api?token=%s" (encode_uri_value tok)


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
  | XHR (`Token tok) ->
    Custom_xml_http_request.post (fmt "/api?token=%s" tok)
      (Protocol.Up_message.serialize msg)
    >>= fun str ->
    begin try
      return (Protocol.Down_message.deserialize_exn str)
    with e -> 
      Log.(s "Deserializing message "
           % big_byte_sequence ~max_length:200 str
           %sp % s "Exn: " % exn e
           @ verbose);
      fail (`Protocol_client (`Parsing_message (str, e)))
    end
  | JSONP url ->
    wrap_deferred
      ~on_exn:(fun e -> `Protocol_client (`JSONP (`Exn e)))
      Lwt.(fun () ->
          pick [
            begin
              Lwt_jsonp.call_custom_url (jsonp_call url msg)
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
      (* The property “message” is set by the Server module (function
         `handle_request`): *)
      let content = (Js.to_string (content##.message)) in
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
        fail (`Protocol_client (`Parsing_message (got, e)))
      end
    | `Timeout ->
      fail (`Protocol_client (`JSONP `Timeout))
    end

module Error = struct
  let to_string = function
  | `JSONP (`Timeout) -> fmt "JSONP Timeout"
  | `JSONP (`Exn e) -> fmt "JSONP Exception: %s" (Printexc.to_string e)
  | `Parsing_message (_, e) ->
    fmt "JSONP Parsing Exception: %s" (Printexc.to_string e)
  | `Wrong_xhr_status (nb, s) ->
    fmt "XHR Wrong status: %d (text: %S)" nb s
  | `Xhr_timeout -> "XHR timeout"
end
