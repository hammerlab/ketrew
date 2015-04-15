open Ketrew_pervasives
    
let error f =
  Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f =
  Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f
let alert f =
  Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith s) f


let do_test () =
  let open Lwt in
  debug "do_test";
  (*
  XmlHttpRequest.(
    perform_raw_url "/api"
      ~content_type:"text/plain"
      ~post_args:["", `String (Js.string message)]
      ~get_args:["token", "notok"]
    >>= fun http_frame ->
    debug "Got %S" http_frame.content;
    return http_frame.content
  )
     *)
  let url_to_call msg callback_name =
    debug "args: %s" (List.map ~f:(fun (x, y) -> fmt "(%s, %s)" x y)
                        Url.Current.arguments
                      |> String.concat ~sep:"; ");
    let token =
      List.find_map Url.Current.arguments  ~f:(function
        | ("?token", t) ->  Some t (* this weird case bypasses https://github.com/ocsigen/js_of_ocaml/issues/272 *)
        | ("token", t) ->  Some t
        | _ -> None) |> Option.value_exn ~msg:"Can't find token"
    in
    Printf.sprintf  "%s//%s%s/apijsonp?token=%s&callback=%s&message=%s"
      Url.Current.protocol
      Url.Current.host
      (Option.value_map Url.Current.port ~f:(fmt ":%d") ~default:"")
      token
      callback_name
      (Ketrew_protocol.Up_message.serialize msg |> Url.urlencode)
  in
  let jsonp_call up =
    Jsonp.call_custom_url (url_to_call up)
    >>= fun content ->
    (* used the example in toplevel.ml:
       https://github.com/ocsigen/js_of_ocaml/commit/65fcc49cfe9d4df8fd193eb5330953923a001618 *)
    let got =  (Js.to_string (content##message)) |> Url.urldecode in
    debug "Got content %S" got;
    return (Ketrew_protocol.Down_message.deserialize_exn got)
  in
  (* debug "url_to_call: %s Vs %s" (url_to_call "MSG" "CALLBACK") Url.Current.as_string; *)
  catch
    begin fun () ->
      jsonp_call (`Get_target_ids `All)
      >>= fun down ->
      (* used the example in toplevel.ml:
         https://github.com/ocsigen/js_of_ocaml/commit/65fcc49cfe9d4df8fd193eb5330953923a001618 *)
      begin match down with
      | `List_of_target_ids ids ->
        debug "got %d ids" (List.length ids);
        let base_div =
          Dom_html.getElementById "ketrew-gui" in
        List.iter ids ~f:(fun id ->
            let box = Dom_html.createDiv Dom_html.document in
            box##innerHTML <- Js.string (fmt "<code>%s</code>" id);
            Dom.appendChild base_div box;
            box##onclick <- Dom_html.handler (fun _ ->
                debug "click %S" id;
                Lwt.async (fun () ->
                    jsonp_call (`Get_targets [id])
                    >>= begin function
                    | `List_of_targets [one] ->
                      box##innerHTML <- Js.string (Ketrew_target.name one);
                      return ()
                    | other -> error "wrong down message"
                    end
                    );
                Js._true
              );
          );
        return ()
      | other -> error "wrong down message"
      end
    end
    (fun e -> debug "Got exception %s" (Printexc.to_string e); return ())

let go _ =
  ignore Lwt.(
    catch begin fun () ->
      debug "START !";
      do_test ()
      >>= fun () ->
      debug "END !";
      return ()
    end
      (fun exn -> error "uncaught exception: %s" (Printexc.to_string exn)));
  Js._true

let _ = Dom_html.window##onload <- Dom_html.handler go
