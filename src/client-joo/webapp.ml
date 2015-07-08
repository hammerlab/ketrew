open Ketrew_pure
open Internal_pervasives

let error f =
  Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f =
  Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f
let alert f =
  Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

module Reactive_signal = struct
  (* Trying to wrap React and ReactiveData stuff *)
  type 'a t = {
    signal: 'a React.S.t;
    set: 'a -> unit;
  }

  let create v =
    let signal, set = React.S.create v in
    {signal; set}

  let set t v = t.set v

  let signal t = t.signal

  let map s ~f = React.S.map f s

  let singleton t =
    let open ReactiveData.RList in
    make_from
      [React.S.value t]
      (React.E.map (fun e -> Set [e]) (React.S.changes t))

  module Option = struct
    type 'a signal = 'a t
    type 'a t =  'a option signal 
    let create () = create None
    let switch t v =
      match signal t |> React.S.value with
      | None -> t.set (Some v)
      | Some v -> t.set None
    let singleton_or_empty (t : _ t) =
      let open ReactiveData.RList in
      let s = signal t in
      make_from
        (match React.S.value s with None -> [] | Some e -> [e])
        (React.E.map (function
           | Some e -> Set [e]
           | None -> Set []
           ) (React.S.changes s))
  end

end

module H5 = struct
  include Tyxml_js.Html5
  module Reactive = Tyxml_js.R.Html5
end

module Lwt_result = struct
  (* Similar to `Pvem_lwt_unix`, we embed the error monad into `Lwt_js`.  *)
  include Pvem.With_deferred(Lwt)
end
open Lwt_result

module Protocol_client = struct

  type connection =
    | JSONP of string

        (* (Protocol.Up_message.t -> string (\* callback for json functions *\) -> string) *)

  type t = {
    connection: connection;
  }

  let create connection = {connection}
  let jsonp_of_raw_url url = create (JSONP url)

  let jsonp_call {connection = JSONP url} =
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
      Some (jsonp_of_url 
              ~protocol:Url.Current.protocol
              ~host:Url.Current.host
              ?port:Url.Current.port
              ~token ())
    | Some tok, prot ->
      Log.(s "There is a token (" % quote tok % s ") but protocol is "
           % quote prot @ warning);
      None
    | None, _ -> None

  let base_url {connection} =
    match connection with
    | JSONP url -> url


  let of_window_object () =
    let base_urls =
      try
        Js.Unsafe.get Dom_html.window (Js.string "ketrew_connections")
        |> Js.to_array
        |> Array.map ~f:Js.to_string
        |> Array.to_list
      with e ->
        Log.(s "getting window.ketrew_connections: "
             % exn e @ warning);
        []
    in
    List.map base_urls ~f:jsonp_of_raw_url

  let call t msg =
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
                Lwt_js.sleep 10.
                >>= fun () ->
                return (`Timeout)
              end;
            ])
      >>= fun result ->
      begin match result with
      | `String content ->
        (* used the example in toplevel.ml:
           https://github.com/ocsigen/js_of_ocaml/commit/65fcc49cfe9d4df8fd193eb5330953923a001618 *)
        let got =  (Js.to_string (content##message)) |> Url.urldecode in
        (* debug "Got content %S" got; *)
        return (Protocol.Down_message.deserialize_exn got)
      | `Timeout ->
        fail (`Protocol_client (`JSONP `Timeout))
      end

  module Error = struct
    let to_string = function
    | `JSONP (`Timeout) -> fmt "JSONP Timeout"
    | `JSONP (`Exn e) -> fmt "JSONP Exception: %s" (Printexc.to_string e)
  end
end




module Target_cache  = struct
  type t = {
    targets: (Target.id, Target.t option Reactive_signal.t) Hashtbl.t;
  }

  let create () = {targets = Hashtbl.create 42}

  let get {targets} ~id =
    try (Hashtbl.find targets id)
    with _ ->
      (* Log.(s "Target-cache: miss on " % s id @ verbose); *)
      let signal = Reactive_signal.create None in
      Log.(s "Created `None` target signal for " % s id @ verbose);
      Hashtbl.replace targets id signal;
      signal

  let add {targets} ~id ~value =
    let signal = get {targets} id in
    Reactive_signal.set signal (Some value);
    Hashtbl.replace targets id signal;
    ()

  let clear {targets} = Hashtbl.clear targets

end

module Single_client = struct

  type status = [ `Unknown | `Ok | `Problem of string]

  type t = {
    protocol_client: Protocol_client.t;
    target_cache: Target_cache.t;
    target_ids: string list Reactive_signal.t;
    status: status Reactive_signal.t;
  }

  let create ~protocol_client () =
    let target_ids = Reactive_signal.create [] in
    let status = Reactive_signal.create `Unknown in
    {
      protocol_client;
      target_cache = Target_cache.create ();
      target_ids; 
      status;
    }

  let log t =
    Log.(s "Protocol-client: "
         % Protocol_client.log t.protocol_client)

  let start_updating t =
    let rec loop () =
      Protocol_client.call t.protocol_client (`Get_target_ids `All)
      >>= begin function
      | `List_of_target_ids l ->
        Reactive_signal.set t.target_ids l;
        Reactive_signal.set t.status `Ok;
        wrap_deferred ~on_exn:(fun e -> `Exn e) begin fun () ->
          Lwt_js.sleep 5.
        end
        >>= fun () ->
        loop ()
      | other ->
        fail (`Wrong_down_message other)
      end
    in
    Lwt.(
      async begin fun () ->
        loop () >>= function
        | `Ok () ->
          Log.(s "This should not have ended with OK: "
               % s "`Single_client.start_updating`" @ error);
          fail_with "WRONG LOOP"
        | `Error e ->
          let problem =
            let open Log in
            match e with
            | `Exn e -> exn e
            | `Protocol_client pc ->
              Protocol_client.Error.to_string pc |> s
            | `Wrong_down_message d -> s "Wrong down-message"
          in
          Log.(s "Error in Single_client.start_updating: "
               % problem @ error);
          Reactive_signal.set t.status
            (`Problem (problem |> Log.to_long_string));
          loop ()
      end
    )

  let get_target_signal t ~id =
    let signal = Target_cache.get t.target_cache ~id in
    let rec update_cache () =
      Protocol_client.call t.protocol_client (`Get_targets [id])
      >>= fun msg_down ->
      begin match msg_down with
      | `List_of_targets l ->
        List.iter l ~f:(fun value ->
            let id = Target.id value in
            Target_cache.add t.target_cache ~id ~value
          );
        wrap_deferred ~on_exn:(fun e -> `Exn e)
          (fun () -> Lwt_js.sleep 5.)
        >>= fun () ->
        update_cache ()
      | other ->
        fail (`Wrong_down_message other)
      end
    in
    Lwt.(async begin fun () ->
        update_cache ()
        >>= function
        | `Ok ()  -> return ()
        | `Error e ->
          let problem =
            let open Log in
            match e with
            | `Exn e -> exn e
            | `Protocol_client pc ->
              Protocol_client.Error.to_string pc |> s
            | `Wrong_down_message d -> s "Wrong down-message"
          in
          Log.(s "Error in Single_client.get_target_signal: "
               % problem @ error);
          return ()
      end);
    signal

  module Html = struct

    let status t =
      let open H5 in
      let display_status =
        function
        | `Ok -> span ~a:[a_style "color: green"] [pcdata "OK"]
        | `Unknown -> span ~a:[a_style "color: orange"] [pcdata "???"]
        | `Problem problem ->
          let help_message = Reactive_signal.Option.create () in
          span [
            span ~a:[a_style "color: red"] [pcdata problem];
            pcdata " ";
            button ~a:[
              a_onclick (fun ev ->
                  Reactive_signal.Option.switch help_message
                    (span [
                        pcdata "Cannot connect, and cannot get decent \
                                error message from the browser. You should try \
                                to open the following link in a new tab, and \
                                see the problem, the most common one being \
                                self-signed TLS certificates, by accepting it \
                                you may fix it for the current session: ";
                        a ~a:[
                          a_href (Protocol_client.base_url t.protocol_client)
                        ] [
                          pcdata (Protocol_client.base_url t.protocol_client)
                        ];
                      ]);
                    false)
            ] [pcdata "Investigate"];
            Reactive.span Reactive_signal.Option.(
                singleton_or_empty help_message
              )
          ]
      in
      let button_style = Reactive_signal.create "color: green" in
      div [
        span ~a:[
          Reactive.a_style (Reactive_signal.signal button_style);
          a_onclick (fun ev ->
              Reactive_signal.set button_style "color: grey";
              let open Lwt in
              async (fun () ->
                  Protocol_client.call t.protocol_client (`Get_targets ["does-not-exist"])
                  >>= function
                  | `Ok _ ->
                    (* ReactiveData.RList.set t.status_handle [`Ok]; *)
                    Reactive_signal.set button_style "color: green";
                    Reactive_signal.set t.status `Ok;
                    return ()
                  | `Error (`Protocol_client (`JSONP (`Timeout))) ->
                    Reactive_signal.set button_style "color: green";
                    Reactive_signal.set t.status (`Problem "Timeout");
                    return ()
                  | `Error (`Protocol_client (`JSONP (`Exn e))) ->
                    Reactive_signal.set button_style "color: green";
                    Reactive_signal.set t.status
                      (`Problem (fmt "ERROR: %s" (Printexc.to_string e)));
                    return ()
                );
              Log.(s "returning after setting status" @ verbose);
              false
            )
        ] [pcdata "Client: "];
        code [pcdata (Protocol_client.log t.protocol_client
                      |> Log.to_long_string)];
        br ();
        Reactive.span
          Reactive_signal.(
            signal t.status
            |> map ~f:display_status
            |> singleton
          )
      ]

    let targets t =
      let open H5 in
      let showing = Reactive_signal.create (`First 10) in
      let navigation =
        Reactive.div Reactive_signal.(
            signal showing
            |> map ~f:(function
              | `First n ->
                span [
                  pcdata (fmt "Showing [%d, %d] " 0 n);
                  span ~a:[
                    a_onclick (fun _ ->
                        Reactive_signal.set showing (`First (n + 10));
                        false);
                  ] [pcdata " >> "];
                ]
              )
            |> singleton)
      in
      let target_table =
        let row_of_id id =
          let target_signal = get_target_signal t ~id in
          Reactive.tr Reactive_signal.(
              signal target_signal
              |> map ~f:(function
                | None -> td [pcdata (fmt "Still fetching %s …" id)]
                | Some trgt ->
                  td [pcdata (Target.log trgt |> Log.to_long_string)]
                )
              |> singleton)
        in
        Reactive.div
          Reactive_signal.(
            signal showing
            |> map ~f:(function
              | `First n ->
                let ids =
                  List.take
                    (signal t.target_ids |> React.S.value)
                    n
                in
                table
                  (List.map ids ~f:row_of_id)
              )
            |> singleton
          )
      in
      div [
        h3 [
          pcdata "Targets (";
          Reactive.pcdata Reactive_signal.(
              signal t.target_ids |> map ~f:(fun l -> fmt "%d" (List.length l)));
          pcdata ")";
        ];
        navigation;
        target_table
      ]
  end

end

module Application_state = struct

  type t = {
    clients: (string * Single_client.t) list;
  }

  let create () =
    let current_client = Protocol_client.of_current () in
    Log.(s "Protocol_client.of_current → " %
         OCaml.option Protocol_client.log current_client
         @ verbose);
    let window_clients =
      Protocol_client.of_window_object ()
    in
    Log.(s "Protocol_client.of_window_object → " %
         OCaml.list Protocol_client.log window_clients
         @ verbose);
    let clients =
      (match current_client with
      | None  -> fun e -> e
      | Some one -> fun l -> ("Current", Single_client.create one ()) :: l)
        (List.mapi ~f:(fun i c -> fmt "W%d" i, Single_client.create c ())
           window_clients)
    in
    {clients}

  let start_background_processing t =
    List.iter t.clients begin fun (_, c) ->
      Single_client.start_updating c
    end

  let to_html t =
    let visible_client = Reactive_signal.create None in
    let open H5 in
    div [
      ul (
        List.map t.clients ~f:(fun (name, client) ->
            li [
              Reactive.span Reactive_signal.(
                  signal visible_client
                  |> map ~f:(function
                    | Some (c, _) when c = name ->
                      strong [pcdata name]
                    | None | Some _ ->
                      a ~a:[
                        a_onclick (fun _ ->
                            Reactive_signal.set visible_client (Some (name, client));
                            false);
                      ] [pcdata name]
                    )
                  |> singleton);
              pcdata ": ";
              (Single_client.Html.status client)
            ]
          )
      );
      Reactive.div Reactive_signal.(
          signal visible_client
          |> map ~f:(function
            | Some (_, t) -> Single_client.Html.targets t
            | None -> span [pcdata "(Pick a client)"]
            )
          |> singleton
        );
    ]

end


let attach_to_page gui =
  let base_div =
    Dom_html.getElementById "ketrew-gui" in
  base_div##appendChild (Tyxml_js.To_dom.of_node gui) |> ignore;
  Lwt.return ()

let go _ =
  ignore Lwt.(
      catch begin fun () ->
        Log.(s "—→ START !!!" @ verbose);
        let application_state = Application_state.create () in
        Application_state.start_background_processing application_state;
        attach_to_page H5.(
            div [
              Application_state.to_html application_state;
            ]
          )
        >>= fun () ->
        Log.(s "→ END !!!" @ verbose);
        return ()
      end (fun exn ->
           error "Uncaught Exception: %s" (Printexc.to_string exn)));
  Js._true

let _ =
  global_log_print_string := (debug "%s%!");
  global_with_color := false;
  Dom_html.window##onload <- Dom_html.handler go
