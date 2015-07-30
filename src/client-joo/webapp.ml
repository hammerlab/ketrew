open Ketrew_pure.Internal_pervasives
open Pvem_js
open Reactive_html5

module Application_state = struct

  type t = {
    clients: Single_client.t list;
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
      | Some one ->
        fun l -> Single_client.create one () :: l)
        (List.mapi ~f:(fun i c -> Single_client.create c ())
           window_clients)
    in
    {clients}

  let start_background_processing t =
    List.iter t.clients ~f:Single_client.start_updating

  let to_html t =
    let visible_tab =
      Reactive.Source.create
        (List.hd t.clients
         |> Option.value_map ~f:(fun c ->
             `Client (Single_client.name c, c)) ~default:`About) in
    let open H5 in
    let navigation =
      let tab_list =
        (List.map t.clients ~f:(fun client ->
             let name = Single_client.name client in
             let active =
               Reactive.(
                 Source.signal visible_tab
                 |> Signal.map ~f:(function
                   | `Client (c, _) when c = name -> true | _ -> false)) in
             Bootstrap.tab_item
               ~active
               ~on_click:(fun _ ->
                   Reactive.Source.set visible_tab (`Client (name, client));
                   false)
               [pcdata (fmt "Client %s " name);
                Single_client.Html.status_icon client]
           ))
        @ [
          let active =
            Reactive.(
              Source.signal visible_tab
              |> Signal.map ~f:(function `About -> true | _ -> false)) in
          Bootstrap.tab_item
            ~active
            ~on_click:(fun _ -> Reactive.Source.set visible_tab `About; false)
            [pcdata "About"]
        ]
      in
      Bootstrap.with_tab_bar ~tabs:Reactive.Source.(create tab_list |> signal)
    in
    (* div ~a:[a_class ["container-fluid"]] [ *)
    (* Bootstrap.panel ~body:[ *)
    div [
      navigation
        ~content:(
          Reactive_node.div Reactive.(
              Source.signal visible_tab
              |> Signal.map ~f:(function
                | `Client (_, client) ->
                  Single_client.Html.render client
                | `About ->
                  let open Ketrew_pure.Metadata in
                  Bootstrap.panel ~body:[
                    pcdata "This is Ketrew's GUI."; br ();
                    ul [
                      li [pcdata "Version: ";
                          code [pcdata (version |> Lazy.force)]];
                      li [pcdata "Git-commit: ";
                          begin match git_commit with
                          | Some g -> code [pcdata g]
                          | None -> pcdata "N/A"
                          end];
                    ];
                    pcdata "See the ";
                    a ~a:[
                      a_href "http://seb.mondet.org/software/ketrew/";
                    ] [pcdata "documentation"];
                    pcdata " for Ketrew."; br ();
                    pcdata "Report issues and ask questions on the Github ";
                    a ~a:[
                      a_href "https://github.com/hammerlab/ketrew";
                    ] [pcdata "repository"];
                    pcdata " page.";
                  ]
                )
              |> Signal.singleton
            ));
    ]

end


let attach_to_page gui =
  let base_div =
    Dom_html.getElementById "ketrew-gui" in
  base_div##appendChild (H5.to_dom gui) |> ignore;
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
          Printf.ksprintf
            (fun s -> Firebug.console##error (Js.string s); failwith s)
            "Uncaught Exception: %s" (Printexc.to_string exn)));
  Js._true

let _ =
  let debug_level =
    try
      Js.Unsafe.get Dom_html.window (Js.string "ketrew_debug_level")
      |> Js.to_string
      |> int_of_string
    with e ->
      Log.(s "getting window.ketrew_debug_level: "
           % exn e @ warning);
      0
  in
  global_debug_level := debug_level;
  let debug f =
    Printf.ksprintf (fun s -> Firebug.console##log(Js.string s)) f in
  global_log_print_string := (debug "%s%!");
  global_with_color := false;
  Dom_html.window##.onload := Dom_html.handler go
