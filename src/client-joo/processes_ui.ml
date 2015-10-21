open Ketrew_pure
open Internal_pervasives
open Pvem_js
open Reactive_html5

type deferred_status = [
  | `Ok
  | `Error of Display_markup.t
  | `In_progress
]
let rec log_deferred_status = function
| `Ok ok -> Log.(s "OK")
| `In_progress -> Log.(s "In-progress")
| `Error err ->
  Log.(s "Error: " % Display_markup.log err)

module Ssh_connection = Protocol.Process_sub_protocol.Ssh_connection
module Ssh_command_output = Protocol.Process_sub_protocol.Command_output
module Ssh_command = Protocol.Process_sub_protocol.Command

type t = {
  client: Protocol_client.t;
  ssh_connections: Ssh_connection.t list Reactive.Source.t;
  ssh_connections_status: deferred_status Reactive.Source.t;
  logs_cache : (string, string option Reactive.Source.t) Hashtbl.t;
  logs_visibility: (string, bool Reactive.Source.t) Hashtbl.t;
  ssh_commands: Ssh_command.t list Reactive.Source.t;
  ssh_command_outputs: Ssh_command_output.t list Reactive.Source.t;
}
let create client =
  let ssh_connections = Reactive.Source.create [] in
  let ssh_commands = Reactive.Source.create [] in
  let ssh_command_outputs = Reactive.Source.create [] in
  let ssh_connections_status = Reactive.Source.create `Ok in
  let logs_cache = Hashtbl.create 42 in
  let logs_visibility = Hashtbl.create 42 in
  {client; ssh_connections; ssh_connections_status;
   logs_cache; logs_visibility; ssh_commands; ssh_command_outputs}

let logs_visibility t ~id =
  match Hashtbl.find t.logs_visibility id with
  | v -> v
  | exception _ ->
    let v = Reactive.Source.create false in
    Hashtbl.add t.logs_visibility id v;
    v
let current_logs t ~id =
    match Hashtbl.find t.logs_cache id with
    | v -> v
    | exception _ ->
      let v = Reactive.Source.create None in
      Hashtbl.add t.logs_cache id v;
      v

let current_logs_markup t ~id =
  let source = current_logs t ~id in
  Reactive.Source.map_signal source ~f:begin function
  | Some logs ->
    begin match Display_markup.deserialize_exn logs with
    | o -> `Ok o
    | exception e -> `Error Display_markup.(textf "%s" (Printexc.to_string e))
    end
  | None -> `Error Display_markup.(text "No logs in cache …")
  end

let logs_signal t ~id =
  Reactive.(
    Signal.tuple_2
      (Source.signal (logs_visibility t ~id))
      (current_logs_markup t ~id)
    |> Signal.map ~f:(function
      | (false, _) -> `Invisible
      | (true, `Ok s) -> `Visible s
      | (true, `Error e) -> `Error e
      )
  )


let send_process_messsage t msg =
  Reactive.Source.set t.ssh_connections_status `In_progress;
  let error err =
    Log.(s "setting ssh_connections to error " % s err @ verbose);
    Reactive.Source.set t.ssh_connections_status
      (`Error Display_markup.(
           description "Error talking to the server" (text err)));
    return () in
  let ok () = Reactive.Source.set t.ssh_connections_status `Ok; return () in
  begin
    Protocol_client.call t.client msg
  end >>< begin function
  | `Ok (`Process (`List_of_ssh_ids l)) ->
    Reactive.Source.set t.ssh_connections l;
    ok ()
  | `Ok (`Process (`Logs (id, s))) ->
    Reactive.Source.set (current_logs t ~id) (Some s);
    ok ()
  | `Ok (`Process `Ok) -> ok ()
  | `Ok (`Process (`Command_output s)) ->
    Reactive.Source.modify t.ssh_command_outputs (fun l -> s :: l);
    ok ()
  | `Ok (`Process (`Error s)) -> error (fmt "Error: %s" s)
  | `Ok other -> error "Protocol error: Wrong down messsage"
  | `Error (`Protocol_client pc) ->
    error (fmt "Protocol_client: %s" (Protocol_client.Error.to_string pc))
  | `Error (`Exn e) ->
    error (fmt "Exception: %s" (Printexc.to_string e))
  end


let launch_update_ssh_connections t =
  asynchronously (fun () ->
      send_process_messsage t (`Process (`Get_all_ssh_ids))
    )

let start_ssh_connection t connection =
  asynchronously (fun () ->
      send_process_messsage t (`Process (`Start_ssh_connetion connection))
    )

let update_logs t ~id =
  Log.(s "update_logs of " % quote id @ verbose);
  asynchronously (fun () ->
      send_process_messsage t (`Process (`Get_logs (id, `Full)))
    )

let send_input_to_askpass t ~id ~content =
  asynchronously (fun () ->
      send_process_messsage t (`Process (`Send_ssh_input (id, content)))
    )
let send_command t ~id ~content =
  let command =
    {Protocol.Process_sub_protocol.Command.
      id = Unique_id.create ();
      command = content;
      connection = id} in
  Reactive.Source.modify t.ssh_commands (fun l -> command :: l);
  asynchronously (fun () ->
      send_process_messsage t (`Process (`Send_command command))
    )

let kill t ~id =
  asynchronously (fun () -> send_process_messsage t (`Process (`Kill id)))

module Html = struct

  let title _ = H5.(span [pcdata "Processes"])

  let summarize_string ?(max_length = 20) s =
    let open H5 in
    let actual_string =
      match String.sub s ~index:0 ~length:max_length with
      | Some s -> s ^ "…"
      | None -> s in
    span ~a:[ a_title s ] [pcdata actual_string]

  module Make_send_input_ui (Parameters : sig
      val is_password_like: bool
      val button_text : string
      val input_question : string
      val send_input : t -> id:string -> content:string -> unit
    end) = struct
    type ui = {
      visibility:
        [`Hidden | `Visible of [ `Text | `Password ] ] Reactive.Source.t;
      mutable content : string;
      tab: t;
      id: string;
    }
    let create tab ~id =
      let visibility = Reactive.Source.create `Hidden in
      {visibility; content = ""; tab; id}

    let make_visible {visibility; _} =
      Reactive.Source.set visibility
        (if Parameters.is_password_like then
           `Visible `Password
         else
           `Visible `Text)

    let state_signal {visibility; _} =
      Reactive.Source.signal visibility

    let button ui =
      let open H5 in
      Reactive_node.div ~a:[ a_inline () ] Reactive.(
          Source.map_signal ui.visibility ~f:(fun vis ->
              let enabled = vis = `Hidden in
              Bootstrap.button [pcdata Parameters.button_text]
                ~enabled
                ~on_click:(fun _ -> make_visible ui; false)
            )
          |> Signal.singleton
        )

    let render ui =
      let open H5 in
      let module Booig = Bootstrap.Input_group in
      Reactive_node.div Reactive.(
          Source.map_signal ui.visibility ~f:(function
            | `Hidden -> span []
            | `Visible input_type ->
              Booig.make [
                Booig.addon [
                  pcdata Parameters.input_question;
                ];
                Booig.text_input input_type
                  ~value:ui.content
                  ~on_input:(fun str -> ui.content <- str)
                  ~on_keypress:(function
                    | 13 ->
                      Log.(s "on_keypress → enter "  @ verbose);
                      Parameters.send_input ui.tab ~id:ui.id ~content:ui.content
                    | other ->
                      Log.(s "on_keypress → " % i other  @ verbose));
                Booig.button_group [
                  Bootstrap.button [pcdata "Go!"] ~on_click:(fun _ ->
                      Parameters.send_input ui.tab ~id:ui.id ~content:ui.content;
                      false);
                  begin match input_type with
                  | `Password ->
                    Bootstrap.button [pcdata "Show Input"]
                      ~on_click:(fun _ ->
                          Reactive.Source.set ui.visibility (`Visible `Text);
                          false)
                  | `Text ->
                    Bootstrap.button [pcdata "Don't Show Input"]
                      ~on_click:(fun _ ->
                          Reactive.Source.set ui.visibility
                            (`Visible `Password);
                          false)
                  end;
                  Bootstrap.button [pcdata "Cancel"]
                    ~on_click:(fun _ ->
                        Reactive.Source.set ui.visibility `Hidden;
                        ui.content <- "";
                        false);
                ]
              ]
            )
          |> Signal.singleton
        )

  end

  module Send_input_to_askpass =
    Make_send_input_ui(
    struct
      let is_password_like = true
      let button_text = "Send Input to ASK-PASS"
      let input_question =
        "Enter your input (most likely a password/passphrase)"
      let send_input = send_input_to_askpass
    end)
  module Send_command_ui =
    Make_send_input_ui(
    struct
      let is_password_like = false
      let button_text = "Run Shell Command on Host"
      let input_question = "Enter your command"
      let send_input = send_command
    end)

  module Kill_ui = struct

    type ui = {
      state:
        [`Passive | `Are_you_sure ] Reactive.Source.t;
      tab: t;
      id: string;
    }
    let create tab ~id =
      let state = Reactive.Source.create `Passive in
      {state; tab; id}

    let render ui =
      let open H5 in
      Reactive_node.div ~a:[ a_inline () ] Reactive.(
          Source.map_signal ui.state ~f:(function
            | `Passive ->
              Bootstrap.button [pcdata "Kill"]
                ~on_click:(fun _ ->
                    Reactive.Source.set ui.state `Are_you_sure;
                    false)
            | `Are_you_sure ->
              let module Booig = Bootstrap.Input_group in
              Booig.make [
                Booig.addon [strong [pcdata "Are you 100% sure? "]];
                Booig.button_group [
                  Bootstrap.button [pcdata "Yes"]
                    ~on_click:(fun _ ->
                        kill ui.tab ~id:ui.id;
                        Reactive.Source.set ui.state `Passive;
                        false);
                  Bootstrap.button [pcdata "No"]
                    ~on_click:(fun _ ->
                        Reactive.Source.set ui.state `Passive;
                        false);
                ]
              ]
            )
          |> Signal.singleton
        )

  end

  let display_output_of_command ~stdout ~stderr =
    let open H5 in
    let standard_somthing ~std ~what =
      let potential_button, content = Bootstrap.collapsable_pre std in
      div [
        strong [pcdata (fmt "Stdandard-%s" what)];
        (match potential_button with
        | None  -> span []
        | Some b -> span [pcdata " "; b]);
        content;
      ] in
    div [
      standard_somthing ~std:stdout ~what:"output";
      standard_somthing ~std:stderr ~what:"error";
    ]



  let display_commands t ~id =
    let open H5 in
    Reactive_node.div Reactive.(
        Signal.tuple_2
          (Source.signal t.ssh_commands)
          (Source.signal t.ssh_command_outputs)
        |> Signal.map ~f:(fun (commands, outputs) ->
            match
              List.filter commands
                ~f:(fun c -> c.Ssh_command.connection = id) with
            | [] -> div []
            | more ->
              div [
                pcdata "Commands: ";
                Bootstrap.collapsable_ul ~ul_kind:`None ~maximum_items:2 (
                  List.map more ~f:(fun command ->
                      let {Ssh_command. id; command; connection} = command in
                      let output =
                        List.find_map outputs ~f:(fun o ->
                            let cid = id in
                            let {Ssh_command_output. id; stderr; stdout} = o in
                            if id = cid
                            then Some (display_output_of_command ~stdout ~stderr)
                            else None)
                      in
                      div [
                        code [pcdata command];
                        begin match output with
                        | None  -> pcdata " => No output … yet"
                        | Some c -> c
                        end;
                      ]
                    )
                );
              ]
          )
        |> Signal.singleton
      )

  let single_connection_reactive_div t ~uri ~id ~status =
    let open H5 in
    let send_input_to_askpass_ui = Send_input_to_askpass.create t ~id in
    let send_command_ui = Send_command_ui.create t ~id in
    let kill_ui = Kill_ui.create ~id t in
    let hide_stuff () =
      Bootstrap.button [pcdata "Unexpand"]
        ~on_click:(fun _ ->
            Reactive.Source.set (logs_visibility t ~id) false;
            false) in
    let reload () =
      Bootstrap.button [pcdata "Reload"]
        ~on_click:(fun _ ->
            update_logs t ~id;
            false) in
    let status_badge () =
      match status with
      | `Alive -> Bootstrap.icon_success ~title:"Alive"
      | `Dead s -> Bootstrap.icon_wrong ~title:(fmt "Dead: %s" s)
    in
    Reactive_node.div Reactive.(
        (logs_signal t ~id) |> Signal.map ~f:(function
          | `Invisible ->
            div [
              status_badge ();
              code [summarize_string ~max_length:50 uri];
              Bootstrap.button [pcdata "Expand"]
                ~on_click:(fun _ ->
                    update_logs t ~id;
                    Reactive.Source.set (logs_visibility t ~id) true;
                    false);
            ]
          | `Error m ->
            div [
              status_badge ();
              code [pcdata uri];
              div [
                hide_stuff ();
                reload ();
                Bootstrap.error_box [Markup.to_html m];
              ];
            ]
          | `Visible m ->
            div [
              status_badge ();
              code [pcdata uri];
              div [
                hide_stuff ();
                reload ();
                Send_input_to_askpass.button send_input_to_askpass_ui;
                Send_command_ui.button send_command_ui;
                Kill_ui.render kill_ui;
                Send_input_to_askpass.render send_input_to_askpass_ui;
                Send_command_ui.render send_command_ui;
                display_commands t ~id;
                Bootstrap.success_box [Markup.to_html m];
              ];
            ]
          )
        |> Signal.singleton
      )

  let controls_header t ~can_update =
    let open H5 in
    let add_interface_visible = Reactive.Source.create false in
    div [
      h3 [pcdata "SSH Connections"];
      Bootstrap.button_group ~justified:false [
        Bootstrap.button ~enabled:can_update [pcdata "Update list"]
          ~on_click:(fun _ ->
              (* set_in_progress ~current_value t; *)
              launch_update_ssh_connections t;
              false);
        Bootstrap.button ~enabled:can_update [pcdata "Add/start new"]
          ~on_click:(fun _ ->
              Reactive.Source.modify add_interface_visible not;
              false);
      ];
      Reactive_node.div Reactive.(
          Source.map_signal add_interface_visible ~f:(function
            | true ->
              [
                Bootstrap.(
                  let ssh_uri = ref "ssh://" in
                  Input_group.make [
                    Input_group.addon [
                      pcdata "Enter SSH URI (like ssh://user@example.com:42)";
                    ];
                    Input_group.text_input `Text
                      ~value:!ssh_uri
                      ~on_input:(fun str -> ssh_uri := str)
                      ~on_keypress:(function
                        | 13 ->
                          start_ssh_connection t !ssh_uri;
                          Log.(s "on_keypress → enter " % s !ssh_uri  @ verbose)
                        | other ->
                          Log.(s "on_keypress → " % i other  @ verbose));
                    Input_group.button_group [
                      button [pcdata "Go!"] ~on_click:(fun _ ->
                          start_ssh_connection t !ssh_uri;
                          false);
                    ]
                  ]
                )
              ]
            | false ->  []
            ) |> Signal.list);
    ]

  let rec display_list_of_connections ?(hide_loading = false) t =
    let open H5 in
    let list_of_connections l =
      ul (
        List.map l ~f:(fun {Ssh_connection.id; uri; status} ->
            li [
              single_connection_reactive_div t ~uri ~id ~status;
            ])
      );
    in
    div [
      Reactive_node.div Reactive.(
          Signal.tuple_2
            (Source.signal t.ssh_connections_status)
            (Source.signal t.ssh_connections)
          |> Signal.map  ~f:(fun current_value ->
              match current_value with
              | `Error m, prev ->
                div [
                  controls_header t ~can_update:true;
                  Bootstrap.error_box [Markup.to_html m];
                  list_of_connections prev;
                ]
              | `In_progress, prev ->
                div [
                  controls_header t ~can_update:false;
                  Bootstrap.warning_box [
                    pcdata "Fetching stuff "; Bootstrap.loader_gif ();
                  ];
                  list_of_connections prev;
                ]
              | `Ok, l ->
                div [
                  controls_header t ~can_update:true;
                  list_of_connections l;
                ]
            )
          |> Signal.singleton)
    ]


  let render t =
    let open H5 in
    div [
      display_list_of_connections t;
    ]
end
