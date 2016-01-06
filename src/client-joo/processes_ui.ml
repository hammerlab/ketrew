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

module Item_visiblity = struct
  type t = No | Simple | Details
  let default = No
  let hide_show = function
  | No -> Simple
  | other -> No
  let with_details details_now =
    function
    | No -> No
    | _ when details_now -> Details
    | _ -> Simple
end

type t = {
  client: Protocol_client.t;
  self_id: Unique_id.t;
  ssh_connections: Ssh_connection.t list Reactive.Source.t;
  ssh_connections_status: deferred_status Reactive.Source.t;
  logs_cache : (string, string option Reactive.Source.t) Hashtbl.t;
  items_visibility: (string, Item_visiblity.t Reactive.Source.t) Hashtbl.t;
  ssh_commands: Ssh_command.t list Reactive.Source.t;
  ssh_command_outputs: Ssh_command_output.t list Reactive.Source.t;
}
(* This function creates/initializes a `Process_ui.t` tab, but the
   exported `create` function is defined later, it calls `create_raw` and
   `start_updating_ssh_connections`. *)
let create_raw client =
  let ssh_connections = Reactive.Source.create [] in
  let ssh_commands = Reactive.Source.create [] in
  let ssh_command_outputs = Reactive.Source.create [] in
  let ssh_connections_status = Reactive.Source.create `Ok in
  let logs_cache = Hashtbl.create 42 in
  let items_visibility = Hashtbl.create 42 in
  {client; ssh_connections; ssh_connections_status;
   self_id = Unique_id.create ();
   logs_cache; items_visibility; ssh_commands; ssh_command_outputs}

let items_visibility t ~id =
  match Hashtbl.find t.items_visibility id with
  | v -> v
  | exception _ ->
    let v = Reactive.Source.create Item_visiblity.default in
    Hashtbl.add t.items_visibility id v;
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
    | o -> `Ok (Some o)
    | exception e -> `Error Display_markup.(textf "%s" (Printexc.to_string e))
    end
  | None -> `Ok None
  end

let logs_signal t ~id =
  let open Item_visiblity in
  Reactive.(
    Signal.tuple_2
      (Source.signal (items_visibility t ~id))
      (current_logs_markup t ~id)
    |> Signal.map ~f:(function
      | (No, _) -> `Invisible
      | (other, `Error e) -> `Error e
      | (Simple, `Ok s) -> `Visible (false, s)
      | (Details, `Ok s) -> `Visible (true, s)
      )
  )

let set_error t err =
  Log.(s "setting ssh_connections to error " % s err @ verbose);
  Reactive.Source.set t.ssh_connections_status
    (`Error Display_markup.(
         description "Error talking to the server" (text err)));
  return ()

let send_process_messsage : type any. _ -> _ -> (unit, any) Pvem_js.t = fun t msg ->
  Reactive.Source.set t.ssh_connections_status `In_progress;
  let error err = set_error t err in
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


let start_updating_ssh_connections t =
  asynchronously (fun () ->
      let rec loop () =
        begin
          Protocol_client.call t.client (`Process (`Get_all_ssh_ids t.self_id))
        end >>< begin function
        | `Ok (`Process (`List_of_ssh_ids l)) ->
          Reactive.Source.set t.ssh_connections l;
          loop ()
        | `Ok (`Process (`Error s)) -> set_error t (fmt "Error: %s" s)
        | `Ok other -> set_error t "Protocol error: Wrong down messsage"
        | `Error (`Protocol_client pc) ->
          set_error t (fmt "Protocol_client: %s" (Protocol_client.Error.to_string pc))
        | `Error (`Exn e) ->
          set_error t (fmt "Exception: %s" (Printexc.to_string e))
        end
        >>= fun () ->
        (* There was an error … *)
        (Pvem_js.sleep 15. >>< fun _ -> return ())
        >>= fun () ->
        loop ()
      in
      loop ()
    )

let start_new_ssh_connection t ~name ~uri =
  asynchronously (fun () ->
      send_process_messsage t
        (`Process (`Start_ssh_connetion (`New (name, uri))))
    )
let start_configured_ssh_connection t ~id =
  asynchronously (fun () ->
      send_process_messsage t
        (`Process (`Start_ssh_connetion (`Configured (id))))
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

let create client =
  let t = create_raw client in
  start_updating_ssh_connections t;
  t

module Html = struct

  let title _ = H5.(span [pcdata "SSH Named Hosts"])

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

  module Send_input_to_askpass = struct
    module Send_input_default = Make_send_input_ui(struct
        let is_password_like = true
        let button_text = "Send Input to ASK-PASS"
        let input_question =
          "Enter your input (most likely a password/passphrase)"
        let send_input = send_input_to_askpass
      end)
    let render t ~id ~questions =
      let open H5 in
      let send_input_to_askpass_ui =
        Send_input_default.create t ~id in
      Bootstrap.warning_box [
        h4 [pcdata "Waiting For Input"];
        begin match questions with
        | [] ->
          div [
            pcdata "There are no questions from the ";
            code [pcdata "ASKPASS"]; pcdata " program.";
          ]
        | more ->
          div [
            code [pcdata "ASKPASS"]; pcdata " program asked: ";
            ul (List.map more ~f:(fun (date, q) ->
                li [
                  pcdata (fmt "On %s: " (Markup.date_to_string date));
                  code [pcdata q]
                ]));
          ]
        end;
        Send_input_default.button send_input_to_askpass_ui;
        Send_input_default.render send_input_to_askpass_ui;
      ]
  end

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

  let single_connection_reactive_div t ~uri ~name ~id ~status =
    let open H5 in
    let send_command_ui = Send_command_ui.create t ~id in
    let kill_ui = Kill_ui.create ~id t in
    let hide_show text =
      Bootstrap.button [strong [pcdata text]]
        ~on_click:(fun _ ->
            Reactive.Source.modify (items_visibility t ~id) ~f:(fun current ->
                update_logs t ~id;
                Item_visiblity.hide_show current);
            false) in
    let status_badge () =
      match status with
      | `Alive `Idle ->
        Bootstrap.label_success [pcdata "Alive & Kicking"]
      | `Alive (`Askpass_waiting_for_input _) ->
        Bootstrap.label_warning [pcdata "Waiting for input"]
      | `Dead s ->
        Bootstrap.label_danger [pcdata (fmt "Dead: %s" s)]
      | `Configured ->
        Bootstrap.label_default [pcdata "Configured"]
      | `Unknown why ->
        Bootstrap.label_warning [pcdata (fmt "Unkown (%s)" why)]
    in
    let name () = strong [pcdata name] ~a:[a_title uri] in
    let first_line hs_text =
      let with_hs = [hide_show hs_text; pcdata " ";] in
      let line = [
        status_badge (); pcdata " ";
        name (); pcdata " ";
        code [summarize_string ~max_length:40 uri];
      ] in
      begin match status with
      | `Configured -> line @ [
          pcdata " ";
          local_anchor [pcdata "Start New Instance"]
            ~on_click:(fun _ ->
                start_configured_ssh_connection t ~id;
                false);
        ]
      | `Alive _ | `Dead _ | `Unknown _ -> line @ with_hs
      end
    in
    let details_div ~with_details mrkopt =
      match with_details with
      | true ->
        begin match mrkopt with
        | Some m -> Bootstrap.success_box [Markup.to_html m]
        | None -> Bootstrap.warning_box [pcdata "No details avaialable"]
        end;
      | false -> div [] in
    let details_button ~with_details =
      Bootstrap.button
        [pcdata (if with_details then "Hide Details" else "Show Details")]
        ~on_click:(fun _ ->
            Reactive.Source.modify (items_visibility t ~id) ~f:(fun current ->
                (if not with_details then update_logs t ~id else ());
                Item_visiblity.with_details (not with_details) current);
            false)
    in
    let visible_stuf ~with_details mrkopt =
      begin match status with
      | `Configured -> div []
      | `Dead _ | `Unknown _ ->
        div [
          details_button ~with_details;
          display_commands t ~id;
          details_div ~with_details mrkopt;
        ]
      | `Alive details ->
        div [
          details_button ~with_details;
          Kill_ui.render kill_ui;
          begin match details with
          |  `Askpass_waiting_for_input questions ->
            Send_input_to_askpass.render t ~id ~questions
          | `Idle -> Send_command_ui.button send_command_ui
          end;
          Send_command_ui.render send_command_ui;
          display_commands t ~id;
          details_div ~with_details mrkopt;
        ]
      end
    in
    let item visiblity =
      match visiblity with
      | `Invisible -> div (first_line  "+")
      | `Error mrk ->
        div (
          (first_line  "-")
          @ [
            div [
              Bootstrap.error_box [Markup.to_html mrk];
            ];
          ]
        )
      | `Visible (with_details, mrkopt) ->
        div (first_line "-" @ [ visible_stuf ~with_details mrkopt ])
    in
    Reactive_node.div Reactive.(
        (logs_signal t ~id) |> Signal.map ~f:item |> Signal.singleton
      )

  let controls_header t ~can_update =
    let open H5 in
    let add_interface_visible = Reactive.Source.create false in
    let main_button interface_visible =
      match interface_visible with
      | true -> strong [pcdata "Create new SSH connection:"]
      | false ->
        local_anchor [strong [pcdata "Add/start new connection"]]
          ~on_click:(fun _ ->
              Reactive.Source.modify add_interface_visible not;
              false);
    in
    Reactive_node.div Reactive.(
        Source.map_signal add_interface_visible ~f:(function
          | true ->
            [
              main_button true;
              Bootstrap.(
                let ssh_uri = ref "ssh://" in
                let host_name = ref "" in
                Input_group.make [
                  Input_group.addon [
                    pcdata "Enter SSH URI (like ssh://user@example.com:42)";
                  ];
                  Input_group.text_input `Text
                    ~value:!ssh_uri
                    ~on_input:(fun str -> ssh_uri := str)
                    ~on_keypress:(function
                      | 13 ->
                        start_new_ssh_connection t ~name:!host_name ~uri:!ssh_uri;
                        Log.(s "on_keypress → enter " % s !ssh_uri  @ verbose)
                      | other ->
                        Log.(s "on_keypress → " % i other  @ verbose));
                  Input_group.addon [
                    pcdata "Enter a name for this connection";
                  ];
                  Input_group.text_input `Text
                    ~value:!host_name
                    ~on_input:(fun str -> host_name := str)
                    ~on_keypress:(function
                      | 13 ->
                        start_new_ssh_connection t ~name:!host_name ~uri:!ssh_uri;
                        Log.(s "on_keypress → enter " % s !ssh_uri  @ verbose)
                      | other ->
                        Log.(s "on_keypress → " % i other  @ verbose));
                  Input_group.button_group [
                    button [pcdata "Go!"] ~on_click:(fun _ ->
                        start_new_ssh_connection t ~name:!host_name ~uri:!ssh_uri;
                        false);
                    button [pcdata "Cancel"] ~on_click:(fun _ ->
                        Reactive.Source.modify add_interface_visible not;
                        false);
                  ];
                ]
              )
            ]
          | false ->  [main_button false]
          ) |> Signal.list)

  let rec display_list_of_connections ?(hide_loading = false) t =
    let open H5 in
    let list_of_connections l =
      ul (
        li [controls_header t ~can_update:true]
        ::
        List.map l ~f:(fun {Ssh_connection.id; name; uri; status} ->
            li [
              single_connection_reactive_div t ~uri ~name ~id ~status;
            ])
      );
    in
    div [
      h3 [pcdata "Manage Named SSH Connections"];
      Reactive_node.div Reactive.(
          Signal.tuple_2
            (Source.signal t.ssh_connections_status)
            (Source.signal t.ssh_connections)
          |> Signal.map  ~f:(fun current_value ->
              match current_value with
              | `Error m, prev ->
                div [
                  Bootstrap.error_box [Markup.to_html m];
                  list_of_connections prev;
                ]
              | `In_progress, prev ->
                div [
                  Bootstrap.warning_box [
                    pcdata "Fetching stuff "; Bootstrap.loader_gif ();
                  ];
                  list_of_connections prev;
                ]
              | `Ok, l ->
                div [
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
