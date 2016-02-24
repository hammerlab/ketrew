(**************************************************************************)
(*    Copyright 2014, 2015:                                               *)
(*          Sebastien Mondet <seb@mondet.org>,                            *)
(*          Leonid Rozenberg <leonidr@gmail.com>,                         *)
(*          Arun Ahuja <aahuja11@gmail.com>,                              *)
(*          Jeff Hammerbacher <jeff.hammerbacher@gmail.com>               *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)

open Ketrew_pure
open Internal_pervasives
open Unix_io

let log_status = 
  let open Log in
  function
  | `Alive `Idle -> s "Alive"
  | `Alive (`Askpass_waiting_for_input l) ->
    s "Asking for passowrd input: " % n %
    indent (separate n @@ List.map l ~f:(fun (t, question) ->
        s "* " % Time.log t % s ": " % quote question))
  | `Dead reason -> s "Dead: " % s reason
  | `Configured -> s "Configured/ready to start"
  | `Unknown err -> s "Unknown status: " % quote err

let perform_call ~configuration msg f =
  Client.as_client ~configuration ~f:begin fun ~client ->
    Client.call_process_holder client msg
    >>= fun down ->
    begin match down with
    | `Error err ->
      fail (`Failure (fmt "Server answered with an ERROR: %s" err))
    | other ->
      begin match f down with
      | Some s -> return s
      | None ->
      fail (`Failure (fmt "Unexpected process-holder down-message"))
      end
    end
  end

let list_ssh_connections ~configuration () =
  let unique = Unique_id.create () in
  perform_call ~configuration (`Get_all_ssh_ids unique) (function
    | `List_of_ssh_ids l ->
      Some l
    | oterh -> None)
  >>= fun list_of_connections ->
  let open Protocol.Process_sub_protocol.Ssh_connection in
  Log.(s "SSH Connections: " % n
       % separate n (
         List.map list_of_connections
           ~f:(fun {id; name; uri; status} ->
               s "* " % s name % n %
               indent (
                 s "ID: " % brakets (s id) % n %
                 s "URI: " % quote uri % n %
                 s "Status: " % log_status status)))
       @ normal);
  return ()

let display_details ~configuration id =
  perform_call ~configuration (`Get_logs (id, `Full)) (function
    | `Logs (id, markup) ->
      Some markup
    | oterh -> None)
  >>= fun serialized_markup ->
  let markup =
    Display_markup.of_json_exn (Yojson.Safe.from_string serialized_markup) in
  Log.(s "Logs of " % s id % s ":" % n %
       Display_markup.log markup
       @ normal);
  return ()

let start_ssh_connection ~configuration how =
  perform_call ~configuration (`Start_ssh_connection how) (function
    | `Ok -> Some ()
    | oterh -> None)
  >>= fun () ->
  Log.(s "Started connection " %
       begin match how with
       | `New (name, sshuri) ->
         s "named " % quote name % s " to " % s sshuri
       | `Configured id ->
         s " from configured " % s id
       end % n %
       s "Please, check its status in a few seconds …"
       @ normal);
  return ()

let sub_commands ~version ~prefix ~configuration_arg () =
  let open Cmdliner in
  let sub_command cmd ~doc term =
    term, Term.info (prefix ^ "-" ^ cmd) ~version ~doc in
  let list_ssh_connections_cmd =
    sub_command "list"
      ~doc:"List the current named SSH hosts"
      Term.(
        pure begin fun configuration ->
          list_ssh_connections ~configuration ()
        end
        $ configuration_arg
      ) in
  let display_details_cmd =
    sub_command "details" ~doc:"Display details for given SSH processes"
      Term.(
        pure begin fun configuration ids ->
          Deferred_list.while_sequential ids ~f:(fun id ->
              display_details ~configuration id)
          >>= fun (_ : unit list) ->
          return ()
        end
        $ configuration_arg
        $ Arg.(
            info [] ~docv:"Ids" ~doc:"IDs of the hosts"
            |> pos_all string [] |> value
          )
      ) in
  let start_new_cmd =
    sub_command "start-new" ~doc:"Start new SSH connection"
      Term.(
        pure begin fun configuration name uri ->
          start_ssh_connection ~configuration (`New (name, uri))
        end
        $ configuration_arg
        $ Arg.(
            info ["N"; "name"] ~docv:"Name" ~doc:"Name of the future named-host"
            |> opt (some string) None
            |> required
          )
        $ Arg.(
            info [] ~docv:"URI" ~doc:"SSH URI to use for the connection"
            |> pos 0 (some string) None
            |> required
          )
      ) in
  let start_configured_cmd =
    sub_command "start-configured" ~doc:"Start a pre-configured SSH connection"
      Term.(
        pure begin fun configuration id ->
          start_ssh_connection ~configuration (`Configured id)
        end
        $ configuration_arg
        $ Arg.(
            info [] ~docv:"ID" ~doc:"ID of the pre-configured connection"
            |> pos 0 (some string) None
            |> required
          )
      ) in
  [list_ssh_connections_cmd; display_details_cmd;
   start_new_cmd; start_configured_cmd]
