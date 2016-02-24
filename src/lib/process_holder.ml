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
open Logging.Global
let log_module = "Module", Display_markup.text "Process_holder"

(* let d fmt = *)
(*   Printf.ksprintf (Printf.eprintf "[%s] %s\n%!" Time.(now () |> to_filename)) fmt *)

module Process = struct
  type t = {
    id: string;
    command: string * (string list);
    lwt_object: Lwt_process.process_full;
    stdout_buffer: Buffer.t;
    stderr_buffer: Buffer.t;
    changes: string Lwt_react.E.t;
    signal_change: string -> unit;
  }
  let create ~command lwt_object =
    let id = Unique_id.create () in
    let stdout_buffer = Buffer.create 42 in
    let stderr_buffer = Buffer.create 42 in
    let changes, signal_change = Lwt_react.E.create () in
    {id; command; lwt_object; stdout_buffer; stderr_buffer;
     changes; signal_change;}

  let get_changes_event t = t.changes
  let id t = t.id
  let stdout t = Buffer.contents t.stdout_buffer
  let stderr t = Buffer.contents t.stderr_buffer


  let markup t =
    let open Display_markup in
    description_list [
      log_module;
      "Id", text t.id;
      "Command",
      description_list [
        "Binary", command (fst t.command);
        "Command-line", List.map ~f:command (snd t.command) |> concat ~sep:(text "; ");
      ];
      "Pid", (try textf "%d" t.lwt_object#pid with _ -> text "Undefined");
      "Status",
      begin match t.lwt_object#state with
      | Lwt_process.Running  -> text "Running"
      | Lwt_process.Exited (Unix.WEXITED d) -> textf "Exited %d" d
      | Lwt_process.Exited (Unix.WSIGNALED d) -> textf "Signaled %d" d
      | Lwt_process.Exited (Unix.WSTOPPED d) -> textf "Stopped %d" d
      | exception _ -> text "Undefined"
      end;
      "Standard-output", code_block (stdout t);
      "Standard-error", code_block (stderr t);
    ]

  let log_process_action process action =
    Logger.(
      description_list [
        log_module;
        "Action", text action;
        "Process", markup process;
      ] |> log)

  let stop process =
    let open Lwt in
    catch begin fun () ->
      process.lwt_object#terminate;
      log_process_action process "Terminating";
      process.lwt_object#close
      >>= fun _ ->
      log_process_action process "Closing";
      process.signal_change "process-closing";
      return (`Ok ())
    end
      (fun e ->
         Log.(s "Error in stop: " % exn e @ error);
         return (`Ok ()))

  let start ?(bin="") argl =
    let command = (bin, Array.of_list argl) in
    let lwt_object = Lwt_process.open_process_full command in
    let process = create ~command:(bin, argl) lwt_object in
    log_process_action process "Starting";
    let rec get_all_output () =
      let open Lwt in
      catch (fun () ->
          pick [
            begin
              Lwt_io.read_char_opt process.lwt_object#stdout
              >>= function
              | None -> return `End
              | Some c ->
                process.signal_change "process-stdout";
                Buffer.add_char process.stdout_buffer c;
                return `Loop
            end;
            begin
              Lwt_io.read_char_opt process.lwt_object#stderr
              >>= function
              | None -> return `End
              | Some c ->
                process.signal_change "process-stderr";
                Buffer.add_char process.stderr_buffer c;
                return `Loop
            end;
            begin
              Lwt_unix.sleep 0.4
              >>= fun () ->
              return `Loop
            end;
          ])
        (fun e ->
           Log.(s "Error: " % exn e @ error);
           return `End)
      >>= function
      | `End -> stop process >>= fun _ -> return ()
      | `Loop -> get_all_output ()
    in
    Lwt.async (fun () -> get_all_output ());
    process

  let wait p =
    wrap_deferred Lwt.(fun () ->
        p.lwt_object#status
        >>= fun _ ->
        return ()
      )
      ~on_exn:(fun e ->
          let msg = fmt "Process.wait: %s" (Printexc.to_string e) in
          `Failure msg)



end

module Ssh_connection = struct

  let make_pipe ?name () =
    let fifo =
      match name with
      | None  -> Filename.temp_file "ketrew-ssh-askpass" ".pipe"
      | Some s -> s in
    (try Unix.unlink fifo with _ -> ());
    Unix.mkfifo fifo 0o700;
    fifo

(*
Overly complex work around OpenSSH's TTY craziness.

In order to get the prompt for a password, on top of the `DISPLAY` and
`SSH_ASKPASS` environment variables we need to ensure we're detached from
any terminal. `setsid` provides that, but on OSX `setsid` is not provided
as a command line tool, so we use OCaml's
[`Unix.setsid`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALsetsid).
Which then requires a `fork` not to mess up with the current process.

We need to comunicate with that process:

- There are two FIFO files that are the I/O plumbing of the extremely
  simple implementation of the `SSH_ASKPASS` program.

*)
  let setsid_ssh
      ~session_id_file ~control_path ~log_to ~pipe_in ~pipe_out ~command
      ~temp_dir
      uri =
    let ssh_connection, port_option =
      let uri = Uri.of_string uri in
      fmt "%s%s"
        (Uri.user uri |> Option.value_map ~default:"" ~f:(fmt "%s@"))
        (Uri.host uri |> Option.value ~default:"127.0.0.1"),
      Uri.port uri |> Option.value_map ~default:"" ~f:(fmt " -p %d") in
    global_with_color := false;
    Filename.set_temp_dir_name temp_dir;
    Log.(s "setsid_ssh to " % quote ssh_connection
         % sf " port-option %S" port_option  @ normal);
    let temp_script = Filename.temp_file "ketrew-ssh-askpass" ".sh" in
    Unix.chmod temp_script 0o700;
    let meta_log markup =
      let content =
        Display_markup.to_yojson markup |> Yojson.Safe.pretty_to_string in
      IO.write_file log_to ~content
    in
    let log_file = Filename.temp_file "ketrew-ssh-askpass" "log" in
    (try Unix.unlink log_file with _ -> ());
    Printf.eprintf "before fork  %s\n%!" Time.(now () |> to_filename);
    begin match Lwt_unix.fork () with
    | 0 ->
      let session_id = Unix.setsid () in
      begin match Lwt_unix.fork () with
      | 0 ->
        let log s =
          IO.with_out_channel (`Append_to_file log_file)
            ~f:(fun out -> IO.write out s) in
        log (fmt "Sesssion ID: %d\n" session_id)
        >>= fun () ->
        IO.write_file session_id_file ~content:(fmt "%d\n" session_id)
        >>= fun () ->
        let session_log ?process status =
          Display_markup.(
            description_list [
              "Status", text status;
              "Session-ID", textf "%d" session_id;
              "Log-File", path log_file;
              "FIFO-in", path pipe_in;
              "FIFO-out", path pipe_out;
              "ASK-PASS", path temp_script;
              "Process",
              Option.value_map ~f:Process.markup process ~default:(text "N/A");
            ]) in
        meta_log (session_log "Session created")
        >>= fun () ->
        let content =
          fmt "#!/bin/sh\n\n\
               echo \"============\n## $* Called on `date`\" >> %s\n\
               echo \"$*\" >> %s\n\
               cat %s\n"
            log_file pipe_out pipe_in
        in
        IO.write_file temp_script ~content
        >>= fun () ->
        meta_log (session_log "Script written")
        >>= fun () ->
        let process =
          Process.start
            ["bash"; "-c";
             fmt
               "unset SSH_AUTH_SOCK; \
                DISPLAY=:0 SSH_ASKPASS=%s \
                ssh %s -o StrictHostKeyChecking=no \
                -o ChallengeResponseAuthentication=no %s %s %s"
               temp_script port_option
               (fmt "-o ControlMaster=auto -o 'ControlPath=%s'" control_path)
               ssh_connection
               (command |> Filename.quote)
            ]
        in
        meta_log (session_log ~process "Process started")
        >>= fun () ->
        log Log.(s "setsid_ssh started"% n
                 % Display_markup.log (Process.markup process)
                 % n |> to_long_string)
        >>= fun () ->
        begin
          Process.wait process
          >>< function
          | `Error (`Failure msg) ->
            meta_log (session_log ~process "Process.wait failed")
            >>= fun () ->
            log Log.(s "setsid_ssh wait failed: " % quote msg % n
                     % Display_markup.log (Process.markup process) % n
                     % s "stdout: " % n % verbatim (Process.stdout process) % n
                     % s "stderr: " % n % verbatim (Process.stderr process) % n
                     % n % n % n
                     |> to_long_string)
          | `Ok () ->
            meta_log (session_log ~process "Process finished")
            >>= fun () ->
            log Log.(s "setsid_ssh done" % n
                     % Display_markup.log (Process.markup process) % n
                     % s "stdout: " % n % verbatim (Process.stdout process) % n
                     % s "stderr: " % n % verbatim (Process.stderr process) % n
                     % n % n % n
                     |> to_long_string)
        end
    (*
    Process.wait process
       *)
      (* (System.sleep 1.0 >>< fun _ -> return ()) >>= fun () -> *)
      | pid ->
        (* second fork *)
        Printf.eprintf "fork #2 → %d %s\n%!" pid Time.(now () |> to_filename);
        return ()
      | exception e ->
        Printf.eprintf "fork #1 → %s %s\n%!" Printexc.(to_string e) Time.(now () |> to_filename);
        return ()
      end
    | pid ->
      Printf.eprintf "fork #1 → %d %s\n%!" pid Time.(now () |> to_filename);
      Log.(s "setsid_ssh main finishing, \
              subprocess is: " % i pid
           @ normal);
      return ()
    | exception e ->
      Printf.eprintf "fork #1 → %s %s\n%!" Printexc.(to_string e) Time.(now () |> to_filename);
      return ()
    end


  let read_fifo fifo =
    let read_buffer = Bytes.make 1 'B' in
    wrap_deferred
      ~on_exn:(fun e -> `Failure (Printexc.to_string e)) (fun () ->
          let open Lwt in
          let open Lwt_unix in
          Log.(s "opening " % quote fifo @ verbose);
          (* make the call blocking until somebody opens for writing: *)
          openfile fifo [O_RDONLY] 0o700
          >>= fun fd ->
          Log.(s "opened " % quote fifo @ verbose);
          let read_char fd =
            read fd read_buffer 0 1
            >>= function
            | 0 -> return None
            | 1 -> return (Some (Bytes.get read_buffer 0))
            | more -> fail (Failure (fmt "read_char got %d bytes" more))
          in
          let rec read_all acc fd =
            read_char fd
            >>= function
            | None -> return (List.rev acc)
            | Some s -> read_all (s :: acc) fd
          in
          read_all [] fd
          >>= fun l ->
          close fd
          >>= fun () ->
          return (String.of_character_list l))


  type t = {
    ketrew_bin: string;
    host_name: string;
    json_log: string;
    fifo_to_daemon: string;
    fifo_from_daemon: string;
    session_id_file: string;
    control_path: string;
    connection: string;
    command: string;
    process: Process.t;
    fifo_questions: (Time.t * string) list Reactive.Source.t;
    mutable fd_to_askpass: Lwt_unix.file_descr option;
  }

  let create ?(ketrew_bin = "ketrew") ?(command = "/bin/sh") ~name connection =
    let json_log = Filename.temp_file "ketrew-json-log" ".json" in
    let fifo_to_daemon = make_pipe () in
    let fifo_from_daemon = make_pipe () in
    let fifo_questions = Reactive.Source.create [] in
    let control_path = Filename.temp_file "ketrew-control-path" ".ssh" in
    (try Unix.unlink control_path with _ -> ());
    let session_id_file = Filename.temp_file "ketrew-ssh-session" ".int" in
    let rec read_fifo_over_and_over () =
      read_fifo fifo_from_daemon
      >>= fun content ->
      (* d "modifying fifo_questions: %s" content; *)
      Reactive.Source.modify fifo_questions (fun l ->
          (Time.now (), content) :: l);
      read_fifo_over_and_over () in
    Lwt.async read_fifo_over_and_over;
    (* The fifo should be created here *)
    let process =
      Process.start ~bin:ketrew_bin [
        ketrew_bin; "internal-ssh";
        "--log-to"; json_log;
        "--fifo-in"; fifo_to_daemon;
        "--fifo-out"; fifo_from_daemon;
        "--control-path"; control_path;
        "--write-session-id"; session_id_file;
        "--temp-dir"; Filename.get_temp_dir_name ();
        "--to"; connection;
        "-c"; command;
      ] in
    let ssh =
      {ketrew_bin; json_log; fifo_to_daemon; fifo_from_daemon; control_path;
       connection; command; process; session_id_file; fifo_questions;
       fd_to_askpass = None; host_name = name}
    in
    ssh

  let name s = s.host_name

  let get_changes_event t =
    let fifo_questions_change_event =
      Reactive.Source.signal t.fifo_questions |> Lwt_react.S.changes
      |> Lwt_react.E.map (fun l -> fmt "ssh-fifo-questions:%d" (List.length l))
    in
    Lwt_react.E.select [
      fifo_questions_change_event;
      Process.get_changes_event t.process;
    ]

  let get_daemon_logs t =
    IO.read_file t.json_log
    >>= fun logs ->
    begin match Yojson.Safe.from_string logs |> Display_markup.of_yojson with
    | `Ok o -> return o
    | exception e ->
      fail (`Failure (fmt "parsing %s: %s" t.json_log (Printexc.to_string e)))
    | `Error e -> fail (`Failure (fmt "parsing %s: %s" t.json_log e))
    end

  let ssh_options t =
    ["-oControlMaster=auto"; fmt "-oControlPath=%s" t.control_path;]

  let host_uri t =
    let uri = Uri.of_string t.connection in
    Uri.add_query_param uri
      ("ssh-option", ssh_options t)
    |> Uri.to_string

  let as_host t =
    host_uri t |> Host.of_string

  let as_ssh_connection t =
    let uri = Uri.of_string t.connection in
    {
      Ketrew_pure.Host.Ssh.
      address = (Uri.host uri |> Option.value ~default:"127.0.0.1");
      port = Uri.port uri;
      user = Uri.user uri;
      add_ssh_options = ssh_options t
    }

  let markup
      {ketrew_bin; json_log; fifo_to_daemon; fifo_from_daemon; connection;
       session_id_file; control_path; host_name;
       command = the_command; process; fifo_questions; fd_to_askpass} =
    let questions = Reactive.Source.value fifo_questions in
    Display_markup.(
      description_list [
        "Name", text host_name;
        "Ketrew-bin", path ketrew_bin;
        "JSON-logfile", path json_log;
        "FIFO-to-daemon", path fifo_to_daemon;
        "FIFO-from-daemon", path fifo_from_daemon;
        "Connection", path connection;
        "Command", command the_command;
        "Sesssion-ID-File", path session_id_file;
        "SSH-ControlPath", path control_path;
        "Fifo-questsions",
        description_list
          (("Length", textf "%d items" (List.length questions))
           :: (List.take questions 3 |> List.mapi ~f:(fun i (time, v) ->
               fmt "%d" i, concat [date time; text ": "; text v])));
        "Process", Process.markup process;
      ]
    )

  let markup_with_daemon_logs t =
    begin
      get_daemon_logs t
      >>< function
      | `Ok m -> return m
      | `Error e -> return Display_markup.(textf "Error: %s" (Error.to_string e))
    end
    >>= fun dm ->
    return Display_markup.(
      description_list [
        "SSH-holder", markup t;
        "Daemon-log", dm;
      ]
    )


  let session_pid t =
    IO.read_file t.session_id_file
    >>| String.strip
    >>= fun content ->
    begin match Int.of_string content with
    | Some i -> return i
    | None -> fail (`Failure (fmt "Session-ID not an integer: %S" content))
    end

  let kill t =
    session_pid t
    >>= fun pid ->
    System.Shell.do_or_fail (fmt "kill -- -%d" pid)

  let file_descriptor_to_askpass t =
    begin match t.fd_to_askpass with
    | Some fd -> return fd
    | None ->
      wrap_deferred
        ~on_exn:(fun e -> `Failure (Printexc.to_string e)) (fun () ->
            let open Lwt in
            Lwt_unix.(openfile t.fifo_to_daemon
                        [O_APPEND; O_NONBLOCK; O_WRONLY] 0o770)
            >>= fun fd ->
            t.fd_to_askpass <- Some fd;
            return fd)
    end

  let can_write_to_fifo t =
    file_descriptor_to_askpass t
    >>< function
    | `Ok fd -> return true
    | `Error s ->
      Log.(s "The fifo: " % quote t.fifo_to_daemon % s " is not writable"
           @ verbose);
      return false

  let write_to_fifo t content =
    file_descriptor_to_askpass t
    >>= fun fd ->
    wrap_deferred
      ~on_exn:(fun e -> `Failure (Printexc.to_string e)) (fun () ->
          let open Lwt in
          Lwt_unix.write_string fd content 0 (String.length content)
          >>= fun (_ : int) ->
          t.fd_to_askpass <- None;
          Lwt_unix.close fd)

  let get_status t =
    (* d "stdout: %S, stderr: %S" *)
    (*   (t.process |> Process.stdout) *)
    (*   (t.process |> Process.stderr) *)
    (* ; *)
    session_pid t
    >>< begin function
    | `Ok pid ->
      System.Shell.execute (fmt "ps -g %d" pid)
      >>= fun (_,_, process_status) ->
      can_write_to_fifo t
      >>= fun can_write ->
      begin match process_status with
      | `Exited 0 when can_write ->
        let questions =
          Reactive.Source.value t.fifo_questions in
        return (`Alive (`Askpass_waiting_for_input questions))
      | `Exited 0 -> return (`Alive `Idle)
      | `Signaled _
      | `Stopped _
      | `Exited _ -> return (`Dead (System.Shell.status_to_string process_status))
      end
    | `Error e ->
      return (`Unknown (Error.to_string e))
    end

  let log t = markup t |> Display_markup.log

end


type active_process = [
  | `Ssh_connection of Ssh_connection.t
  | `Process of Process.t
]
module Ui_client = struct
  type t = {
    created: Time.t;
    mutable last_answered: Time.t;
  }
  let create () = {created = Time.now (); last_answered = Time.now ()}
end
type t = {
  preconfigured: (string * Configuration.ssh_connection) list;
  mutable active_processes: (string, active_process) Hashtbl.t;
  ui_clients: (Unique_id.t, Ui_client.t) Hashtbl.t;

  (*
     The `!change_event` is the “output” event, the want to wait on.
     `subscribe_event` adds (or “merges … with”) a new event to the
     `change_event`
     `signal_change` triggers the event.
  *)
  events: string Lwt_react.E.t list ref;
  get_change_event: unit -> string Lwt_react.E.t;
  subscribe_event: string Lwt_react.E.t -> unit;
  signal_change: string -> unit;
  signal_future_change: string -> unit;
}

let create ?(preconfigure=[]) () =
  let initial_change_event, signal_change = Lwt_react.E.create () in
  let events = ref [initial_change_event] in
  let get_change_event () = Lwt_react.E.select !events in
  let subscribe_event ev = events := ev :: !events in
  let signal_future_change msg =
    Lwt.async Lwt.(fun () ->
        Lwt_unix.sleep 1.
        >>= fun () ->
        signal_change msg;
        return ()
      )
  in
  {
    preconfigured = List.map preconfigure ~f:(fun p -> Unique_id.create (), p);
    active_processes = Hashtbl.create 42;
    ui_clients = Hashtbl.create 42;
    events; get_change_event; signal_change; subscribe_event;
    signal_future_change;
  }

let kill_all t =
  let as_list =
    Hashtbl.fold (fun id p prev -> (id, p) :: prev) t.active_processes []
  in
  Deferred_list.for_concurrent as_list ~f:(fun (id, proc) ->
      begin match proc with
      | `Ssh_connection ssh ->
        Log.(s "Killing " % Ssh_connection.log ssh  @ verbose);
        Ssh_connection.kill ssh
      | `Process process ->
        Process.stop process
      end
    )
  >>= fun ((_ : unit list), errors) ->
  begin match errors with
  | [] -> return ()
  | _ :: _ as e -> fail (`List e)
  end

let load ?preconfigure () =
  let t = create ?preconfigure () in
  return t

let unload t =
  kill_all t

let start_process t ?bin argl =
  let p = Process.start ?bin argl in
  let id = Process.id p in
  Hashtbl.add t.active_processes id (`Process p);
  t.signal_change "start_process";
  return id

let start_ssh_connection t ?ketrew_bin ?command ~name connection =
  let s = Ssh_connection.create ?ketrew_bin ?command ~name connection in
  let id = Unique_id.create  () in
  Hashtbl.add t.active_processes id (`Ssh_connection s);
  t.signal_change "start_ssh_connection";
  t.subscribe_event (Ssh_connection.get_changes_event s);
  return s

let get t ~id =
  match Hashtbl.find t.active_processes id with
  | t -> return t
  | exception _ -> fail (`Missing_data id)

let get_ssh_connection t ~id =
  get t ~id
  >>= begin function
  | `Ssh_connection ssh -> return ssh
  | _ -> fail (`Not_an_ssh_connection id)
  end

let all_ssh_ids_and_names t =
  let configured =
    List.map t.preconfigured ~f:(fun (id, sshc) ->
        let name, uri = Configuration.ssh_connection_name_uri sshc in
        {Protocol.Process_sub_protocol.Ssh_connection.
          id; name; uri; status = `Configured}) in
  Hashtbl.fold (fun id x prev_m ->
      prev_m >>= fun prev ->
      begin match x with
      | `Ssh_connection ssh ->
        Ssh_connection.get_status ssh
        >>= fun status ->
        let name = Ssh_connection.name ssh in
        let next =
          {Protocol.Process_sub_protocol.Ssh_connection.
            id; name; uri = Ssh_connection.host_uri ssh; status} :: prev in
        return next
      | `Process _ -> return prev
      end
    )
    t.active_processes (return [])
  >>= fun started ->
  let ret = (configured @ started) in
  Log.(s "all_ssh_ids_and_names: " % i (List.length  ret)
       % s " elements: "
       % OCaml.list (fun s ->
           quote s.Protocol.Process_sub_protocol.Ssh_connection.uri)
         ret
       @ verbose);
  return  ret

let answer_get_all_ssh_ids t ~client_id =
  begin match Hashtbl.find t.ui_clients client_id with
  | ui_client ->
    begin
      Deferred_list.pick_and_cancel [
        (System.sleep 20. >>< fun _ -> return "time");
        Lwt.(Lwt_react.E.next (t.get_change_event ())
             >>= fun x ->
             return (`Ok (fmt "event:%s" x))); 
      ]
      >>= fun why_woken_up ->
      (* d "because of %s%!" why_woken_up; *)
      all_ssh_ids_and_names t
      >>= fun all ->
      ui_client.Ui_client.last_answered <- Time.now ();
      return (`List_of_ssh_ids all)
    end
  | exception _ ->
    let ui_client = Ui_client.create () in
    Hashtbl.add t.ui_clients client_id ui_client;
    all_ssh_ids_and_names t
    >>= fun all ->
    return (`List_of_ssh_ids all)
  end

let answer_message t ~host_io (msg : Protocol.Process_sub_protocol.up) :
  (Protocol.Process_sub_protocol.down, 'a) Deferred_result.t =
  begin match msg with
  | `Start_ssh_connection spec ->
    begin match spec with
    | `New (name, connection) ->
      start_ssh_connection t ~ketrew_bin:global_executable_path ~name connection
      >>= fun ssh ->
      return (name, ssh)
    | `Configured id ->
      begin match List.find t.preconfigured ~f:(fun (i, _) -> i = id) with
      | Some (_, sshc) ->
        let name, uri = Configuration.ssh_connection_name_uri sshc in
        start_ssh_connection t ~ketrew_bin:global_executable_path ~name uri
        >>= fun ssh ->
        return (name, ssh)
      | None ->
        fail (`Missing_data id)
      end
    end
    >>= fun (name, (ssh : Ssh_connection.t)) ->
    let ssh_connection = Ssh_connection.as_ssh_connection ssh in
    Host_io.set_named_host host_io ~name ssh_connection
    >>= fun () ->
    return (`Ok)
  | `Get_all_ssh_ids client_id ->
    answer_get_all_ssh_ids t ~client_id
  | `Get_logs (id, `Full) ->
    get_ssh_connection t ~id
    >>= fun ssh ->
    Ssh_connection.markup_with_daemon_logs ssh
    >>= fun markup ->
    return (`Logs (id, Display_markup.serialize markup))
  | `Send_ssh_input (id, content) ->
    get_ssh_connection t ~id
    >>= fun ssh ->
    System.with_timeout 3.0 ~f:begin fun () ->
      Ssh_connection.write_to_fifo ssh content
    end
    >>= fun () ->
    t.signal_future_change "send_ssh_input";
    return `Ok
  | `Send_command
      { Protocol.Process_sub_protocol.Command.connection; id; command } ->
    get_ssh_connection t ~id:connection
    >>= fun ssh ->
    of_result (Ssh_connection.as_host ssh)
    >>= fun host ->
    Host_io.get_shell_command_output host_io ~host command
    >>= fun (stdout, stderr) ->
    t.signal_future_change "send_command";
    return (`Command_output {
        Protocol.Process_sub_protocol.Command_output.id; stdout; stderr
      })
  | `Kill id ->
    get_ssh_connection t ~id
    >>= fun ssh ->
    Ssh_connection.kill ssh
    >>= fun () ->
    t.signal_future_change "kill";
    return `Ok
  end
  >>< function
  | `Ok msg -> return msg
  | `Error e ->
    let msg =
      match e with
      | `IO _ as e -> IO.error_to_string e
      |  `Missing_data id -> fmt "cannot find process %S" id
      | `System (`With_timeout  _, `Exn e) ->
        fmt "timed-wait failed: %s" (Printexc.to_string e)
      | `Timeout f -> fmt "Operation timeouted: %f s." f
      | `Not_an_ssh_connection id -> fmt "not an SSH connection: %s" id
      | `Failure s -> fmt "failure: %s" s
      | `Host_uri_parsing_error (uri, e) ->
        fmt "Error while parsing Host URI %S: %s" uri e
      | `Host _
      | `Shell _ as eshell -> Error.to_string eshell
    in
    return (`Error msg)
