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

module Process = struct
  type t = {
    id: string;
    command: string * (string list);
    lwt_object: Lwt_process.process_full;
    stdout_buffer: Buffer.t;
    stderr_buffer: Buffer.t;
  }
  let create ~command lwt_object =
    let id = Unique_id.create () in
    let stdout_buffer = Buffer.create 42 in
    let stderr_buffer = Buffer.create 42 in
    {id; command; lwt_object; stdout_buffer; stderr_buffer;}

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
    ]

  let id t = t.id
  let stdout t = Buffer.contents t.stdout_buffer
  let stderr t = Buffer.contents t.stderr_buffer

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
              | Some c -> Buffer.add_char process.stdout_buffer c; return `Loop
            end;
            begin
              Lwt_io.read_char_opt process.lwt_object#stderr
              >>= function
              | None -> return `End
              | Some c -> Buffer.add_char process.stderr_buffer c; return `Loop
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
as a command line tools, so we use OCaml's
[`Unix.setsid`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALsetsid).
Which then requires a `fork` not to mess up with the current process.

We need to comunicate with that process:

- There are two FIFO files that are the I/O plumbing of the extremely
  simple implementation of the `SSH_ASKPASS` program.

*)
  let setsid_ssh
      ?session_id_file ?control_path ?log_to ?pipe_in ?pipe_out ?command uri =
    let ssh_connection, port_option =
      let uri = Uri.of_string uri in
      fmt "%s%s"
        (Uri.user uri |> Option.value_map ~default:"" ~f:(fmt "%s@"))
        (Uri.host uri |> Option.value ~default:"127.0.0.1"),
      Uri.port uri |> Option.value_map ~default:"" ~f:(fmt " -p %d") in
    global_with_color := false;
    Log.(s "setsid_ssh to " % quote ssh_connection
         % sf " port-option %S" port_option  @ normal);
    let temp_script = Filename.temp_file "ketrew-ssh-askpass" ".sh" in
    Unix.chmod temp_script 0o700;
    let fifo_in =
      match pipe_in with None  ->  make_pipe () | Some s -> s in
    let fifo_out =
      match pipe_out with None  ->  make_pipe () | Some s -> s in
    let log_json =
      match log_to with
      | None  -> Filename.temp_file "ketrew-ssh-log" ".json"
      | Some f -> f in
    let meta_log markup =
      let content =
        Display_markup.to_yojson markup |> Yojson.Safe.pretty_to_string in
      IO.write_file log_json ~content
    in
    let log_file = "/tmp/ketrew-ssh-askpass.log" in
    (try Unix.unlink log_file with _ -> ());
    begin match Unix.fork () with
    | 0 ->
      let session_id = Unix.setsid () in
      begin match Unix.fork () with
      | 0 ->
        let log s =
          IO.with_out_channel (`Append_to_file log_file)
            ~f:(fun out -> IO.write out s) in
        log (fmt "Sesssion ID: %d\n" session_id)
        >>= fun () ->
        begin match session_id_file with
        | None  -> return ()
        | Some s -> IO.write_file s ~content:(fmt "%d\n" session_id)
        end
        >>= fun () ->
        let session_log ?process status =
          Display_markup.(
            description_list [
              "Status", text status;
              "Session-ID", textf "%d" session_id;
              "Log-File", path log_file;
              "FIFO-in", path fifo_in;
              "FIFO-out", path fifo_out;
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
            log_file fifo_out fifo_in
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
               (Option.value_map ~default:"" control_path
                  ~f:(fmt "-o ControlMaster=auto -o 'ControlPath=%s'"))
               ssh_connection
               (Option.value_map ~default:"" command ~f:Filename.quote)
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
        return ()
      end
    | pid ->
      Log.(s "setsid_ssh main finishing, \
              subprocess is: " % i pid
           @ normal);
      return ()
    end


  let read_fifo fifo =
    let read_buffer = String.make 1 'B' in
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
            | 1 -> return (Some (String.get_exn read_buffer 0))
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

  let create ?(ketrew_bin = "ketrew") ?(command = "/bin/sh") connection =
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
        "--to"; connection;
        "-c"; command;
      ] in
    let ssh =
      {ketrew_bin; json_log; fifo_to_daemon; fifo_from_daemon; control_path;
       connection; command; process; session_id_file; fifo_questions;
       fd_to_askpass = None}
    in
    ssh

  let get_daemon_logs t =
    IO.read_file t.json_log
    >>= fun logs ->
    begin match Yojson.Safe.from_string logs |> Display_markup.of_yojson with
    | `Ok o -> return o
    | exception e ->
      fail (`Failure (fmt "parsing %s: %s" t.json_log (Printexc.to_string e)))
    | `Error e -> fail (`Failure (fmt "parsing %s: %s" t.json_log e))
    end

  let host_uri t =
    let uri = Uri.of_string t.connection in
    Uri.add_query_param uri
      ("ssh-option", [
          "-oControlMaster=auto";
          fmt "-oControlPath=%s" t.control_path;
        ])
    |> Uri.to_string

  let as_host t =
    host_uri t |> Host.of_string

  let markup
      {ketrew_bin; json_log; fifo_to_daemon; fifo_from_daemon; connection;
       session_id_file; control_path;
       command = the_command; process; fifo_questions; fd_to_askpass} =
    let questions = Reactive.Source.value fifo_questions in
    Display_markup.(
      description_list [
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
    session_pid t
    >>= fun pid ->
    System.Shell.execute (fmt "ps -g %d" pid)
    >>= fun (_,_, process_status) ->
    can_write_to_fifo t
    >>= fun can_write ->
    begin match process_status with
    | `Exited 0 ->
      return (`Alive (if can_write then `Askpass_waiting_for_input else `Idle))
    | `Signaled _
    | `Stopped _ 
    | `Exited _ -> return (`Dead (System.Shell.status_to_string process_status))
    end

end


type active_process = [
  | `Ssh_connection of Ssh_connection.t
  | `Process of Process.t
]
type t = {
  mutable active_processes: (string, active_process) Hashtbl.t;
}

let create () = {active_processes = Hashtbl.create 42}

let load () =
  let t = create () in
  return t

let start_process t ?bin argl =
  let p = Process.start ?bin argl in
  let id = Process.id p in
  Hashtbl.add t.active_processes id (`Process p);
  return id

let start_ssh_connection t ?ketrew_bin ?command connection =
  let s = Ssh_connection.create ?ketrew_bin ?command connection in
  let id = Unique_id.create  () in
  Hashtbl.add t.active_processes id (`Ssh_connection s);
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
  Hashtbl.fold (fun id x prev_m ->
      prev_m >>= fun prev ->
      begin match x with
      | `Ssh_connection ssh ->
        Ssh_connection.get_status ssh
        >>= fun status ->
        let next =
          {Protocol.Process_sub_protocol.Ssh_connection.
            id; uri = Ssh_connection.host_uri ssh; status} :: prev in
        return next
      | `Process _ -> return prev
      end
    )
    t.active_processes (return [])

let answer_message t ~host_io msg :
  (Protocol.Process_sub_protocol.down, 'a) Deferred_result.t =
  begin match msg with
  | `Start_ssh_connetion connection ->
    start_ssh_connection t ~ketrew_bin:global_executable_path connection
    >>= fun (_ : Ssh_connection.t) ->
    return (`Ok)
  | `Get_all_ssh_ids ->
    all_ssh_ids_and_names t
    >>= fun all ->
    return (`List_of_ssh_ids all)
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
    return `Ok
  | `Send_command 
      { Protocol.Process_sub_protocol.Command.connection; id; command } ->
    get_ssh_connection t ~id:connection
    >>= fun ssh ->
    of_result (Ssh_connection.as_host ssh)
    >>= fun host ->
    Host_io.get_shell_command_output host_io ~host command
    >>= fun (stdout, stderr) ->
    return (`Command_output {
        Protocol.Process_sub_protocol.Command_output.id; stdout; stderr
      })
  | `Kill id ->
    get_ssh_connection t ~id
    >>= fun ssh ->
    Ssh_connection.kill ssh
    >>= fun () ->
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

