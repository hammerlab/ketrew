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
    (process.id, process)

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

(*
Overly complex work around OpenSSH TTY craziness.

In order to get the prompt for a password, on top the
`DISPLAY` and `SSH_ASKPASS` environment variables we need to ensure
we're detached from any terminal. `setsid` provides that, on OSX
`setsid` is not provided as a command line tools, so we use OCaml's
[`Unix.setsid`](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALsetsid).
Which then requires a `fork` not to mess up with the current process.

We need to comunicate with that process:

- There is a FIFO file that is the implementation of the `SSH_ASKPASS`
  program.

*)
let setsid_ssh ?log_to ?pipe ?command uri =
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
  let make_pipe opt =
    let fifo = 
      match pipe with
      | None  -> Filename.temp_file "ketrew-ssh-askpass" ".pipe"
      | Some s -> s in
    (try Unix.unlink fifo with _ -> ());
    Unix.mkfifo fifo 0o700;
    fifo
  in
  let temp_fifo = make_pipe pipe in
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
      log (fmt "Post Sesssion ID: %d\n" session_id)
      >>= fun () ->
      log (fmt "Post Post Sesssion ID: %d\n" session_id)
      >>= fun () ->
      let session_log ?process status =
        Display_markup.(
          description_list [
            "Status", text status;
            "Session-ID", textf "%d" session_id;
            "Log-File", path log_file;
            "FIFO", path temp_fifo;
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
          log_file temp_fifo temp_fifo
      in
      IO.write_file temp_script ~content
      >>= fun () ->
      meta_log (session_log "Script written")
      >>= fun () ->
      let (_, process) =
        Process.start 
          ["bash"; "-c";
           fmt
             "unset SSH_AUTH_SOCK; \
              DISPLAY=:0 SSH_ASKPASS=%s \
              ssh %s -o StrictHostKeyChecking=no \
              -o ChallengeResponseAuthentication=no %s %s"
             temp_script port_option ssh_connection
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
           % s "You should talk to: " %n
           % s temp_fifo
           @ normal);
      return ()
  end


type t = {
  mutable active_processes: (string, Process.t) Hashtbl.t;
}

let create () = {active_processes = Hashtbl.create 42}

let load () =
  let t = create () in
  return t

let start t ?bin argl =
  let (id, p) = Process.start ?bin argl in
  Hashtbl.add t.active_processes id p;
  return id

let get t ~id =
  match Hashtbl.find t.active_processes id with
  | t -> return t
  | exception _ -> fail (`Missing_data id)

