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
open Ketrew.Unix_io

let failwithf fmt = Printf.ksprintf failwith fmt

(*

This test runs `./ketrew internal-ssh ...` and then talks to the named
pipe to enter the SSH password.
It first enters a wrong password and then the right one.

*)
let test_ssh () =
  let open Ketrew.Process_holder in
  let get_env s =
    try Sys.getenv s with _ -> failwithf "Test needs variable: %S" s in
  let ssh_connection = get_env "KHP_SSH_URI" in
  let ssh_password = get_env "KHP_SSH_PASSWORD" in
  let json_log = "/tmp/khp_log.json" in
  let fifo = "/tmp/khp_fifo.pipe" in
  let unique_blob = Unique_id.create () in
  load ()
  >>= fun holder ->
  (* It's actually a bit silly to use the process-holder with
     `internal-ssh` since it daemonizes, but the API gets tested *)
  start holder [
    "./ketrew"; "internal-ssh";
    "--log-to"; json_log;
    "--fifo"; fifo;
    "--to"; ssh_connection;
    "-c"; fmt "echo %s >> /tmp/hello" unique_blob;
  ]
  >>= fun proc_id ->
  get holder proc_id
  >>= fun process ->
  Log.(s "Started " % Display_markup.log (Process.markup process) %n
       % s "stdout: " % quote (Process.stdout process) % n
       % s "stderr: " % quote (Process.stderr process) % n
       @ normal);
  (*
  (System.sleep 1.0 >>< fun _ -> return ()) >>= fun () ->
  Log.(s "After 1 second " % Display_markup.log (Process.markup process) %n
       % s "stdout: " % quote (Process.stdout process) % n
       % s "stderr: " % quote (Process.stderr process) % n
       @ normal);
     *)
  (* (System.sleep 1.0 >>< fun _ -> return ()) >>= fun () -> *)
  let display_logs () =
    IO.read_file json_log
    >>= fun logs ->
    let markup =
      Yojson.Safe.from_string logs |> Display_markup.of_yojson 
      |> function
      | `Ok o -> o
      | `Error e -> failwithf "parsing %s: %s" json_log e in
    Log.(s "Logs: " % n % Display_markup.log markup @ normal);
    return ()
  in
  display_logs ()
  >>= fun () ->
  let read_fifo () =
    wrap_deferred
      ~on_exn:(fun e -> `Failure (Printexc.to_string e)) (fun () ->
          let open Lwt in
          let open Lwt_unix in
          Log.(s "opening " % quote fifo @ normal);
          openfile fifo [O_RDONLY] 0o700 (* make the call blocking until somebody opens for writing *)
          >>= fun fd ->
          Log.(s "opened " % quote fifo @ normal);
          let read_char fd =
            let one = String.make 1 'B' in
            read fd one 0 1
            >>= function
            | 0 -> return None
            | 1 -> return (Some (String.get_exn one 0))
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
    >>= fun content ->
    Log.(s "Read fifo: " % quote fifo % s ": "
         % quote content
         @ normal);
    return () in
  let write_to_fifo content =
    IO.with_out_channel (`Append_to_file fifo) ~f:(fun out ->
        IO.write out content)
  in
  read_fifo ()
  >>= fun () ->
  write_to_fifo "blablalablablabablablablablablaaaaa"
  >>= fun () ->
  read_fifo ()
  >>= fun () ->
  write_to_fifo ssh_password
  >>= fun () ->
  (System.sleep 1.0 >>< fun _ -> return ())
  >>= fun () ->
  display_logs ()
  >>= fun () ->
  return ()

let () =
  Lwt_main.run begin
    match Sys.argv.(1) with
    | "ssh" -> test_ssh ()
    | other -> return ()
  end |> function
  | `Ok () ->
    Log.(OCaml.list quote (Array.to_list Sys.argv) % s " → OK." @ normal);
    ()
  | `Error e ->
    Log.(OCaml.list quote (Array.to_list Sys.argv) % s " → Error : " % s (Ketrew.Error.to_string e) @ error);
    ()
