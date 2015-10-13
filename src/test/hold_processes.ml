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
  let (t : Ssh_connection.t) =
    let command =
      let unique_blob = Unique_id.create () in
      fmt "echo %s >> /tmp/hello" unique_blob in
    Ssh_connection.create ~ketrew_bin:"./ketrew"  ~command
      ssh_connection
  in
  Ssh_connection.markup_with_daemon_logs t
  >>= fun full_markup ->
  Log.(s "Just Started " % Display_markup.log full_markup
       @ normal);
  (System.sleep 1.0 >>< fun _ -> return ())
  >>= fun () ->
  Ssh_connection.markup_with_daemon_logs t
  >>= fun full_markup ->
  Log.(s "After a second " % Display_markup.log full_markup
       @ normal);
  (System.sleep 1.0 >>< fun _ -> return ())
  >>= fun () ->
  Ssh_connection.write_to_fifo t "blablalablablabablablablablablaaaaa"
  >>= fun () ->
  (System.sleep 1.0 >>< fun _ -> return ())
  >>= fun () ->
  Ssh_connection.markup_with_daemon_logs t
  >>= fun full_markup ->
  Log.(s "After wrong password + 1s " % Display_markup.log full_markup
       @ normal);
  Ssh_connection.write_to_fifo t ssh_password
  >>= fun () ->
  (System.sleep 1.0 >>< fun _ -> return ())
  >>= fun () ->
  Ssh_connection.markup_with_daemon_logs t
  >>= fun full_markup ->
  Log.(s "At the end " % Display_markup.log full_markup
       @ normal);
  return ()

let () =
  global_debug_level := 2;
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
