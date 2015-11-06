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
   
Once the connection established, we use the result of
`Ssh_connection.host_uri` as a `Ketrew.Host.t` by running `uname -a`
on it.
   
Example setup:
   
- Create a Vagrant VM and retrieve its SSH port, user, and password
  (may have to set one). We do not use the ssh-config provided by
  vragrant to get SSH to ask for a password.
- Call:

    KHP_SSH_PASSWORD=ubuntu64 KHP_SSH_URI=ssh://vagrant@127.0.0.1:2222 ./ketrew-test-process-holding ssh

*)
let test_ssh () =
  let open Ketrew.Process_holder in
  let get_env s =
    try Sys.getenv s with _ -> failwithf "Test needs variable: %S" s in
  let ssh_connection = get_env "KHP_SSH_URI" in
  let ssh_password = get_env "KHP_SSH_PASSWORD" in
  let (t : Ssh_connection.t) =
    Ssh_connection.create ~ketrew_bin:"./ketrew"
      ~name:"hold_processes-test" ssh_connection in
  let display_logs fmt =
    Printf.ksprintf (fun msg ->
        Ssh_connection.markup_with_daemon_logs t
        >>= fun full_markup ->
        Log.(s msg % n % Display_markup.log full_markup @ normal);
        return ()) fmt in
  let pause () = (System.sleep 1.0 >>< fun _ -> return ()) in
  display_logs "Just started" >>= fun () ->
  pause () >>= fun () ->
  display_logs "After a second "
  >>= fun () ->
  pause () >>= fun () ->
  Ssh_connection.write_to_fifo t "blablalablablabablablablablablaaaaa"
  >>= fun () ->
  pause () >>= fun () ->
  display_logs "after wrong password + 1 sec "
  >>= fun () ->
  Ssh_connection.write_to_fifo t ssh_password
  >>= fun () ->
  pause () >>= fun () ->
  display_logs "after good password + 1 sec " >>= fun () ->
  let host = Ketrew.EDSL.Host.parse (Ssh_connection.host_uri t) in
  let host_io = Ketrew.Host_io.create () in
  Ketrew.Host_io.get_shell_command_output host_io ~host
    ~timeout:(`Seconds 2.)
    "uname -a"
  >>= fun (stdout, stderr) ->
  Ssh_connection.kill t
  >>= fun () ->
  pause () >>= fun () ->
  display_logs "After Kill + 1s "
  >>= fun () ->
  Log.(s "The Host: "  % Host.log host % n
       % s "Stdout: " % quote stdout % n
       % s "Stderr: " % quote stderr % n
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
