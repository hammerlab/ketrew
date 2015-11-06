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

(** A container for internal server-side processes (for now SSH
    connections/tunnels). *)

(** A module encapsulating daemonized SSH connections, the functions
    {!Ssh_connection.setsid_ssh} is exposed to the {!Command_line} module
    for the ["ketrew internal-ssh"] command.

    The other items exported are used only for tests.
*)
module Ssh_connection : sig

  val setsid_ssh:
    ?session_id_file:Unix_io.IO.path ->
    ?control_path:string ->
    ?log_to:Unix_io.IO.path ->
    ?pipe_in:string ->
    ?pipe_out:string ->
    ?command:string ->
    string ->
    (unit,
     [> `IO of
          [> `Exn of exn
          | `File_exists of string
          | `Write_file_exn of Unix_io.IO.path * exn
          | `Wrong_path of string ] ])
      Unix_io.t
  (** Daemonize an SSH connection with a control-path as a
      control-master, one can communicate with the client (as an SSH
      ["ASKPASS"] program) with the [pipe_in] and [pipe_out] named
      fifo files. *)

  (**/**)

  type t
  val create:
    ?ketrew_bin:string -> ?command:string -> name: string -> string -> t
  val markup_with_daemon_logs :
    t ->
    (Ketrew_pure.Internal_pervasives.Display_markup.t, 'a) Unix_io.t
  val write_to_fifo:
    t -> bytes -> (unit, [> `Failure of bytes ]) Unix_io.t
  val host_uri: t -> string
  val kill :
    t ->
    (unit,
     [> `Failure of bytes
     | `IO of [> `Read_file_exn of bytes * exn ]
     | `Shell of
          bytes *
          [> `Exited of int
          | `Exn of exn
          | `Signaled of int
          | `Stopped of int ] ])
      Unix_io.t

  (**/**)

end

type t
(** The container for mnoitored server-side processes. *)

val load : unit -> (t, 'a) Unix_io.t
(** Create a new process-holder. *)

val answer_message: t ->
  host_io:Host_io.t ->
  Ketrew_pure.Protocol.Process_sub_protocol.up ->
  (Ketrew_pure.Protocol.Process_sub_protocol.down, 'a) Unix_io.t
(** Answer a request from the sub-protocol of the process-holder. *)
