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

(** Definition of a host; a place to run commands or handle files. *)
open Ketrew_pervasives

(** Definitions specific to “SSH” hosts (see {!connection}). *)
module Ssh : sig

  type t = {
    address: string;
    port: int option;
    user: string option;
    add_ssh_options: string list;
  } [@@deriving yojson]
  (** The type of SSH-based hosts. *)

  val configure_ssh_batch_option :
    [ `Custom of string | `Dropbear | `Openssh ] -> unit
    (** Configure global “Batch option”,
      (call [ssh] without password/question):
      {ul
        {li for OpenSSH, it is ["-oBatchMode=yes"],}
        {li for DropBear, it is ["-s"].  }
      }*)

  val ssh_batch_option: t -> string
  (** Get the right option for the SSH client, for now this does not
      actually depend on the Host. *)

end


type default_shell [@@deriving yojson]
(** Specification of the default shell of a Host. *)


type t [@@deriving yojson]
(** Host container.

  A host is the current machine, or an SSH-accessed distant host.
  It may have a plaground: a directory where Ketrew can create runtime-files.
  It keeps track of a default-shell to use (the “default” [default_shell], is
  [("sh", "-c")]).
    
*)

val default_shell :
  ?binary:string ->
  ?options:string list ->
  ?command_option:string ->
  string ->
  default_shell
(** Use
  [default_shell ~binary:"/bin/sh" ~options:["-l"; "--something"; "blah" ]
      ~command_option:"-c" "sh"]
  to define a default-shell calling ["sh -l --something blah -c <command>"].
*)

val localhost:
  ?execution_timeout:Time.t ->
  ?default_shell:default_shell ->
  ?playground:Ketrew_path.t ->
  ?name:string -> unit -> t
(** The host ["localhost"] (i.e. not over SSH).  *)

val tmp_on_localhost: t
(** The host ["localhost"], with ["/tmp"] as [playground]. *)

val ssh :
  ?execution_timeout:Time.t ->
  ?add_ssh_options:string list ->
  ?default_shell:default_shell ->
  ?playground:Ketrew_path.t ->
  ?port:int -> ?user:string -> ?name:string -> string -> t
(** Create an SSH host. *)

val shell_of_default_shell: t -> string -> string list

val of_uri: Uri.t -> t
(** Get a [Host.t] from an URI (library {{:https://github.com/mirage/ocaml-uri}ocaml-uri});
    the “path” part of the URI is the playground.

    Optional arguments can be added to the URL:

    - a ["shell"] argument defines the [default_shell].
    - a list of ["ssh-option"] parameters can be added for SSH-based host, they
    add options to SSH/SCP calls.
    - a ["timeout"] value can be defined (in seconds) for all system/SSH calls.

    For example
    [of_string "//user@SomeHost:42/tmp/pg?shell=bash,-l,--init-file,bouh,-c&timeout=42"]
    will be like using 
    {[
      ssh ~default_shell:(default_shell  "bash"
                            ~command_name ~options:["-l"; "--init-file"; "bouh"]
                            ~command_option:"-c")
        ~execution_timeout:42.
        ~port:42 ~user:"user" "SomeHost"]}

*)

val of_string: string -> t
(** Parse an {{:http://www.ietf.org/rfc/rfc3986.txt}RFC-3986}-compliant
  string into a host, see {!of_uri}. *)

val to_uri: t -> Uri.t
(** Convert a [Host.t] to an URI representing it. *)

val to_string_hum : t -> string
(** Get a display-friendly string for the host. *)

val log : t -> Log.t
(** Get a {!Log.t} document. *)

val execution_timeout: t -> Time.t option
(** The execution timeout configured for the host. *)

val connection: t -> [ `Localhost | `Ssh of Ssh.t ]
val playground: t -> Ketrew_path.t option
