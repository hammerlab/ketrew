(**************************************************************************)
(*  Copyright 2014, Sebastien Mondet <seb@mondet.org>                     *)
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

  type t = Ketrew_gen_base_v0.Ssh_host.t
  (** The type of SSH-based hosts. *)

  val configure_ssh_batch_option :
    [ `Custom of string | `Dropbear | `Openssh ] -> unit
    (** Configure global “Batch option”,
      (call [ssh] without password/question):
      {ul
        {li for OpenSSH, it is ["-oBatchMode=yes"],}
        {li for DropBear, it is ["-s"].  }
      }*)

  val scp_push: t -> src:string list -> dest:string -> string list
  (** Generate an SCP command for the given host with the destination
      directory or file path. *)

  val scp_pull: t -> src:string list -> dest:string -> string list
  (** Generate an SCP command for the given host as source. *)

end


type default_shell
(** Specification of the default shell of a Host. *)

type t = Ketrew_gen_base_v0.Host.t
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
  Ketrew_gen_base_v0.Default_shell.t
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


module Error: sig

  type 'a execution = 'a constraint 'a = [>
    | `Unix_exec of string
    | `Execution of
        <host : string; stdout: string option; stderr: string option; message: string>
    | `Ssh_failure of
        [> `Wrong_log of string
        | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string 
    | `System of [> `Sleep of float ] * [> `Exn of exn ]
    | `Timeout of float
  ]

  type 'a non_zero_execution = 'a constraint 'a = 
    [> `Non_zero of (string * int) ] execution

  val classify :
    [ `Execution of
        < host : string; message : string; stderr : string option;
          stdout : string option >
    | `Non_zero of string * int
    | `System of [ `Sleep of float ] * [ `Exn of exn ]
    | `Timeout of float
    | `Ssh_failure of
        [> `Wrong_log of string
        | `Wrong_status of Ketrew_unix_process.Exit_code.t ] *
        string
    | `Unix_exec of string ] ->
    [ `Execution | `Ssh | `Unix ]
  (**
     Get a glance at the gravity of the situation: {ul
     {li [`Unix]: a function of the kind {!Unix.exec} failed.}
     {li [`Ssh]: SSH failed to run something but it does not mean that the
     actual command is wrong.}
     {li [`Execution]: SSH/[Unix] succeeded but the command failed.}
     } *)

  val log :
    [< `Unix_exec of string
    | `Non_zero of (string * int)
    | `System of [< `Sleep of float ] * [< `Exn of exn ]
    | `Timeout of float
    | `Execution of
         < host : string; message : string; stderr : string option;
           stdout : string option; .. >
    | `Ssh_failure of
         [< `Wrong_log of string
         | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string ] ->
    Log.t

end


val default_timeout_upper_bound: float ref
(** Default (upper bound) of the `?timeout` arguments. *)

type timeout = [ 
  | `Host_default
  | `None
  | `Seconds of float
  | `At_most_seconds of float
]
(** Timeout specification for execution functions below.
    
    - [`Host_default] → use the [excution_timeout] value of the host.     
    - [`None] → force no timeout even if the host has a [execution_timeout].
    - [`Seconds f] → use [f] seconds as timeout.
    - [`At_most_seconds f] -> use [f] seconds, unless the host has a smaller
    [execution_timeout] field.

    The default value is [`At_most_seconds !default_timeout_upper_bound].

*)

val execute: ?timeout:timeout -> t -> string list ->
  (<stdout: string; stderr: string; exited: int>,
   [> `Host of _ Error.execution ]) Deferred_result.t
(** Generic execution which tries to behave like [Unix.execv] even
    on top of SSH. *)

type shell = string -> string list
(** A “shell” is a function that takes a command and returns, and
     execv-style string list; the default for each host
     is ["sh"; "-c"; cmd] *)

val shell_sh: sh:string -> shell
(** Call sh-style commands using the command argument (e.g. [shell_sh "/bin/sh"]
   for a known path or command). *)

val get_shell_command_output :
  ?timeout:timeout ->
  ?with_shell:shell ->
  t ->
  string ->
  (string * string, [> `Host of  _ Error.non_zero_execution]) Deferred_result.t
(** Run a shell command on the host, and return its [(stdout, stderr)] pair
    (succeeds {i iff } the exit status is [0]). *)

val get_shell_command_return_value :
  ?timeout:timeout ->
  ?with_shell:shell ->
  t ->
  string ->
  (int, [> `Host of _ Error.execution ]) Deferred_result.t
(** Run a shell command on the host, and return its exit status value. *)

val run_shell_command :
  ?timeout:timeout ->
  ?with_shell:shell ->
  t ->
  string ->
  (unit, [> `Host of  _ Error.non_zero_execution])  Deferred_result.t
(** Run a shell command on the host (succeeds {i iff } the exit status is [0]).
*)

val do_files_exist :
  ?timeout:timeout ->
  ?with_shell:shell ->
  t ->
  Ketrew_path.t list ->
  (bool, [> `Host of _ Error.execution ])
  Deferred_result.t
(** Check existence of a list of files/directories. *)

val get_fresh_playground :
  t -> Ketrew_path.t option
(** Get a new subdirectory in the host's playground *)

val ensure_directory :
  ?timeout:timeout ->
  ?with_shell:shell ->
  t ->
  path:Ketrew_path.t ->
  (unit, [> `Host of _ Error.non_zero_execution ]) Deferred_result.t
(** Make sure the directory [path] exists on the host. *)

val put_file :
  ?timeout:timeout ->
  t ->
  path: Ketrew_path.t ->
  content:string ->
  (unit,
   [> `Host of _ Error.execution
    | `IO of [> `Write_file_exn of Ketrew_pervasives.IO.path * exn ] ])
  Deferred_result.t
(** Write a file on the host at [path] containing [contents]. *)

val get_file :
  ?timeout:timeout ->
  t ->
  path:Ketrew_path.t ->
  (string,
   [> `Cannot_read_file of string * string
    | `Timeout of Time.t ])
  Deferred_result.t
(** Read the file from the host at [path]. *)

val grab_file_or_log:
  ?timeout:timeout ->
  t -> 
  Ketrew_path.t ->
  (string, Log.t) Deferred_result.t
(** Weakly typed version of {!get_file}, it fails with a {!Log.t}
    (for use in “long-running” plugins).  *)
