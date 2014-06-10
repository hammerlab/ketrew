(** Definition of a host; a place to run commands or handle files. *)
open Ketrew_pervasives

(** Definitions specific to “SSH” hosts (see {!connection}). *)
module Ssh : sig

  val configure_ssh_batch_option :
    [ `Custom of string | `Dropbear | `Openssh ] -> unit
    (** Configure global “Batch option”,
      (call [ssh] without password/question):
      {ul
        {li for OpenSSH, it is ["-oBatchMode=yes"],}
        {li for DropBear, it ["-s"].  }
      }*)

  type t
   (** Container of SSH connection parameters. *)

end

type connection = [ `Localhost | `Ssh of Ssh.t ]
(** Specification of connection methods. *)

type t = Ketrew_gen_base_v0_t.host
(** Host container. *)

val create :
  ?connection:connection ->
  ?playground:Ketrew_path.absolute_directory -> string -> t
(** Create a [Host.t], the playground is an optional directory on the host,
    where {i Ketrew} may put temporary scripts, logs, pid-files, etc. *)

val localhost : t
(** The host [localhost] is [`Localhost] with ["/tmp/"] as playground. *)

val ssh :
  ?playground:Ketrew_path.absolute_directory ->
  ?port:int -> ?user:string -> ?name:string -> string -> t
(** Create an SSH host. *)

val of_string: string -> t
(** Parse an {{:http://www.ietf.org/rfc/rfc3986.txt}RFC-3986}-compliant
  string into a host, see {!of_uri}. *)

val of_uri: Uri.t -> t
(** Get a [Host.t] from an URI (library {{:https://github.com/mirage/ocaml-uri}ocaml-uri});
  the “path” part of the URI is the playground. *)

val to_string_hum : t -> string
(** Get a display-friendly string for the host (“name”, or hostname). *)

val get_shell_command_output :
  t ->
  string ->
  (string * string,
   [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t
(** Run a shell command on the host, and return its [(stdout, stderr)] pair
    (succeeds {i iff } the exit status is [0]). *)

val get_shell_command_return_value :
  t ->
  string ->
  (int, [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t
(** Run a shell command on the host, and return its exit status value. *)

val run_shell_command :
  t ->
  string ->
  (unit, [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t
(** Run a shell command on the host (succeeds {i iff } the exit status is [0]).
*)

val do_files_exist :
  t ->
  < kind : 'a; relativity : 'b > Ketrew_path.t list ->
  (bool, [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t
(** Check existence of a list of files/directories. *)

val get_fresh_playground :
  t -> Ketrew_path.absolute_directory option
(** Get a new subdirectory in the host's playground *)

val ensure_directory :
  t ->
  path:<kind: Ketrew_path.directory; relativity: 'a> Ketrew_path.t ->
  (unit,
   [> `Host of [> `Execution of string * string * string * string ]
    | `System of
        [> `Make_directory of string ] *
        [> `Exn of exn | `Wrong_access_rights of int ] ])
  Deferred_result.t
  (** Make sure the directory [path] exists on the host. *)

val put_file :
  t ->
  path:<kind: Ketrew_path.file; ..>  Ketrew_path.t ->
  content:string ->
  (unit,
   [> `Host of [> `Execution of string * string * string * string ]
    | `IO of [> `Write_file_exn of Ketrew_pervasives.IO.path * exn ] ])
  Deferred_result.t
(** Write a file on the host at [path] containing [contents]. *)

val get_file :
  t ->
  path:<kind: Ketrew_path.file; ..> Ketrew_path.t ->
  (string,
   [> `Cannot_read_file of string * string
    | `IO of [> `Read_file_exn of Ketrew_pervasives.IO.path * exn ] ])
  Deferred_result.t
(** Read the file from the host at [path]. *)

