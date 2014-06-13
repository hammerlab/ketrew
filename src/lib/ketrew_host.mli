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
        {li for DropBear, it is ["-s"].  }
      }*)

end


type default_shell
(** Specification of the default shell of a Host. *)

type t = Ketrew_gen_base_v0_t.host
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
  Ketrew_gen_base_v0_t.default_shell
(** Use
  [default_shell ~binary:"/bin/sh" ~options:["-l"; "--something"; "blah" ]
      ~command_option:"-c" "sh"]
  to define a default-shell calling ["sh -l --something blah -c <command>"].
*)

val localhost:
  ?default_shell:default_shell ->
  ?playground:Ketrew_path.absolute_directory ->
  ?name:string -> unit -> t
(** The host ["localhost"] (i.e. not over SSH).  *)

val tmp_on_localhost: t
(** The host ["localhost"], with ["/tmp"] as [playground]. *)

val ssh :
  ?default_shell:default_shell ->
  ?playground:Ketrew_path.absolute_directory ->
  ?port:int -> ?user:string -> ?name:string -> string -> t
(** Create an SSH host. *)

val of_uri: Uri.t -> t
(** Get a [Host.t] from an URI (library {{:https://github.com/mirage/ocaml-uri}ocaml-uri});
    the “path” part of the URI is the playground.
    If present a ["shell"] argument defines the [default_shell].
    example
    [of_string "//user@SomeHost:42/tmp/pg?shell=bash,-l,--init-file,bouh,-c"]
    will be [ssh ~default_shell:(default_shell  "bash"
    ~command_name ~options:["-l"; "--init-file"; "bouh"] ~command_option:"-c")
     ~port:42 ~user:"user" "SomeHost"]

*)

val of_string: string -> t
(** Parse an {{:http://www.ietf.org/rfc/rfc3986.txt}RFC-3986}-compliant
  string into a host, see {!of_uri}. *)

val to_string_hum : t -> string
(** Get a display-friendly string for the host (“name”, or hostname). *)

module Error: sig

  type 'a execution = 'a constraint 'a =
    [> `Exec_failure of string
    | `Execution of
         <host : string; stdout: string option; 
       stderr: string option; message: string>
                                       | `Ssh_failure of
         [> `Wrong_log of string
         | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string ]

  type 'a non_zero_execution = 'a constraint 'a = 
    [> `Non_zero of (string * int) ] execution

  val log :
    [< `Exec_failure of string
    | `Non_zero of (string * int)
    | `Execution of
         < host : string; message : string; stderr : string option;
           stdout : string option; .. >
    | `Ssh_failure of
         [< `Wrong_log of string
         | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string ] ->
    Log.t

end

val execute: t -> string list ->
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
  ?with_shell:shell ->
  t ->
  string ->
  (string * string, [> `Host of  _ Error.non_zero_execution]) Deferred_result.t
(** Run a shell command on the host, and return its [(stdout, stderr)] pair
    (succeeds {i iff } the exit status is [0]). *)

val get_shell_command_return_value :
  ?with_shell:shell ->
  t ->
  string ->
  (int, [> `Host of _ Error.execution ]) Deferred_result.t
(** Run a shell command on the host, and return its exit status value. *)

val run_shell_command :
  ?with_shell:shell ->
  t ->
  string ->
  (unit, [> `Host of  _ Error.non_zero_execution])  Deferred_result.t
(** Run a shell command on the host (succeeds {i iff } the exit status is [0]).
*)

val do_files_exist :
  ?with_shell:shell ->
  t ->
  < kind : 'a; relativity : 'b > Ketrew_path.t list ->
  (bool, [> `Host of _ Error.execution ])
  Deferred_result.t
(** Check existence of a list of files/directories. *)

val get_fresh_playground :
  t -> Ketrew_path.absolute_directory option
(** Get a new subdirectory in the host's playground *)

val ensure_directory :
  ?with_shell:shell ->
  t ->
  path:<kind: Ketrew_path.directory; relativity: 'a> Ketrew_path.t ->
  (unit, [> `Host of _ Error.non_zero_execution ]) Deferred_result.t
(** Make sure the directory [path] exists on the host. *)

val put_file :
  t ->
  path:<kind: Ketrew_path.file; ..>  Ketrew_path.t ->
  content:string ->
  (unit,
   [> `Host of _ Error.execution
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

