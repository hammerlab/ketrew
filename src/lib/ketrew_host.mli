open Ketrew_pervasives

module Ssh : sig
  val configure_ssh_batch_option :
    [ `Custom of string | `Dropbear | `Openssh ] -> unit
  (* type t = { address : string; port : int option; user : string option; } *)
  type t
    (*
  val do_ssh : t -> string -> string list
  val scp_push : t -> src:string list -> dest:string -> string list
  val scp_pull : t -> src:string list -> dest:string -> string list
  *)
end

type connection = [ `Localhost | `Ssh of Ssh.t ]

type t

val create :
  ?connection:connection ->
  ?playground:Ketrew_path.absolute_directory -> string -> t

val localhost : t

val ssh :
  ?playground:Ketrew_path.absolute_directory ->
  ?port:int -> ?user:string -> ?name:string -> string -> t

val to_string : t -> string

val fail_exec :
  t ->
  ?out:string ->
  ?err:string ->
  'a ->
  ('b, [> `Host of [> `Execution of string * string * string * 'a ] ])
  Deferred_result.t

val get_shell_command_output :
  t ->
  string ->
  (string * string,
   [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t

val get_shell_command_return_value :
  t ->
  string ->
  (int, [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t

val run_shell_command :
  t ->
  string ->
  (unit, [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t

val do_files_exist :
  t ->
  < kind : 'a; relativity : 'b > Ketrew_path.t list ->
  (bool, [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t

val get_fresh_playground :
  t -> Ketrew_path.absolute_directory option

val ensure_directory :
  t ->
  path:<kind: Ketrew_path.directory; relativity: 'a> Ketrew_path.t ->
  (unit,
   [> `Host of [> `Execution of string * string * string * string ]
    | `System of
        [> `Make_directory of string ] *
        [> `Exn of exn | `Wrong_access_rights of int ] ])
  Deferred_result.t

val put_file :
  t ->
  path:'a Ketrew_path.t ->
  content:string ->
  (unit,
   [> `Host of [> `Execution of string * string * string * string ]
    | `IO of [> `Write_file_exn of Ketrew_pervasives.IO.path * exn ] ])
  Deferred_result.t

val get_file :
  t ->
  path:'a Ketrew_path.t ->
  (string,
   [> `Cannot_read_file of string * string
    | `IO of [> `Read_file_exn of Ketrew_pervasives.IO.path * exn ] ])
  Deferred_result.t

