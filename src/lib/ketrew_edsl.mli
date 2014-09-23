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

(** Easy interface to the library {b for end users}. *)
(**

  This is a more stable EDSL/API for end-users to make workflows and deal with
  the system.

  Many functions may raise exceptions when called on improperly, but this
  should happen while building the workflow, not after it starts running. *)


(** {3 Hosts} *)

type host = Ketrew_host.t
(** Alias for the host type. *)

val parse_host : string -> host
(** Parse an URI string into a host.

    For example:
    ["ssh://user@SomeHost:42/tmp/pg?shell=bash,-l,--init-file,bouh,-c&timeout=42&ssh-option=-K"]

    - ["ssh:"] means to connect with SSH (if a hostname is defined this is the
    default and only way).
    - ["user"] is the user to connect as.
    - ["SomeHost"] is the hostname, if the “host-connection” part of the URI is
    not provided, “localhost” will be assumed (and SSH won't be used).
    - ["42"] is the port.
    - ["/tmp/pg"] is the “playground”; a directory where the Ketrew-engine will
    create temporary and monitoring files.
    - ["shell=bash,-l,--init-file,bouh,-c"] the option [shell] define the
    shell, and the options, to use on the host.
    - ["timeout=42.5"] is the execution timeout, an optional float setting the
    maximal duration Ketrew will wait for SSH commands to return.
    - ["ssh-option=-K"] are options to pass to the SSH client.

    See also {!Ketrew_host.of_uri}. *)

val host_cmdliner_term :
  ?doc:string -> 
  [ `Required of int | `Flag of string list ] ->
  Ketrew_host.t Cmdliner.Term.t
(** Cmdliner term which creates a host argument or flag.
    [`Required n] will be an anonymous argument at position [n]; 
    [`Flag ["option-name"; "O"]] will create an optional
    flag ["--option-name"] (aliased to ["-O"]) whose default value is
    the host ["/tmp/"] (i.e. Localhost with ["/tmp"] as “playground”).
    *)

(** {3 Build Programs} *)

(** Build “things to run”, i.e. shell scripts on steroids. *)
module Program: sig

  type t = Ketrew_program.t
  (** Something to run {i is} a {!Ketrew_program.t}. *)

  val sh: string -> t
  (** Create a program that runs a shell command. *)

  val shf: ('a, unit, string, t) format4 -> 'a
  (** Printf-like function to create shell commands. *)

  val exec: string list -> t
  (** Create a program that run in [Unix.exec] mode (i.e. does not need shell
      escaping). *)

  val (&&): t -> t -> t
  (** [a && b] is a program than runs [a] then [b] iff [a] succeeded. *)

  val chain: t list -> t
  (** Chain a list of programs like with [&&]. *)

  val copy_files :
    source:Ketrew_host.t * string list ->
    destination:Ketrew_host.t * string ->
    f:(?host:Ketrew_host.t -> t -> 'a) -> 'a
    (** The call 
      [copy_files ~source:(host, files) ~dest:(dest_host, dest_path) ~f]
      calls [f] with a program and (potential) host to run a copy 
      (involving ["cp"] or ["scp"] depending on the source and destination
      hosts.

      If both source and destination are SSH-based hosts, [copy_files]
      will try its best, but it's difficult to ensure the correctness
      of such a command (Ketrew does not know which kind of ssh client is
      installed on the source host, and the destination host could not be
      reachable with the same parameters from there).
    *)

end

(** {3 Artifacts} *)

(** Artifacts are things to be built, or already existing, most often
    file-tree-locations on a given [host] (see also {!Ketrew_artifact.t}).
*)
class type user_artifact = object

  method path : string
  (** Return the path of the artifact if the artifact is a volume containing
      a single file or directory. *)

  method exists : Ketrew_target.Condition.t
  (** Get “exists” condition (for the [~done_when] argument of {!target}. *)

  method is_bigger_than: int -> Ketrew_target.Condition.t
  (** Get the “is bigger than <size>” condition. *)
end

val file: ?host:Ketrew_host.t -> string -> user_artifact
(** Create a volume containing one file. *)

val unit : user_artifact
(** The artifact that is “never done” (i.e. the target associated will always
    be (re-)run if activated). *)

(** {3 Targets} *)

(** Targets are the nodes in the workflow arborescence (see also
    {!Ketrew_target.t}). *)
class type user_target =
  object

    method name : string
    (** Get the name of the target *)

    method metadata: Ketrew_artifact.Value.t
    (** The metadata that has been set for the target ({i work-in-progress}). *)

    method product: user_artifact
    (** The user-artifact produced by the target, if known (raises exception if
        unknown). *)

    (**/**)
    method activate : unit
    (** Activate the target. *)
    method is_active: bool
    method id: Ketrew_pervasives.Unique_id.t
    method render: Ketrew_target.t
    method dependencies: user_target list
    method if_fails_activate: user_target list
    (**/**)
  end

val target :
  ?active:bool ->
  ?dependencies:user_target list ->
  ?make:Ketrew_target.Build_process.t ->
  ?done_when:Ketrew_target.Condition.t ->
  ?metadata:Ketrew_artifact.Value.t ->
  ?product:user_artifact ->
  ?equivalence:Ketrew_target.Equivalence.t ->
  ?if_fails_activate:user_target list ->
  ?tags: string list ->
  string -> user_target
(** Create a new target. *)

val file_target:
  ?dependencies:user_target list ->
  ?make:Ketrew_target.Build_process.t ->
  ?metadata:Ketrew_artifact.Value.t ->
  ?name:string ->
  ?host:host ->
  ?equivalence:Ketrew_target.Equivalence.t ->
  ?if_fails_activate:user_target list ->
  ?tags: string list ->
  string ->
  user_target
(** Create a file {!user_artifact} and the {!user_target} that produces it. *)

val daemonize :
  ?starting_timeout:float ->
  ?using:[`Nohup_setsid | `Python_daemon] ->
  ?host:Ketrew_host.t ->
  Program.t ->
  Ketrew_target.Build_process.t
(** Create a “daemonize” build process. *)

val direct_execution :
  ?host:Ketrew_host.t -> Program.t -> Ketrew_target.Build_process.t
(** Create a direct process (not “long-running”). *)

val direct_shell_command :
  ?host:Ketrew_host.t -> string -> Ketrew_target.Build_process.t
(** Shortcut for [direct_execution ?host Program.(sh cmd)]. *)

val lsf :
  ?host:Ketrew_host.t ->
  ?queue:string ->
  ?name:string ->
  ?wall_limit:string ->
  ?processors:[ `Min of int | `Min_max of int * int ] ->
  Program.t -> Ketrew_target.Build_process.t
(** Create an “LSF” build process. *)

(** {3 Workflows} *)

val run:
  ?override_configuration:Ketrew_configuration.t ->
  user_target ->
  unit
(** Submit and activate a [user_target] (the next time Ketrew runs a step, the
    target will be started/run (all the graph of dependencies and fallbacks is
    submitted at once). *)


