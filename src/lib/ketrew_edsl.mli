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

(** Easy interface to the library {b for end users}. *)
(**

  This is a more stable EDSL/API for end-users to make workflows and deal with
  the system.

  Many functions may raise exceptions when called improperly, but this
  should happen while building the workflow, not after it starts running. *)


(** {3 Hosts} *)

module Host: sig

  type t = Ketrew_host.t
  (** Alias for the host type. *)

  val parse : string -> t
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

  val tmp_on_localhost: t

  val ssh: 
    ?add_ssh_options:string list ->
    ?playground:string ->
    ?port:int -> ?user:string -> ?name:string -> string -> t

  val cmdliner_term :
    ?doc:string -> 
    [ `Required of int | `Flag of string list ] ->
    t Cmdliner.Term.t
    (** Cmdliner term which creates a host argument or flag.
        [`Required n] will be an anonymous argument at position [n]; 
        [`Flag ["option-name"; "O"]] will create an optional
        flag ["--option-name"] (aliased to ["-O"]) whose default value is
        the host ["/tmp/"] (i.e. Localhost with ["/tmp"] as “playground”).
    *)
end

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


end

(** {3 Conditions } *)


module Condition: sig

  type t = Ketrew_target.Condition.t

  val (&&): t -> t -> t
  val chain_and: t list -> t
  val never : t
  val program: ?returns:int -> ?host:Ketrew_host.t -> Program.t -> t

end

(** {3 Artifacts} *)

(** Artifacts are things to be built (they may already exist), most often
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

    method metadata: [ `String of string ] option
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
    method depends_on: user_target list
    method on_failure_activate: user_target list
    method on_success_activate: user_target list
    (**/**)
  end

val target :
  ?active:bool ->
  ?depends_on:user_target list ->
  ?make:Ketrew_target.Build_process.t ->
  ?done_when:Ketrew_target.Condition.t ->
  ?metadata:[ `String of string ] ->
  ?product:user_artifact ->
  ?equivalence:Ketrew_target.Equivalence.t ->
  ?on_failure_activate:user_target list ->
  ?on_success_activate:user_target list ->
  ?tags: string list ->
  string -> user_target
(** Construct a new target, the node of a workflow graph. The main
    argument (the [string]) is its name, then all optional arguments mean:

  - [?active]: whether this target should be started by the engine or
    wait to be ativated by a dependency machanism or alike (default:
    [false]). Usual workflows should not set this value since
    the function {!Ketrew.Cliean.submit} will activate the toplevel
    target automatically.
  - [?depends_on]: list of dependencies of the target.
  - [?make]: the build-process used to “build” the target; where the
    computation happens.
  - [?done_when]: the condition that the target ensures (checked
    before potentially running and after running).
  - [?metadata]: arbitrary metadata to attach to the target.
  - [?product]: the {!user_artifact} that the target embeds (returned
    by the [#product] method of the target).
  - [?equivalence]: how to tell if two targets are equivalent (and
    then will be merged by the engine). The default is
    [`Same_active_condition] which means that if two targets have the
    same non-[None] [?done_when] argument they will be considered
    equivalent (i.e. they try to “ensure the same condition”).
  - [?on_failure_activate]: targets to activate when this target fails.
  - [?on_success_activate]: targets to activate when this target succeeds.
  - [?tags]: arbitrary tags to add to the target (e.g. for
    search/filter in the UI)

*)

val file_target:
  ?depends_on:user_target list ->
  ?make:Ketrew_target.Build_process.t ->
  ?metadata:[ `String of string ] ->
  ?name:string ->
  ?host:Host.t ->
  ?equivalence:Ketrew_target.Equivalence.t ->
  ?on_failure_activate:user_target list ->
  ?on_success_activate:user_target list ->
  ?tags: string list ->
  string ->
  user_target
(** Create a file {!user_artifact} and the {!user_target} that produces it.

    The [?product] of the target will be the file on the given [?host]
    (default: localhost using ["/tmp"]).
    
    The [?done_when] condition will be the existence of that file.
    
    This can be seen as a classical [make]-like file-producing target,
    but on any arbitrary host.
*)

val daemonize :
  ?starting_timeout:float ->
  ?call_script:(string -> string list) ->
  ?using:[`Nohup_setsid | `Python_daemon] ->
  ?host:Host.t ->
  ?no_log_is_ok: bool ->
  Program.t ->
  Ketrew_target.Build_process.t
(** Create a “daemonize” build process:

    - [?host]: the [Host.t] on which the program is to be run.
    - [?starting_timeout]: how long to wait before considering that a
      script failed to start (default: [5.] seconds).
    - [?call_script]: function creating a [Unix.exec]-style command
      given a shell script path 
      (default: [(fun script -> ["bash"; script])]).
    - [?using]: which method to use when damonizing on the [host]
    (see {!Ketrew_daemonize} for more details).
    - [?no_log_is_ok]: consider that if the script run does not
      produce a log file, the process still has succeeded (the default
      and most common is [false], this can be useful for example when
      the [Program.t] or [call_script] do something special over the
      network).

*)

val lsf :
  ?host:Host.t ->
  ?queue:string ->
  ?name:string ->
  ?wall_limit:string ->
  ?processors:[ `Min of int | `Min_max of int * int ] ->
  ?project:string ->
  Program.t -> Ketrew_target.Build_process.t
(** Create an “LSF” build process. *)

val pbs :
  ?host:Ketrew_host.t ->
  ?queue:string ->
  ?name:string ->
  ?wall_limit:[ `Hours of float ] ->
  ?processors:int ->
  ?email_user:[ `Always of string | `Never ] ->
  ?shell:string ->
  Program.t ->
  [> `Long_running of string * string ]
(** Create a “PSB” build process. *)


val yarn_application :
  ?host:Ketrew_host.t ->
  ?daemonize_using:[ `Nohup_setsid | `Python_daemon ] ->
  ?daemon_start_timeout:float ->
  Ketrew_program.t -> [> `Long_running of string * string ]
(** Create a build process that requests resources from Yarn, the
    command must be an application in the Yarn sense (i.e.
    a program that is going to contact Yarn by itself to request
    containers):

    - [?host]: the “login” node of the Yarn cluster (default: localhost).
    - [?daemonize_using]: how to daemonize the process that calls and
      waits-for the application-manager (default: [`Python_daemon]).
    - [?daemon_start_timeout]: the timeout for the daemon.

*)

val yarn_distributed_shell :
  ?host:Ketrew_host.t ->
  ?daemonize_using:[ `Nohup_setsid | `Python_daemon ] ->
  ?daemon_start_timeout:float ->
  ?hadoop_bin:string ->
  ?distributed_shell_shell_jar:string ->
  container_memory:[ `GB of int | `MB of int | `Raw of string ] ->
  timeout:[ `Raw of string | `Seconds of int ] ->
  application_name:string ->
  Ketrew_program.t -> [> `Long_running of string * string ]
(** Create a build process that will use Hadoop's `DistributedShell`  class
    to request a container to run the given arbitrary program.

    - [?host]: the “login” node of the Yarn cluster (default: localhost).
    - [?daemonize_using]: how to daemonize the process that calls and
      waits-for the application-manager (default: [`Python_daemon]).
    - [?daemon_start_timeout]: the timeout for the daemon.
    - [hadoop_bin]: the [hdaoop] executable (default: ["hadoop"]).
    - [distributed_shell_shell_jar]:
    path to the Jar file containing the
    [org.apache.hadoop.yarn.applications.distributedshell.Client] class
    (default: ["/opt/cloudera/parcels/CDH/lib/hadoop-yarn/hadoop-yarn-applications-distributedshell.jar"]
    which seems to be the default installation path when using Cloudera-manager).
    - [container_memory]: how much memory to request from Yarn for the container
    ([`GB 42] for 42 GB; [`Raw some_string] to pass directly [some_string]
    to the option ["-container_memory"] of [distributedshell.Cllient]).
    - [timeout]: the “whole application” timeout
    ([`Seconds (24 * 60 * 60)] for about a day, [`Raw some_string] to
    pass directly [some_string] to the option ["-timeout"] of
    [distributedshell.Cllient]).
    - [application_name]: name of the application for Yarn (it is not
      sanitized by Ketrew and, at least with some configurations, Yarn
      can fail if this string contains spaces for example).

*)


(** {2 Utilities } *)

val to_display_string :
  ?ansi_colors:bool ->
  ?indentation:int ->
  user_target ->
  string
(** Build a display-friendly string summarizing the workflow. *)
