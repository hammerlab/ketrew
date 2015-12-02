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

open Ketrew_pure

(** {3 Hosts} *)

module Host: sig

  type t = Ketrew_pure.Host.t
  (** Alias for the host type. *)

  val parse : string -> t
  (** Parse a
      {{:https://en.wikipedia.org/wiki/Uniform_Resource_Identifier}URI}
      string into a Ketrew-host.

      The “scheme” and “host” part of the URI define the connection type:

      - {b SSH Hosts}: use ["ssh:"] as the scheme means (if a hostname is
        defined but no scheme, SSH is the default).
        Then you can add a user, a hostname, and a port number.
        You can aslo add options to pass to the SSH client through the
        URI's query parameters ["ssh-option=-K"].
      - {b Named hosts}: use the scheme ["named://"] and the hostname
        as a {i name}. Named hosts are dynamic connections managed by the
        Ketrew server.
      - {b Engine's host}: if no scheme, and no host part are
        provided, Ketrew will create a host for the local-host (the
        host where the Engine is running).

      The “path” part of the URI defines the “playground”; a directory
      where the Ketrew-engine will create temporary and monitoring files.
      
      Other query options can be used to configure the host:
      
      - ["shell=<comma-separated-list-of-command-line-arguments"]
        defines the shell, with more command-line options, to use on
        the host.
      - ["timeout=<float>"] is the execution timeout, an optional
        float setting the maximal duration Ketrew will wait for SSH
        commands to return.

      Examples:

      - ["ssh://user@SomeHost:42/tmp/pg?shell=bash,-l,--init-file,bouh,-c&timeout=42&ssh-option=-K"]
        is an SSH host.
      - ["named://TheNameOfTheHost/tmp/ketrew-playground/"] is a named
        host: Ketrew will try to find its active connection called
        ["TheNameOfTheHost"] and use it at every call.
      - ["/tmp/KT?shell=ksh,-c"] is the engine's host, using ["tmp/KT"]
        as a playground, and ["ksh"] as a shell.

      See also {!Ketrew_pure.Host.of_uri}. *)

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

  type t = Ketrew_pure.Program.t
  (** Something to run {i is} a {!Program.t}. *)

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

  type t = Target.Condition.t

  val (&&): t -> t -> t
  val chain_and: t list -> t
  val never : t
  val program: ?returns:int -> ?host:Host.t -> Program.t -> t

end

(** {3 Workflow Nodes and Edges} *)

module Internal_representation : sig
  (* We expose this type because we want to preserve backwards
     compatibility. The function `Ketrew.Client.submit` has to expect an
     `EDSL.user_target` so to avoid code duplcation we mimic it with the
     “workflow_node” API. The ideal would be to have
     `Ketrew.Client.submit` take a list of `Target.t` values as argument
     and have `workflow_node#render` produce directly that list. 
  *)
  (**/**)
  type t =
    < activate : unit;
      add_tags : string list -> unit;
      id : Ketrew_pure.Internal_pervasives.Unique_id.t;
      depends_on : t list;
      on_failure_activate : t list;
      on_success_activate : t list;
      render : Ketrew_pure.Target.t;  >
    (**/**)
end

type 'a product = 'a
  constraint 'a = < is_done : Condition.t option ; .. >

(* The main building bloc of the worfklow graph: *)
type 'product workflow_node = <
  product : 'product product;
  render: Internal_representation.t;
>

type workflow_edge

val depends_on: 'any workflow_node -> workflow_edge
val on_success_activate: 'any workflow_node -> workflow_edge
val on_failure_activate: 'any workflow_node -> workflow_edge

val workflow_node:
  ?name:string ->
  ?active:bool ->
  ?make:Ketrew_pure.Target.Build_process.t ->
  ?done_when:Condition.t ->
  ?metadata:[ `String of string ] ->
  ?equivalence:Ketrew_pure.Target.Equivalence.t ->
  ?tags:string list ->
  ?edges:workflow_edge list ->
  'product product ->
  'product workflow_node

type unknown_product = < is_done : Condition.t option >

type not_already_done = < is_done : Condition.t option >
val without_product : not_already_done

type single_file = <
  exists: Ketrew_pure.Target.Condition.t;
  is_done: Ketrew_pure.Target.Condition.t option;
  path : string;
  is_bigger_than: int -> Ketrew_pure.Target.Condition.t;
>
val single_file: ?host:Host.t -> string -> single_file product


type list_of_files = <
  is_done: Ketrew_pure.Target.Condition.t option;
  paths : string list;
>
val list_of_files:
  ?host:Host.t ->
  string list -> list_of_files product

val forget_product:
  'any_product workflow_node ->
  unknown_product workflow_node 

(** {3 Legacy Deprecated API: Artifacts and Targets} *)

(** {4 Artifacts} *)

(** Artifacts are things to be built (they may already exist), most often
    file-tree-locations on a given [host] (see also {!Artifact.t}).
*)
class type user_artifact = object

  method path : string
  (** Return the path of the artifact if the artifact is a volume containing
      a single file or directory. *)

  method exists : Target.Condition.t
  (** Get “exists” condition (for the [~done_when] argument of {!target}. *)

  method is_bigger_than: int -> Target.Condition.t
  (** Get the “is bigger than <size>” condition. *)
end

val file: ?host:Host.t -> string -> user_artifact
(** Create a volume containing one file. *)

val unit : user_artifact
(** The artifact that is “never done” (i.e. the target associated will always
    be (re-)run if activated). *)

(** {4 Targets} *)

(** Targets are the nodes in the workflow arborescence (see also
    {!Target.t}). *)
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
    method id: Internal_pervasives.Unique_id.t
    method render: Target.t
    method depends_on: user_target list
    method on_failure_activate: user_target list
    method on_success_activate: user_target list
    method add_tags: string list -> unit
    (**/**)
  end

val target :
  ?active:bool ->
  ?depends_on:user_target list ->
  ?make:Target.Build_process.t ->
  ?done_when:Target.Condition.t ->
  ?metadata:[ `String of string ] ->
  ?product:user_artifact ->
  ?equivalence:Target.Equivalence.t ->
  ?on_failure_activate:user_target list ->
  ?on_success_activate:user_target list ->
  ?tags: string list ->
  string -> user_target
(** Construct a new target, the node of a workflow graph. The main
    argument (the [string]) is its name, then all optional arguments mean:

  - [?active]: whether this target should be started by the engine or
    wait to be ativated by another target (through [depends_on] or
    [on_{success,failure}_activate]) (default:
    [false], i.e., inactive). Usual workflows should not set this
    value since the function {!Ketrew.Client.submit} will activate the
    toplevel target automatically.
  - [?depends_on]: list of the dependencies of the target.
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
  ?make:Target.Build_process.t ->
  ?metadata:[ `String of string ] ->
  ?name:string ->
  ?host:Host.t ->
  ?equivalence:Target.Equivalence.t ->
  ?on_failure_activate:user_target list ->
  ?on_success_activate:user_target list ->
  ?tags: string list ->
  string ->
  user_target
(** Create a file {!user_artifact} and the {!user_target} that produces it.

    The [?product] of the target will be the file given as argument on
    the host given by the [?host] option (default: localhost using ["/tmp"]).
    
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
  Target.Build_process.t
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
  Program.t -> Target.Build_process.t
(** Create an “LSF” build process. *)

val pbs :
  ?host:Host.t ->
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
  ?host:Host.t ->
  ?daemonize_using:[ `Nohup_setsid | `Python_daemon ] ->
  ?daemon_start_timeout:float ->
  Program.t -> [> `Long_running of string * string ]
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
  ?host:Host.t ->
  ?daemonize_using:[ `Nohup_setsid | `Python_daemon ] ->
  ?daemon_start_timeout:float ->
  ?hadoop_bin:string ->
  ?distributed_shell_shell_jar:string ->
  ?container_vcores : int ->
  container_memory:[ `GB of int | `MB of int | `Raw of string ] ->
  timeout:[ `Raw of string | `Seconds of int ] ->
  application_name:string ->
  Program.t -> [> `Long_running of string * string ]
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
    - [container_vcores]: how many virtual cores to request (default [1]).
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


