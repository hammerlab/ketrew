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
  val program: ?returns:int -> ?host:Host.t -> Program.t -> t

  (** “Volumes” are arbitrary file-system structures i.e. file a and
      (sub-directories). *)
  module Volume: sig
    type t = Ketrew_pure.Target.Volume.t
    type structure = Ketrew_pure.Target.Volume.structure

    (** Create a volume from a [Host.t] and an absolute directory [~root]
        (function raises an exception if [root] is not absolute). *)
    val create :
      host:Host.t ->
      root: string ->
      structure -> t

    val file : string -> structure
    val dir : string -> structure list -> structure
  end

  (** Condition that ensures the all the structure of a [Volume.t] is present. *)
  val volume_exists: Volume.t -> t

  (* Condition that sums up the sizes of all the files in a volume and
     compares it to an integer (useful for example to ensure a program did not
     create empty files). *)
  val volume_size_greater_of_equal: Volume.t -> int -> t

end

(** {3 Build Processes } *)

module Build_process : sig
  type t = Target.Build_process.t
end

val daemonize :
  ?starting_timeout:float ->
  ?call_script:(string -> string list) ->
  ?using:[`Nohup_setsid | `Python_daemon] ->
  ?host:Host.t ->
  ?no_log_is_ok: bool ->
  Program.t ->
  Build_process.t
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
  ?request_memory: [ `GB of int | `MB of int ] ->
  ?raw_options: string list ->
  Program.t -> Build_process.t
(** Create an “LSF” build process. *)

val pbs :
  ?host:Host.t ->
  ?queue:string ->
  ?name:string ->
  ?wall_limit:[ `Hours of float ] ->
  ?processors:int ->
  ?email_user:[ `Always of string | `Never ] ->
  ?shell:string ->
  Program.t -> Build_process.t
(** Create a “PSB” build process. *)


val yarn_application :
  ?host:Host.t ->
  ?daemonize_using:[ `Nohup_setsid | `Python_daemon ] ->
  ?daemon_start_timeout:float ->
  Program.t -> Build_process.t
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
  Program.t -> Build_process.t
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
    < name : string;
      activate : unit;
      add_tags : string list -> unit;
      add_recursive_tags : string list -> unit;
      get_recursive_tags : string list;
      id : Ketrew_pure.Internal_pervasives.Unique_id.t;
      depends_on : t list;
      on_failure_activate : t list;
      on_success_activate : t list;
      render : Ketrew_pure.Target.t;  >
    (**/**)
end

type 'a product = 'a
  constraint 'a = < is_done : Condition.t option ; .. >
(** The type of the things produced by workflow nodes.
    A product is an object that has at least one method giving its
    {!Condition.t}. *)

type 'product workflow_node = <
  product : 'product product;
  render: Internal_representation.t;
>
(** The main building bloc of the worfklow graph is workflow node, it
    carries a “product” accessible with the [#product] method.

    The [#render] method is used internally by
    {!Ketrew.Client.submit_workflow}.
 *)

type workflow_edge
(** The edges of the graph ([?edges] argument of the {!workflow_node}
    function). *)

val depends_on: 'any workflow_node -> workflow_edge
(** Create a “dependency” edge, in other words, the node using the
    edges “depends on” the node given as argument. *)

val on_success_activate: 'any workflow_node -> workflow_edge
(** Create an edge to a node that will be activated after a node {i
    succeeds}. *)

val on_failure_activate: 'any workflow_node -> workflow_edge
(** Create an edge to a node that will be activated after a node {i
    fails}. *)

type ensures_option = [
  | `Nothing
  | `Is_verified of Condition.t
  | `Product_is_done
]
(** The specification of when a workflow-node is considered “ensured”; cf. the
    [?ensures] argument of {!workflow_node}. *)

val workflow_node:
  ?name:string ->
  ?active:bool ->
  ?make:Build_process.t ->
  ?ensures: ensures_option ->
  ?metadata:[ `String of string ] ->
  ?equivalence:Ketrew_pure.Target.Equivalence.t ->
  ?tags:string list ->
  ?edges:workflow_edge list ->
  'product_type product ->
  'product_type workflow_node
(** Create a workflow node:

    - [?name]: give a name to the node (visible in the UIs).
    - [?active]: whether this node should be started by the engine or
    wait to be ativated by another node (through an edge)
    The default is [false], i.e., inactive, normal workflows should not
    set this value since the function {!Ketrew.Client.submit_workflow}
    will activate the toplevel node automatically.
    - [?make]: the build-process used to “run/build” the node; where the
    computation happens.
    - [?ensures]: decides which condition the node has to ensure (checked
    before potentially running and after running): {ul
      {- [`Nothing]: no condition, i.e. only dependencies will be
      activated and when all succeed the node will run the [?make] and only this
      will be checked for success.}
      {- [`Is_verified cond]: use the condition [cond] (overrides the one
      potentially provided by the ['a product]).}
      {- [`Product_is_done]: use the condition of the ['a product]; this is the
      {b default}, if the product has no condition, this is equivalent to
      [`Nothing].}
    }
    - [?metadata]: arbitrary metadata to attach to the node.
    - [?equivalence]: how to tell if two nodes are equivalent (and
    then will be merged by the engine). The default is
    [`Same_active_condition] which means that if two nodes have the
    same non-[None] [?done_when] argument they will be considered
    equivalent (i.e. they try to “ensure the same condition”).
    - [?tags]: arbitrary tags to add to the node (e.g. for
    search/filter in the UI)
    - [?edges]: links to other nodes from the current node
    (list of edges created with the {!depends_on},
    {!on_failure_activate}, and {!on_success_activate} functions).
    - ['product_type product]: the main argument of the function is
    the artifact produced by the node (returned by the [#product]
    method of the node).
 *)

type not_already_done = < is_done : Condition.t option >
(** The type of “empty” products. *)

val without_product : not_already_done
(** Create an “empty” product, it won't require checking any condition,
    so the node carrying it (unless
    forced with the [?is_done] argument) will always run.

    This can be understood as a [".PHONY"] target in
    {{:https://www.gnu.org/software/make/manual/html_node/Phony-Targets.html}make}.
*)

type single_file = <
  exists: Ketrew_pure.Target.Condition.t;
  is_done: Ketrew_pure.Target.Condition.t option;
  path : string;
  is_bigger_than: int -> Ketrew_pure.Target.Condition.t;
  host: Host.t;
>
(** The type of products that carry a simple file (on a given
    [Host.t]). *)

val single_file: ?host:Host.t -> string -> single_file product
(** Create a [single_file] product.

    The path argument should be absolute since the notion of “current
    directory” is very ill-defined when it comes to this kind of
    distributed application.

    The condition returned by the [#is_done] method (used by default
    in any worfklow-node that uses the [single_file product]) is to
    check the existence of the file.
 *)


type list_of_files = <
  is_done: Ketrew_pure.Target.Condition.t option;
  paths : string list;
>
(** The type of products that carry a list of files (on a same [Host.t]). *)

val list_of_files:
  ?host:Host.t ->
  string list -> list_of_files product
(** Create a [list_of_files] product ([#is_done] checks the existence
    of all these files). *)

type unknown_product = < is_done : Condition.t option >
(** The type of the products that have been “hidden” or “forgotten,”
    see {!forget_product}. *)

val forget_product:
  'any_product workflow_node ->
  unknown_product workflow_node
(** “Cast” a node to the [unknown_product workflow_node] type, this is
    useful to make some programs that generate workflows type check
    (putting nodes into lists, or in different branches of a [match
    .. with]). *)

(** {4 Utilities For Workflow Nodes} *)

val workflow_to_string:
  ?ansi_colors:bool ->
  ?indentation:int ->
  'any workflow_node -> string
(** Build a display-friendly string summarizing the workflow. *)

val add_tags :
  ?recursive:bool ->
  'a workflow_node ->
  string list ->
  unit
(** Add tags imperatively to a node, if [recursive] is [true],  follow the
    [edges] recursively to add tags. *)

val node_id : 'a workflow_node -> string
(** Get the unique ID of the workdlow node. *)

val node_name : 'a workflow_node -> string
(** Get the name of the workdlow node. *)

(** {3 Legacy Deprecated API: Artifacts and Targets}

This is the old and deprecated API to build workflows (deprecated
since Ketrew 2.1.0).

Using functions like {!target} is still possible but they will trigger
a compilation warning e.g. ["Warning 3: deprecated: Ketrew.EDSL.target"].

*)

(** {4 Artifacts}

Artifacts are things to be built (they may already exist), most often
file-tree-locations on a given [host].
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
    method add_recursive_tags : string list -> unit
    method get_recursive_tags : string list
    (**/**)
  end

val target :
  ?active:bool ->
  ?depends_on:user_target list ->
  ?make:Build_process.t ->
  ?done_when:Target.Condition.t ->
  ?metadata:[ `String of string ] ->
  ?product:user_artifact ->
  ?equivalence:Target.Equivalence.t ->
  ?on_failure_activate:user_target list ->
  ?on_success_activate:user_target list ->
  ?tags: string list ->
  string -> user_target
  [@@ocaml.deprecated]
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
  ?make:Build_process.t ->
  ?metadata:[ `String of string ] ->
  ?name:string ->
  ?host:Host.t ->
  ?equivalence:Target.Equivalence.t ->
  ?on_failure_activate:user_target list ->
  ?on_success_activate:user_target list ->
  ?tags: string list ->
  string ->
  user_target
  [@@ocaml.deprecated]
(** Create a file {!user_artifact} and the {!user_target} that produces it.

    The [?product] of the target will be the file given as argument on
    the host given by the [?host] option (default: localhost using ["/tmp"]).

    The [?done_when] condition will be the existence of that file.

    This can be seen as a classical [make]-like file-producing target,
    but on any arbitrary host.
*)
(** {4 Utilities } *)

val to_display_string :
  ?ansi_colors:bool ->
  ?indentation:int ->
  user_target ->
  string
(** Build a display-friendly string summarizing the workflow. *)
