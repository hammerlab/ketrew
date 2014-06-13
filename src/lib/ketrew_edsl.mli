(** Easy interface to the library {b for end users}. *)
(**
  This is a more hopefully stable EDSL/API to make workflows and
  deal with the system.

  Many functions may raise exceptions when called on improperly, but this
  should happen while building the workflow, not after it starts running. *)


(** {3 Hosts} *)

type host = Ketrew_host.t
(** Alias for the host type. *)

val parse_host : string -> host
(** See {!Ketrew_host.of_string}. *)

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

(** {3 Artifacts} *)

(** Wrapper for {!Ketrew_artifact.t} and {!Ketrew_artifact.Type.t}. *)
class type user_artifact = object
  (**/**)
  method artifact_type : Ketrew_artifact.Type.t
  (**/**)

  method path : string
  (** Return the path of the artifact if the artifact is a volume containing
      a single file or directory. *)
end

val file: ?host:Ketrew_host.t -> string -> user_artifact
(** Create a volume containing one file. *)

val unit : user_artifact
(** The artifact that is “never ready” (i.e. the target associated will always
    be (re-)run if activated). *)

(** {3 Targets} *)

(** Wrapper around {!Ketrew_target.t}. *)
class type user_target =
  object

    method activate : unit
    (** Activate the target. *)

    method name : string
    (** Get the name of the target *)

    (**/**)
    method is_active: bool
    method id: Ketrew_pervasives.Unique_id.t
    method render: Ketrew_target.t
    method dependencies: user_target list
    (**/**)
  end

(** Create a new target. *)
val target :
  ?active:bool ->
  ?dependencies:user_target list ->
  ?make:Ketrew_target.build_process ->
  ?returns:user_artifact -> string -> user_target

(** Create a new target but with [~active:true]. *)
val active :
  ?dependencies:user_target list ->
  ?make:Ketrew_target.build_process ->
  ?returns:user_artifact -> string -> user_target

val nohup_setsid :
  host:Ketrew_host.t -> string list -> Ketrew_target.build_process
(** Create a nohup_setsid build process. *)

val direct_shell_command :
  ?host:Ketrew_host.t -> string -> Ketrew_target.build_process
(** Create a shell command process (not “long-running”). *)

(** {3 Workflows} *)

val run: user_target ->  Ketrew_command_line.user_todo list
(** Activate [user_target] and “make” the workflow. *)

val ketrew_fail: 
  ('a, unit, string, Ketrew_command_line.user_todo list) format4 -> 'a
(** Printf-like function to return ask Ketrew to fail (complementary of
    {!run}). *)
