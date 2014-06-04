open Ketrew_pervasives

(** Artifacts are input values or files, or results of computations. *)

(** Module defining “Volumes” which are definitions of a given file structure
    on a given {!Ketrew_host.t}. *)
module Volume : sig

    type structure
    (** The structure of a volume defines the hierarchy of files relative to
        a [root] path, one constructs structures with {!file} and {!dir}:
        {[
          let src_dir_structure =
            dir "src" [
              dir "lib" [
                file "ketrew.ml";
                file "ketrew_artifact.ml";
                file "ketrew_artifact.mli";
                (* ... *)
              ];
              dir "test" [
                file "main.ml";
              ];
            ]
          ]}
    
    *)

    val file : string -> structure
    (** Define a “file” structure. *)

    val dir : string -> structure list -> structure
    (** Define a “directory” structure. *)

    type t
    (** The container of volumes. *)

    val create :
      host:Ketrew_host.t -> root:Ketrew_path.absolute_directory -> structure ->
      t
    (** Create a volume. Example {[
          let sources =
            Volume.create ~host:deployment_server ~root:git_repository
              src_dir_structure ]}
    *)

    val all_paths : t -> <relativity: Ketrew_path.absolute; ..> Ketrew_path.t list
    (** Get all the paths of the given Volume (files and directories). *) 

    val exists :
      t ->
      (bool,
       [> `Host of [> `Execution of string * string * string * string ] ])
      Deferred_result.t
    (** Check whether the whole structure of the Volume exists on the host. *)

    val to_string_hum : t -> string
    (** Get a Human-readable string. *)

  end

(** Module defining an artifact's “type”;  its “specification”. *)
module Type : sig

    type value_type = [ `Number | `String | `Unit ]
    (** The specification of a value's type. *)

    type t = [ `Value of value_type | `Volume of Volume.t ]
    (** The specification of an artifact. *)

    val value : value_type -> t
    (** Construct a “value-type” artifact type. *)

    val string_value : t
    (** Shortcut for [`Value `String]. *)

    val volume : Volume.t -> t
    (** Construct a “volume-specification” artifact type. *)

    val to_string_hum : t -> string
    (** Get a Human-readable string. *)

  end

type value = [ `Number of float | `String of string | `Unit ]
(** Literal values. *)

val unit : value
(** Alias for [`Unit]. *)

type t = [ `Value of value | `Volume of Volume.t ]
(* Literal artifact, i.e. a literal value, or the specification of a volume
   (used for now also to represent its “value”). *)

val is_ready :
  Type.t ->
  (bool, [> `Host of [> `Execution of string * string * string * string ] ])
  Deferred_result.t
(** Check whether an aritfact is ready, given its type.
A “value” artifact is {i never } ready, a “volume” one is checked with {!Volume.exists}.
This is meant to change. *)

val of_type : Type.t -> t
(** Get the value given it's type, fails with [Invalid_argument _] on “value”
    artifacts. This is {i also } meant to change.
*)
