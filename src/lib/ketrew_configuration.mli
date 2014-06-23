(** Definition of the configuration (input to state creation; contents of the
    config-file). *)

open Ketrew_pervasives

type t
(** The contents of the configuration. *)

val create :
  ?turn_unix_ssh_failure_into_target_failure: bool ->
  ?persistent_state_key:string -> database_parameters:string -> unit -> t
(** Create a configuration, [persistent_state_key] is the “key” of the
    state storage in the database, [database_parameters] are used to call
    {!Ketrew_database.load}.

    The parameter [turn_unix_ssh_failure_into_target_failure] tells
    Ketrew whether it should kill targets when a failure is not
    assuredly “their fault” (e.g. a call to [ssh] may fail
    because of network settings, and succeed when tried again later);
    the default value is [false].
*)

val default_configuration_path: string
(** Default path to the configuration file. *)

val default_database_path: string
(** Default path to the database (used when generating custom configuration
    files). *)

val database_parameters: t -> string
(** Get the database parameters. *)

val persistent_state_key: t -> string
(** Get the “key” of the state values in the database. *)

val is_unix_ssh_failure_fatal: t -> bool
(** Should we kill targets on ssh/unix errors. *)

val parse :
  string ->
  (t, [> `Configuration of [> `Parsing of string ] ]) Result.t
(** Parse the contents of a configuration file. *)

val get_configuration :
  ?override_configuration:t ->
  string ->
  (t,
   [> `Configuration of [> `Parsing of string ]
   | `IO of [> `Read_file_exn of string * exn ] ]) Deferred_result.t
(** The call [get_configuration file] reads and parses the file [f], unless
    [override_configuration] is provided. *)

val log: t -> Log.t list
(** Get a display-friendly list of configuration items. *)
