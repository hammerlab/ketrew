open Ketrew_pervasives

module Volume :
  sig
    type structure = File of string | Directory of string * structure list
    type t = {
      host : Ketrew_host.t;
      root : Ketrew_path.absolute_directory;
      structure : structure;
    }
    val create :
      host:Ketrew_host.t -> root:Ketrew_path.absolute_directory -> structure -> t
    val file : string -> structure
    val dir : string -> structure list -> structure
    val all_structure_paths :
      structure -> Ketrew_path.relative_directory list
    val all_paths : t -> Ketrew_path.absolute_directory list
    val exists :
      t ->
      (bool,
       [> `Host of [> `Execution of string * string * string * string ] ])
      Deferred_result.t
    val to_string : t -> string
  end
module Type :
  sig
    type value_type = [ `Number | `String | `Unit ]
    val value_type_to_string : [< `Number | `String | `Unit ] -> string
    type t = [ `Value of value_type | `Volume of Volume.t ]
    val value : value_type -> t
    val string_value : t
    val volume : 'a -> [> `Volume of 'a ]
    val to_string :
      [< `Value of [< `Number | `String | `Unit ] | `Volume of Volume.t ] ->
      string
  end
type value = [ `Number of float | `String of string | `Unit ]
val unit : value
type t = [ `Value of value | `Volume of Volume.t ]
val is_ready :
  [< `Value of 'a | `Volume of Volume.t ] ->
  (bool, [> `Host of [> `Execution of string * string * string * string ] ])
  Ketrew_pervasives.t
val of_type : Type.t -> t
