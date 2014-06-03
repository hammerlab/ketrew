
open Ketrew_pervasives

module Path = Ketrew_path

module Host = Ketrew_host

module Volume = struct

  type structure =   
    | File of string
    | Directory of string * structure list

  type t = {
    host: Host.t;
    root: Path.absolute_directory;
    structure: structure;
  }
  let create ~host ~root structure = {host; root; structure}
  let file s = File s
  let dir name contents = Directory (name, contents)

  let rec all_structure_paths s =
    match s with
    | File s -> [Path.relative_file_exn s |> Path.any_kind ]
    | Directory (name, children) ->
      let children_paths = 
        List.concat_map ~f:all_structure_paths children in
      let this_one = Path.relative_directory_exn name |> Path.any_kind in
      this_one :: List.map ~f:(Path.concat this_one) children_paths

  let all_paths t =
    List.map ~f:(Path.concat t.root) (all_structure_paths t.structure)

  let exists t =
    let paths = all_paths t in
    Host.do_files_exist t.host paths

  let to_string {host; root; structure} =
    fmt "Vol(%s:%s)" (Host.to_string host) (Path.to_string root)
end

module Type = struct

  type value_type = [`Unit | `String | `Number]
  let value_type_to_string = function
  | `Unit -> "Unit"
  | `String -> "String"
  | `Number -> "Number"

  type t = [
    (* | `Fresh_file *)
    | `Value of value_type
    | `Volume of Volume.t
  ]
  let value vt : t = `Value vt
  let string_value : t = `Value `String
  let volume v = `Volume v

  let to_string = function
  | `Value v -> fmt "Value %s" (value_type_to_string v)
  | `Volume v -> fmt "Volume %s" (Volume.to_string v)

end

type value = [ `Unit | `String of string | `Number of float ]

let unit : value = `Unit

type t = [
  (* | `Tree of File_tree.t *)
  (* | `File of File_tree.file *)
  | `Value of value
  | `Volume of Volume.t
]


(* TODO those two functions should be more type-safe *)
let is_ready specification =
  match specification with
  | `Value _ -> return false
  | `Volume v -> Volume.exists v

let of_type: Type.t -> t = function
  | `Value v -> invalid_argument_exn ~where:"Artifact" "specification_to_value"
  | `Volume v -> `Volume v



