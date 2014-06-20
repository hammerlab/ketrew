
open Ketrew_pervasives

module Path = Ketrew_path

module Host = Ketrew_host

module Volume = struct

  open Ketrew_gen_base_v0_t
  type structure = volume_structure =
    | File of string
    | Directory of (string * structure list)

  type t = volume = {
    host: Host.t;
    root: Path.absolute_directory;
    structure: structure;
  }
  let create ~host ~root structure = {host; root; structure}
  let file s = File s
  let dir name contents = Directory (name, contents)

  let rec all_structure_paths : 
    type any. structure -> <kind: any; relativity: Path.relative > Path.t list =
    fun s ->
    match s with
    | File s -> [Path.relative_file_exn s |> Path.any_kind ]
    | Directory (name, children) ->
      let children_paths = 
        List.concat_map ~f:all_structure_paths children in
      let this_one = Path.relative_directory_exn name in
      (this_one |> Path.any_kind) :: List.map ~f:(Path.concat this_one) children_paths

  let all_paths t: <kind: 'any; relativity: Path.absolute> Path.t list =
    List.map ~f:(Path.concat t.root) (all_structure_paths t.structure)

  let exists t =
    let paths = all_paths t in
    Host.do_files_exist t.host paths

  let log {host; root; structure} =
    Log.(s "Vol" % parens (Host.log host % s":" % s (Path.to_string root)))
  let to_string_hum v =
    Log.to_long_string (log v)
end

module Type = struct

  type value_type = Ketrew_gen_base_v0_t.value_type
  let value_type_to_string = function
  | `Unit -> "Unit"
  | `String -> "String"
  | `Number -> "Number"

  type t = Ketrew_gen_base_v0_t.artifact_type
  let value vt : t = `Value vt
  let string_value : t = `Value `String
  let volume v = `Volume v

  let to_string_hum = function
  | `Value v -> fmt "Value %s" (value_type_to_string v)
  | `Volume v -> fmt "Volume %s" (Volume.to_string_hum v)

end

type value = Ketrew_gen_base_v0_t.artifact_value

let unit = `Unit

type t = Ketrew_gen_base_v0_t.artifact

(* TODO those two functions should be more type-safe *)
let is_ready specification =
  match specification with
  | `Value _ -> return false
  | `Volume v -> Volume.exists v

let of_type: Type.t -> t option =
  function
  | `Value `Unit -> Some (`Value `Unit)
  | `Value v -> None
  | `Volume v -> Some (`Volume v)



