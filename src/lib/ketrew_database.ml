
open Ketrew_pervasives

type stupid_db = (string * string) list
type action =
  | Set of string * string
  | Sequence of action list
  | Check of string * string option
let set ~key value = Set (key, value)
let seq l = Sequence l
let contains ~key v = Check (key, Some v) 
let is_not_set key = Check (key, None)

type t = {
  mutable db: stupid_db;
  (* mutable history: (action * stupid_db) list; *)
  parameters: string;
}
let create parameters = {db = []; parameters} 

let save t =
  let content = Marshal.to_string t [] in
  IO.write_file t.parameters ~content

let load path =
  begin System.file_info ~follow_symlink:true path
    >>= function
    | `Regular_file _ ->
      Log.(s "Loading database at " % s path @ very_verbose);
      IO.read_file path
      >>= fun content ->
      begin try return (Marshal.from_string content 0 : t) with
      | e -> fail (`Database (`Load, path))
      end
    | _ -> 
      Log.(s "Creating new database at " % s path @ very_verbose);
      let db = create path in
      save db (* should create + save be in `Database`? *)
      >>= fun () ->
      return db
  end

let get t ~key =
  List.find_map t.db ~f:(fun (k, v) -> if k = key then Some v else None)
  |> return
let act t ~action =
  let current = ref t.db in
  let rec go = 
    function
    | Set (key, value) ->
      current := (key, value) :: !current;
      true
    | Check (key, value) ->
      begin match List.find !current (fun (k, v) -> key = k) with
      | Some (k, v) when Some v = value -> true
      | None when value = None -> true
      | _ -> false
      end
    | Sequence actions ->
      List.for_all actions go
  in
  if go action 
  then begin
    t.db <- !current;
    save t >>= fun () ->
    return `Done
  end
  else return `Not_done


