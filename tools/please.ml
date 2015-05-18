

let () =
  try
    Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;
#use "topfind"
#require "unix,nonstd,sosa"

open Nonstd
module String = struct
  include Sosa.Native_string
end

let cmd_exn fmt = ksprintf (fun s ->
    match Sys.command s with
    | 0 -> ()
    | ret ->
      ksprintf failwith "Cmd-error:\n%S\nreturned: %d" s ret
  ) fmt

let read_process command =
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = String.make buffer_size 'B' in
  let in_channel = Unix.open_process_in command in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input in_channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  ignore (Unix.close_process_in in_channel);
  Buffer.contents buffer

let read_cmd fmt = ksprintf read_process fmt

let write_file n ~content =
  let o = open_out n in
  output_string o content;
  close_out o

let version_string () =
  let default = "0.0.0+master" in
  try
    read_cmd "git describe --tags --always --dirty"
    |> String.strip
    |> (function "" -> default | s -> s)
  with _ -> default

let pure_findlib_packages = [
  "sosa"; "nonstd";
  "docout"; "pvem"; "yojson"; "uri";
  "cohttp"; (* → measurements refer to cohttp “pure” lib *)
  "ppx_deriving_yojson"; "ppx_deriving.show"; "ppx_blob";
]
let unix_findlib_packages = [
  "threads"; "trakeva_sqlite";
  "pvem_lwt_unix"; "cmdliner"; "cohttp.lwt"; "lwt"; "ssl";
  "conduit"; "dynlink"; "findlib";
]
let all_findlib_packages = pure_findlib_packages @ unix_findlib_packages

let homepage = "http://seb.mondet.org/software/ketrew/"

let oasis_meta_variable_version = "%%VERSION%%"
let oasis_meta_variable_pure_findlib_packages = "%%PURE_FINDLIB_PACKAGES%%"
let oasis_meta_variable_unix_findlib_packages = "%%UNIX_FINDLIB_PACKAGES%%"


let find_all ?(name="*") dir =
  String.split ~on:(`Character '\n')
    (read_cmd "find %s -type f -name '%s'" dir name)
  |> List.filter ~f:((<>) "")

let say_stuff =
  function
  | ["something"] ->
    printf "Version: %S\nPackages:%s\n%!"
      (version_string ()) (String.concat ~sep:", " all_findlib_packages)
  | "ocamlfind-package-options" :: [] ->
    printf "%s"
      (List.map ~f:(sprintf "-package %s") all_findlib_packages
       |> String.concat ~sep:" ")
  | "ocamlfind-package-list-for-require" :: [] ->
    printf "%s" (String.concat ~sep:"," all_findlib_packages)
  | "lib-mli-files" :: [] ->
    find_all "src/lib" ~name:"*.mli" |> List.iter ~f:(printf "%s\n%!");
    find_all "src/pure" ~name:"*.mli" |> List.iter ~f:(printf "%s\n%!")
  | "lib-ml-files" :: [] ->
    find_all "src/lib" ~name:"*.ml" |> List.iter ~f:(printf "%s\n%!");
    find_all "src/pure" ~name:"*.ml" |> List.iter ~f:(printf "%s\n%!")
  | other ->
    ksprintf failwith "don't know what to say: %S" (String.concat ~sep:"; " other)

let () =
  match Array.to_list Sys.argv |> List.tl_exn with
  | "say" :: stuff ->
    say_stuff stuff
  | "generate" :: "metadata" :: [] ->
    cmd_exn "mkdir -p _build/";
    write_file "_build/VERSION" ~content:(version_string ());
    write_file "_build/FINDLIB_PACKAGES"
      ~content:(List.map all_findlib_packages ~f:(sprintf "%s")
                |> String.concat ~sep:" ");
    ()
  | "make" :: "_oasis" :: [] ->
    let with_bisect = try Sys.getenv "WITH_BISECT" = "true" with _ -> false in
    let packages l =
      (if with_bisect then "bisect_ppx" :: l else l)
      |> String.concat ~sep:", " in
    cmd_exn "sed 's/%s/%s/g' tools/_oasis.in | sed 's/%s/%s/'  | sed 's/%s/%s/' > _oasis"
      oasis_meta_variable_version (version_string ())
      oasis_meta_variable_pure_findlib_packages
      (packages pure_findlib_packages)
      oasis_meta_variable_unix_findlib_packages
      (packages unix_findlib_packages)
  | [] -> printf "Nothing to do"
  | others ->
    printf "Don't know what to do with: %s" (String.concat ~sep:", " others);
    exit 2
