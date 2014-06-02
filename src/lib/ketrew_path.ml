open Ketrew_pervasives

type relative 
type absolute
type file
type directory
type 'c t =
  {kind: [`File | `Directory]; path: string}
  constraint 'c = <relativity: 'relativity; kind: 'file_kind>
type absolute_directory = <relativity : absolute; kind: directory> t
type absolute_file = <relativity : absolute; kind: file> t
type relative_directory = <relativity : relative; kind: directory> t
type relative_file = <relativity : relative; kind: file> t

let file path : <relativity : 'a; kind: file>  t =
  {kind = `File; path}

let directory path : <relativity : 'a; kind: directory> t  =
  {kind = `Directory; path}

let root : <relativity : absolute; kind: directory> t = directory "/"

let absolute_file_exn s : <relativity : absolute; kind: file> t =
  if Filename.is_relative s
  then invalid_argument_exn ~where:"Path" "absolute_file_exn"
  else file s
let absolute_directory_exn s : <relativity : absolute; kind: directory> t =
  if Filename.is_relative s
  then invalid_argument_exn ~where:"Path" "absolute_directory_exn"
  else directory s
let relative_directory_exn s : <relativity : relative; kind: directory> t =
  if Filename.is_relative s
  then directory s
  else invalid_argument_exn ~where:"Path" "relative_directory_exn"
let relative_file_exn s : <relativity: relative; kind: file> t =
  if Filename.is_relative s
  then file s
  else invalid_argument_exn ~where:"Path" "relative_file_exn"

let concat: <relativity: 'a; kind: directory> t ->
  <relativity: relative; kind: 'b> t -> <relativity: 'a; kind: 'b> t =
  fun x y ->
    { kind = y.kind; path = Filename.concat x.path y.path}

let to_string: 'a t -> string = fun x -> x.path
let to_string_quoted: 'a t -> string = fun x -> Filename.quote x.path

let any_kind: <relativity: 'a; kind: 'b> t -> <relativity: 'a; kind: 'c> t =
  fun x -> { x with kind = x.kind }

let exists_shell_condition = function
| {kind = `File; path } ->  fmt "[ -f %S ]" path
| {kind = `Directory; path } ->  fmt "[ -d %S ]" path

