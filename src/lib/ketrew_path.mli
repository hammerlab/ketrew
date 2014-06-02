type relative

type absolute

type file

type directory

type 'a t = { kind : [ `Directory | `File ]; path : string; }
  constraint 'a = < kind : 'file_kind; relativity : 'relativity >

type absolute_directory = < kind : directory; relativity : absolute > t

type absolute_file = < kind : file; relativity : absolute > t

type relative_directory = < kind : directory; relativity : relative > t

type relative_file = < kind : file; relativity : relative > t

val file : string -> < kind : file; relativity : 'a > t

val directory : string -> < kind : directory; relativity : 'a > t

val root : < kind : directory; relativity : absolute > t

val absolute_file_exn : string -> < kind : file; relativity : absolute > t

val absolute_directory_exn :
  string -> < kind : directory; relativity : absolute > t

val relative_directory_exn :
  string -> < kind : directory; relativity : relative > t

val relative_file_exn : string -> < kind : file; relativity : relative > t

val concat :
  < kind : directory; relativity : 'a > t ->
  < kind : 'b; relativity : relative > t -> < kind : 'b; relativity : 'a > t

val to_string : < kind : 'a; relativity : 'b > t -> string

val to_string_quoted : < kind : 'a; relativity : 'b > t -> string

val any_kind :
  < kind : 'b; relativity : 'a > t -> < kind : 'c; relativity : 'a > t
     
val exists_shell_condition : < kind : 'a; relativity : 'b > t -> string
