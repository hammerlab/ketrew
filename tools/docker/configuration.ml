
let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ();;
#use "topfind"
#thread
#require "ketrew"
open Nonstd
open Ketrew.Configuration
let debug_level = 1

let engine =
  engine ~database_parameters:"/tmp/ketrew/database" ()

let env_exn s =
  try Sys.getenv s with _ -> ksprintf failwith "Missing environment variable: %S" s

let port =
  env_exn "PORT" |> Int.of_string
  |> Option.value_exn ~msg:"$PORT is not an integer"

let server =
  server ~engine
    ~authorized_tokens:[
      authorized_token ~name:"From-env" (env_exn "AUTH_TOKEN");
     ]
    ~return_error_messages:true
    ~log_path:"/tmp/ketrew/logs/"
    ~command_pipe:"/tmp/ketrew/command.pipe"
    (`Tls ("/tmp/ketrew/certificate.pem", "/tmp/ketrew/privkey-nopass.pem", port))

let () =
  output [
    profile "default" (create ~debug_level (server));
  ]
