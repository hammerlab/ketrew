open Ketrew_pervasives
module Path = Ketrew_path

module Ssh = struct

  let _configuration_ssh_batch_option = ref ""

  let configure_ssh_batch_option spec =
    let op =
      match spec with
      | `Openssh -> "-oBatchMode=yes"
      | `Dropbear -> "-s"
      | `Custom s -> s
    in
    _configuration_ssh_batch_option := op

  let () = configure_ssh_batch_option `Openssh

  type t = {
    address: string;
    port: int option;
    user: string option;
  }

  (** Generate a proper SSH command for the given host. *)
  let do_ssh ssh command =
    ["ssh"; !_configuration_ssh_batch_option]
    @ (match ssh.port with
      | Some p -> ["-p"; "port"]
      | None -> [])
    @ (match ssh.user with
      | None -> [ssh.address]
      | Some u -> [fmt "%s@%s" u ssh.address])
    @ [command]

  (** Generate an SCP command for the given host with the destination
      directory or file path. *)
  let scp_push ssh ~src ~dest =
    ["scp"; !_configuration_ssh_batch_option]
    @ (match ssh.port with
      | Some p -> ["-P"; "port"]
      | None -> [])
    @ src
    @ (match ssh.user with
      | None -> [fmt "%s:%s" ssh.address dest]
      | Some u -> [fmt "%s@%s:%s" u ssh.address dest])

  (** Generate an SCP command for the given host as source. *)
  let scp_pull  ssh ~src ~dest =
    ["scp"; !_configuration_ssh_batch_option]
    @ (match ssh.port with
      | Some p -> ["-P"; "port"]
      | None -> [])
    @ (List.map src ~f:(fun src_item ->
        match ssh.user with
        | None -> fmt "%s:%s" ssh.address src_item
        | Some u -> fmt "%s@%s:%s" u ssh.address src_item))
    @ [dest]

end
type connection = [
  | `Localhost
  | `Ssh of Ssh.t
]
type t = {
  name: string;
  connection: connection;
  playground: Path.absolute_directory option;
}
let create ?(connection=`Localhost) ?playground name =
  {name; connection; playground}

let localhost = 
  create ~playground:(Path.absolute_directory_exn "/tmp")  "localhost"

let ssh ?playground ?port ?user ?name address =
  create ?playground Option.(value name ~default:address)
    ~connection:(`Ssh {Ssh. address; port; user})

let to_string t = t.name

let fail_exec t ?(out="") ?(err="") msg =
  fail (`Host (`Execution (to_string t, out, err, msg)))

let get_shell_command_output t cmd =
  match t.connection with
  | `Localhost ->
    begin System.Shell.execute cmd
      >>< function
      | `Ok (out, err, `Exited 0) -> return (out, err)
      | `Ok (out, err, other) -> 
        fail_exec t ~out ~err (System.Shell.status_to_string other)
      | `Error (`Shell _ as e) ->
        fail_exec t (System.error_to_string e)
    end
  | `Ssh ssh ->
    let ssh_cmd = Ssh.(do_ssh ssh cmd) in
    begin Ketrew_unix_process.succeed ssh_cmd
      >>< function
      | `Ok (out, err) -> return (out, err)
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_cmd 
             % s " failed: " %s msg @ verbose);
        fail_exec t msg
    end
let get_shell_command_return_value t cmd =
  match t.connection with
  | `Localhost ->
    begin System.Shell.execute cmd
      >>< function
      | `Ok (out, err, `Exited n) -> return n
      | `Ok (out, err, other) -> 
        fail_exec t ~out ~err (System.Shell.status_to_string other)
      | `Error (`Shell _ as e) ->
        fail_exec t (System.error_to_string e)
    end
  | `Ssh ssh ->
    let ssh_cmd = Ssh.(do_ssh ssh cmd) in
    begin Ketrew_unix_process.exec ssh_cmd
      >>< function
      | `Ok (out, err, `Exited n) -> return n
      | `Ok (out, err, other) -> 
        fail_exec t ~out ~err (System.Shell.status_to_string other)
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_cmd 
             % s " failed: " %s msg @ verbose);
        fail_exec t msg
    end

let run_shell_command t cmd =
  get_shell_command_output t cmd
  >>= fun (_, _) ->
  return ()

let do_files_exist t paths =
  let cmd =
    List.map paths ~f:Path.exists_shell_condition 
    |> String.concat ~sep:" && " in
  match t.connection with
  | `Localhost ->
    begin System.Shell.execute cmd
      >>< function
      | `Ok (_, _, `Exited 0) -> return true
      | `Ok (_, _, `Exited 1) -> return false
      | `Ok (out, err, other) -> 
        fail_exec t ~out ~err (System.Shell.status_to_string other)
      | `Error (`Shell _ as e) -> fail_exec t (System.error_to_string e)
    end
  | `Ssh ssh ->
    let ssh_cmd = Ssh.(do_ssh ssh cmd) in
    begin Ketrew_unix_process.exec ssh_cmd
      >>< function
      | `Ok (_, _, `Exited 0) -> return true
      | `Ok (_, _, `Exited 1) -> return false
      | `Ok (out, err, other) -> 
        fail_exec t ~out ~err (System.Shell.status_to_string other)
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_cmd 
             % s " failed: " %s msg @ verbose);
        fail_exec t msg
    end

let get_fresh_playground t = 
  let fresh = Unique_id.create () in
  Option.map  t.playground (fun pg ->
      Path.(concat pg (relative_directory_exn fresh)))

let ensure_directory t ~path =
  match t.connection with
  | `Localhost -> System.ensure_directory_path Path.(to_string path)
  | `Ssh ssh ->
    let cmd = fmt "mkdir -p %s" Path.(to_string_quoted path) in
    let ssh_cmd = Ssh.(do_ssh ssh cmd) in
    begin Ketrew_unix_process.succeed ssh_cmd
      >>< function
      | `Ok (out, err) -> return ()
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_cmd 
             % s " failed: " %s msg @ verbose);
        fail_exec t msg
    end

let put_file t ~path ~content =
  match t.connection with
  | `Localhost -> IO.write_file ~content Path.(to_string path)
  | `Ssh ssh ->
    let temp = Filename.temp_file "ketrew" "ssh_put_file" in
    IO.write_file ~content temp
    >>= fun () ->
    let scp_cmd = Ssh.(scp_push ssh ~src:[temp] ~dest:(Path.to_string path)) in
    begin Ketrew_unix_process.succeed scp_cmd
      >>< function
      | `Ok (out, err) -> return ()
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Scp-cmd " % OCaml.list (sf "%S") scp_cmd 
             % s " failed: " %s msg @ verbose);
        fail_exec t msg
    end

let get_file t ~path =
  match t.connection with
  | `Localhost -> 
    begin IO.read_file Path.(to_string path)
      >>< function
      | `Ok c -> return c
      | `Error _ -> 
        fail (`Cannot_read_file ("localhost", Path.(to_string path)))
    end
  | `Ssh ssh ->
    let temp = Filename.temp_file "ketrew" "ssh_get_file" in
    let scp_cmd = Ssh.(scp_pull ssh ~dest:temp ~src:[Path.to_string path]) in
    begin Ketrew_unix_process.succeed scp_cmd
      >>< function
      | `Ok (out, err) -> IO.read_file temp
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Scp-cmd " % OCaml.list (sf "%S") scp_cmd 
             % s " failed: " %s msg @ verbose);
        fail (`Cannot_read_file (ssh.Ssh.address, Path.(to_string path)))
    end


