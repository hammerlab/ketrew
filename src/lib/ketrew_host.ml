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

  type t = Ketrew_gen_base_v0_t.ssh_host = {
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

  (** Strong version of an SSH call, trying to be like [Unix.exec]. 
      It “stores” the value of ["$?"] in the stderr channel
      enclosing the error log of the actual command between (hopefully) unique
      strings.

      It calls the command (list of strings, [argv]-like) with [exec]
      inside a sub-shell, and escapes all the arguments with [Filename.quote].

      Then it forces the “script” to return ['0'], if the overall execution of
      the whole SSH command does not return ['0'], we know that the problem
      is with the SSH call, not the command.
  *)
  let generic_ssh_exec ssh command =
    let unique_tag = Unique_id.create () in
    let spicied_command =
      fmt "echo -n %s >&2 ; \
           (exec %s) ; 
           echo -n %s$? >&2 ; 
           exit 0" 
        unique_tag
        (List.map command ~f:(Filename.quote) |> String.concat ~sep:" ")
        unique_tag
    in
    let ssh_exec = do_ssh ssh spicied_command in
    let parse_error_log out err =
      let fail_parsing msg = fail (`Ssh_failure (`Wrong_log msg, err)) in
      let pieces = String.split ~on:(`String unique_tag) err in
      match pieces with
      | "" :: actual_stderr :: return_value :: [] ->
        begin match Int.of_string (String.strip return_value) with
        | Some r -> return (out, actual_stderr, r)
        | None -> fail_parsing "Return value not an integer"
        end
      | somehting_else -> fail_parsing "Cannot parse error log"
    in
    begin Ketrew_unix_process.exec ssh_exec
      >>< function
      | `Ok (out, err, `Exited 0) -> parse_error_log out err
      | `Ok (out, err, other) ->
        fail (`Ssh_failure (`Wrong_status other, err))
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_exec 
             % s " failed: " %s msg @ verbose);
        fail (`Exec_failure msg)
    end

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
type connection = Ketrew_gen_base_v0_t.connection 
                    
type t = Ketrew_gen_base_v0_t.host = {
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

let of_uri uri =
  let connection =
    Option.value_map ~default:`Localhost (Uri.host uri) ~f:(fun address ->
        `Ssh {Ssh.address; port = Uri.port uri; user = Uri.userinfo uri})
  in
  let playground =
    match Uri.path uri with
    | "" -> None
    | p -> Some (Path.absolute_directory_exn p)
  in
  create ?playground ~connection (Uri.to_string uri)

let of_string s =
  let uri = Uri.of_string s in
  of_uri uri

let to_string_hum t = t.name

(* Experimenting with even more structurally typed error values: *)
type 'a execution_error = 'a constraint 
  'a = [> `Execution of <host : string; stdout: string option; 
          stderr: string option; message: string> ]

let fail_host e = fail (`Host e)

let fail_exec t ?out ?err msg: 
  (_, [> `Host of _ execution_error ]) Deferred_result.t =
  let v = object
    method host = to_string_hum t 
    method stdout = out
    method stderr = err
    method message  = msg
  end in
  fail_host (`Execution v)

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

let execute t argl =
  let ret out err exited =
    return (object
      method stdout = out method stderr = err method exited = exited
    end)
  in
  match t.connection with
  | `Localhost ->
    begin Ketrew_unix_process.exec argl
      >>< function
      | `Ok (out, err, `Exited n) -> ret out err n
      | `Ok (out, err, other) ->
        fail_exec t ~out ~err (System.Shell.status_to_string other)
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Ssh-cmd " % OCaml.list (sf "%S") argl 
             % s " failed: " %s msg @ verbose);
        fail_exec t msg
    end
  | `Ssh ssh ->
    begin Ssh.generic_ssh_exec ssh argl
      >>< function
      | `Ok (out, err, exited) -> ret out err exited
      | `Error e -> fail (`Host e)
    end

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


