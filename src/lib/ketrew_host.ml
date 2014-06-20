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
      | Some p -> ["-p"; Int.to_string p]
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
        fail (`Unix_exec msg)
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
                    
type default_shell = Ketrew_gen_base_v0_t.default_shell

type t = Ketrew_gen_base_v0_t.host = {
  name: string;
  connection: connection;
  playground: Path.absolute_directory option;
  default_shell: default_shell
}

let default_shell ?binary ?(options=[]) ?(command_option="-c") command_name =
  {Ketrew_gen_base_v0_t. binary; command_name; options; command_option}

let shell_sh_minus_c = default_shell "sh"

let create ~connection ?(default_shell=shell_sh_minus_c) ?playground name =
  {name; connection; playground; default_shell}

let localhost ?default_shell ?playground ?(name="localhost") () = 
  create ~connection:`Localhost ?default_shell ?playground name

let tmp_on_localhost = 
  localhost ~playground:(Path.absolute_directory_exn "/tmp")
    ~name:"file://tmp" ()

let ssh ?default_shell ?playground ?port ?user ?name address =
  create ?playground ?default_shell Option.(value name ~default:address)
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
  let default_shell =
    Uri.get_query_param uri "shell"
    |> Option.bind ~f:(fun s -> 
        match String.split ~on:(`Character ',') s with
        | [] -> None
        | one :: [] -> Some (default_shell one)
        | one :: two :: [] -> Some (default_shell ~command_option:two one)
        | one :: more -> 
          let command_option = List.last more |> Option.value_exn ~msg:"bug" in
          let options = List.split_n more (List.length more - 1) |> fst in
          Some (default_shell ~command_option one ~options)
      )
  in
  create ?playground ~connection ?default_shell (Uri.to_string uri)

let of_string s =
  let uri = Uri.of_string s in
  of_uri uri

let log t = Log.(s t.name)
let to_string_hum t = t.name

module Error = struct

  type 'a execution = 'a constraint 'a =
  [> `Unix_exec of string
  | `Execution of
       <host : string; stdout: string option; stderr: string option; message: string>
  | `Ssh_failure of
       [> `Wrong_log of string
       | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string ]

  type 'a non_zero_execution = 'a constraint 'a = 
    [> `Non_zero of (string * int) ] execution

  let classify (e : _ non_zero_execution) =
    match e with
    | `Unix_exec _ -> `Unix
    | `Execution _ | `Non_zero _ -> `Execution
    | `Ssh_failure _ -> `Ssh

  let log e =
    let kv k v = Log.(brakets (s k % s " → " % v)) in
    match e with
    | `Unix_exec failure -> Log.(s "Unix-exec-error: " % s failure)
    | `Non_zero (cmd, ex) -> Log.(s "Cmd " % sf "%S" cmd % s " returned " % i ex)
    | `Execution exec ->
      Log.(
        s "Process execution failed: "
        % kv "Host" (s exec#host)
        % kv "Message" (s exec#message)
        % kv "Stdout" (option s exec#stdout)
        % kv "Stderr" (option s exec#stderr))
    | `Ssh_failure (`Wrong_log log, msg) ->
      Log.(s "SSH failed parsing log:" % s msg % kv "Log" (sf "%S" log))
    | `Ssh_failure (`Wrong_status exit_code, msg) ->
      Log.(s "SSH failed:" % s msg 
           % kv "Exit code" (Ketrew_unix_process.Exit_code.to_log exit_code))
end

let fail_host e = fail (`Host e)

let fail_exec t ?out ?err msg: 
  (_, [> `Host of _ Error.execution ]) Deferred_result.t =
  let v = object
    method host = to_string_hum t 
    method stdout = out
    method stderr = err
    method message  = msg
  end in
  fail_host (`Execution v)

let execute t argl =
  Log.(s "Host.execute " % s (to_string_hum t) % OCaml.list s argl
       @ very_verbose);
  let ret out err exited =
    let kv k v = Log.(brakets (s k % s " → " % v) |> indent) in
    Log.(s "Host: " % s (to_string_hum t)
         % s " executed: " % OCaml.list (sf "%S") argl
         % kv "status" (sf "exit:%d" exited)
         % kv "stdout" (sf "%S" out)
         % kv "stderr" (sf "%S" err) @ very_verbose);
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

type shell = string -> string list

let shell_sh ~sh cmd = [sh; "-c"; cmd]

let shell_of_default_shell t cmd = 
  let open Ketrew_gen_base_v0_t in
  t.default_shell.command_name :: 
  t.default_shell.options
  @ [t.default_shell.command_option; cmd]

let override_shell ?with_shell t =
  let shell = Option.value ~default:(shell_of_default_shell t) with_shell in
  shell

let get_shell_command_output ?with_shell t cmd =
  execute t (override_shell ?with_shell t cmd)
  >>= fun execution ->
  match execution#exited with
  | 0 -> return (execution#stdout, execution#stderr)
  | n -> fail_host (`Non_zero (cmd, n))

let get_shell_command_return_value ?with_shell t cmd =
  execute t (override_shell ?with_shell t cmd)
  >>= fun execution ->
  return execution#exited
    
let run_shell_command ?with_shell t cmd =
  get_shell_command_output ?with_shell t cmd
  >>= fun (_, _) ->
  return ()


let do_files_exist ?with_shell t paths =
  let cmd =
    List.map paths ~f:Path.exists_shell_condition 
    |> String.concat ~sep:" && " in
  get_shell_command_return_value ?with_shell t cmd
  >>= fun ret ->
  return (ret = 0)

let get_fresh_playground t = 
  let fresh = Unique_id.create () in
  Option.map  t.playground (fun pg ->
      Path.(concat pg (relative_directory_exn fresh)))

let ensure_directory ?with_shell t ~path =
  let cmd = fmt "mkdir -p %s" Path.(to_string_quoted path) in
  run_shell_command ?with_shell t cmd

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


