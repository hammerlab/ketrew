(**************************************************************************)
(*  Copyright 2014, Sebastien Mondet <seb@mondet.org>                     *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)

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

  type t = Ketrew_gen_base_v0.Ssh_host.t = {
    address: string;
    port: int option;
    user: string option;
    add_ssh_options: string list;
  }

  (** Generate a proper SSH command for the given host. *)
  let do_ssh ssh command =
    ["ssh"; !_configuration_ssh_batch_option]
    @ ssh.add_ssh_options
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
    @ ssh.add_ssh_options
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
    @ ssh.add_ssh_options
    @ (match ssh.port with
      | Some p -> ["-P"; "port"]
      | None -> [])
    @ (List.map src ~f:(fun src_item ->
        match ssh.user with
        | None -> fmt "%s:%s" ssh.address src_item
        | Some u -> fmt "%s@%s:%s" u ssh.address src_item))
    @ [dest]

end
type connection = Ketrew_gen_base_v0.Connection.t 
                    
type default_shell = Ketrew_gen_base_v0.Default_shell.t

type t = Ketrew_gen_base_v0.Host.t = {
  name: string;
  connection: connection;
  playground: Path.t option;
  default_shell: default_shell;
  execution_timeout: Time.t option;
}

let default_shell ?binary ?(options=[]) ?(command_option="-c") command_name =
  {Ketrew_gen_base_v0.Default_shell.
    binary; command_name; options; command_option}

let shell_sh_minus_c = default_shell "sh"

let create ~connection ?execution_timeout ?(default_shell=shell_sh_minus_c) ?playground name =
  {name; connection; playground; default_shell; execution_timeout}

let localhost 
    ?execution_timeout ?default_shell ?playground ?(name="localhost") () = 
  create ~connection:`Localhost ?default_shell ?playground name

let tmp_on_localhost = 
  localhost ~playground:(Path.absolute_directory_exn "/tmp")
    ~name:"file://tmp" ()

let ssh 
    ?execution_timeout ?(add_ssh_options=[]) ?default_shell ?playground
    ?port ?user ?name address =
  create ?playground ?default_shell Option.(value name ~default:address)
    ~connection:(`Ssh {Ssh. address; port; user; add_ssh_options})

let of_uri uri =
  let connection =
    Option.value_map ~default:`Localhost (Uri.host uri) ~f:(fun address ->
        let add_ssh_options =
          Option.value ~default:[] (Uri.get_query_param' uri "ssh-option") in
        let user = Uri.userinfo uri in
        `Ssh {Ssh.address; port = Uri.port uri; user; add_ssh_options})
  in
  let playground =
    match Uri.path uri with
    | "" -> None
    | rel when Filename.is_relative rel ->
      let cwd = Sys.getcwd () in
      Some (Path.absolute_directory_exn (Filename.concat cwd rel))
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
  let execution_timeout =
    Uri.get_query_param uri "timeout" 
    |> Option.bind ~f:Float.of_string in
  create ?playground ~connection ?default_shell ?execution_timeout
    (Uri.to_string uri)

let to_uri t =
  let scheme, host, port, userinfo =
    match t.connection with
    | `Ssh {Ssh.address; port; user;} -> Some "ssh", Some address, port, user
    | `Localhost -> None, None, None, None
  in
  let query =
    let {Ketrew_gen_base_v0.Default_shell.
          binary; command_name; options; command_option} =
      t.default_shell in
    let shell_spec = [command_name] @ options @ [command_option] in
    ["shell", [String.concat ~sep:"," shell_spec]]
  in
  Uri.make ?scheme ?userinfo ?host ?port 
    ?path:(Option.map ~f:Path.to_string t.playground)
    ~query ()

let of_string s =
  let uri = Uri.of_string s in
  of_uri uri

let log t = 
  Log.(brakets ( s "Host " % s (Uri.to_string (to_uri t))))

let to_string_hum t = Log.to_long_string (log t)

module Error = struct

  type 'a execution = 'a constraint 'a =
  [> `Unix_exec of string
  | `Execution of
       <host : string; stdout: string option; stderr: string option; message: string>
  | `System of [> `Sleep of float ] * [> `Exn of exn ]
  | `Timeout of float
  | `Ssh_failure of
       [> `Wrong_log of string
       | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string ]

  type 'a non_zero_execution = 'a constraint 'a = 
    [> `Non_zero of (string * int) ] execution

  let classify (e : _ non_zero_execution) =
    match e with
    | `System _ | `Timeout _
    | `Unix_exec _ -> `Unix
    | `Execution _ | `Non_zero _ -> `Execution
    | `Ssh_failure _ -> `Ssh

  let log e =
    let kv k v = Log.(brakets (s k % s " → " % v)) in
    match e with
    | `Unix_exec failure -> Log.(s "Unix-exec-error: " % s failure)
    | `Non_zero (cmd, ex) -> Log.(s "Cmd " % sf "%S" cmd % s " returned " % i ex)
    | `System (`Sleep time, `Exn e) ->
      Log.(s "System error: sleep " % f time % s " failed: " % exn e)
    | `Timeout t -> Log.(s "Timed-out " % parens (f t % s " sec"))
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

type timeout = [ 
  | `Host_default
  | `None
  | `Seconds of float
  | `At_most_seconds of float
]

let default_timeout_upper_bound = ref 60.

let run_with_timeout ?timeout t ~run =
  let actual_timeout = 
    let pick_minimum f =
      match t.execution_timeout with
      | Some fe when fe < f -> Some fe
      | _ -> Some f
    in
    match timeout with
    | Some `None -> None
    | None -> pick_minimum !default_timeout_upper_bound
    | Some (`At_most_seconds f) -> pick_minimum f
    | Some (`Host_default) -> t.execution_timeout
    | Some (`Seconds t) -> Some t in
  let log = Log.(parens (s "timeout: " % OCaml.option f actual_timeout)) in
  match actual_timeout with
  | None -> run ~log ()
  | Some t -> 
    Deferred_list.pick_and_cancel [
      begin
        System.sleep t
        >>= fun () ->
        fail (`Timeout t)
      end;
      run ~log ();
    ]
    (* Pvem_lwt_unix.System.with_timeout t ~f:(run ~log) *)

let execute ?timeout t argl =
  let final_log = ref Log.empty in
  let ret out err exited =
    let kv k v = Log.(brakets (s k % s " → " % v) |> indent) in
    final_log := Log.(
        !final_log %n % s "Success: " % kv "status" (sf "exit:%d" exited)
        % kv "stdout" (sf "%S" out) % kv "stderr" (sf "%S" err));
    return (object
      method stdout = out method stderr = err method exited = exited
    end)
  in
  let run ~log () =
    final_log := Log.( !final_log % s "Host.execute " % s (to_string_hum t)
                       % OCaml.list s argl % sp % log);
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
  in
  begin run_with_timeout ?timeout t ~run
    >>< fun result ->
    Log.(!final_log @ verbose);
    match result with
    | `Ok o -> return o
    | `Error (`Host e) -> fail (`Host e)
    | `Error (`System _ as e)
    | `Error (`Timeout _ as e) -> fail (`Host e)
  end

type shell = string -> string list

let shell_sh ~sh cmd = [sh; "-c"; cmd]

let shell_of_default_shell t cmd = 
  let open Ketrew_gen_base_v0.Default_shell in
  let open Ketrew_gen_base_v0.Host in
  t.default_shell.command_name :: 
  t.default_shell.options
  @ [t.default_shell.command_option; cmd]

let override_shell ?with_shell t =
  let shell = Option.value ~default:(shell_of_default_shell t) with_shell in
  shell

let get_shell_command_output ?timeout ?with_shell t cmd =
  execute ?timeout t (override_shell ?with_shell t cmd)
  >>= fun execution ->
  match execution#exited with
  | 0 -> return (execution#stdout, execution#stderr)
  | n -> fail_host (`Non_zero (cmd, n))

let get_shell_command_return_value ?timeout ?with_shell t cmd =
  execute ?timeout t (override_shell ?with_shell t cmd)
  >>= fun execution ->
  return execution#exited
    
let run_shell_command ?timeout ?with_shell t cmd =
  get_shell_command_output ?timeout ?with_shell t cmd
  >>= fun (_, _) ->
  return ()


let do_files_exist ?timeout ?with_shell t paths =
  let cmd =
    List.map paths ~f:Path.exists_shell_condition 
    |> String.concat ~sep:" && " in
  get_shell_command_return_value ?timeout ?with_shell t cmd
  >>= fun ret ->
  return (ret = 0)

let get_fresh_playground t = 
  let fresh = Unique_id.create () in
  Option.map  t.playground (fun pg ->
      Path.(concat pg (relative_directory_exn fresh)))

let ensure_directory ?timeout ?with_shell t ~path =
  let cmd = fmt "mkdir -p %s" Path.(to_string_quoted path) in
  run_shell_command ?timeout ?with_shell t cmd

let put_file ?timeout t ~path ~content =
  match t.connection with
  | `Localhost -> IO.write_file ~content Path.(to_string path)
  | `Ssh ssh ->
    let temp = Filename.temp_file "ketrew" "ssh_put_file" in
    let run ~log () =
      IO.write_file ~content temp
      >>= fun () ->
      let scp_cmd = Ssh.(scp_push ssh ~src:[temp] ~dest:(Path.to_string path)) in
      begin Ketrew_unix_process.succeed scp_cmd
        >>< function
        | `Ok (out, err) -> return ()
        | `Error (`Process _ as process_error) ->
          let msg = Ketrew_unix_process.error_to_string process_error in
          Log.(s "Scp-cmd " % OCaml.list (sf "%S") scp_cmd  % sp % log
               % s " failed: " %s msg @ verbose);
          fail_exec t msg
      end
    in
    begin run_with_timeout ?timeout t ~run
      >>< function
      | `Ok o -> return o
      | `Error (`IO _ as e) -> fail e
      | `Error (`Host e) -> fail (`Host e)
      | `Error (`System _ as e)
      | `Error (`Timeout _ as e) -> fail (`Host e)
    end

let get_file ?timeout t ~path =
  match t.connection with
  | `Localhost -> 
    begin IO.read_file Path.(to_string path)
      >>< function
      | `Ok c -> return c
      | `Error (`IO (`Read_file_exn (path, ex))) ->
        Log.(s "I/O, writing " % s path % s " → " % exn ex @ verbose);
        fail (`Cannot_read_file ("localhost", path))
    end
  | `Ssh ssh ->
    let temp = Filename.temp_file "ketrew" "ssh_get_file" in
    let scp_cmd = Ssh.(scp_pull ssh ~dest:temp ~src:[Path.to_string path]) in
    begin run_with_timeout ?timeout t 
        ~run:(fun ~log () ->
            Ketrew_unix_process.succeed scp_cmd
            >>= fun _ ->
            IO.read_file temp)
      >>< function
      | `Ok c -> return c
      | `Error (`IO (`Read_file_exn (path, ex))) ->
        Log.(s "I/O, writing " % s path % s " → " % exn ex @ verbose);
        fail (`Cannot_read_file ("localhost", path))
      | `Error (`System (`Sleep time, `Exn e)) ->
        Log.(s "Scp-cmd " % OCaml.list (sf "%S") scp_cmd 
             % s " failed: System.sleep " % f time % s " error: " % exn e @ error);
        fail (`Cannot_read_file (ssh.Ssh.address, Path.(to_string path)))
      | `Error (`Process _ as process_error) ->
        let msg = Ketrew_unix_process.error_to_string process_error in
        Log.(s "Scp-cmd " % OCaml.list (sf "%S") scp_cmd 
             % s " failed: " %s msg @ verbose);
        fail (`Cannot_read_file (ssh.Ssh.address, Path.(to_string path)))
      | `Error (`Timeout _ as t) -> fail t
    end


let grab_file_or_log ?timeout host path =
  begin get_file ?timeout host ~path
    >>< function
    | `Ok c -> return c
    | `Error (`Cannot_read_file _) ->
      fail Log.(s "cannot read file" % s (Path.to_string path))
    | `Error (`IO _ as e) ->
      fail Log.(s "I/O error: " % s (IO.error_to_string e))
    | `Error (`Timeout time) ->
      fail Log.(s "Timeout: " % f time)
  end

