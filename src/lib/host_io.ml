(**************************************************************************)
(*    Copyright 2014, 2015:                                               *)
(*          Sebastien Mondet <seb@mondet.org>,                            *)
(*          Leonid Rozenberg <leonidr@gmail.com>,                         *)
(*          Arun Ahuja <aahuja11@gmail.com>,                              *)
(*          Jeff Hammerbacher <jeff.hammerbacher@gmail.com>               *)
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

open Ketrew_pure.Internal_pervasives
open Unix_io

module Ssh = struct

  open Ketrew_pure.Host.Ssh

  (** Generate a proper SSH command for the given host. *)
  let do_ssh ssh command =
    ["ssh"; ssh_batch_option ssh]
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
        | None ->
          fail_parsing (fmt "Return value not an integer: %S" return_value)
        end
      | somehting_else -> fail_parsing "Cannot parse error log"
    in
    begin Unix_process.exec ssh_exec
      >>< function
      | `Ok (out, err, `Exited 0) -> parse_error_log out err
      | `Ok (out, err, other) ->
        fail (`Ssh_failure (`Wrong_status other, err))
      | `Error (`Process _ as process_error) ->
        let msg = Unix_process.error_to_string process_error in
        Log.(s "Ssh-cmd " % OCaml.list (sf "%S") ssh_exec
             % s " failed: " %s msg @ verbose);
        fail (`Unix_exec msg)
    end

  (** Generate an SCP command for the given host with the destination
      directory or file path. *)
  let scp_push ssh ~src ~dest =
    ["scp"; ssh_batch_option ssh]
    @ ssh.add_ssh_options
    @ (match ssh.port with
      | Some p -> ["-P"; Int.to_string p]
      | None -> [])
    @ src
    @ (match ssh.user with
      | None -> [fmt "%s:%s" ssh.address dest]
      | Some u -> [fmt "%s@%s:%s" u ssh.address dest])

  (** Generate an SCP command for the given host as source. *)
  let scp_pull  ssh ~src ~dest =
    ["scp"; ssh_batch_option ssh]
    @ ssh.add_ssh_options
    @ (match ssh.port with
      | Some p -> ["-P"; Int.to_string p]
      | None -> [])
    @ (List.map src ~f:(fun src_item ->
        match ssh.user with
        | None -> fmt "%s:%s" ssh.address src_item
        | Some u -> fmt "%s@%s:%s" u ssh.address src_item))
    @ [dest]

end

module Error = struct

  type classified = [
    | `Fatal of string
    | `Recoverable of string
  ]

  
  type 'a execution = 'a constraint 'a =
  [> `Unix_exec of string
  | `Execution of
       <host : string; stdout: string option; stderr: string option; message: string>
  | `System of [> `Sleep of float ] * [> `Exn of exn ]
  | `Timeout of float
  | `Named_host_not_found of bytes
  | `Ssh_failure of
       [> `Wrong_log of string
       | `Wrong_status of Unix_process.Exit_code.t ] * string ]

  type 'a non_zero_execution = 'a constraint 'a =
    [> `Non_zero of (string * int) ] execution

  let classify (e : _ non_zero_execution) =
    match e with
    | `System _
    | `Unix_exec _ -> `Local_system
    | `Execution _
    | `Non_zero _ -> `Command_execution
    | `Timeout _
    | `Named_host_not_found _
    | `Ssh_failure _ -> `Connectivity

  let log e =
    let kv k v = Log.(brakets (s k % s " → " % v)) in
    match e with
    | `Unix_exec failure -> Log.(s "Unix-exec-error: " % s failure)
    | `Non_zero (cmd, ex) -> Log.(s "Cmd " % sf "%S" cmd % s " returned " % i ex)
    | `System (`Sleep time, `Exn e) ->
      Log.(s "System error: sleep " % f time % s " failed: " % exn e)
    | `Timeout t -> Log.(s "Timed-out " % parens (f t % s " sec"))
    | `Named_host_not_found name ->
      Log.(s "Named-host not found: " % quote name)
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
           % kv "Exit code" (Unix_process.Exit_code.to_log exit_code))
end

let fail_host e = fail (`Host e)

let fail_exec t ?out ?err msg:
  (_, [> `Host of _ Error.execution ]) Deferred_result.t =
  let v = object
    method host = Ketrew_pure.Host.to_string_hum t
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


type host = Ketrew_pure.Host.t

type t = {
  named_hosts: (string, Ketrew_pure.Host.Ssh.t) Hashtbl.t;
}
let create () = {named_hosts = Hashtbl.create 42}

let resolve_named_host t ~name ~host =
  try
    let ssh_connection = Hashtbl.find t.named_hosts name in
    return (Ketrew_pure.Host.with_ssh_connection host ssh_connection)
  with _ -> fail_host (`Named_host_not_found name)

let set_named_host t ~name ssh =
  Hashtbl.add t.named_hosts name ssh;
  return ()

let delete_named_host t ~name =
  Hashtbl.remove t.named_hosts name;
  return ()


(* TODO: put this in `t` *)
let default_timeout_upper_bound = ref 60.

let run_with_timeout ?timeout t ~host ~run =
  let actual_timeout =
    let pick_minimum f =
      match Ketrew_pure.Host.execution_timeout host  with
      | Some fe when fe < f -> Some fe
      | _ -> Some f
    in
    match timeout with
    | Some `None -> None
    | None -> pick_minimum !default_timeout_upper_bound
    | Some (`At_most_seconds f) -> pick_minimum f
    | Some (`Host_default) -> Ketrew_pure.Host.execution_timeout host 
    | Some (`Seconds s) -> Some s in
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

let execute ?timeout t ~host argl =
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
  let rec run ~log ~host () =
    final_log := Log.( !final_log % s "Host.execute "
                       % s (Ketrew_pure.Host.to_string_hum host)
                       % OCaml.list s argl % sp % log);
    match Ketrew_pure.Host.connection host  with
    | `Localhost ->
      begin Unix_process.exec argl
        >>< function
        | `Ok (out, err, `Exited n) -> ret out err n
        | `Ok (out, err, other) ->
          fail_exec host ~out ~err (System.Shell.status_to_string other)
        | `Error (`Process _ as process_error) ->
          let msg = Unix_process.error_to_string process_error in
          Log.(s "Ssh-cmd " % OCaml.list (sf "%S") argl
               % s " failed: " %s msg @ verbose);
          fail_exec host msg
      end
    | `Ssh ssh ->
      begin Ssh.generic_ssh_exec ssh argl
        >>< function
        | `Ok (out, err, exited) -> ret out err exited
        | `Error e -> fail (`Host e)
      end
    | `Named name ->
      resolve_named_host t ~name ~host
      >>= fun host ->
      run ~log ~host ()
  in
  begin run_with_timeout ?timeout t ~host ~run:(run ~host)
    >>< fun result ->
    Log.(!final_log @ very_verbose);
    match result with
    | `Ok o -> return o
    | `Error (`Host e) -> fail (`Host e)
    | `Error (`System _ as e)
    | `Error (`Timeout _ as e) -> fail (`Host e)
  end

type shell = string -> string list

let shell_sh ~sh cmd = [sh; "-c"; cmd]


let override_shell ?with_shell t =
  let shell =
    Option.value ~default:(Ketrew_pure.Host.shell_of_default_shell t) with_shell
  in
  shell

let get_shell_command_output ?timeout ?with_shell t ~host cmd =
  execute ?timeout t ~host (override_shell ?with_shell host cmd)
  >>= fun execution ->
  match execution#exited with
  | 0 -> return (execution#stdout, execution#stderr)
  | n -> fail_host (`Non_zero (cmd, n))

let get_shell_command_return_value ?timeout ?with_shell t ~host cmd =
  execute ?timeout t ~host (override_shell ?with_shell host cmd)
  >>= fun execution ->
  return execution#exited

let run_shell_command ?timeout ?with_shell t ~host cmd =
  get_shell_command_output ?timeout ?with_shell t ~host cmd
  >>= fun (_, _) ->
  return ()


let do_files_exist ?timeout ?with_shell t ~host paths =
  let cmd =
    List.map paths ~f:Ketrew_pure.Path.exists_shell_condition
    |> String.concat ~sep:" && " in
  get_shell_command_return_value ?timeout ?with_shell t ~host cmd
  >>= fun ret ->
  return (ret = 0)

let get_fresh_playground t ~host =
  let fresh = Unique_id.create () in
  Option.map (Ketrew_pure.Host.playground host) (fun pg ->
      Ketrew_pure.Path.(concat pg (relative_directory_exn fresh)))

let ensure_directory ?timeout ?with_shell t ~host ~path =
  let cmd = fmt "mkdir -p %s" Ketrew_pure.Path.(to_string_quoted path) in
  run_shell_command ?timeout ?with_shell t ~host cmd

let rec put_file ?timeout t ~host ~path ~content =
  match Ketrew_pure.Host.connection host with
  | `Localhost -> IO.write_file ~content Ketrew_pure.Path.(to_string path)
  | `Named name ->
    resolve_named_host t ~name ~host
    >>= fun host ->
    put_file ?timeout t ~host ~path ~content
  | `Ssh ssh ->
    let temp = Filename.temp_file "ketrew" "ssh_put_file" in
    let run ~log () =
      IO.write_file ~content temp
      >>= fun () ->
      let scp_cmd =
        Ssh.(scp_push ssh ~src:[temp] ~dest:(Ketrew_pure.Path.to_string path)) in
      begin Unix_process.succeed scp_cmd
        >>< function
        | `Ok (out, err) -> return ()
        | `Error (`Process _ as process_error) ->
          let msg = Unix_process.error_to_string process_error in
          Log.(s "Scp-cmd " % OCaml.list (sf "%S") scp_cmd  % sp % log
               % s " failed: " %s msg @ verbose);
          fail_exec host msg
      end
    in
    begin run_with_timeout ?timeout t ~host ~run
      >>< function
      | `Ok o -> return o
      | `Error (`IO _ as e) -> fail e
      | `Error (`Host e) -> fail (`Host e)
      | `Error (`System _ as e)
      | `Error (`Timeout _ as e) -> fail (`Host e)
    end

let rec get_file ?timeout t ~host ~path =
  let open Ketrew_pure in
  match Ketrew_pure.Host.connection host with
  | `Localhost ->
    begin IO.read_file Path.(to_string path)
      >>< function
      | `Ok c -> return c
      | `Error (`IO (`Read_file_exn (path, ex))) ->
        Log.(s "I/O, writing " % s path % s " → " % exn ex @ verbose);
        fail (`Cannot_read_file ("localhost", path))
    end
  | `Named name ->
    resolve_named_host t ~name ~host
    >>= fun host ->
    get_file ?timeout t ~host ~path
  | `Ssh ssh ->
    let temp = Filename.temp_file "ketrew" "ssh_get_file" in
    let scp_cmd = Ssh.(scp_pull ssh ~dest:temp ~src:[Path.to_string path]) in
    begin run_with_timeout ?timeout t ~host
        ~run:(fun ~log () ->
            Unix_process.succeed scp_cmd
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
        fail (`Cannot_read_file (ssh.Ketrew_pure.Host.Ssh.address,
                                 Path.(to_string path)))
      | `Error (`Process _ as process_error) ->
        let msg = Unix_process.error_to_string process_error in
        Log.(s "Scp-cmd " % OCaml.list (sf "%S") scp_cmd
             % s " failed: " %s msg @ verbose);
        fail (`Cannot_read_file
                (ssh.Ketrew_pure.Host.Ssh.address, Path.(to_string path)))
      | `Error (`Timeout _ as t) -> fail t
    end


let grab_file_or_log ?timeout t ~host path =
  begin get_file ?timeout t ~host ~path
    >>< function
    | `Ok c -> return c
    | `Error (`Cannot_read_file _) ->
      fail Log.(s "cannot read file" % s (Ketrew_pure.Path.to_string path))
    | `Error (`IO _ as e) ->
      fail Log.(s "I/O error: " % s (IO.error_to_string e))
    | `Error (`Timeout time) ->
      fail Log.(s "Timeout: " % f time)
    | `Error (`Host (`Named_host_not_found name)) ->
      fail Log.(s "Named host not found: " % quote name)
  end
