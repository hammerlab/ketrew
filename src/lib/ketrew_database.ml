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

let global_debug_level = ref 4
let local_verbosity () = `Debug !global_debug_level

type action =
  | Set of string * string
  | Unset of string
  | Sequence of action list
  | Check of string * string option
let set ~key value = Set (key, value)
let seq l = Sequence l
let contains ~key v = Check (key, Some v) 
let is_not_set key = Check (key, None)
let unset key = Unset key

let rec log_action =
  let open Log in
  function
  | Set (k, v) -> brakets(s "Set " % s k % sp % s v)
  | Check (k, vo) -> brakets(s "Check " % s k % sp % OCaml.option s vo)
  | Unset k -> brakets(s "Uet " % s k)
  | Sequence l -> OCaml.list log_action l

type error =
  [ `Act of action | `Get of string | `Load of string | `Close ] * string

let log_error = function
| `Database (what, msg) ->
  Log.(s "Database" % sp
       % parens (match what with
         | `Load path -> s "Loading " % s path
         | `Get k -> s "Getting " % s k
         | `Act a -> s "Processing " % log_action a
         | `Close -> s "Closing")
       % s " → "
       % s msg)

(* type stupid_db = Ketrew_gen_base_v0_t.stupid_db *)
type t = {
  (* mutable db: Dbm.t; *)
  (* mutable history: (action * stupid_db) list; *)
  path: string;
  mutex: Lwt_mutex.t;
  exec_style: [`Shell | `Exec];
}
let create path = {exec_style = `Shell; mutex = Lwt_mutex.create (); path} 

module Debug = struct

  type t =  No | After_write of string 
         | After_git_add of string  | After_git_rm of string 
  let global_debug = ref No

  exception E
  let after_write k =
    match !global_debug with
    | After_write s when s = k ->
      Log.(s "Throwing Debug exn: After_write" % sp % s k @ warning);
      raise E
    | _ -> ()

  let after_git_add k  =
    match !global_debug with
    | After_git_add s when s = k ->
      Log.(s "Throwing Debug exn: After_git_add" % sp % s k @ warning);
      raise E
    | _ -> ()

  let after_git_rm k  =
    match !global_debug with
    | After_git_rm s when s = k ->
      Log.(s "Throwing Debug exn: After_git_rm" % sp % s k @ warning);
      raise E
    | _ -> ()

end

let db_process_shell ~loc cmd =
  System.Shell.do_or_fail (String.concat ~sep:" " (List.map cmd ~f:Filename.quote))
  >>< function
  | `Ok () -> return ()
  | `Error e ->
    Log.(s "Database-command failure (Shell):"
         % sp % OCaml.(list string cmd) @ very_verbose);
    fail (`Database (loc, System.error_to_string e))

let db_process_exec ~loc cmd =
  Ketrew_unix_process.succeed cmd
  >>< function
  | `Ok (_, _) -> return ()
  | `Error e ->
    Log.(s "Database-command failure (exec):"
         % sp % OCaml.(list string cmd) @ very_verbose);
    fail (`Database (loc, Ketrew_unix_process.error_to_string e))

let call_git ~loc t cmd =
  let actualexec = [
    "git"; "--git-dir"; Filename.concat t.path ".git"; "--work-tree"; t.path;
  ] @ cmd in
      match t.exec_style with
      | `Shell -> db_process_shell ~loc actualexec
      | `Exec -> db_process_exec ~loc actualexec

let load init_path =
  let path = 
    if Filename.is_relative init_path
    then Filename.concat (Sys.getcwd ()) init_path
    else init_path in
  let creation_witness =  (Filename.concat path "_ketrew_database_init") in
  begin
    System.file_info ~follow_symlink:true creation_witness
    >>= fun file_info ->
    begin match file_info with
    | `Regular_file _ -> 
      let t = create path in
      call_git  ~loc:(`Load path)  t ["checkout"; "master"]
      >>= fun () ->
      return t
    | `Absent ->
      System.ensure_directory_path ~perm:0o700 path
      >>= fun () ->
      let t = create path in
      call_git  ~loc:(`Load path)  t ["init"]
      >>= fun () ->
      IO.write_file creation_witness ~content:"OK"
      >>= fun () ->
      call_git  ~loc:(`Load path)  t ["add"; creation_witness]
      >>= fun () ->
      call_git  ~loc:(`Load path)  t ["commit"; "-m"; "Initialize database"]
      >>= fun () ->
      return t
    | other ->
      fail (`Database (`Load path, fmt "%S not a file: %s" path 
                         (System.file_info_to_string other)))
    end
  end >>< function
  | `Ok o -> return o
  | `Error e ->
    begin match e with
    | `Database _ as e -> fail e
    | `IO _ as e -> fail (`Database (`Load path, IO.error_to_string e))
    | `System _ as e -> fail (`Database (`Load path, System.error_to_string e))
    end

let close t =
  return ()

let get_no_mutex t ~key =
  IO.read_file (Filename.concat t.path key)
  >>< function
  | `Ok o -> return (Some o)
  | `Error (`IO (`Read_file_exn (s, e))) ->
    return None

let checkout_master = ["checkout"; "master"; "-f"]

let get t ~key =
  Lwt_mutex.with_lock t.mutex (fun () -> 
      call_git t ~loc:(`Get key) checkout_master
      >>= fun () ->
      get_no_mutex t ~key)

let act t ~action =
  let branch_name = Unique_id.create () in
  let call_git = call_git ~loc:(`Act action) t in
  let rec go =
    function
    | Set (key, value) ->
      let path = Filename.concat t.path key in
      IO.write_file path ~content:value
      >>= fun () ->
      Debug.after_write key;
      call_git ["add"; path]
      >>= fun () ->
      Debug.after_git_add key;
      let msg = fmt "Set %s" key in
      call_git ["commit"; "--allow-empty"; "-m"; msg]
    | Unset key ->
      let path = Filename.concat t.path key in
      call_git ["rm"; "--ignore-unmatch"; path]
      >>= fun () ->
      Debug.after_git_rm key;
      let msg = fmt "UnSet %s" key in
      call_git ["commit"; "--allow-empty"; "-m"; msg]
    | Check (key, value_opt) ->
      get_no_mutex t key
      >>= fun content_opt ->
      if content_opt = value_opt
      then return ()
      else fail (`Check_failed (key, value_opt, content_opt))
    | Sequence l ->
      Deferred_list.while_sequential l ~f:go
      >>= fun (_ : unit list) ->
      return ()
  in
  begin
    Lwt_mutex.with_lock t.mutex (fun () ->
        Log.(s "Starting transaction " % s branch_name @ very_verbose);
        call_git checkout_master
        >>= fun () ->
        call_git ["checkout"; "-b"; branch_name]
        >>= fun () ->
        go action
        >>= fun () ->
        call_git ["checkout"; "master"]
        >>= fun () ->
        call_git ["merge"; branch_name]
      ) end
  >>< function
  | `Ok () -> return `Done
  | `Error e ->
    begin match e with
    | `Check_failed (key, v, c) -> 
      begin call_git ["branch"; "-a"] >>< fun _ -> return () end
      >>= fun () ->
      Log.(s "Database transaction" % sp % quote branch_name  % sp
           % s "failed because check failed:" % sp
           % s "at" % sp % OCaml.string key % sp % OCaml.(option string c) %sp
           % s " instead of " % OCaml.(option string v) @ verbose);
      return `Not_done
    | `Database (`Get k, s) -> fail (`Database (`Act action, 
                                                fmt "getting %S: %s" k s))
    | `Database (`Act _, _) as e -> fail e
    | `IO _ as e -> fail (`Database (`Act action, IO.error_to_string e))
    end

      

