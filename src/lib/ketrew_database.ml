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

type key = {key: string ; collection: string option}

type action =
  | Set of key * string
  | Unset of key
  | Sequence of action list
  | Check of key * string option
let _key ?collection key = {key; collection}
let set ?collection ~key value = Set (_key ?collection key, value)
let seq l = Sequence l
let contains ?collection ~key v = Check (_key ?collection key, Some v) 
let is_not_set ?collection key = Check (_key ?collection key, None)
let unset ?collection key = Unset (_key ?collection key)

let log_key {key; collection} = 
  let open Log in
  match collection with
  | Some c -> s c % s "/" % s key
  | None -> s key
let key_to_string {key;collection} =
  Option.value_map collection ~f:(fmt "%s/") ~default:"" ^ key

let rec log_action =
  let open Log in
  function
  | Set (k, v) -> brakets(s "Set " % log_key k % sp  % s v)
  | Check (k, vo) -> brakets(s "Check " % log_key k % sp % OCaml.option s vo)
  | Unset k -> brakets(s "Uet " % log_key k)
  | Sequence l -> OCaml.list log_action l

type error =
  [ `Act of action | `Get of key | `Get_all of string
  | `Load of string | `Close ] * string

let log_error = function
| `Database ((what, msg) : error) ->
  Log.(s "Database" % sp
       % parens (match what with
         | `Load path -> s "Loading " % s path
         | `Get k -> s "Getting " % log_key  k
         | `Get_all c -> s "Getting collection " % quote c
         | `Act a -> s "Processing " % log_action a
         | `Close -> s "Closing")
       % s " → "
       % s msg)

module Cache = struct

  type collection = {
    hashtbl: (string, string option) Hashtbl.t;
  }
  module String_map = Map.Make(String)
  type t = {
    mutable map: collection String_map.t;
  }
  let create () = { map = String_map.empty; }
  let add_or_replace t ?(collection="") ~key v =
    try
      let {hashtbl} = String_map.find collection t.map in
      Hashtbl.replace hashtbl key v
    with
    | _ ->
      let hashtbl = Hashtbl.create 42 in
      Hashtbl.replace hashtbl key v;
      t.map <- String_map.add collection {hashtbl} t.map;
      ()

  let get t ?(collection="") ~key = 
    try
      let {hashtbl} = String_map.find collection t.map in
      `Set (Hashtbl.find hashtbl key)
    with _ -> `Unset

  let remove t ?(collection="") ~key =
    try
      let {hashtbl} = String_map.find collection t.map in
      Hashtbl.remove hashtbl key
    with _ -> ()

end


type t = {
  path: string;
  mutex: Lwt_mutex.t;
  exec_style: [`Shell | `Exec];
  cache: Cache.t;
}
let create path =
  {exec_style = `Exec; mutex = Lwt_mutex.create (); 
   path; cache = Cache.create ()} 

module Debug = struct

  type t =  No | After_write of string 
         | After_git_add of string  | After_git_rm of string 
  let global_debug = ref No

  exception E
  let after_write k =
    match !global_debug with
    | After_write s when s = k.key ->
      Log.(s "Throwing Debug exn: After_write" % sp % log_key k @ warning);
      raise E
    | _ -> ()

  let after_git_add k  =
    match !global_debug with
    | After_git_add s when s = k.key ->
      Log.(s "Throwing Debug exn: After_git_add" % sp % log_key k @ warning);
      raise E
    | _ -> ()

  let after_git_rm k  =
    match !global_debug with
    | After_git_rm s when s = k.key ->
      Log.(s "Throwing Debug exn: After_git_rm" % sp % log_key k @ warning);
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
  let creation_witness_relative =  (Filename.basename creation_witness) in
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
      call_git  ~loc:(`Load path)  t ["init"] >>= fun () ->
      call_git  ~loc:(`Load path) t
        ["config"; "user.email"; "ketrew@ketrew.org"]
      >>= fun () ->
      call_git  ~loc:(`Load path) t
        ["config"; "user.name"; "Ketrew"]
      >>= fun () ->
      IO.write_file creation_witness ~content:"OK"
      >>= fun () ->
      call_git  ~loc:(`Load path)  t ["add"; creation_witness_relative]
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

let sanitize k  = 
  match k with
  | "" -> ".empty"
  | other ->
    String.map other ~f:(function
      | '/' -> '_'
      | '.' -> '_'
      | e -> e)

let path_of_key t {key ; collection} =
  begin match collection with
  | Some c ->
    let coldir = sanitize c in
    let dir = t.path // coldir in
    System.ensure_directory_path ~perm:0o700 dir
    >>= fun () ->
    return (coldir // (sanitize key))
  | None -> return (sanitize key)
  end
  >>= fun relative ->
  return (object
    method relative = relative
    method absolute = t.path // relative
  end)

let get_no_mutex t ~key =
  begin
    path_of_key t key
    >>= fun path ->
    IO.read_file path#absolute
  end
  >>< function
  | `Ok o -> return (Some o)
  | `Error (`IO (`Read_file_exn (s, e))) ->
    return None
  | `Error (`System _ as e) ->
    fail (`Database (`Get key, System.error_to_string e))

let cleap_up t ~loc =
  let checkout_master = ["checkout"; "master"; "-f"] in
  call_git t ~loc checkout_master
  >>= fun () ->
  call_git t ~loc ["clean"; "-f"]

let get ?collection t ~key =
  let metakey = _key ?collection key in
  Lwt_mutex.with_lock t.mutex (fun () -> 
      match Cache.get t.cache ?collection ~key with
      | `Set v -> return v
      | `Unset ->
        cleap_up t ~loc:(`Get metakey)
        >>= fun () ->
        get_no_mutex t ~key:metakey
        >>= fun v ->
        Cache.add_or_replace t.cache ?collection ~key v;
        return v
    )

let get_all t ~collection =
  Lwt_mutex.with_lock t.mutex (fun () -> 
      cleap_up t ~loc:(`Get_all collection)
      >>= fun () ->
      let path = (t.path // sanitize collection) in
      System.file_info ~follow_symlink:true path
      >>= fun file_info ->
      begin match file_info with
      | `Directory ->
        let stream = Lwt_unix.files_of_directory path in
        let rec build_list acc =
          wrap_deferred ~on_exn:(fun e -> `Ls e) (fun () ->
              Lwt_stream.get stream)
          >>= begin function
          | None -> return acc
          | Some p ->
            System.file_info ~follow_symlink:true (path // p)
            >>= fun file_info ->
            begin match file_info with
            | `Regular_file _ ->
              IO.read_file (path // p)
              >>= fun v ->
              build_list (v :: acc)
            | _ ->
              build_list acc
            end
          end
        in
        build_list []
      | other -> return []
      end
    )
  >>< let dfail s = fail (`Database (`Get_all collection, s)) in
    function
    | `Ok o -> return o
    | `Error (`Database _ as e) -> fail e
    | `Error (`IO _ as e) -> dfail (IO.error_to_string e)
    | `Error (`Ls e) -> 
      Log.(s "Ls in the collection failed: " % exn e @ verbose);
      return []
    | `Error (`System _ as e) ->
      dfail (System.error_to_string e)

let act t ~action =
  let branch_name = Unique_id.create () in
  let call_git = call_git ~loc:(`Act action) t in
  let rec go =
    function
    | Set (key, value) ->
      path_of_key t key
      >>= fun path ->
      IO.write_file path#absolute ~content:value
      >>= fun () ->
      Debug.after_write key;
      call_git ["add"; path#relative]
      >>= fun () ->
      Debug.after_git_add key;
      let msg = fmt "Set %s" (key_to_string key) in
      call_git ["commit"; "--allow-empty"; "-m"; msg]
    | Unset key ->
      path_of_key t key
      >>= fun path ->
      call_git ["rm"; "--ignore-unmatch"; path#relative]
      >>= fun () ->
      Debug.after_git_rm key;
      let msg = fmt "UnSet %s" (key_to_string key) in
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
  let rec go_cache =
    function
    | Set (key, value) ->
      Cache.add_or_replace t.cache
        ?collection:key.collection ~key:key.key (Some value)
    | Unset key ->
      Cache.add_or_replace t.cache
        ?collection:key.collection ~key:key.key None
    | Check (key, value_opt) -> ()
    | Sequence l -> List.iter l ~f:go_cache
  in
  begin
    Lwt_mutex.with_lock t.mutex (fun () ->
        Log.(s "Starting transaction " % s branch_name @ very_verbose);
        cleap_up t ~loc:(`Act action)
        >>= fun () ->
        call_git ["checkout"; "-b"; branch_name]
        >>= fun () ->
        go action
        >>= fun () ->
        call_git ["checkout"; "master"]
        >>= fun () ->
        call_git ["merge"; branch_name]
        >>= fun () ->
        go_cache action;
        (* update cache *)
        return ()
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
           % s "at" % sp % log_key key % sp % OCaml.(option string c) %sp
           % s " instead of " % OCaml.(option string v) @ verbose);
      return `Not_done
    | `Database (`Get k, s) ->
      fail (`Database (`Act action, fmt "getting %S: %s" (key_to_string k) s))
    | `Database (`Act _, _) as e -> fail e
    | `IO _ as e -> fail (`Database (`Act action, IO.error_to_string e))
    | `System _ as e -> fail (`Database (`Act action, System.error_to_string e))
    end

      

