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
  mutable db: Dbm.t;
  (* mutable history: (action * stupid_db) list; *)
  parameters: string;
}
let create db parameters = {db; parameters} 

(* let save t = *)
(*   let content = Ketrew_gen_base_v0_j.string_of_database t in *)
(*   IO.write_file t.parameters ~content *)

exception In_thread_exception of string
let failwithf fmt = Printf.ksprintf (fun s -> raise (In_thread_exception s)) fmt

let wrap_dbm_call ~error_location f =
  Lwt_preemptive.detach (fun () ->
      try
        `Ok (f ())
      with
      | Dbm.Dbm_error s ->
        `Error (`Database (error_location, s))
      | In_thread_exception s ->
        `Error (`Database (error_location, s))
    ) ()
  >>< function
  | `Ok o -> return o
  | `Error (`Database _ as e) -> fail e

let load path =
  wrap_dbm_call (fun () -> 
      let db =
        Dbm.opendbm path [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o600 in
      at_exit (fun () -> Dbm.close db);
      db
    )
    ~error_location:(`Load path)
  >>= fun db ->
  return (create db path)

let close t =
  wrap_dbm_call (fun () -> Dbm.close t.db) ~error_location:(`Close)

let dbm_get t key = try Some (Dbm.find t.db key) with Not_found ->  None

let get t ~key =
  wrap_dbm_call (fun () -> dbm_get t key) ~error_location:(`Get key)
  >>= fun o ->
  Log.(s "got " % sf "%S" key % s ", " % OCaml.option s o  @ local_verbosity ());
  return o

(* Sqlite3 does not work on Maxosx, Dbm does not have transactions,
   so here is a non-ACID transaction implementation. *)
let act_thread t ~action =
  let open Result in
  let go_backwards = ref [] in
  let rec go act = 
    Log.(s "DB: " % s "go " % log_action act @ local_verbosity ());
    match act with
    | Set (key, value) ->
      let previous = 
        match dbm_get t key with
        | None -> Unset key
        | Some s -> Set (key, s)
      in
      Dbm.replace t.db key value;
      go_backwards :=  previous :: !go_backwards;
      Log.(s "success: " % log_action act @ local_verbosity ());
      true
    | Unset key ->
      begin match dbm_get t key with
      | None -> true (* nothing to do *)
      | Some s ->
        Dbm.remove t.db key;
        go_backwards := Set (key, s) :: !go_backwards;
        true
      end
    | Check (key, value) ->
      let indb = dbm_get t key in
      indb = value 
    | Sequence actions -> List.for_all actions go
  in
  if go action 
  then `Done
  else (if go (Sequence !go_backwards)
        then  `Not_done 
        else
          failwithf "fatal: transaction failed to go back")

let act t ~action =
  wrap_dbm_call (fun () -> act_thread t ~action) 
    ~error_location:(`Act action)

