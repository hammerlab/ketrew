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
open Ketrew.Unix_io



let many_targets ~sexp ~host_str () =
  let open Ketrew.EDSL in
  let open Sexplib in
  let host = Host.parse host_str in
  let run cmd =
    daemonize ~using:`Python_daemon ~host Program.(sh cmd) in
  let tags = [Unique_id.create (); "test"; "synthetic"; "many_targets"] in
  let metadata = `String sexp in
  let count_nodes = ref 0 in
  let rec make_targets =
    function
    | Sexp.Atom cmd ->
      let make = run cmd in
      workflow_node ~make ~name:cmd ~tags:("atom" :: tags) ~metadata
        without_product
    | Sexp.List l ->
      let edges = List.map (fun v -> depends_on (make_targets v)) l in
      let name = incr count_nodes; fmt "node-%d" !count_nodes in
      workflow_node ~name ~edges ~tags:("list" :: tags) ~metadata
        without_product
  in
  Sexp.of_string sexp |> make_targets
(*
   
  kdsynth sexp view /tmp '()'
  kdsynth sexp view /tmp '("free" (((("finger $USER")))) "du -sh /tmp" ("echo bouh" "sleep 4") "ls -R $HOME" ("ls" "df" "ls /"))'



*)


let () =
  match Sys.argv |> Array.to_list |> List.tl_exn with
  | "sexp" :: "view" :: host_str :: sexp :: [] ->
    let str =
      many_targets ~sexp ~host_str () |> Ketrew.EDSL.workflow_to_string in
    Log.(s "Workflow: "%n % verbatim str @ normal);
    ()
  | "sexp" :: "submit" :: host_str :: sexp :: [] ->
    many_targets ~sexp ~host_str () |> Ketrew.Client.submit_workflow
  | other ->
    Log.(s "Usage: " %n
         %s "  ./...  sexp HOST SEXP " % n
         %s "Got: " % OCaml.list quote other
         @ error);
    exit 1
