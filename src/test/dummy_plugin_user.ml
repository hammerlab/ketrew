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

(*M

This is a workflow script using `Dummy_plugin` to create a (local) target.

M*)
open Printf
let () =
  let open Ketrew.EDSL in
  Ketrew.Client.submit_workflow (
    workflow_node without_product
      ~name:(sprintf "%S with dummy-plugin" Sys.argv.(1))
      ~make:(Dummy_plugin_test_lib.Dummy_plugin.create
               ~host:(Host.parse "/tmp")
               (Program.sh Sys.argv.(1)))
  )
