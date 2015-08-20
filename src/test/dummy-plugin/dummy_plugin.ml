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

Dummy Plugin
============

This plugin “inherits” from the implementation of `Ketrew.Daemonize` and adds a
new “query” (that just retrives the result of the `date` command; pretty
useless).

M*)
open Ketrew_pure.Internal_pervasives
open Ketrew.Unix_io

(*M

The name has to be “unique”; the `create` function calls
`Ketrew.Daemonize.create` and changes the `name`:

M*)
let name = "dummy"

let create ~host program =
  let `Long_running  (_, serialized) =
    Ketrew.Daemonize.create ~using:`Python_daemon ~host program in
  `Long_running (name, serialized)

(*M
Implementation
--------------

The module `Another_long_running` is the actual implementation of the plugin.

The functions `additional_queries` and `query` deal with the `"date"` query,
or pass the baby to the `Ketrew.Daemonize` module.

M*)

module Another_long_running : Ketrew.Long_running.LONG_RUNNING = struct
  include Ketrew.Daemonize
  let name = "dummy"

  let additional_queries run_param =
    ("date", Log.(s "Display the date, not even on the right host"))
    :: additional_queries run_param

  let query run_param item =
    if item = "date" then
      begin Ketrew.Host_io.get_shell_command_output
          (Ketrew_pure.Host.of_string "/tmp") "date"
        >>< function
        | `Ok (o, _) -> return o
        | `Error e ->
          fail Log.(s "Command `date` failed: " % s (Ketrew.Error.to_string e))
      end
    else (* call Ketrew.Daemonize's function: *)
      query run_param item
end

(*M
Registration
------------
  
Registering the plugin is a simple function call using first class modules:
M*)
let () =
  Ketrew.Plugin.register_long_running_plugin ~name (module Another_long_running)


