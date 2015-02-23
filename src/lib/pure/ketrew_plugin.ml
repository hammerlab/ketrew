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


open Ketrew_long_running
module Daemonize = Ketrew_daemonize

let default_plugins = [
  Daemonize.name, (module Daemonize: LONG_RUNNING);
  Ketrew_lsf.name, (module Ketrew_lsf: LONG_RUNNING);
  Ketrew_pbs.name, (module Ketrew_pbs: LONG_RUNNING);
]
let global_list_of_plugins: (string * (module LONG_RUNNING)) list ref =
  ref default_plugins

let register_long_running_plugin ~name m =
  global_list_of_plugins := (name, m) :: !global_list_of_plugins

let dynlink path =
  wrap_preemptively (fun () ->
      let adapted = Dynlink.adapt_filename path in
      Log.(s "Loading: " % quote adapted @ verbose);
      Dynlink.loadfile adapted
    )
    ~on_exn:(function
      | Dynlink.Error e -> `Dyn_plugin (`Dynlink_error e)
      | other ->
        `Failure (fmt "Unknown dynlink-error: %s" (Printexc.to_string other))
      )

let ketrew_deep_ancestors () =
  Lazy.force (
    lazy (
      Findlib.package_deep_ancestors ["native"] Ketrew_metadata.findlib_packages
    ))

let load_plugins plugins_to_load =
  wrap_preemptively Findlib.init ~on_exn:(fun e -> `Dyn_plugin (`Findlib e))
  >>= fun () ->
  Deferred_list.while_sequential plugins_to_load ~f:(function
    | `Compiled path -> dynlink path
    | `OCamlfind package ->
      let predicates = ["native"; "plugin"; "mt"] in
      let deps = Findlib.package_deep_ancestors predicates [package] in
      let to_load =
        List.concat_map deps ~f:(fun dep ->
            if List.mem dep ~set:(ketrew_deep_ancestors ())
            then []
            else (
              let base = Findlib.package_directory dep in
              let archives =
                try
                  Findlib.package_property predicates dep "archive"
                  |> String.split ~on:(`Character ' ')
                  |> List.filter ~f:((<>) "")
                  |> List.map ~f:(Findlib.resolve_path ~base)
                with _ ->  []
              in
              archives
            ))
      in
      Log.(s "Going to load: " % OCaml.list quote to_load @ verbose);
      Deferred_list.while_sequential to_load ~f:dynlink
      >>= fun (_ : unit list) ->
      return ()
    )
  >>= fun (_ : unit list) ->
  return ()

let find_plugin plugin_name =
  List.find !global_list_of_plugins (fun (n, _) -> n = plugin_name)
  |> Option.map ~f:(fun (_, m) -> m)

let long_running_log plugin_name content =
  begin match find_plugin plugin_name with
  | Some m ->
    let module Long_running = (val m : LONG_RUNNING) in
    begin try
      let c = Long_running.deserialize_exn content in
      Long_running.log c
    with e -> 
      let log = Log.(s "Serialization exception: " % exn e) in
      Log.(log @ error);
      ["Error", log]
    end
  | None -> 
    let log = Log.(s "Plugin not found: " % sf "%S" plugin_name) in
    Log.(log @ error);
    ["Error", log]
  end

let additional_queries target =
  let module Target = Ketrew_target in
  match Target.(build_process target) with
  | `Long_running (plugin, _) ->
    begin match Target.latest_run_parameters target with
    | Some rp ->
      begin match find_plugin plugin with
      | Some m ->
        let module Long_running = (val m : LONG_RUNNING) in
        begin try
          let c = Long_running.deserialize_exn rp in
          Long_running.additional_queries c
        with e -> 
          let log = Log.(s "Serialization exception: " % exn e) in
          Log.(log @ error);
          []
        end
      | None ->
        let log = Log.(s "Plugin not found: " % sf "%S" plugin) in
        Log.(log @ error);
        []
      end
    | None ->
      Log.(s "Target has no run-parameters: " % Target.log target @ error);
      []
    end
  | other -> []


let call_query ~target query =
  let module Target = Ketrew_target in
  match Target.build_process target with
  | `Long_running (plugin, _) ->
    begin match Target.latest_run_parameters target with
    | Some rp ->
      begin match find_plugin plugin with
      | Some m ->
        let module Long_running = (val m : LONG_RUNNING) in
        begin try
          let c = Long_running.deserialize_exn rp in
          Long_running.query c query
        with e ->
          fail Log.(s "Run-parameters deserialization" % exn e)
        end
      | None ->
        let log = Log.(s "Plugin not found: " % sf "%S" plugin) in
        fail log
      end
    | None -> fail Log.(s "Target has no run-parameters: " % Target.log target)
    end
  | other -> fail Log.(s "Target has no queries: " % Target.log target)

