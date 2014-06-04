open Ketrew_pervasives

let display_info ~all ~item_format =
  fail 1

type user_todo = [
  | `Fail of Log.t
]

let cmdliner_main ?argv user_actions_term () =
  let open Cmdliner in
  let version = "0.0.0" in
    let info_cmd =
      let doc = "Get info about this instance." in
      let man = [] in
      Term.(
        pure (fun all item_format -> display_info ~item_format ~all)
        $ Arg.(value & flag & info ["A"; "all"] 
                 ~doc:"Display all processes even the completed ones.")
        $ Arg.(value & 
               opt string "- $key:\n  $name\n  $status_with_reason\n$history_list"
               & info ["-F"; "item-format"] ~docv:"FORMAT-STRING"
                 ~doc:"Use $(docv) as format for displaying jobs")
      ),
      Term.info "info" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man
    in
    let call_cmd =
      let doc = "Call a user-defined “command line”." in
      let man = [] in
      (Term.((
           pure (fun dry_run (todos: user_todo list) ->
               if dry_run
               then begin
                 return ()
               end else begin
                 fail 2
               end)
           $ Arg.(value & flag & info ["n"; "dry-run"]
                    ~doc:"Only display the TODO-items that would have been \
                          activated.")
           $ user_actions_term
         )),
       Term.info "call" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man)
    in
    let cmds =
      [info_cmd; call_cmd] in
    match Term.eval_choice ?argv info_cmd cmds with
    | `Ok f -> f
    | `Error _ -> exit 1
    | _ -> exit 0


let run_main ?argv additional_terms =
  match Lwt_main.run (cmdliner_main ?argv additional_terms ()) with
  | `Ok () -> exit 0
  | `Error n -> exit n
