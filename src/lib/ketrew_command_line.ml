open Ketrew_pervasives

type user_todo = [
  | `Fail of Log.t
  | `Make of Ketrew_target.t * Ketrew_target.t list
]

module Return_code = struct
  let user_todo_failure = 2
  let not_implemented = 3
  let cmdliner_error = 4
  let ketew_error = 5
  let wrong_command = 6

  let of_error = function
  | e -> 
    Log.(s "Error: " % s (Error.to_string e) @ error);
    ketew_error

  let transform_error = function
  | `Ok () -> return ()
  | `Error e -> fail (of_error e)

end

let display_info ~state ~all ~item_format =
  begin
    Log.(s "display_info !" @ verbose);
    Ketrew_state.current_targets state
    >>= fun targets ->
    let format_target t =
      let open Ketrew_target in
      let buf = Buffer.create 42 in
      Buffer.add_substitute buf (function
        | "n" -> "\n"
        | "id" -> id t
        | "name" -> t.name
        | "dependencies" -> String.concat t.dependencies ~sep:", "
        | "status" -> 
          begin match t.history with
          | `Created _ -> "Created"
          | `Activated _ -> "Activated"
          | `Running _ -> "Running"
          | `Dead _ -> "Dead"
          | `Successful _ -> "Successful"
          end
        | some_unknown -> fmt "$%s" some_unknown) item_format;
      Buffer.contents buf
    in
    List.iter targets (fun t -> format_target t |> print_string);
    return ()
  end
  >>< Return_code.transform_error

let with_state ?plugins ~configuration f =
  Ketrew_state.create ?plugins configuration
  >>= fun state ->
  f ~state

let log_user_todo = function
| `Fail l -> Log.(s "Fail with: " % brakets l)
| `Make (act, deps) -> 
  Log.(s "Make target :" % Ketrew_target.log act
       % (match deps with
         | [] -> empty
         | more ->
           parens (s "with " % separate (s ", ") 
                     (List.map ~f:Ketrew_target.log more))))

let run_user_todo_list ~state todo_list =
  Deferred_list.while_sequential todo_list (function
    | `Make (active, dependencies) ->
      begin 
        Deferred_list.while_sequential (active :: dependencies) (fun t ->
            Ketrew_state.add_target state t)
        >>= fun (_ : unit list) ->
        return ()
      end
      >>< Return_code.transform_error
    | `Fail l ->
      Log.(s "Fail: " % l @ error);
      fail Return_code.user_todo_failure)
  >>= fun (_ : unit list) ->
  return ()

let log_list ~empty l =
  let empty_log = empty in (* renaming because og Log.empty *)
  let open Log in
  let if_empty = sp % empty_log in
  match l with 
  | [] -> if_empty
  | more ->
    n % indent (separate n (List.map more ~f:(fun item -> s "- " % item)))

let run_state ~state ~how =
  let log_happening ~what_happened =
    let open Log in
    let step_count = List.length what_happened in
    let happening_list =
      List.mapi what_happened ~f:(fun step_index happening_list ->
          List.map happening_list ~f:(fun happening ->
              brakets (s "step " % i (step_index + 1)) % sp % 
              s (Ketrew_state.what_happened_to_string happening)))
      |> List.concat
    in
    let step_sentence =
      match step_count with
      | 0 -> s "No step was executed"
      | 1 -> s "One step was executed" 
      | more -> i more % s " steps were executed"
    in
    Log.(step_sentence % s ":"  %
         log_list ~empty:(s "Nothing happened") happening_list
         @ normal); 
    return ()
  in
  begin match how with
  | ["step"] ->
    Ketrew_state.step state
    >>= fun what_happened ->
    log_happening ~what_happened:[what_happened]
  | ["fix"] ->
    let rec fix_point ~count history =
      Ketrew_state.step state
      >>= fun what_happened ->
      let count = count + 1 in
      begin match history with
      | _ when what_happened = [] ->
        return (count, what_happened :: history)
      | previous :: _ when previous = what_happened ->
        return (count, what_happened :: history)
      | _ -> fix_point ~count (what_happened :: history)
      end
    in
    fix_point 0 []
    >>= fun (step_count, what_happened) ->
    log_happening ~what_happened:List.(rev what_happened)
  | sl -> 
    Log.(s "Unknown state-running command: " % OCaml.list (sf "%S") sl
         @ error);
    fail (`Wrong_command_line sl)
  end
  >>< Return_code.transform_error

let cmdliner_main ?plugins ~configuration ?argv user_actions_term () =
  let open Cmdliner in
  let version = Ketrew_version.version in
  let sub_command ~info ~term = (term, info) in
  let info_cmd =
    sub_command
      ~info:(Term.info "info" ~version ~sdocs:"COMMON OPTIONS" ~man:[]
               ~doc:"Get info about this instance.")
      ~term: Term.(
          pure (fun all item_format ->
              with_state ?plugins ~configuration
                (display_info ~item_format ~all))
          $ Arg.(value & flag & info ["A"; "all"] 
                   ~doc:"Display all processes even the completed ones.")
          $ Arg.(value
                 & opt string
                   "- $id:$n  Name: $name → $status\n\
                   \  Deps: $dependencies$n"
                 & info ["-F"; "item-format"] ~docv:"FORMAT-STRING"
                   ~doc:"Use $(docv) as format for displaying jobs")
        ) in
  let call_cmd =
    let open Term in
    sub_command
      ~info:(info "call" ~version ~sdocs:"COMMON OPTIONS" 
               ~doc:"Call a user-defined “command line”"
               ~man:[])
      ~term:(
        pure (fun dry_run (todos: user_todo list) ->
            if dry_run
            then begin
              let log =
                List.mapi todos (fun idx todo ->
                    Log.(s "- " % brakets (i idx) 
                         % s ": " % log_user_todo todo)) in
              Log.(s "Would do:" %n % separate n log @ normal);
              return ()
            end else begin
              with_state ?plugins ~configuration (run_user_todo_list todos)
            end)
        $ Arg.(value & flag & info ["n"; "dry-run"]
                 ~doc:"Only display the TODO-items that would have been \
                       activated.")
        $ user_actions_term) in
    let run_cmd =
      let open Term in
      sub_command
        ~term:(
          pure (fun how ->
              with_state ?plugins ~configuration (run_state ~how))
          $ Arg.(non_empty @@ pos_all string [] @@
                 info [] ~docv:"HOW" ~doc:"Use $(docv) method: `step`"))
        ~info:(
          info "run" ~version ~sdocs:"COMMON OPTIONS" 
            ~doc:"Run steps of the engine." 
            ~man:[])
    in
    let default_cmd = 
      let doc = "A Workflow Engine for Complex Experimental Workflows" in 
      let man = [] in
      sub_command
        ~term:Term.(ret (pure (`Help (`Plain, None))))
        ~info:(Term.info "ketrew" ~version ~doc ~man) in
    let cmds = [info_cmd; call_cmd; run_cmd] in
    match Term.eval_choice ?argv default_cmd cmds with
    | `Ok f -> f
    | `Error _ -> exit Return_code.cmdliner_error
    | `Version | `Help -> exit 0


let run_main ?plugins ?argv ~configuration additional_terms =
  match Lwt_main.run (cmdliner_main ?plugins ?argv ~configuration additional_terms ()) with
  | `Ok () -> exit 0
  | `Error n -> exit n
