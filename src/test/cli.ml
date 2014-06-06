(*

  This “test” provides a Ketrew command line with some default user defined
  targets.

*)

open Ketrew_pervasives

let db_file = "/tmp/ketrew_cli_test_database"

(*

A first dummy, non-EDSL, workflow that can compile the documentation, switch to
the gh-pages branch, commit the new version of the documentation, and got
back to the original branch.

It can also fail in many ways (e.g. Git checkout forbidden because of dirty
tree) which show ketrew's handling of errors in dependencies.

*)
let deploy_website more_args =
  let open Ketrew in
  let branch_file = Unique_id.create () in
  let write_branch =
    Target.(
      create ~name:"Get current branch"
        ~make:(`Direct_command (Command.shell
                              (fmt "git symbolic-ref --short HEAD > /tmp/%s" branch_file)))
        (`Volume Artifact.Volume.(create ~host:Host.localhost 
                                    ~root:(Path.absolute_directory_exn "/tmp")
                                    (file branch_file))))
  in
  let make_doc = 
    Target.(
      create ~name:"Make doc"
        ~make:(`Get_output (Command.shell "please.sh doc"))
        (`Value `String)) in
  let check_out_gh_pages =
    Target.(
      create ~name:"check-out-gh-pages"
        ~dependencies:[Target.id write_branch; Target.id make_doc]
        ~make:(`Get_output (Command.shell (fmt "git checkout gh-pages || git checkout -t origin/gh-pages")))
        (`Value `String))
  in
  let move_website =
    Target.(
      create ~name:"move-doc"
        ~dependencies:[Target.id check_out_gh_pages]
        ~make:(`Get_output (Command.shell (fmt "cp -r _doc/* .")))
        (`Value `String))
  in
  let commit_website =
    Target.(
      create ~name:"commit-doc"
        ~dependencies:[Target.id move_website]
        ~make:(`Get_output (Command.shell (fmt "git add api && git ci -a -m 'update website' ")))
        (`Value `String)) in
  let active =
    Target.(
      active ~name:"check-out-original-branch"
        ~dependencies:[Target.id commit_website]
        ~make:(`Get_output (Command.shell (fmt "[ -f /tmp/%s ] && git checkout `cat /tmp/%s`" branch_file branch_file)))
        (`Value `String))
  in
  Log.(s "branch file: " % s branch_file @ verbose);
  [`Make (active, [write_branch; check_out_gh_pages; make_doc; move_website; commit_website])]

let run_main () =
  let additional_term =
    let open Cmdliner in
    let all_args =
      let doc = " TODO: write some doc here " in
      Arg.(value & pos_all string [] & info [] ~docv:"SPECIFICATION" ~doc)
    in
    Term.(pure (function
      | "website" :: more_args -> deploy_website more_args
      | args -> 
        [`Fail Log.(s "Don't know what to do: " % OCaml.list s args)])
          $ all_args)
  in
  Ketrew.Command_line.run_main additional_term


let `Never_returns =
  let configuration = Ketrew_state.Configuration.create db_file () in
  run_main ~configuration ()
