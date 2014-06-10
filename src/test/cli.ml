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
        ~make:(`Direct_command (Command.shell "please.sh doc"))
        (`Value `Unit)) in
  let check_out_gh_pages =
    Target.(
      create ~name:"check-out-gh-pages"
        ~dependencies:[Target.id write_branch; Target.id make_doc]
        ~make:(`Direct_command (Command.shell (fmt "git checkout gh-pages || git checkout -t origin/gh-pages")))
        (`Value `Unit))
  in
  let move_website =
    Target.(
      create ~name:"move-doc"
        ~dependencies:[Target.id check_out_gh_pages]
        ~make:(`Direct_command (Command.shell (fmt "cp -r _doc/* .")))
        (`Value `Unit)) in
  let commit_website =
    Target.(
      create ~name:"commit-doc"
        ~dependencies:[Target.id move_website]
        ~make:(`Direct_command (Command.shell (fmt "git add api && git ci -a -m 'update website' ")))
        (`Value `Unit)) in
  let active =
    Target.(
      active ~name:"check-out-original-branch"
        ~dependencies:[Target.id commit_website]
        ~make:(`Direct_command (Command.shell (fmt "[ -f /tmp/%s ] && git checkout `cat /tmp/%s`" branch_file branch_file)))
        (`Value `Unit))
  in
  Log.(s "branch file: " % s branch_file @ verbose);
  [`Make (active, [write_branch; check_out_gh_pages; make_doc; move_website; commit_website])]

let make_targz_on_host ?(gpg=true) ?dest_prefix ~host ~dir =
  (* make tar.gz, md5sum, gpg, rm *)
  let open Ketrew.EDSL in
  let dest_base =
    match dest_prefix with
    | Some s -> s 
    | None -> fmt  "/tmp/backup_%s" Time.(now () |> to_filename)
  in
  let destination ext = file ~host (fmt "%s.%s" dest_base ext) in
  let targz = destination "tar.gz" in
  let make_targz =
    target ~returns:targz "make-tar.gz" ~dependencies:[]
      ~make:(nohup_setsid ~host [
          fmt "tar cfz '%s' '%s'" targz#path dir
        ])
  in
  let md5 = destination "md5" in
  let md5_targz =
    target ~returns:md5 "make-md5-of-tar.gz" ~dependencies:[make_targz]
      ~make:(nohup_setsid ~host [
          fmt "md5sum '%s' > '%s'" targz#path md5#path
        ]) 
  in
  let gpg =
    match gpg with
    | false -> []
    | true ->
      let gpg_file = destination "gpg" in
      let make_it =
        target "make-gpg-of-tar.gz" ~returns:gpg_file ~dependencies:[make_targz]
          ~make:(nohup_setsid ~host [
              fmt "gpg -c --passphrase bouh -o '%s' '%s'"
                gpg_file#path targz#path
            ]) in
      let clean_up =
        target "rm-tar.gz" ~dependencies:[make_it]
          ~make:(nohup_setsid ~host [ fmt "rm -f '%s'" targz#path ]) in
      [make_it; clean_up]
  in
  let active =
    active "make-targz common ancestor"
      ~dependencies:(gpg @ [make_targz; md5_targz])
      (* the redundant make_targz dep is voluntary, it's for testing *)
      ~make:(direct_shell_command "echo Done")
  in
  make_workflow [active]

let make_targz_command_line argl =
  let open Ketrew.EDSL in
  let open Cmdliner in
  let term =
    Term.(
      pure (fun gpg host dir dest_prefix ->
          make_targz_on_host ~gpg ~host ~dir ?dest_prefix)
      $ Arg.(value & flag & info ["with-gpg"; "G"] ~doc:"Also run GPG.")
      $ host_cmdliner_term (`Required 0)
      $ Arg.(required & pos 1 (some string) None & info [] ~doc:"The Source." ~docv:"SRC")
      $ Arg.(value & opt (some string) None 
             & info ["destination"; "D"] ~doc:"Destination prefix." ~docv:"STR")
    ) in
  let info = 
    Term.info "make_targz_on_host" ~version:"0.0.0"
      ~doc:"Build a backup tar.gz and optionally encrypt it on a given host."
  in
  match Term.eval ~argv:(Array.of_list ("tgz" :: argl)) (term, info) with
  | `Ok todo -> todo
  | `Error `Exn
  | `Error `Parse
  | `Error `Term -> [`Fail Log.(s "Command line")]
  | `Help | `Version -> []


let run_main () =
  let additional_term =
    let open Cmdliner in
    let all_args =
      let doc = " TODO: write some doc here " in
      Arg.(value & pos_all string [] & info [] ~docv:"SPECIFICATION" ~doc)
    in
    Term.(pure (function
      | "website" :: more_args -> deploy_website more_args
      | "tgz" :: more_args -> make_targz_command_line more_args
      | args -> 
        [`Fail Log.(s "Don't know what to do: " % OCaml.list s args)])
          $ all_args)
  in
  Ketrew.Command_line.run_main additional_term


let `Never_returns =
  let configuration = Ketrew_state.Configuration.create db_file () in
  run_main ~configuration ()
