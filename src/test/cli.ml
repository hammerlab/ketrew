(*

  This “test” provides a Ketrew command line with some default user defined
  targets.

*)

open Ketrew_pervasives

let db_file = "/tmp/ketrew_cli_test_database"

(*

A first dummy workflow that can compile the documentation, switch to
the gh-pages branch, commit the new version of the documentation, and get
back to the original branch.

It can also fail in many ways (e.g. Git checkout forbidden because of dirty
tree) which show ketrew's handling of errors in dependencies.

*)
let deploy_website more_args =
  let open Ketrew.EDSL in
  let branch_file = Filename.concat "/tmp" (Unique_id.create ()) in
  let write_branch =
    target "Get current branch"
      ~returns:(file branch_file)
      ~make:(direct_shell_command 
               (fmt "git symbolic-ref --short HEAD > %s" branch_file))
  in
  let make_doc = 
    target "Make doc" ~make:(direct_shell_command "please.sh doc")
      ~returns:unit in
  let check_out_gh_pages =
    target "Check out gh-pages"
      ~dependencies:[write_branch; make_doc]
      ~make:(direct_shell_command
               (fmt "git checkout gh-pages || git checkout -t origin/gh-pages"))
  in
  let move_website =
    target "Move _doc/" ~dependencies:[check_out_gh_pages]
        ~make:(direct_shell_command (fmt "cp -r _doc/* .")) in
  let commit_website =
    target "Commit" ~dependencies:[move_website]
      ~make:(direct_shell_command
               (fmt "git add api && git ci -a -m 'update website' ")) in
  let get_back =
    target "Check-out original branch" ~dependencies:[ commit_website]
      ~make:(direct_shell_command
               (fmt "[ -f %s ] && git checkout `cat %s`"
                  branch_file branch_file))
  in
  run get_back

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

let run_command_with_lsf ~host ~queue cmd =
  let open Ketrew.EDSL in
  let host = parse_host host in
  Log.(s "LSF on " % s (Ketrew.Host.to_string_hum host) @ normal);
  make_workflow [
    active "run_command_with_lsf"
      ~make:(Ketrew_lsf.create ~queue
               ~wall_limit:"1:30" ~processors:(`Min_max (1,1))
               ~host [cmd])
  ]

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
      | "lsf" :: host :: queue :: cmd :: [] -> 
        run_command_with_lsf ~host ~queue cmd
      | args -> 
        [`Fail Log.(s "Don't know what to do: " % OCaml.list s args)])
          $ all_args)
  in
  Ketrew.Command_line.run_main additional_term


let `Never_returns =
  let configuration = Ketrew_state.Configuration.create db_file () in
  run_main ~configuration ()
