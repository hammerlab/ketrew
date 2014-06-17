(*

  This “test” provides a Ketrew command line with some default user defined
  targets.

*)

open Printf
let say fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

(*

A first dummy workflow that can compile the documentation, switch to
the gh-pages branch, commit the new version of the documentation, and get
back to the original branch.

It can also fail in many ways (e.g. Git checkout forbidden because of dirty
tree) which show ketrew's handling of errors in dependencies.

*)
let deploy_website () =
  let open Ketrew.EDSL in
  let branch_file = Filename.temp_file "ketrewcli" "git_branch" in
  let write_branch =
    (* The target that produces `branch_file` *)
    target "Get current branch"
      ~returns:(file branch_file)
      ~make:(direct_shell_command 
               (sprintf "git symbolic-ref --short HEAD > %s" branch_file))
  in
  let make_doc = 
    target "Make doc" ~make:(direct_shell_command "please.sh doc")
      ~returns:unit in
  let check_out_gh_pages =
    (* first target with dependencies: the two previous ones *)
    target "Check out gh-pages"
      ~dependencies:[write_branch; make_doc]
      ~make:(direct_shell_command
               (sprintf "git checkout gh-pages || git checkout -t origin/gh-pages"))
      (* The default value for `?returns` is unit, so this
         target will always be recomputed. 
         It can fail pretty often, e.g. is the git-tree is not clean, for `git
         checkout`.  *)
  in
  let move_website =
    target "Move _doc/" ~dependencies:[check_out_gh_pages]
        ~make:(direct_shell_command (sprintf "cp -r _doc/* .")) in
  let commit_website =
    target "Commit" ~dependencies:[move_website]
      ~make:(direct_shell_command
               (sprintf "git add api && git ci -a -m 'update website' ")) in
  let get_back =
    target "Check-out original branch" ~dependencies:[ commit_website]
      ~make:(direct_shell_command
               (sprintf "[ -f %s ] && git checkout `cat %s`"
                  branch_file branch_file))
  in
  (* `run` will activate the target `get_back`, and add all its (transitive)
     dependencies to the system.

     Ketrew will use the default configuration file unless the configuration is
     overridden:
  *)
  let custom_database_file = "/tmp/ketrew_cli_test_db" in
  say "Using database: %S" custom_database_file;
  run get_back
    ~override_configuration:
      (Ketrew_configuration.create
         ~database_parameters:custom_database_file ())

(*
  This second target could the beginning of a “backup” workflow.

  Take a directory, make a tar.gz, save its MD5 sum, and if `gpg` is `true`,
  call `gpg -c` and delete the tar.gz.

  Warning:
  Ketrew does not have (yet) “hidden” data, so please, do not use
  `gpg --password` in real-world settings. Ketrew may write the
  command in many log files for instance.
*)
let make_targz_on_host ?(gpg=true) ?dest_prefix ~host ~dir () =
  let open Ketrew.EDSL in
  let dest_base =
    match dest_prefix with
    | Some s -> s 
    | None -> sprintf  "/tmp/backup_from_ketrew_cli"
  in
  let destination ext =
    (* `destination` creates “volume-artifacts” for a given file
       extension. *)
    file ~host (sprintf "%s.%s" dest_base ext)
  in
  let targz = destination "tar.gz" in
  let make_targz =
    target ~returns:targz "make-tar.gz" ~dependencies:[]
      ~make:(nohup_setsid ~host 
               (Program.shf  "tar cfz '%s' '%s'" targz#path dir))
      (* A first target using `nohup_setsid`
        see [nohup(1)](http://linux.die.net/man/1/nohup)
        and [setsid(1)](http://linux.die.net/man/1/setsid). *)
  in
  let md5 = destination "md5" in
  let md5_targz =
    target ~returns:md5 "make-md5-of-tar.gz" ~dependencies:[make_targz]
      ~make:(nohup_setsid ~host
               Program.(shf "md5sum '%s' > '%s'" targz#path md5#path))
  in
  let gpg = (* `gpg` is a list of targets, `[]` or `[gpg-c, rm-tar.gz]` *)
    match gpg with
    | false -> []
    | true ->
      let gpg_file = destination "gpg" in
      let make_it =
        target "make-gpg-of-tar.gz" ~returns:gpg_file ~dependencies:[make_targz]
          ~make:(nohup_setsid ~host 
                   Program.(shf "gpg -c --passphrase bouh -o '%s' '%s'"
                              gpg_file#path targz#path)) in
      let clean_up =
        target "rm-tar.gz" ~dependencies:[make_it]
          ~make:(nohup_setsid ~host (Program.shf "rm -f '%s'" targz#path )) in
      [make_it; clean_up]
  in
  let common_ancestor =
    (* A Target that does nothing, but is used as root of the dependency tree. *)
    target "make-targz common ancestor"
      ~dependencies:(gpg @ [make_targz; md5_targz])
  in
  (* By running the common-ancestor we pull and activate all the targets to do. *)
  run common_ancestor


(*
  The third workflow is a parametrized version of the example in the
  `README.md` file (`host` and `queue` come from the command line).
*)
let run_command_with_lsf ~host ~queue cmd =
  let open Ketrew.EDSL in
  let host = parse_host host in
  run (
    target "run_command_with_lsf"
      ~make:(lsf (Program.sh cmd)
               ~queue ~wall_limit:"1:30" ~processors:(`Min_max (1,1)) ~host)
  )


(*
   Command line parsing for this multi-workflow script.

   Here it is kept very basic but one may use usual OCaml tools
   (`Arg` module, or `Cmdliner` -- which is already linked-in with Ketrew).

*)
let () =
  let argl = Array.to_list Sys.argv in
  match List.tl argl with
  | "website" :: more_args -> 
    if more_args <> [] then
      say "Ignoring: [%s]" (String.concat ", " more_args);
    deploy_website ()
  | "tgz" :: more_args ->
    begin match more_args with
    | host_uri :: dir :: [] ->
      make_targz_on_host ~host:(Ketrew.EDSL.parse_host host_uri) ~dir ()
    | host :: dir :: dest_prefix :: [] ->
      make_targz_on_host ~dest_prefix ~host:(Ketrew.EDSL.parse_host host) ~dir ()
    | host :: dir :: dest_prefix :: "with-gpg" :: []  ->
      make_targz_on_host ~gpg:true ~dest_prefix ~host:(Ketrew.EDSL.parse_host host) ~dir ()
    | other ->
      say "usage: %s tgz <host> <uri> [dest-prefix] [\"with-gpg\"]" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | "lsf" :: more ->
    begin match more with
    | host :: queue :: cmd :: [] -> 
      run_command_with_lsf ~host ~queue cmd
    | other -> 
      say "usage: %s call lsf <host> <queue> <cmd>" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | args -> 
    say "usage: %s call [website|tgz|lsf] ..." Sys.argv.(0);
    say "Don't know what to do with %s" (String.concat ", " args);
    failwith "Wrong command line"

