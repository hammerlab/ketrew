
(*M

Workflow Examples
-----------------

This “test” provides a few functions that create increasingly complex workflows.
It gets compiled to a command line application that submits the workflows to
the Ketrew engine.


First, the mandatory license:

    Copyright 2014, Sebastien Mondet <seb@mondet.org>

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
    implied.  See the License for the specific language governing
    permissions and limitations under the License.

And some preliminary OCaml settings:
M*)
open Printf
module List = ListLabels
let say fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
(*M

Workflows
---------

### Example From The README

This function is a more “parametrized” version of the example in the
`README.md` file (`host` and `queue` come from the command line).
It actually calls `submit` directly itself.

M*)
let run_command_with_lsf ~host ~queue cmd =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  Ketrew_client.submit (
    target "run_command_with_lsf"
      ~make:(lsf (Program.sh cmd)
               ~queue ~wall_limit:"1:30" ~processors:(`Min_max (1,1)) ~host)
  )

(*M

### Daemonize With Nohup/Setsid

This function is like `run_command_with_lsf` but uses
`daemonize` with the “nohup-setsid hack” instead of the batch scheduler.

M*)
let run_command_with_nohup ~host cmd =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  Ketrew_client.submit (
    target (sprintf "NhSs: %S" cmd)
      ~make:(daemonize ~using:`Nohup_setsid  (Program.sh cmd) ~host)
  )
(*M

This kind of “daemonized” job should work on any decent Unix system.


### Daemonize With “The Python Hack”

This function is like `run_command_with_nohup` but uses
the “python daemon hack” instead.

M*)
let run_command_with_python_hack ~host cmd =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  Ketrew_client.submit (
    target (sprintf "Pyd: %S" cmd)
      ~make:(daemonize ~using:`Python_daemon (Program.sh cmd) ~host)
  )
(*M

The `` `Python_daemon`` way of daemonizing was hacked together because
MacOSX does not support the `nohup` and `setsid` commands any more.


### A First Dependency Chain

This function runs a workflow with no less than 2 nodes!

- `target2` depends on `target1`.
- We call `submit` on `target2` (the last target, i.e. the root of the dependency
  arborescence).
- `target1` will run and if it succeeds `target2` will run.

M*)
let run_2_commands_with_python_hack ~host cmd1 cmd2 =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  let target1 =
    target (sprintf "Pyd: %S" cmd1)
      ~make:(daemonize ~using:`Python_daemon (Program.sh cmd1) ~host)
  in
  let target2 =
    target (sprintf "Pyd: %S" cmd2)
      ~dependencies:[ target1 ]
      ~make:(daemonize ~using:`Python_daemon (Program.sh cmd2) ~host)
  in
  Ketrew_client.submit target2
(*M
For example:

    <test-exec> two-py /tmp "du -sh $HOME" "du -sh $HOME/tmp/"

will run `du -sh` in `$HOME` and then in `$HOME/tmp` in the host `"/tmp"`
(which is `localhost` with `/tmp` as playground).

### With a Condition

This function creates a 2-nodes workflow that fails because of the condition
if the first target is not ensured by the build-process it runs.

- the function `make_target` is partial application of `EDSL.target` with an
  additional argument `~cmd`.
- `target_with_condition` is a target that is supposed to ensure that
`impossible_file` exists (which `ls /tmp` won't do).
    - `impossible_file` is a datastructure representing a file on a given host.
    - The condition is specified with the `~done_when` argument.
- `target2` is just a target that depends on `target_with_condition`.

M*)
let fail_because_of_condition ~host =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  let make_target ~cmd =
    target ~make:(daemonize ~using:`Python_daemon Program.(sh cmd) ~host)
  in
  let target_with_condition =
    let impossible_file = file ~host "/some-inexistent-file" in 
    make_target "Failing-target"
      ~cmd:"ls /tmp"
      ~done_when:impossible_file#exists
  in
  let target2 =
    make_target "Won't-run because of failed dependency"
      ~cmd:"ls /tmp"
      ~dependencies:[ target_with_condition ]
  in
  Ketrew_client.submit target2
(*M

The Explorer™ will show the failed targets:

    * [1]: Won't-run because of failed dependency
    ketrew_2014-09-23-21h55m01s460ms-UTC_994326685
    Failed: Dependencies died:
    ketrew_2014-09-23-21h55m01s460ms-UTC_089809344 died
    * [2]: Failing-target
    ketrew_2014-09-23-21h55m01s460ms-UTC_089809344
    Failed: Process Failure: the target did not ensure (Volume
    {([Host /tmp?shell=sh%2C-c]) (Root: /) (Tree: Single path:
    "some-inexistent-file")} exists)

### Website Building Workfow

A function that creates a workflow that builds the website for given list of
Git-branches.

It used to be simple but got pretty complex with time as it
was really used to build Ketrew's
[website](http://seb.mondet.org/software/ketrew/):

- take a list of branch names (`[]` meaning “default branch”)
- clone the repository in a temporary locations
- *for each branch:* checkout the branch, and build the documentation
- checkout the `gh-pages` branch
- put `_doc/` at the top-level
- commit and push back to the **current repository

The workflow can also fail in many ways (e.g. Git commit forbidden because of
nothing is new) which show ketrew's handling of errors in dependencies.

M*)
let deploy_website branches =
  let open Ketrew.EDSL in
  let host = (Host.parse "/tmp") in
  let local_deamonize = daemonize ~host ~using:`Python_daemon in
  let current_path = Sys.getenv "PWD" in
  let dest_path =
    sprintf "/tmp/deploy_website_%s" (Ketrew_pervasives.Unique_id.create ())
  in
  let target ?(add_tags=[]) = target ~tags:("website" :: add_tags) in
  let clone_repo =
    let readme = file (sprintf "%s/ketrew/README.md" dest_path) in
    target (sprintf "Clone to %s" dest_path)
      ~done_when:(readme#exists)
      ~make:(local_deamonize
               Program.(
                 shf "mkdir -p %s" dest_path
                 && shf "cd %s" dest_path
                 && shf "git clone %s" current_path))
  in
  let in_cloned_repo = Program.shf "cd %s/ketrew" dest_path in
  (* A function that creates a target that checks-out a given git branch
     (and actively verifies that it was successful) *)
  let check_out ~dependencies branch =
    target (sprintf "Check out %s" branch)
      ~dependencies:(clone_repo :: dependencies)
      ~make:(local_deamonize
               Program.(
                 in_cloned_repo
                 && shf "git checkout %s || git checkout -t origin/%s"
                   branch branch
                 && shf "[ \"`git symbolic-ref --short HEAD`\" = \"%s\" ]" branch
               ))
  in
  let build_doc_prgram =
    Program.(
      in_cloned_repo && sh "bash ./please.sh clean build doc") in
  let make_doc =
    match branches with
    | [] ->
      target "Make documentation (default branch)" 
        ~dependencies:[clone_repo]
        ~make:(local_deamonize build_doc_prgram)
    | more -> 
      (* we build a chain of targets depending on each other:
         clone_repo -> check_out branch1 -> build_doc ->
         -> check_out branch2 -> build_doc -> ...
         -> check_out last_branch -> build_doc
      *)
      List.fold_left ~init:clone_repo more ~f:(fun previous_dep branch ->
          let co = check_out ~dependencies:[previous_dep] branch in
          target (sprintf "Make documentation (branch %s)" branch)
            ~dependencies:[co]
            ~make:(local_deamonize build_doc_prgram))
  in
  let check_out_gh_pages =
    (* first target with dependencies: the two previous ones *)
    check_out ~dependencies:[make_doc] "gh-pages" in
  let move_website =
    target "Move _doc/" ~dependencies:[check_out_gh_pages]
      ~make:(local_deamonize 
               Program.(in_cloned_repo && sh "rsync -a _doc/ .")) in
  (* if there is nothing to commit, `git commit -a` will return 1, 
     so we use `ancestor` (without dependencies) as a fallback.
     But `ancestor` will also be run in case of success. *)
  let ancestor ~dependencies status =
    target (sprintf "Common ancestor: %s" status) ~dependencies
      ~add_tags:["end-of-workflow"]
      ~make:(local_deamonize
               Program.(
                 shf "echo 'Status: %S'" status
                 && chain
                   (List.map branches ~f:(function
                      | "master" ->
                        shf "echo 'See file://%s/ketrew/index.html'" dest_path
                      | br ->
                        shf "echo 'See file://%s/ketrew/%s/index.html'"
                          dest_path br)
                   )))
  in
  let commit_website =
    target "Commit" ~dependencies:[move_website]
      ~make:(local_deamonize
               Program.(
                 in_cloned_repo
                 && shf "git add api *.html *.svg %s "
                   (String.concat " " (List.filter ~f:((<>) "master") branches))
                 && sh "git commit -a -m 'update website'"
                 && shf "git push origin gh-pages"
               ))
      ~if_fails_activate:[ancestor "Failed" ~dependencies:[]]
  in
  (* `submit` will activate the target `ancestor` and then `commit_website`, and
     add all their (transitive) dependencies to the system.  *)
  Ketrew_client.submit (ancestor "Success" ~dependencies:[commit_website])

(*M

### Example “Backup” Workflow

This an example of a “backup” workflow.

Take a directory, make a tar.gz, save its MD5 sum, and if `gpg` is `true`,
call `gpg -c` and delete the tar.gz.

The passphrase for GPG must be in a file `~/.backup_passphrase` as

  ```shell
  export BACKUP_PASSPHRASE="some passsphraaase"
  ```

  M*)
let make_targz_on_host ?(gpg=true) ?dest_prefix ~host ~dir () =
  let open Ketrew.EDSL in
  let daemonize = daemonize ~using:`Python_daemon in
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
    target ~done_when:targz#exists "make-tar.gz" ~dependencies:[]
      ~make:(daemonize ~host
               (Program.shf  "tar cfz '%s' '%s'" targz#path dir))
      (* A first target using `daemonize`
         see [nohup(1)](http://linux.die.net/man/1/nohup)
         and [setsid(1)](http://linux.die.net/man/1/setsid). *)
  in
  let md5 = destination "md5" in
  let md5_targz =
    target ~done_when:md5#exists "make-md5-of-tar.gz" ~dependencies:[make_targz]
      ~make:(daemonize ~host
               Program.(shf "md5sum '%s' > '%s'" targz#path md5#path))
  in
  let gpg_targets () = (* `gpg` is a list of targets, lazily `[gpg-c, rm-tar.gz]` *)
    let gpg_file = destination "tar.gz.gpg" in
    let make_it =
      target "make-gpg-of-tar.gz" ~done_when:gpg_file#exists
        ~dependencies:[make_targz; md5_targz]
        ~make:(daemonize ~host
                 Program.(
                   sh ". ~/.backup_passphrase"
                   && shf "gpg -c --passphrase $BACKUP_PASSPHRASE -o '%s' '%s'"
                     gpg_file#path targz#path)) in
    let clean_up =
      target "rm-tar.gz" ~dependencies:[make_it]
        ~make:(daemonize ~host (Program.shf "rm -f '%s'" targz#path )) in
    [make_it; clean_up]
  in
  let common_ancestor =
    let dependencies =
      match gpg with
      | false -> [make_targz; md5_targz]
      | true ->  gpg_targets ()
    in
    target "make-targz common ancestor" ~dependencies
  in
  (* By running the common-ancestor we pull and activate all the targets to do. *)
  Ketrew_client.submit common_ancestor

(*M

### Build Ketrew On a Vagrant VM

This function builds completely different workflows depedending on the input
command:
  
- `prepare` → prepare a vagrant VM, ready to SSH to
- `go` → build Ketrew on the on vagrant VM
- `clean` → shutdown and delete the VM

M*)
let run_ketrew_on_vagrant what_to_do =
  let open Ketrew.EDSL in
  let (//) = Filename.concat in
  let tmp_dir =  Sys.getenv "HOME"  // "tmp/ketrew_vagrant" in
  let vagrant_tmp = tmp_dir // "vagr_vm" in
  let vagrant_host = Host.parse (tmp_dir // "playground") in
  let do_on_vagrant_host p =
    daemonize ~using:`Python_daemon ~host:vagrant_host p in
  let ssh_config = file ~host:vagrant_host (tmp_dir // "ssh_config") in
  match what_to_do with
  | `Prepare ->
    let init =
      file_target ~name:"init-vagrant" (vagrant_tmp // "Vagrantfile")
        ~host:vagrant_host 
        ~make:(do_on_vagrant_host Program.(
            exec ["mkdir"; "-p"; vagrant_tmp]
            && exec ["cd"; vagrant_tmp]
            && exec ["vagrant"; "init"; "hashicorp/precise64"]
          ))
    in
    let running =
      target "vagrant-up"
        ~dependencies:[init]
        ~make:(do_on_vagrant_host Program.(
            exec ["cd"; vagrant_tmp]
            && exec ["vagrant"; "up"]))
    in
    let make_ssh_config =
      target (* not a `file_target`, because we want this file regenerated 
                every time *)
        "vagrant-ssh-config" ~dependencies:[running]
        ~make:(do_on_vagrant_host Program.(
            exec ["cd"; vagrant_tmp]
            && shf "vagrant ssh-config --host Vagrew > %s" ssh_config#path))
    in
    make_ssh_config
  | `Go ->
    let vagrant_box =
      let open Ketrew in
      Host.ssh ~add_ssh_options:["-F"; ssh_config#path]
        ~playground:(Path.absolute_directory_exn "/tmp/KT/") "Vagrew"
    in
    let do_on_vagrant_box p =
      daemonize ~using:`Nohup_setsid ~host:vagrant_box p in
    let get_c_dependencies =
      target "apt-get-c-deps"
        ~dependencies:[]
        ~make:(do_on_vagrant_box Program.(
            sh "echo GO"
            && sh "sudo apt-get update"
            && sh "sudo apt-get install -y m4 build-essential libgdbm-dev"
          ))
    in
    let get_opam =
      file_target ~name:"apt-get-opam"
        ~host:vagrant_box "/usr/bin/opam"
        ~dependencies:[get_c_dependencies]
        (* `get_opam` does not really depend on `get_c_dependencies` but
           `apt-get` does not work in parallel, so we need to make things
           sequential. *)
        ~make:(do_on_vagrant_box Program.(
            sh "echo GO"
            && sh "sudo apt-get update"
            && sh "sudo apt-get install -y python-software-properties"
          && sh "sudo add-apt-repository -y ppa:avsm/ocaml41+opam11"
          && sh "sudo apt-get update -qq"
          && sh "sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra aspcud opam"
        ))
    in
    let init_opam =
      file_target ~name:"init-opam"
        "/home/vagrant/.opam/opam-init/init.sh"
        ~dependencies:[get_opam]
        ~host:vagrant_box
        ~make:(do_on_vagrant_box Program.( sh "opam init"))
    in
    let install_ketrew compiler =
      file_target ~name:"opam-install-ketrew"
        ~dependencies:[init_opam; get_c_dependencies]
        ~host:vagrant_box
        (sprintf "/home/vagrant/.opam/%s/bin/ketrew" compiler)
        ~make:(do_on_vagrant_box Program.(
            shf "opam switch %s" compiler
            && sh "opam remote add smondet https://github.com/smondet/dev-opam-repo.git || echo smondet_remote_already_there"
            && sh "opam remove ocamlfind || echo ocamlfind_not_installed"
            && sh "opam pin ketrew https://github.com/smondet/ketrew/ || echo ketrew_already_pinned"
            && sh "opam install -y ketrew"
          ))
    in
    install_ketrew "system"
  | `Clean ->
    let kill =
      target "kill-vagrant"
        ~make:(do_on_vagrant_host Program.(
            exec ["cd"; vagrant_tmp]
            && exec ["vagrant"; "destroy"; "-f"]
          ))
    in
    let rm_temp =
      target "rm-temp"
        ~dependencies:[kill]
        ~make:(do_on_vagrant_host Program.(exec ["rm"; "-fr"; vagrant_tmp]))
    in
    rm_temp

(*M

## “Main” Of the Module  

Command line parsing for this multi-workflow script.

Here it is kept very basic but one may use usual OCaml tools
(`Arg` module, or `Cmdliner` -- which is already linked-in with Ketrew).

M*)
let () =
  let argl = Array.to_list Sys.argv in
  match List.tl argl with
  | "website" :: more_args ->
    say "Deploying website for %s"
      (match more_args with
       | [] -> "The current branch"
       | _ -> "branches: " ^ String.concat ", " more_args);
    deploy_website more_args
  | "tgz" :: more_args ->
    begin match more_args with
    | host_uri :: dir :: [] ->
      make_targz_on_host ~host:(Ketrew.EDSL.Host.parse host_uri) ~dir ()
    | host :: dir :: dest_prefix :: [] ->
      make_targz_on_host ~dest_prefix ~host:(Ketrew.EDSL.Host.parse host) ~dir ()
    | host :: dir :: dest_prefix :: "with-gpg" :: []  ->
      make_targz_on_host ~gpg:true ~dest_prefix ~host:(Ketrew.EDSL.Host.parse host) ~dir ()
    | other ->
      say "usage: %s tgz <host> <dir> [dest-prefix] [\"with-gpg\"]" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | "lsf" :: more ->
    begin match more with
    | host :: queue :: cmd :: [] ->
      run_command_with_lsf ~host ~queue cmd
    | other ->
      say "usage: %s lsf <host> <queue> <cmd>" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | "nohup-setsid" :: more
  | "nhss" :: more ->
    begin match more with
    | host :: cmd :: [] -> run_command_with_nohup ~host cmd
    | other ->
      say "usage: %s nhss <host> <cmd>" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | "python-daemon" :: more
  | "pyd" :: more ->
    begin match more with
    | host :: cmd :: [] -> run_command_with_python_hack ~host cmd
    | other ->
      say "usage: %s pyd <host> <cmd>" Sys.argv.(0);
      failwith "Wrong command line" end
  | "two-py" :: more ->
    begin match more with
    | host :: cmd1 :: cmd2 :: [] -> 
      run_2_commands_with_python_hack ~host cmd1 cmd2
    | other ->
      say "usage: %s two-py <host> <cmd1> <cmd2>" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | "failing-product" :: host :: [] ->
    fail_because_of_condition ~host
  | "CI" :: more ->
    begin match more with
    | "prepare" :: [] ->
      Ketrew_client.submit (run_ketrew_on_vagrant `Prepare)
    | "go" :: [] ->
      Ketrew_client.submit (run_ketrew_on_vagrant `Go)
    | "clean" :: [] ->
      Ketrew_client.submit (run_ketrew_on_vagrant `Clean)
    | other ->
      say "usage: %s CI {prepare,go,clean}" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | args ->
    say "usage: %s [website|tgz|lsf|nhss|pyd|two-py|failing-product|CI] ..."
      Sys.argv.(0);
    say "Don't know what to do with %s" (String.concat ", " args);
    failwith "Wrong command line"

