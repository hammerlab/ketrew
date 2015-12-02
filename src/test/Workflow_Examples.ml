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
open Nonstd
module String = Sosa.Native_string
let say fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
(*M

Workflows
---------

### Simple Example With LSF

This function is a more “parametrized” version of the example in the
`README.md` file (`host` and `queue` come from the command line).
It actually calls `submit` directly itself.

M*)
let run_command_with_lsf ~host ~queue cmd =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  Ketrew.Client.submit_workflow (
    workflow_node without_product
      ~name:"run_command_with_lsf"
      ~make:(lsf (Program.sh cmd)
               ~queue ~wall_limit:"1:30" ~processors:(`Min_max (1,1)) ~host)
  )

(*M

### Daemonize With Nohup/Setsid

This function is like `run_command_with_lsf` but uses
`daemonize` with the “nohup-setsid” method instead of the batch scheduler.

M*)
let run_command_with_nohup ~host cmd =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  Ketrew.Client.submit_workflow (
    workflow_node without_product
      ~name:(sprintf "NhSs: %S" cmd)
      ~make:(daemonize ~using:`Nohup_setsid  (Program.sh cmd) ~host)
  )
(*M

This kind of “daemonized” job should work on any decent Unix system.


### Daemonize With “The Python Hack”

This function is like `run_command_with_nohup` but uses
the “python daemon” method instead.

M*)
let run_command_with_python_method ~host cmd =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  Ketrew.Client.submit_workflow (
    workflow_node without_product
      ~name:(sprintf "Pyd: %S" cmd)
      ~make:(daemonize (Program.sh cmd)
               ~using:`Python_daemon ~host
               ~call_script:(fun script -> ["bash"; "--verbose"; script]))
  )
(*M

The `` `Python_daemon`` way of daemonizing was hacked together because
MacOSX does not support the `nohup` and `setsid` commands any more.

### Get a Container fron “Apache Yarn”

This function is like `run_command_with_lsf` but uses the
`Ketrew.Yarn` backend.
[Yarn](http://hadoop.apache.org/docs/current/hadoop-yarn/hadoop-yarn-site/YARN.html)
comes from the Hadoop ecosystem and is used to request resources.

  M*)
let run_command_with_yarn ~host cmd =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  let make =
    yarn_distributed_shell 
      ~host ~container_memory:(`GB 12)
      ~timeout:(`Seconds 3600)
      ~application_name:"YarnKetrewExample"
      (* do not put spaces up there, it can break Yarn *)
      (Program.sh cmd)
  in
  Ketrew.Client.submit_workflow (
    workflow_node without_product ~name:(sprintf "Yarn: %S" cmd) ~make
  )

(*M
  

### A First Dependency Chain

This function runs a workflow with no less than 2 nodes!

- `node2` depends on `node1`.
- We call `submit_workflow` on `node2` (the last one, i.e. the root of
  the dependency arborescence).
- `node1` will run and if it succeeds `node2` will run.

M*)
let run_2_commands_with_python_method ~host cmd1 cmd2 =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  let node1 =
    workflow_node without_product
      ~name:(sprintf "Pyd: %S" cmd1)
      ~make:(daemonize ~using:`Python_daemon (Program.sh cmd1) ~host)
  in
  let node2 =
    workflow_node without_product
      ~name:(sprintf "Pyd: %S" cmd2)
      ~edges:[depends_on node1]
      ~make:(daemonize ~using:`Python_daemon (Program.sh cmd2) ~host)
  in
  Ketrew.Client.submit_workflow node2
(*M
For example:

    <test-exec> two-py /tmp "du -sh $HOME" "du -sh $HOME/tmp/"

will run `du -sh` in `$HOME` and then in `$HOME/tmp` in the host `"/tmp"`
(which is `localhost` with `/tmp` as playground).

### With a Condition

This function creates a 2-nodes workflow that fails because of the condition
if the first target is not ensured by the build-process it runs.

- the function `make_node` is partial application of
  `EDSL.workflow_node` with an additional argument `~cmd`.
- `target_with_condition` is a target that is supposed to ensure that
`impossible_file` exists (which `ls /tmp` won't do).
    - `impossible_file` is a datastructure representing a file on a given host.
    - The condition is specified with the `~done_when` argument.
- `target2` is just a target that depends on `target_with_condition`.

M*)
let fail_because_of_condition ~host =
  let open Ketrew.EDSL in
  let host = Host.parse host in
  let make_node ~cmd =
    workflow_node ~make:(daemonize ~using:`Python_daemon Program.(sh cmd) ~host)
  in
  let target_with_condition =
    let impossible_file = single_file ~host "/some-inexistent-file" in 
    make_node impossible_file
      ~name:"Failing-target"
      ~cmd:"ls /tmp"
  in
  let target2 =
    make_node without_product
      ~name:"Won't-run because of failed dependency"
      ~cmd:"ls /tmp"
      ~edges:[depends_on target_with_condition ]
  in
  Ketrew.Client.submit_workflow target2
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


M*)

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
  let run_program: Program.t -> Build_process.t =
    fun p -> daemonize ~using:`Python_daemon ~host p in
  let dest_base =
    match dest_prefix with
    | Some s -> s
    | None -> sprintf  "/tmp/backup_from_ketrew_cli"
  in
  let destination_product ext =
    (* `destination` creates “volume-artifacts” for a given file
       extension. *)
    single_file ~host (sprintf "%s.%s" dest_base ext)
  in
  let make_targz : single_file workflow_node = 
    let product = (destination_product "tar.gz") in
    workflow_node product ~name:"make-tar.gz"
      ~make:(run_program
               Program.(
                 shf "cd %s/../" dir
                 &&
                 shf  "tar cfz '%s' '%s'" product#path (Filename.basename dir)))
  in
  let md5_targz =
    let md5_product = destination_product "md5" in  
    workflow_node md5_product
      ~name:"make-md5-of-tar.gz"
      ~edges:[depends_on  make_targz]
      ~make:(run_program
               Program.(shf "md5sum '%s' > '%s'"
                          make_targz#product#path md5_product#path))
  in
  let gpg_targets () =
    let delete_the_targz =
      workflow_node without_product
        ~name:"rm-tar.gz"
        ~make:(run_program (Program.shf "rm -f '%s'" make_targz#product#path))
    in
    let gpg_file = destination_product "tar.gz.gpg" in
    let make_it =
      workflow_node gpg_file
        ~name:"make-gpg-of-tar.gz"
        ~edges:[
          depends_on make_targz;
          depends_on md5_targz;
          on_success_activate delete_the_targz;
        ]
        ~make:(run_program
                 Program.(
                   sh ". ~/.backup_passphrase"
                   && shf "gpg -c --passphrase $BACKUP_PASSPHRASE -o '%s' '%s'"
                     gpg_file#path make_targz#product#path)) in
    make_it
  in
  let common_ancestor =
    let edges =
      match gpg with
      | false -> [ depends_on md5_targz]
      | true ->  [ depends_on (gpg_targets ())]
    in
    workflow_node without_product
      ~name:"Backup workflow common ancestor" ~edges
  in
  (* By running the common-ancestor we pull and activate all the targets to do. *)
  Ketrew.Client.submit_workflow common_ancestor

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
        ~depends_on:[init]
        ~make:(do_on_vagrant_host Program.(
            exec ["cd"; vagrant_tmp]
            && exec ["vagrant"; "up"]))
    in
    let make_ssh_config =
      target (* not a `file_target`, because we want this file regenerated 
                every time *)
        "vagrant-ssh-config" ~depends_on:[running]
        ~make:(do_on_vagrant_host Program.(
            exec ["cd"; vagrant_tmp]
            && shf "vagrant ssh-config --host Vagrew > %s" ssh_config#path))
    in
    make_ssh_config
  | `Go ->
    let vagrant_box =
      let open Ketrew in
      Host.ssh ~add_ssh_options:["-F"; ssh_config#path]
        ~playground:"/tmp/KT/" "Vagrew"
    in
    let do_on_vagrant_box p =
      daemonize ~using:`Nohup_setsid ~host:vagrant_box p in
    let get_c_dependencies =
      target "apt-get-c-deps"
        ~depends_on:[]
        ~make:(do_on_vagrant_box Program.(
            sh "echo GO"
            && sh "sudo apt-get update"
            && sh "sudo apt-get install -y m4 build-essential libgdbm-dev"
          ))
    in
    let get_opam =
      file_target ~name:"apt-get-opam"
        ~host:vagrant_box "/usr/bin/opam"
        ~depends_on:[get_c_dependencies]
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
        ~depends_on:[get_opam]
        ~host:vagrant_box
        ~make:(do_on_vagrant_box Program.( sh "opam init"))
    in
    let install_ketrew compiler =
      file_target ~name:"opam-install-ketrew"
        ~depends_on:[init_opam; get_c_dependencies]
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
        ~depends_on:[kill]
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
  match List.tl_exn argl with
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
    | host :: cmd :: [] -> run_command_with_python_method ~host cmd
    | other ->
      say "usage: %s pyd <host> <cmd>" Sys.argv.(0);
      failwith "Wrong command line" end
  | "yarn" :: more ->
    begin match more with
    | host :: cmd :: [] -> run_command_with_yarn ~host cmd
    | other ->
      say "usage: %s yarn <host> <cmd>" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | "two-py" :: more ->
    begin match more with
    | host :: cmd1 :: cmd2 :: [] -> 
      run_2_commands_with_python_method ~host cmd1 cmd2
    | other ->
      say "usage: %s two-py <host> <cmd1> <cmd2>" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | "failing-product" :: host :: [] ->
    fail_because_of_condition ~host
  | "CI" :: more ->
    begin match more with
    | "prepare" :: [] ->
      Ketrew.Client.submit (run_ketrew_on_vagrant `Prepare)
    | "go" :: [] ->
      Ketrew.Client.submit (run_ketrew_on_vagrant `Go)
    | "clean" :: [] ->
      Ketrew.Client.submit (run_ketrew_on_vagrant `Clean)
    | other ->
      say "usage: %s CI {prepare,go,clean}" Sys.argv.(0);
      failwith "Wrong command line"
    end
  | args ->
    say "usage: %s [website|tgz|lsf|nhss|pyd|two-py|failing-product|CI|pythological] ..."
      Sys.argv.(0);
    say "Don't know what to do with %s" (String.concat ~sep:", " args);
    failwith "Wrong command line"

