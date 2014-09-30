(*M

Big Integration Test
--------------------

M*)

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

open Nonstd
let say fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
let (//) = Filename.concat
let failwithf fmt = ksprintf failwith fmt

let shellf fmt =
  ksprintf (fun s ->
      match Sys.command s with
      | 0 -> ()
      | other -> failwithf "Shell command %S returned %d" s other
    ) fmt

module Test_host = struct

  let as_host () = Ketrew.EDSL.Host.parse "/tmp/ketrew-integration-test"

  let do_on ~program = 
    let open Ketrew.EDSL in
    let host = as_host () in
    daemonize ~using:`Python_daemon ~host program

end

module Vagrant_box = struct

  type t = {
    dir: string;
    ssh_config: string;
    hostname: string;
  }

  let create hostname =
    let dir =
      Sys.getenv "HOME"  // "tmp/vagrant" // hostname in
    {dir; ssh_config = dir // "ssh_config"; hostname}

  let in_dir t =
    let open Ketrew.EDSL in
    Program.(
      exec ["mkdir"; "-p"; t.dir]
      && exec ["cd"; t.dir]
    )
  let as_host t = 
    let open Ketrew.EDSL in
    Host.ssh ~add_ssh_options:["-F"; t.ssh_config]
      ~playground:"/tmp/KT/" t.hostname

  let do_on t ~program =
    let open Ketrew.EDSL in
    let host = as_host t in
    daemonize ~using:`Nohup_setsid ~host program

  let internal_hostname _ = "precise64"
  let internal_username _ = "vagrant"
  let prepare ?success_triggers t =
    let open Ketrew.EDSL in
    let host = Test_host.as_host () in
    let init =
      file_target ~name:"init-vagrant" (t.dir // "Vagrantfile")
        ~host
        ~make:(Test_host.do_on Program.(
            in_dir t
            && exec ["vagrant"; "init"; "hashicorp/precise64"]
          ))
    in
    let running =
      target "vagrant-up"
        ~dependencies:[init]
        ~make:(Test_host.do_on Program.(
            in_dir t && exec ["vagrant"; "up"]))
    in
    let make_ssh_config =
      target (* not a `file_target`, because we want this file regenerated 
                every time. We set the `product` but not the “condition”. *)
        "vagrant-ssh-config" ~dependencies:[running] ?success_triggers
        ~product:(file ~host:(Test_host.as_host ()) t.ssh_config)
        ~make:(Test_host.do_on Program.(
            in_dir t
            && shf "vagrant ssh-config --host %s > %s" t.hostname t.ssh_config))
    in
    make_ssh_config
    
  let destroy t =
    let open Ketrew.EDSL in
    let kill =
      target "kill-vagrant"
        ~make:(Test_host.do_on Program.(
            in_dir t && exec ["vagrant"; "destroy"; "-f"]
          ))
    in
    let rm_temp =
      target "rm-temp"
        ~dependencies:[kill]
        ~make:(Test_host.do_on Program.(exec ["rm"; "-fr"; t.dir]))
    in
    rm_temp

  let ssh t =
    shellf "cd %s && vagrant ssh" t.dir

  let exec_is_installed t ~exec =
    let host = as_host t in
    let shell_cmd = sprintf "which %s" exec in
    let open Ketrew.EDSL in
    Condition.program ~host Program.(sh shell_cmd)

  let do_on t ~program =
    let open Ketrew.EDSL in
    let host = as_host t in
    daemonize ~using:`Nohup_setsid ~host program

  let with_installed t ~packages =
    let open Ketrew.EDSL in
    let name = sprintf "apt-getting-%s" (String.concat "-" packages) in
    target name
      ~tags:["integration"; "apt-get"]
      ~make:(do_on t Program.(
          sh "echo GO"
          && sh "sudo apt-get update"
          && shf "sudo apt-get install -y %s" (String.concat " " packages)
        ))

  let file t ~path =
    let host = as_host t in
    Ketrew.EDSL.file ~host path

end

(*M

See [openlava.org](http://www.openlava.org/home.html).

M*)
let config_files ~hostname ~usernames () = [
  "lsb.hosts", `Inline "
Begin Host
HOST_NAME     MXJ JL/U   r1m    pg    ls     tmp  DISPATCH_WINDOW  # Keywords
#host0        1    1   3.5/4.5  15/   12/15  0      ()		   # Example
#host1       ()   2     3.5  15/18   12/    0/  (5:19:00-1:8:30 20:00-8:30)
#host2        ()   ()   3.5/5   18    15     ()     ()		   # Example
default       2   ()     ()    ()    ()     ()     ()		   # Example
End Host
    ";
  "lsb.params", `Inline "
Begin Parameters
DEFAULT_QUEUE  = normal   #default job queue name
MBD_SLEEP_TIME = 10       #mbatchd scheduling interval (60 secs is default)
SBD_SLEEP_TIME = 7        #sbatchd scheduling interval (30 secs is default)
JOB_ACCEPT_INTERVAL = 1   #interval for any host to accept a job 
                          # (default is 1 (one-fold of MBD_SLEEP_TIME))
End Parameters
";
  "lsb.queues", `Inline "
Begin Queue
QUEUE_NAME   = normal
PRIORITY     = 30
NICE         = 20
DESCRIPTION  = For normal low priority jobs, running only if hosts are \
lightly loaded.
End Queue
";
  "lsb.users", `Inline (sprintf "
Begin UserGroup
GROUP_NAME       GROUP_MEMBER              
#develop         (jwang long david ming)  
#system          (all)                    
#eng_users       (develop zhang ahmedk pangj) 
End UserGroup
Begin User
USER_NAME	MAX_JOBS	JL/P
#develop@        20              8
#support         50              -
%s
End User
  " (List.map usernames (sprintf "%s 50 -")
     |> String.concat "\n")
);
  sprintf "lsf.cluster.%s" hostname, `Inline (sprintf "
Begin   ClusterAdmins
Administrators = (root %s)
End    ClusterAdmins

Begin   Host
HOSTNAME          model          type  server  r1m  RESOURCES
%s               !              !     1       -       -
End     Host

Begin ResourceMap
RESOURCENAME  LOCATION
# tmp2          [default]
# nio           [all]
# console       [default]
End ResourceMap
  " 
    (List.map usernames (sprintf "%s") |> String.concat " ")
    hostname
);

  "lsf.shared", `Inline (sprintf "
Begin Cluster
ClusterName			# Keyword
%s
End Cluster
Begin HostType
TYPENAME                        # Keyword
linux
End HostType
Begin HostModel
MODELNAME  CPUFACTOR   ARCHITECTURE # keyword
# CPU factors are only comparisons.
IntelI5      100        (x86_64)
End HostModel
Begin Resource
RESOURCENAME  TYPE    INTERVAL INCREASING  DESCRIPTION 	      # Keywords
   fs         Boolean ()       ()          (File server)
   cs         Boolean ()       ()          (Compute server)
End Resource
" hostname);
  "lsf.task", `Inline "
Begin RemoteTasks
ar
as
cc/cpu
c89/cpu
gcc/cpu
g++/cpu
CC
compress/-:cpu:mem
compressdir/cpu:mem
deroff
diff
ditroff
dvi2ps
egrep
eqn
f77/cpu
fgrep/
gcc/cpu
gprof
grap/-
grep/-
ispell
lint
latex/cpu
make/cpu
nroff/cpu
od
pic
psroff
sort
spell
split
tbl
tpc
troff/cpu
ttc
uncompress/cpu:mem
uuencode/cpu
wc
what
zcat/cpu:mem
zmore/cpu
End RemoteTasks
  ";
  "lsf.conf", `File "config/lsf.conf";
  (*
    hack found here:
https://groups.google.com/forum/#!topic/openlava-users/ezYDlyeb1wk
  *)
  "hosts", `Inline (sprintf "
127.0.0.1 %s
127.0.1.1 %s
" hostname hostname);
]
let install_lsf ~box =
  let packages = ["build-essential"; "libncurses-dev"; "tk8.4-dev"] in
  let open Ketrew.EDSL in
  target "install-lsf"
    ~done_when:Condition.( Vagrant_box.exec_is_installed box ~exec:"bsub")
    ~tags:["intergration"]
    ~dependencies:[Vagrant_box.with_installed ~packages box]
    ~make:(
      Vagrant_box.do_on box Program.(
          exec ["wget"; "http://www.openlava.org/tarball/openlava-2.2.tar.gz"]
          && exec ["sudo"; "sh"; "-c"; "cd /usr/include && rm -fr tcl && ln -s tcl8.4 tcl"]
          && exec ["rm"; "-fr"; "openlava-2.2"]
          && exec ["tar"; "xvfz"; "openlava-2.2.tar.gz"]
          && exec ["cd"; "openlava-2.2"]
          && sh "./configure --prefix /usr"
          && exec ["make"]
          && exec ["sudo"; "make"; "install"]
        ))

let ensure_lsf_is_running ~box =
  let open Ketrew.EDSL in
  let installed = install_lsf ~box in
  let hostname = Vagrant_box.internal_hostname box in
  let usernames = [Vagrant_box.internal_username box] in
  let config_files = config_files ~hostname ~usernames () in
  let config_directory = "/usr/etc" in
  let conditions = 
    Condition.chain_and (List.map config_files (fun (name, _) ->
        let file = Vagrant_box.file box (config_directory // name) in
        file#exists))
  in
  let open Ketrew.EDSL in
  target "start-lsf"
    ~done_when:Condition.(
        conditions
        && program ~returns:0
          ~host:(Vagrant_box.as_host box)
          Program.(exec ["timeout"; "1"; "sh"; "-c"; "bjobs; exit 0"]))
    ~tags:["integration"]
    ~dependencies:[ installed; ]
    ~make:( Vagrant_box.do_on box Program.(
        chain (List.map config_files ~f:(fun (name, content) ->
            match content with
            | `Inline c ->
              exec ["sudo"; "sh"; "-c";
                    sprintf "echo %s > %s" Filename.(quote c)
                      (config_directory // name)]
            | `File f -> exec ["sudo"; "cp"; "openlava-2.2" // f; config_directory // name]
          ))
        && exec ["sudo"; "/usr/etc/openlava"; "start"]
      ))

let lsf_job ?success_triggers ?if_fails_activate ~box () =
  let open Ketrew.EDSL in
  let output = "/tmp/du-sh-dollar-home" in
  let host = Vagrant_box.as_host box in
  let name = "lsf-1" in
  file_target ~host ~name output ?success_triggers ?if_fails_activate
    ~dependencies:[ ensure_lsf_is_running ~box ]
    ~tags:["integration"; "lsf"]
    ~make:(
      lsf ~host ~name Program.(shf "du -sh $HOME > %s" output
                               && exec ["cat"; output])
        ~queue:"normal"
    )


let test =
  let lsf_host = Vagrant_box.create "LsfTestHost" in
  object  (self)
    method prepare =
      Vagrant_box.prepare lsf_host
    method go =
      lsf_job ~box:lsf_host ()
    method clean_up =
      Vagrant_box.destroy lsf_host
    method do_all =
      let clean = Vagrant_box.destroy lsf_host in
      Vagrant_box.prepare lsf_host
        ~success_triggers:[
          lsf_job ~box:lsf_host ()
            ~success_triggers:[clean]
            ~if_fails_activate:[clean]
        ]
    method box_names = ["LSF"]
    method ssh = function
    | "LSF" -> Vagrant_box.ssh lsf_host
    | other -> failwithf "Unkown “Box”: %S" other
  end

let () =
  let open Cmdliner in
  let version = Ketrew_metadata.version in
  let sub_command ~info ~term = (term, info) in
  let sub_command_of_target ~name ~doc make_target =
    let the_name = name in
    sub_command
      ~info:Term.(info the_name ~version ~doc)
      ~term:Term.(
          pure (fun () ->
              Ketrew.EDSL.run (
                make_target ()
              ))
          $ pure ()
        )
  in
  let prepare =
    sub_command_of_target ~name:"prepare" ~doc:"Setup the VM(s)"
      (fun () -> test#prepare) in
  let go =
    sub_command_of_target ~name:"go" ~doc:"Do the test"
      (fun () -> test#go) in
  let clean_up =
    sub_command_of_target ~name:"clean-up" ~doc:"Destroy the VM(s)"
      (fun () -> test#clean_up) in
  let do_all =
    sub_command_of_target ~name:"all" ~doc:"Do the whole test at once"
      (fun () -> test#do_all) in
  let ssh =
    let doc =
      sprintf "SSH into a VM (%s)"
        (String.concat ", " (List.map ~f:(sprintf "%S") test#box_names)) in
    sub_command ~info:Term.(info "ssh" ~version ~doc)
      ~term:Term.(
        pure (fun name -> test#ssh name)
        $ Arg.(required @@ pos 0 (some string) None
               @@ info [] ~doc:"Name of the VM"))
  in
  let default_cmd =
    let doc = "Integration Test for Ketrew" in
    let man = [] in
    sub_command
      ~term:Term.(ret (pure (`Help (`Plain, None))))
      ~info:(Term.info "ketrew-integration-test" ~version ~doc ~man) in
  let cmds = [
    prepare;
    go;
    clean_up;
    do_all;
    ssh;
  ] in
  match Term.eval_choice  default_cmd cmds with
  | `Ok () -> ()
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0

