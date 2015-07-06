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

Big Integration Test
--------------------

M*)

open Nonstd
module String = Sosa.Native_string
let say fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
let (//) = Filename.concat
let failwithf fmt = ksprintf failwith fmt

let shellf ?(returns=Some 0) fmt =
  Printf.(ksprintf (fun s ->
      match Sys.command s, returns with
      | n, Some m when n = m -> ()
      | _, None -> ()
      | other, Some m ->
        failwith (sprintf "Command %S returned %d instead of %d" s other m)
    ) fmt)

let test_dir =
  Sys.getenv "PWD" // "_integration_test"

module Test_host = struct

  let as_host () = Ketrew.EDSL.Host.parse "/tmp/ketrew-integration-test"

  let do_on ~program = 
    let open Ketrew.EDSL in
    let host = as_host () in
    daemonize ~using:`Python_daemon ~host program

  let write_file path ~content =
    let open Ketrew.EDSL in
    let host = as_host () in
    daemonize ~using:`Python_daemon ~host Program.(
        shf "mkdir -p %s" Filename.(quote (dirname path))
        && shf "echo %s > %s"
          Filename.(quote content)
          Filename.(quote path)
      )

  let tmp file = test_dir // file

  let to_kill_file = test_dir // "targets-to-kill"

  let test_target ?depends_on ?(and_then = []) ~name ~make should =
    let open Ketrew.EDSL in
    let result_file = tmp (name ^ "-result") in
    let expectations_file = tmp (name ^ "-expectation") in
    let on_success_activate =
      target (sprintf "register-success-of-%s" name)
        ~make:(write_file ~content:"OK" result_file)
      :: and_then in
    let on_failure_activate =
      target (sprintf "register-failure-of-%s" name)
        ~make:(write_file ~content:"KO" result_file)
      :: and_then in
    let t =
      target name
        ~tags:["to-kill"] ?depends_on ~make
        ~on_success_activate ~on_failure_activate in
    begin match should with
    | `Should_succeed -> shellf "echo OK > %s" expectations_file
    | `Should_fail -> shellf "echo KO > %s" expectations_file
    | `Should_be_killed ->
      shellf "echo KO > %s" expectations_file;
      shellf "echo %s >> %s" t#id to_kill_file;
    end;
    t

  let should_to_3rd_person_verb = function
  | `Should_fail -> "fails"
  | `Should_succeed -> "succeeds"

  let check_test_targets () =
    shellf "for exp in %s/*-expectation ; do echo $exp ; \
            diff $exp ${exp%%-expectation}-result ; done"
      test_dir

end

module Vagrant_box = struct

  (* 
  About Torque:
  https://github.com/audy/vagrant-torque
  https://redditjs.com/r/bioinformatics/comments/29xz66/vagranttorque_run_a_single_machine_torque_cluster/

https://github.com/vangj/vagrant-hadoop-2.4.1-spark-1.0.1

  *)
  type initialization = [
    | `Precise64
    | `Audy_torque
    | `Hadoop_spark_cluster
  ]

  type t = {
    dir: string;
    ssh_config: string;
    hostname: string;
    initialization: initialization;
  }

  let tmp_dir = Test_host.tmp "vagrant-boxes"

  let create initialization hostname =
    let dir = tmp_dir // hostname in
    {dir; ssh_config = dir // "ssh_config"; hostname; initialization;}

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

  let internal_hostname t =
    match t.initialization with
    | `Precise64 -> "precise64"
    | `Audy_torque -> "master"
    | `Hadoop_spark_cluster -> "TODO"

  let internal_username _ = "vagrant"

  let namify t sub =
    sprintf "%s: %s" t.hostname sub

  let prepare ?(force_hostname=true)
      ?on_success_activate ?on_failure_activate t =
    let open Ketrew.EDSL in
    let host = Test_host.as_host () in
    let init =
      let from_git url =
        let open Program in
        [shf "if [ -d %s ] ; then echo 'Already cloned' ; else \
              mkdir -p %s && cd %s && \
              git clone %s.git && \
              mv %s %s ; fi"
           (Filename.quote t.dir)
           (Filename.quote tmp_dir) (Filename.quote tmp_dir)
           url
           (Filename.basename url) (Filename.quote t.dir);
        ]
      in
      file_target ~name:(namify t "init-vagrant")
        (t.dir // "Vagrantfile")
        ~host
        ~make:(Test_host.do_on Program.(
            chain (
              match t.initialization with
              | `Precise64 -> [
                  in_dir t;
                  exec ["vagrant"; "init"; "hashicorp/precise64"];
                ]
              | `Audy_torque ->
                from_git "https://github.com/audy/vagrant-torque"
                  (*
                [shf "if [ -d %s ] ; then echo 'Already cloned' ; else \
  mkdir -p %s && cd %s && \
  git clone git@github.com:audy/vagrant-torque && \
                           mv vagrant-torque %s ; fi"
                   (Filename.quote t.dir)
                   (Filename.quote tmp_dir) (Filename.quote tmp_dir)
                   (Filename.quote t.dir);
                ] *)
              | `Hadoop_spark_cluster -> 
                (*
                sh "vagrant box add \
                 centos65 \
                 https://github.com/2creatives/vagrant-centos/releases/download/v6.5.1/centos65-x86_64-20131205.box"
                :: *)
                from_git "https://github.com/smondet/vagrant-hadoop-2.4.1-spark-1.0.1"
            )
          ))
    in
    let running =
      (* re-running `vagrant up` on a running machine does not fail. *)
      target (namify t "vagrant-up")
        ~depends_on:[init]
        ~make:(Test_host.do_on Program.(
            in_dir t && exec ["vagrant"; "up"]))
    in
    let make_ssh_config =
      target (* not a `file_target`, because we want this file regenerated 
                every time. We set the `product` but not the “condition”. *)
        (namify t "vagrant-ssh-config")
        ~depends_on:[running] ?on_success_activate ?on_failure_activate
        ~product:(file ~host:(Test_host.as_host ()) t.ssh_config)
        ~make:(Test_host.do_on Program.(
            in_dir t
            && shf "vagrant ssh-config %s > %s"
              (match force_hostname with
              | false  -> ""
              | true  -> sprintf "--host %s" t.hostname)
              t.ssh_config))
    in
    make_ssh_config

  let destroy t =
    let open Ketrew.EDSL in
    let rm_temp =
      target (namify t "rm-temp")
        ~make:(Test_host.do_on Program.(exec ["rm"; "-fr"; t.dir]))
    in
    let signal_failure =
      target (namify t "rm-temp")
        ~make:(Test_host.(
            write_file (tmp
                        @@ "results" // sprintf "%s-destroy-failure" t.hostname)
              ~content:t.ssh_config
          ))
    in
    target (namify t "kill-vagrant")
      ~make:(Test_host.do_on Program.(
          in_dir t && exec ["vagrant"; "destroy"; "-f"]
        ))
      ~on_success_activate:[rm_temp]
      ~on_failure_activate:[rm_temp; signal_failure]

  let ssh t =
    shellf "cd %s && vagrant ssh" t.dir

  let is_running t =
    try
      shellf ~returns:(Some 0) "cd %s && vagrant status | grep running" t.dir;
      true
    with _ -> false

  let exec_is_installed t ~exec =
    let host = as_host t in
    let shell_cmd = sprintf "which %s" exec in
    let open Ketrew.EDSL in
    Condition.program ~host Program.(sh shell_cmd)

  let do_on t ~program =
    let open Ketrew.EDSL in
    let host = as_host t in
    daemonize ~using:`Nohup_setsid ~host program

  let sudo cmd =
    let open Ketrew.EDSL in
    Program.( shf "sudo su -c %s" (Filename.quote cmd))

  let with_installed t ~packages =
    let open Ketrew.EDSL in
    let name = sprintf "apt-getting-%s" (String.concat ~sep:"-" packages) in
    target name
      ~tags:["integration"; "apt-get"]
      ~make:(do_on t Program.(
          sh "echo GO"
          && sh "sudo apt-get update"
          && shf "sudo apt-get install -y %s" (String.concat ~sep:" " packages)
        ))

  let file t ~path =
    let host = as_host t in
    Ketrew.EDSL.file ~host path

end

module Vagrant_hadoop_cluster = struct

  type t = {
    box: Vagrant_box.t;
  }
  let create () =
    let box = Vagrant_box.create `Hadoop_spark_cluster "HadoopSpark" in
    {box}
  let prepare {box} =
    Vagrant_box.prepare ~force_hostname:false box
  let destroy {box} =
    Vagrant_box.destroy box
  let ssh {box} node =
    shellf "cd %s && vagrant ssh %s" box.Vagrant_box.dir node
  let is_running {box} = Vagrant_box.is_running box

  let host {box} name = 
    let open Ketrew.EDSL in
    Host.ssh ~add_ssh_options:["-F"; box.Vagrant_box.ssh_config]
      ~playground:"/tmp/KT/" name

  let finish_setup t =
    let open Ketrew.EDSL in
    (* From https://github.com/vangj/vagrant-hadoop-2.4.1-spark-1.0.1/blob/master/README.md *)
    let namenode_setup = [
      "$HADOOP_PREFIX/bin/hdfs namenode -format myhadoop -force -nonInteractive";
      (* We add `-force -nonInteractive` because the commad asks `Y/N`
         questions by default *)
    ] in
    let hdfs_setup = [
      "$HADOOP_PREFIX/sbin/hadoop-daemon.sh \
       --config $HADOOP_CONF_DIR --script hdfs start namenode";
      "$HADOOP_PREFIX/sbin/hadoop-daemons.sh --config $HADOOP_CONF_DIR  \
       --script hdfs start datanode";
    ] in
    let yarn_setup = [
      "$HADOOP_YARN_HOME/sbin/yarn-daemon.sh --config $HADOOP_CONF_DIR start resourcemanager";
      "$HADOOP_YARN_HOME/sbin/yarn-daemons.sh --config $HADOOP_CONF_DIR start nodemanager";
      "$HADOOP_YARN_HOME/sbin/yarn-daemon.sh start proxyserver --config $HADOOP_CONF_DIR";
      "$HADOOP_PREFIX/sbin/mr-jobhistory-daemon.sh start historyserver --config $HADOOP_CONF_DIR";
    ] in
    let make = daemonize ~using:`Nohup_setsid in
    let on_node1 =
      let host = host t "node1" in
      target "Namenode + HDFS Setup"
        ~make:Program.(
            make ~host
              (chain (List.map ~f:(shf "sudo %s") (namenode_setup @ hdfs_setup)))
          )
    in
    let on_node2 =
      let host = host t "node2" in
      target "Yarn Setup"
        ~make:Program.(
            make ~host
              (chain (List.map ~f:(shf "sudo %s") yarn_setup))
          )
        ~depends_on:[on_node1]
    in
    let test_yarn_setup =
      let host = host t "node2" in
      let witness = "/home/vagrant/pi-2-100" in
      file_target ~name:"Test Yarn Setup"
        witness ~host
        ~make:Program.(
            make ~host (
              shf "yarn jar \
                  /usr/local/hadoop/share/hadoop/mapreduce/hadoop-mapreduce-examples-2.4.1.jar \
                   pi 2 100 > %s" witness
            )
          )
        ~depends_on:[on_node2]
    in
    test_yarn_setup

  let yarn_du_minus_sh ({box} as t) should =
    let open Ketrew.EDSL in
    let host = host t "node2" in
    let application_name =
      sprintf "yarn-du-sh-%s"
        (Test_host.should_to_3rd_person_verb should) in
    Test_host.test_target ~name:application_name should
      ~make:(yarn_distributed_shell 
               ~host ~container_memory:(`MB 120)
               ~timeout:(`Seconds 1800)
               ~distributed_shell_shell_jar:"/usr/local/hadoop-2.4.1/share/hadoop/yarn/hadoop-yarn-applications-distributedshell-2.4.1.jar"
               ~application_name Program.(
                   sh "hostname"
                   && (match should with
                       | `Should_succeed -> sh "du -sh /usr/local/"
                       | `Should_fail -> sh "du -sh /doesnotexist/")
                 ))

  let yarn_job_to_kill t =
    let open Ketrew.EDSL in
    let host = host t "node2" in
    let application_name = sprintf "yarn-job-to-kill" in
    Test_host.test_target ~name:application_name `Should_be_killed
      ~make:(yarn_distributed_shell 
               ~host ~container_memory:(`Raw "110")
               ~timeout:(`Raw "1800000")
               ~distributed_shell_shell_jar:"/usr/local/hadoop-2.4.1/share/hadoop/yarn/hadoop-yarn-applications-distributedshell-2.4.1.jar"
               ~application_name Program.(
                   sh "hostname" && sh "sleep 400"
                 ))


  let run_all_tests t =
    let open Ketrew.EDSL in
    target "Hadoop-tests coordinator"
      ~depends_on:[
        target "HadoopSpark-cluster post-install setup"
          ~depends_on:[finish_setup t]
          ~on_success_activate:[
            yarn_du_minus_sh t `Should_succeed;
            yarn_du_minus_sh t `Should_fail;
          ];
      ]

  let test_to_kill t =
    let open Ketrew.EDSL in
    target "Hadoop-to-kill-tests common ancestor"
      ~depends_on:[
        yarn_job_to_kill t;
      ]
end

module Lsf = struct
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
       |> String.concat ~sep:"\n")
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
    (List.map usernames ~f:(sprintf "%s") |> String.concat ~sep:" ")
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
      ~depends_on:[Vagrant_box.with_installed ~packages box]
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
      ~depends_on:[ installed; ]
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

  let lsf_job ?on_success_activate ?on_failure_activate ~box () =
    let open Ketrew.EDSL in
    let output = "/tmp/du-sh-dollar-home" in
    let host = Vagrant_box.as_host box in
    let name = "lsf-1" in
    file_target ~host ~name output ?on_success_activate ?on_failure_activate
      ~depends_on:[ ensure_lsf_is_running ~box ]
      ~tags:["integration"; "lsf"]
      ~make:(
        lsf ~host ~name Program.(shf "du -sh $HOME > %s" output
                                 && exec ["cat"; output])
          ~queue:"normal"
      )

  let test_lsf_job box should =
    let open Ketrew.EDSL in
    let host = Vagrant_box.as_host box in
    let name =
      sprintf "lsf-test-that-%s"
        (Test_host.should_to_3rd_person_verb should) in
    Test_host.test_target ~name should
      ~depends_on:[
        lsf_job ~box ();
        ensure_lsf_is_running ~box;
      ]
      ~make:(
        lsf ~host ~name ~queue:"normal"
          Program.(
            match should with
            | `Should_fail -> sh "ls /somewierd/path/"
            | `Should_succeed -> sh "du -sh $HOME"
          )
        )

  let run_all_tests box =
    Ketrew.EDSL.target "LSF tests common ancestor"
      ~depends_on:[
        test_lsf_job box `Should_fail;
        test_lsf_job box `Should_succeed;
      ]
end

(* Old stuff:
   let setup_pbs ~box () =
   let packages =
    ["torque-server"; "torque-scheduler"; "torque-client"; "torque-mom"] in
   let open Ketrew.EDSL in
   let setup = [
    sprintf "echo '%s' > /var/spool/torque/server_name"
      (Vagrant_box.internal_hostname box);
    "sed -i 's/127.0.1.1/127.0.0.1/' /etc/hosts";
   ] in
   target "install-pbs"
    (* ~done_when:Condition.( Vagrant_box.exec_is_installed box ~exec:"qsub") *)
    ~tags:["intergration"]
    ~depends_on:[Vagrant_box.with_installed ~packages box]
    ~make:(
      Vagrant_box.do_on box ~program:Program.(
          List.map setup Vagrant_box.sudo |> chain
        ))
*)
let finish_pbs_setup ~box () =
  let open Ketrew.EDSL in
  let host = Vagrant_box.as_host box in
  let witness = file ~host "/home/vagrant/.ssh/done" in
  let ssh_config =
    "Host master\n\
    \  StrictHostKeyChecking no\n\
    \  UserKnownHostsFile=/dev/null\n\
    " in
  let setup = [
    "ssh-keygen -t dsa -P '' -f ~/.ssh/id_dsa";
    "cat .ssh/id_dsa.pub >> .ssh/authorized_keys";
    sprintf "echo %s >> .ssh/config" (Filename.quote ssh_config);
    sprintf "echo Done > %s" witness#path;
  ] in
  target "install-pbs"
    (* ~done_when:Condition.( Vagrant_box.exec_is_installed box ~exec:"qsub") *)
    ~done_when:(witness#exists)
    ~tags:["intergration"]
    ~make:(Vagrant_box.do_on box
             ~program:Program.( chain (List.map ~f:sh setup)))

let pbs_job ?on_success_activate ?on_failure_activate ~box kind =
  let open Ketrew.EDSL in
  let setup = finish_pbs_setup ~box () in
  match kind with
  | `Always_runs ->
    let host = Vagrant_box.as_host box in
    let name = "pbs-2" in
    target name ?on_success_activate ?on_failure_activate
      ~depends_on:[setup]
      ~tags:["integration"; "pbs"]
      ~make:( pbs ~host ~name Program.( 
          shf "echo \"PBS-2 Starts:\n%s\""
            (List.map ~f:(fun s -> sprintf "%s: $%s\n" s s)
               ["PBS_O_HOST"; "PBS_SERVER"]
            |> String.concat ~sep:"")
          && sh "echo PBS-2 Starts >&2"
          && sh "sleep 10"
          && sh "echo PBS-2 Ends"
        ))
  | `File_target ->
    let output = "/tmp/du-sh-slash" in
    let host = Vagrant_box.as_host box in
    let name = "pbs-1" in
    file_target ~host ~name output ?on_success_activate ?on_failure_activate
      ~depends_on:[setup]
      ~tags:["integration"; "pbs"]
      ~make:(
        pbs ~host ~name Program.(
            sh "sleep 42"
            && shf "du -sh $HOME > %s" output
            && exec ["cat"; output])
        (* ~queue:"normal" *)
      )


let test =
  let lsf_host = Vagrant_box.create `Precise64 "LsfTestHost" in
  let pbs_host = Vagrant_box.create `Audy_torque "PbsTestHost" in
  let hadoop_host =
    Vagrant_hadoop_cluster.create () in
  object  (self)
    method prepare what =
      match what with
      | "ALL" ->
        let prepare_hadoop = Vagrant_hadoop_cluster.prepare hadoop_host in
        let prepare_pbs =
          Vagrant_box.prepare pbs_host
            ~on_success_activate:[prepare_hadoop]
            ~on_failure_activate:[prepare_hadoop]
        in
        let prepare_lsf = 
          Vagrant_box.prepare lsf_host
            ~on_success_activate:[prepare_pbs]
            ~on_failure_activate:[prepare_pbs]
        in
        Ketrew.EDSL.target "Trigger VM preparation" ~depends_on:[prepare_lsf]
      | "LSF" -> Vagrant_box.prepare lsf_host
      | "PBS" -> Vagrant_box.prepare pbs_host
      | "Hadoop" -> Vagrant_hadoop_cluster.prepare hadoop_host
      | other ->
        failwithf "Don't know how to “prepare” %S" other
    method go =
      Ketrew.EDSL.target "Run Tests"
        ~depends_on:[
          Lsf.run_all_tests lsf_host;
          pbs_job ~box:pbs_host `Always_runs;
          pbs_job ~box:pbs_host `File_target;
          Vagrant_hadoop_cluster.run_all_tests hadoop_host;        
        ]
    method start_jobs_to_kill =
      Ketrew.EDSL.target "Jobs to kill"
        ~depends_on:[
          Vagrant_hadoop_cluster.test_to_kill hadoop_host;        
        ]
    method clean_up =
      Ketrew.EDSL.target "Destroy VMs"
        ~depends_on:[
          Vagrant_box.destroy lsf_host;
          Vagrant_box.destroy pbs_host;
          Vagrant_hadoop_cluster.destroy hadoop_host;
        ]
    method box_names = ["LSF"; "PBS"; "Hadoop"]
    method ssh spec =
      match String.split spec ~on:(`Character ':') with
      | "LSF" :: [] -> Vagrant_box.ssh lsf_host
      | "PBS" :: [] -> Vagrant_box.ssh pbs_host
      | "Hadoop" :: node :: [] -> Vagrant_hadoop_cluster.ssh hadoop_host node
      | other -> failwithf "Unkown “Box”: %S" spec
    method is_running = function
    | "LSF" -> Vagrant_box.is_running lsf_host
    | "PBS" -> Vagrant_box.is_running pbs_host
    | "Hadoop" -> Vagrant_hadoop_cluster.is_running hadoop_host;
    | other -> failwithf "Unkown “Box”: %S" other

  end

let () =
  let open Cmdliner in
  let version = Lazy.force Ketrew_metadata.version in
  let sub_command ~info ~term = (term, info) in
  let sub_command_of_target ~name ~doc make_target =
    let the_name = name in
    sub_command
      ~info:Term.(info the_name ~version ~doc)
      ~term:Term.(
          pure (fun () ->
              Ketrew_client.submit (
                make_target ()
              ))
          $ pure ()
        )
  in
  let prepare =
    sub_command_of_target ~name:"prepare" ~doc:"Setup the VM(s)"
      (fun () -> test#prepare "ALL") in
  let hadoop_prepare =
    sub_command_of_target ~name:"hadoop-prepare" ~doc:"Setup the VM(s)"
      (fun () -> test#prepare "Hadoop") in
  let go =
    sub_command_of_target ~name:"go" ~doc:"Do the test"
      (fun () -> test#go) in
  let start_jobs_to_kill =
    sub_command_of_target ~name:"start-jobs-to-kill"
      ~doc:"Start jobs that should be killed"
      (fun () -> test#start_jobs_to_kill) in
  let clean_up =
    sub_command_of_target ~name:"clean-up" ~doc:"Destroy the VM(s)"
      (fun () -> test#clean_up) in
  let ssh =
    let doc =
      sprintf "SSH into a VM (%s)"
        (String.concat ~sep:", " (List.map ~f:(sprintf "%S") test#box_names)) in
    sub_command ~info:Term.(info "ssh" ~version ~doc)
      ~term:Term.(
          pure (fun name -> test#ssh name)
          $ Arg.(required @@ pos 0 (some string) None
                 @@ info [] ~doc:"Name of the VM"))
  in
  let is_running =
    let doc =
      sprintf "Check if a VM is running (%s)"
        (String.concat ~sep:", " (List.map ~f:(sprintf "%S") test#box_names)) in
    sub_command ~info:Term.(info "is-running" ~version ~doc)
      ~term:Term.(
          pure (fun name ->
              if test#is_running name then exit 0 else exit 3)
          $ Arg.(required @@ pos 0 (some string) None
                 @@ info [] ~doc:"Name of the VM"))
  in
  let check_tests =
    let doc = "Check that tests succeeded or failed appropriately" in
    sub_command ~info:Term.(info "check-tests" ~version ~doc)
      ~term:Term.(
          pure (fun () ->
              Test_host.check_test_targets ())
          $ pure ()) in
  let to_kill_list =
    let doc = "Display list of IDs to kill externally" in
    sub_command ~info:Term.(info "to-kill" ~version ~doc)
      ~term:Term.(
          pure (fun () ->
              shellf "cat %s" Test_host.to_kill_file)
          $ pure ()) in
  (* $ Arg.(unit)) in *)
  let default_cmd =
    let doc = "Integration Test for Ketrew" in
    let man = [] in
    sub_command
      ~term:Term.(ret (pure (`Help (`Plain, None))))
      ~info:(Term.info "ketrew-integration-test" ~version ~doc ~man) in
  let cmds = [
    prepare;
    hadoop_prepare;
    go;
    start_jobs_to_kill;
    clean_up;
    ssh;
    is_running;
    check_tests;
    to_kill_list;
  ] in
  match Term.eval_choice  default_cmd cmds with
  | `Ok () -> ()
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0

