Ketrew: Keep Track of Experimental Workflows
============================================

**Ketrew** is:

1. An OCaml library providing an EDSL API to define complex and convoluted
workflows (interdependent steps/programs using a lot of data, with many
parameter variations, running on different hosts with various schedulers).
2. A client-server application to interact with these workflows.
The engine at heart of the server takes care of orchestrating workflows,
and keeps track of everything that succeeds, fails, or gets lost.

This is the master branch of Ketrew.
See also the documentation for the latest release:
[2.0.0](http://seb.mondet.org/software/ketrew/doc.2.0.0/index.html).

If you have any questions, you may submit an
[issue](https://github.com/hammerlab/ketrew/issues), or join
the authors on the public “Slack” channel of the Hammer Lab:
[![Slack Status](http://publicslack.hammerlab.org/badge.svg)](http://publicslack.hammerlab.org)

Build & Install
---------------

Ketrew requires at least OCaml **4.02.2** and should be able to build & work on
any Unix platform.

### From Opam

If you have [opam](http://opam.ocaml.org/) up and running, just install Ketrew
while choose a database backend (you may pick both and choose later in the
config-file):

    opam install  (sqlite3 | postgresql) [ssl | tls]  ketrew

- you need to choose a database backend `sqlite` or `postgresql`
  (you may install both and choose later in the config-file),
- if you want Ketrew to use HTTPS you need to get it linked
  with OpenSSL (package `ssl`) or [nqsb-TLS](https://nqsb.io/) (package
  `tls`, *experimental*).

This gets you

- a `ketrew` executable that can be used to schedule and run workflows,
- an OCaml library also called `ketrew` that handles the messy orchestration of
  those tasks and exports the `Ketrew.EDSL` module used to write workflows.

Remember that at runtime you'll need `ssh` in your `$PATH` to execute commands on
foreign hosts.

*Optional*: Ketrew, like any [Lwt](http://ocsigen.org/lwt/)-based piece of
software, will be much faster and scalable when `libev` is detected and used
as a backend. Use `opam install conf-libev` to tell opam that `libev` is
[installed](http://opam.ocaml.org/packages/conf-libev/conf-libev.4-11/), which
you can ensure with

  - `brew install libev` on MacOSX
  - `apt-get install libev-dev`on Debian/Ubuntu,
  - `yum install libev-devel` on CentOS (which requires
    `export C_INCLUDE_PATH=/usr/include/libev/` and `export LIBRARY_PATH=/usr/lib64/`

before `opam install conf-libev`.

### Without Opam

See the [development documentation](src/doc/Developer_Documentation.md) to find
out how to build Ketrew (and its dependencies) from source.


Getting Started
---------------

Ketrew is very flexible and hence may seem difficult to understand at first. 
Let's get a minimal workflow running.

Before you can use Ketrew, you need to configure it:

    $ ketrew init

By default this will write a configuration file & list of authorized tokens
for the Ketrew server in

    $ ls $HOME/.ketrew/
    authorized_tokens	configuration.ml

You can check that the client or the server are configured (the client is
returned by default) by using the `print-configuration` subcommand:

    $ ketrew print-configuration
    [ketrew]
        Mode: Client
        Connection: "http://127.0.0.1:8756"
        Auth-token: "755nRor8Q5z5nx7W22C6C078HF3YoY5PS29sEgNXxP4="
        UI:
            Colors: with colors
            Get-key: uses `cbreak`
            Explorer:
                Default request: Targets younger than 1.5 days
                Targets-per-page: 6
                Targets-to-prefectch: 6
        Misc:
            Debug-level: 0
            Plugins: None
            Tmp-dir: Not-specified (using /tmp/)

For the server (using `pc`, a command alias for `print-configuration`):

    $ ketrew pc server
    [ketrew] 
        Mode: Server
        Engine:
            Database: "/home/hammerlab/.ketrew/database"
            Unix-failure: does not turn into target failure
            Host-timeout-upper-bound:
            Maximum-successive-attempts: 10
            Concurrent-automaton-steps: 4
            Archival-age-threshold: 10.000000 days
        UI:
            Colors: with colors
            Get-key: uses `cbreak`
            Explorer:
                Default request: Targets younger than 1.5 days
                Targets-per-page: 6
                Targets-to-prefectch: 6
        HTTP-server:
            Authorized tokens:
                Inline (Name: l8Tm7Gv6veO1vYB9Fvc-ZnDwwsXXKbaKE4Vn5zcopOk=,
                Value: "l8Tm7Gv6veO1vYB9Fvc-ZnDwwsXXKbaKE4Vn5zcopOk=")
                Path: "/home/hammerlab/.ketrew/authorized_tokens"
            Daemonize: false
            Command Pipe: Some "/home/hammerlab/.ketrew/command.pipe"
            Log-path: Some "/home/hammerlab/.ketrew/server-log"
            Return-error-messages: true
            Max-blocking-time: 300.
            Listen: HTTP: 8756
      Misc:
          Debug-level: 0
          Plugins: None
          Tmp-dir: Not-specified (using /tmp/)

Furthermore, `daemon` is a shortcut for starting the `server` in
[daemon](https://en.wikipedia.org/wiki/Daemon_%28computing%29) mode. You may
now start a server:

    $ ketrew start-server --configuration-profile daemon

Let's open the GUI:

    $ ketrew gui

Which should open your browser.

<div>
<img  width="100%"
src="https://cloud.githubusercontent.com/assets/617111/11070327/07e7f63e-87a9-11e5-9a55-1e5f1baedb29.png"
>
</div>

Back at the command line you can always check the server's status (using the
shorter command line argument `-P`, instead of `--configuration-profile`):

    $ ketrew status -P daemon
    [ketrew] The server appears to be doing well.

The `ketrew submit` sub-command can create tiny workflows:

    ketrew submit --wet-run --tag 1st-workflow --tag command-line --daemonize /tmp/KT,"du -sh $HOME"

The job will appear on the WebUI and you can inspect/restart/kill it.

<div>
<img width="100%"
  src="https://cloud.githubusercontent.com/assets/617111/9421006/17bceb36-483a-11e5-8845-bb2234697a14.gif">
</div>


If you don't like Web UI's you can use the text-based UI:

    $ ketrew interact
    [ketrew]
        Main menu
        Press a single key:
        * [q]: Quit
        * [v]: Toggle verbose
        * [s]: Display current status
        * [l]: Loop displaying the status
        * [k]: Kill targets
        * [e]: The Target Explorer™

Finally to stop the server:

    $ ketrew stop -P daemon
    [ketrew] Server killed.

As you can see, just from the command line, you can use `ketrew submit` to
launch tasks. But to go further we need to use an EDSL.

The EDSL: Defining Workflows
----------------------------

### Overview

The EDSL is an OCaml library where functions are used to build a workflow
data-structure. `Ketrew.Client.submit_workflow` is used to submit that
datastructure to the engine.

A workflow is a graph of “workflow-nodes” (sometimes called “targets”).

There are three kinds of links (edges) between nodes:

- `depends_on`: nodes that need to be ensured or satisfied before a node
  can start,
- `on_failure_activate`: nodes that will be activated if the node fails, and
- `on_success_activate`: nodes that will be activated only *after* a node
  succeeds.

See the `Ketrew.EDSL.workflow_node` function documentation for details. Any
OCaml program can use the EDSL (script, compiled, or even inside the
toplevel). See the documentation of the EDSL API (`Ketrew.EDSL`).

### Example

The following script extends the previous shell-based example with the
capability to send emails upon the success or failure of your command.

```ocaml
#use "topfind"
#thread
#require "ketrew"

let run_command_with_daemonize ~cmd ~email =
  let module KEDSL = Ketrew.EDSL in

  (* Where to run stuff *)
  let host = KEDSL.Host.tmp_on_localhost in

  (* A “program” is a datastructure representing an “extended shell script”. *)
  let program = KEDSL.Program.sh cmd in

  (* A “build process” is a method for making things.

     In this case, `daemonize` creates a datastructure that represents a job
     running our program on the host. *)
  let build_process = KEDSL.daemonize ~host program in
  (* On Mac OSX
  let build_process = KEDSL.daemonize ~using:`Python_daemon ~host program in
  *)

  (* A node that Ketrew will activate after cmd completes *)
  let email_target ~success =
    let sstring = if success then "succeeded" else "failed" in
    let e_program =
      KEDSL.Program.shf "echo \"'%s' %s\" | mail -s \"Status update\" %s"
        cmd sstring
        email
    in
    let e_process =
      KEDSL.daemonize ~using:`Python_daemon ~host e_program in
    KEDSL.workflow_node KEDSL.without_product
      ~name:("email result " ^ sstring)
      ~make:e_process
  in

  (* The function `KEDSL.workflow_node` creates a node in the workflow graph.
     The value `KEDSL.without_product` means this node does not
     “produce” anything, it is like a `.PHONY` target in `make`. *)
  KEDSL.workflow_node KEDSL.without_product
    ~name:"daemonize command"
    ~make:build_process
    ~edges:[
      KEDSL.on_success_activate (email_target true);
      KEDSL.on_failure_activate (email_target false);
    ]

let () =
  (* Grab the command line arguments. *)
  let cmd   = Sys.argv.(1) in
  let email = Sys.argv.(2) in

  (* Create the  workflow with the first argument of the command line: *)
  let workflow = run_command_with_daemonize ~cmd ~email in

  (* Then, `Client.submit_workflow` is the only function that “does”
     something, it submits the workflow to the engine: *)
  Ketrew.Client.submit_workflow workflow
```

You can run this [script](src/example_scripts/daemonize_workflow.ml) from the
shell with

    ocaml daemonize_workflow.ml 'du -sh $HOME' myaddress@email.com
    
Checking in with the gui, we'll have a couple of new targets:

<div>
<img width="100%"
src="https://cloud.githubusercontent.com/assets/617111/11070354/32521fb2-87a9-11e5-993e-550db476cbd7.png"
</div>


To learn more about the EDSL, you can also explore [examples of more and more
complicated workflows](src/test/Workflow_Examples.ml) (*work-in-progress*).

Troubleshooting
---------------

- Trying to use use Sqlite3 on MacOSX, and `opam` fail? These
  [instructions](https://github.com/smondet/trakeva#sqlite3-on-macosx)
  should be helpful.
- `opam` and `ssl` errors when install `ketrew`? Please see this
  [issue](https://github.com/hammerlab/ketrew/issues/214).
- When reconfiguring `Ketrew` between versions it may be helpful to delete old
  configurations:

        $ rm -fr $HOME/.ketrew/

- During configuration it is recommended that you pass an authentication token,
  as opposed to having Ketrew generate one for you:

        $ ketrew init --with-token my-secret-token

- If you are trying the example workflow on a system that does not have Python
  installed you can use another deamonization method (we use `` `Python_daemon``
  by default above because
  [setsid](http://man7.org/linux/man-pages/man1/setsid.1.html) is missing on
  MacOSX):
        
        let build_process = KEDSL.daemonize ~using:`Nohup_setsid ~host program in



Where to Go Next
----------------

From here:

- To write workflows for Ketrew,
see [`src/test/Workflow_Examples.ml`](src/test/Workflow_Examples.ml) for
examples and the [documentation of the EDSL API](src/lib/eDSL.mli).
- To configure Ketrew use the configuration file
[documentation](src/doc/The_Configuration_File.md).
- If you don't want a server running and listening on HTTP(S), Ketrew can run a
  *degraded* mode called [“standalone.”](src/doc/Standalone_Mode.md)
- You may want to “extend” Ketrew with new ways of running “long-running"
computations:  see the documentation on
[plugins](src/doc/Long-Running_Plugins.md),
and the examples in the library:
like [`Ketrew.Lsf`](src/lib/lsf.mli) or in the tests:
[`src/test/dummy_plugin.ml`](src/test/dummy_plugin.ml).
- You may want to extend Ketrew, or preconfigure it, *without* configuration
files or dynamically loaded libraries: just
[create](src/doc/Alternative_CLI_Application.md) your own comand-line app.
- If you are using Ketrew in server mode, you may want to know about the
[commands](src/doc/Server_Commands.md) that the server can understand as it
listens on a Unix-pipe.
- You may want to call out directly to the [HTTP API](src/doc/The_HTTP_API.md)
(i.e. without `ketrew` as a client).
- If you want to help or simply to understand Ketrew
see the [development](src/doc/Developer_Documentation.md)
documentation, and have a look at the modules
like [`Ketrew.Engine`](src/lib/engine.mli).

License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).

Badges
------

[![master Branch Build Status](https://travis-ci.org/hammerlab/ketrew.svg?branch=master)](https://travis-ci.org/hammerlab/ketrew)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.45295.svg)](http://dx.doi.org/10.5281/zenodo.45295)


