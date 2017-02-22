Ketrew: Keep Track of Experimental Workflows
============================================

**Ketrew** is:

1. An OCaml library providing an EDSL API to define complex and convoluted
workflows (interdependent steps/programs using a lot of data, with many
parameter variations, running on different hosts with various schedulers).
2. A client-server application to interact with these workflows.
The engine at heart of the server takes care of orchestrating workflows,
and keeps track of everything that succeeds, fails, or gets lost.

This is the `master` branch of Ketrew.
See also the documentation for the latest release:
[3.0.0](http://www.hammerlab.org/docs/ketrew/doc.3.0.0/index.html).

If you have any questions, you may submit an
[issue](https://github.com/hammerlab/ketrew/issues), or join
the authors on the public “Slack” channel of the Hammer Lab:
[![Slack Status](http://publicslack.hammerlab.org/badge.svg)](http://publicslack.hammerlab.org)

Build & Install
---------------

See the specific documentation
on [building and installing](src/doc/Build_and_install.md).
*TD;LR for OCaml hackers:*
 
    opam switch 4.03.0
    opam install tls ketrew

Getting Started
---------------

Ketrew is very flexible and hence may seem difficult to understand and setup at
first.
Let's get a minimal setup ready and a workflow running on it.

### Server-side Setup

We use 
[`hammerlab/secotrec`](https://github.com/hammerlab/secotrec),
to get a practical Ketrew/Coclobas local setup.

For this we'll require Docker installed (usable by a regular UNIX user, and its
daemon running), to get a container environment with everything installed:

    docker pull hammerlab/secotrec
    docker run -v /var/run/docker.sock:/var/run/docker.sock -it hammerlab/secotrec bash

Then from this new shell:

    secotrec-local up

After a few minutes you can check that everything is setup by visiting
<http://127.0.0.1:8123/gui?token=nekot>:

<div>
<img  width="100%"
src="https://cloud.githubusercontent.com/assets/617111/23189040/047945d4-f85f-11e6-9453-feb3515fb7ca.png"
>
</div>

At any moment you can take everything down with:

    secotrec-local down

### Client

In this “demo” setup, we cannot access the Ketrew server from the current
container (docker “limitation”), so we jump to another container which is in the
right network:

    secotrec-local docker-compose exec coclo opam config exec bash

We can now create a Ketrew client configuration:

    ketrew init --configuration-path $HOME/kclient-config/ \
        --just-client http://kserver:8080/gui?token=nekot

The `ketrew submit` sub-command can create one-command workflows:

    ketrew submit --configuration-file $HOME/kclient-config/configuration.ml \
         --wet-run --tag 1st-workflow --tag command-line \
         --daemonize /tmp/KT,"du -sh $HOME"

The job will appear on the WebUI and you can inspect/restart/kill it.

<div>
<img width="100%"
  src="https://cloud.githubusercontent.com/assets/617111/9421006/17bceb36-483a-11e5-8845-bb2234697a14.gif">
</div>


If you don't like Web UI's you can use the text-based UI:

    $ ketrew interact --configuration-file $HOME/kclient-config/configuration.ml
    [ketrew]
        Main menu
        Press a single key:
        * [q]: Quit
        * [v]: Toggle verbose
        * [s]: Display current status
        * [l]: Loop displaying the status
        * [k]: Kill targets
        * [e]: The Target Explorer™

As you can see, just from the command line, you can use `ketrew submit` to
launch *daemonized* tasks. To go further we need to use Ketrew's EDSL.

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
  (* If the Ketrew server (i.e. `host`) is running Mac OSX, please use:
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


Where to Go Next
----------------

From here:

- To write workflows for Ketrew,
see [`src/test/Workflow_Examples.ml`](src/test/Workflow_Examples.ml) for
examples and the [documentation of the EDSL API](src/lib/eDSL.mli).
- To configure Ketrew use the configuration file
[documentation](src/doc/The_Configuration_File.md).
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


