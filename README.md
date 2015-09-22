Ketrew: Keep Track of Experimental Workflows
============================================

[![Build Status](https://travis-ci.org/hammerlab/ketrew.svg?branch=master)](https://travis-ci.org/hammerlab/ketrew)

**Ketrew** is:

- an OCaml library providing an EDSL API to define complex and convoluted
workflows (interdependent steps/programs using a lot of data, with many
parameter variations, running on different *hosts* with various schedulers).
- an engine taking care of orchestrating the run of those workflows,
and keeping track everything that succeeds, fails, or gets lost.
Ketrew can be a standalone application, or use a client-server architecture.

This is the master branch of Ketrew.
See also the documentation for the latest release: 
[2.0.0](http://seb.mondet.org/software/ketrew/doc.2.0.0/index.html).

Build & Install
---------------

Ketrew requires at least OCaml **4.02.2** and should be able to build & work on
any Unix platform.

### From Opam

If you have `opam` up and running, just install Ketrew while choose a database
backend (you may pick both and choose later in the config-file):

    opam install  (sqlite | postgresql)  ketrew

Then you need at runtime `ssh` in the `$PATH`.

This gets you the `ketrew` executable and the `ketrew_pure` and `ketrew`
libraries.

### Without Opam

See the [development documentation](src/doc/Developer_Documentation.md) to find
out how to build Ketrew (and its dependencies) from the sources.


Getting Started
---------------

Ketrew is very flexible and hence may seem difficult to understand, let's get a
very minimalistic workflow running.

The first time you use Ketrew, you need to configure it, simplest by calling
`ketrew init`, and please choose an authentication token:

    rm -fr ~/.ketrew/ # if you had a previous configuration there
    ketrew init --with-token my-not-so-secret-token
    
by default this will configure Ketrew in `$HOME/.ketrew/` with a client/server
mode **not** using TLS on port `8756` and with a local Sqlite database
(use the option `--use-database URI` to choose another
[database backend](src/doc/Database_Backends.md)).
See `ketrew init --help` for more
options, you can even ask it to generate self-signed TLS certificates.
See also the [documentation](src/doc/The_Configuration_File.md)
on the configuration file learn how to tweak it.

You can check that the client or the server are configured:

    ketrew print-configuration
    ketrew print-configuration --configuration-profile server
    ketrew print-configuration -P daemon

You may then start a server:

    KETREW_PROFILE=daemon ketrew start-server

and then open the GUI:

    ketrew gui

which is just trying to load
<http://127.0.0.1:8756/gui?token=my-not-so-secret-token> ☺

You can always stop the server or check its status:

    ketrew stop -P daemon
    ketrew status -P daemon

The `ketrew submit` sub-command can create tiny workflows:

    ketrew submit --wet-run --tag 1st-workflow --tag command-line --daemonize /tmp/KT,"du -sh $HOME"
    
The job will appear on the WebUI and you can inspect/restart/kill it.

<div>
<img width="100%"
  src="https://cloud.githubusercontent.com/assets/617111/9421006/17bceb36-483a-11e5-8845-bb2234697a14.gif">
</div>

If you don't like Web UI's you can use the text-based UI:

    ketrew interact


The EDSL: Defining Workflows
----------------------------

The previous section uses `ketrew submit` to launch an extremely simple
workflow, to go further we need the EDSL.

### Overview

The EDSL is an OCaml library where all the functions are used to build a
workflow data-structure. Then, one function: `Ketrew.Client.submit` is used to
submit workflows to the engine.

A workflow is a Graph of “**targets**”.

There are 3 kinds of links between targets:

- *dependencies:* targets that need to be ensured or
run before a target can start,
- *fallbacks:* targets that will be activated
if the target fails, and
- *success-triggers:* targets that will be activated only *after* a target
succeeds.

Any OCaml program can use the EDSL (script, compiled, or even inside the
toplevel), see the [documentation of the EDSL API](src/lib/eDSL.mli).


### Example

This example is a “single-target” workflow that runs an arbitrary shell command
on an [LSF-based](http://en.wikipedia.org/wiki/Platform_LSF) cluster:

```ocaml
#use "topfind"
#thread
#require "ketrew"
let run_command_with_lsf cmd =
  let module KEDSL = Ketrew.EDSL in
  let host =
    (* `Host.parse` takes an URI and creates a “Host” datastructue: a place to
       run stuff.  *)
    KEDSL.Host.parse
      "ssh://user42@MyLSFCluster/home/user42/ketrew-playground/?shell=bash"
    (* This one is an SSH host, named `MyLSFCluster`.
       The directory `/home/user42/ketrew-playground/` will be used by Ketrew
       to monitor the jobs. *)
  in
  let program =
    (* A “program” is a datastructure representing “extended shell scripts”.
       `Program.sh` creates one out a shell command. *)
    KEDSL.Program.sh cmd in
  let lsf_build_process =
    (* “build process” is a method for making things:
       `lsf` creates a datastructure that represents a job running a `program`
       with the LSF scheduling engine, on the host `host`.  *)
    KEDSL.lsf
      ~queue:"normal-people" ~wall_limit:"1:30"
      ~processors:(`Min_max (1,1)) ~host program
  in
  (* The function `KEDSL.target` creates a node in the workflow graph.
     This one is very simple, it has a name and a build-process,
     and since it doesn't have dependencies or fallbacks, it is a
     “single-node” workflow: *)
  KEDSL.target
     "run_command_with_lsf"
     ~make:lsf_build_process

let () =
  let workflow =
     (* Create the  workflow with the first argument of the command line: *)
     run_command_with_lsf Sys.argv.(1) in
  (* Then, `Client.submit` is the only function that “does” something, it
     submits the workflow to the engine: *)
  Ketrew.Client.submit workflow
  (* If Ketrew is in Standalone mode, this means writing the workflow in the
     database (nothing runs yet, you need to run Ketrew's engine yourself).
     If Ketrew is in Client-Server mode, this means sending the workflow to the
     server over HTTPS. The server will start running the workflow right away.  *)
```

If you actually have access to an LSF cluster and want to try this workflow,
put it in a file `my_second_workflow.ml`, and simply:

    ocaml my_second_workflow.ml 'du -sh $HOME'

To learn more about the EDSL, you can also explore [examples of more and more
complicated workflows](src/test/Workflow_Examples.ml) (*work-in-progress*).


Where to Go Next
----------------

From here:

- To write workflows for Ketrew,
see [`src/test/Workflow_Examples.ml`](src/test/Workflow_Examples.ml) for
examples and the [documentation of the EDSL API](src/lib/ketrew_edsl.mli).
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



