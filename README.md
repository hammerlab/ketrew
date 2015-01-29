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

This is Ketrew `0.0.0+master` the development version of the system,
see also the
[`0.0.0`](http://seb.mondet.org/software/ketrew/doc.0.0.0/index.html) release.

Build & Install
---------------

Ketrew requires at least OCaml **4.01.0** and should be able to build & work on
any Unix platform.

### From Opam

If you have `opam` up and running:

    opam remote add smondet git@github.com:smondet/dev-opam-repo
    opam install ketrew

Then you need at runtime `ssh` in the `$PATH`.

This gets you the `ketrew` executable and the `ketrew_data` and `ketrew`
libraries.

### Without Opam

See the [development documentation](src/doc/Developer_Documentation.md) to find
out how to build Ketrew (and its dependencies) from the sources.


The EDSL: Defining Workflows
----------------------------

### Overview

The EDSL is an OCaml library where all the functions are used to build a
workflow data-structure *except* one: `Ketrew.EDSL.run` which is used to submit
workflows to the engine.

A workflow is a Graph of “**targets**”.

There are 3 kinds of links between targets:

- *dependencies:* targets that need to be ensured or
run before a target can start,
- *fallbacks:* targets that will be activated
if the target fails, and
- *success-triggers:* targets that will be activated only *after* a target
succeeds.

Any OCaml program can use the EDSL (script, compiled, or even inside the
toplevel), see the [documentation of the EDSL API](src/lib/ketrew_edsl.mli).


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
  (* Then, `run` is the only function that “does” something, it submits the
     workflow to the engine: *)
  Ketrew.EDSL.run workflow
  (* If Ketrew is in Standalone mode, this means writing the workflow in the
     database (nothing runs yet, you need to run Ketrew's engine yourself).
     If Ketrew is in Client-Server mode, this means sending the workflow to the
     server over HTTPS. The server will start running the workflow right away.  *)
```

If you actually have access to an LSF cluster and want to try this workflow see
below: [“For The Impatient”](#ForTheImpatient).

To learn more about the EDSL, you can also explore [examples of more and more
complicated workflows](src/test/Workflow_Examples.ml) (*work-in-progress*).

The Engine: Running Stuff
-------------------------

### For The Impatient

Let's say the example above is in a file `my_first_workflow.ml`:

The first time you use Ketrew, you need to call `init`:

    ketrew init

Then you can *submit* your workflow:

    ocaml my_first_workflow.ml 'du -sh $HOME'

When the function `Ketrew.EDSL.run` is called, the workflow will be *submitted*
but not yet running. To run the *engine* do:

    ketrew run loop

The engine will run until you type `q` or until there is nothing left to do.

Anytime, you can go a check the status and do many things with your
workflows, for example with:

    ketrew interact

which is an interactive text-based interface.

### Initialization

Let's go back to the beginning; to create a configuration file, run:

    ketrew init

This creates `$HOME/.ketrew/configuration.toml` (see `ketrew init --help` to
choose another path).

By default this configures Ketrew in **Standalone** mode;
See the [documentation](src/doc/The_Configuration_File.md)
on the configuration file to tweak it.

### Standalone Ketrew

The default for Ketrew is to run in “Standalone” mode.
From the command-line client, one can both query and run the engine.  See
first: `ketrew --help`; then:

- To display the current status: `ketrew status --help`.
- To run as many steps as possible until a “fix-point” is reached:
`ketrew run fix` (see `ketrew run-engine --help`).
- To kill running jobs use `ketrew kill` + the target Identifier,<br/>
or do an interactive murder: `ketrew kill --interactive`
(see `ketrew kill --help`).
- Once targets are dead, one can “archive” them, i.e. put them in a
less-visible list: `ketrew archive --help`.

See also `ketrew interact --help` or `ketrew explore --help` for fun
*one-key-based* navigation.

Ketrew can try to be clever about killing an archiving;
see `ketrew autoclean --help`.

### Client-Server Mode

In this mode, the Ketrew engine runs a proper server which is
accessed over an HTTP API.

See the commands `ketrew start-server --help`
and `ketrew stop-server --help`.

The client works in the same way as in “Standalone” mode.

Where to Go Next
----------------

From here:

- To write workflows for Ketrew,
see [`src/test/Workflow_Examples.ml`](src/test/Workflow_Examples.ml) for
examples and the [documentation of the EDSL API](src/lib/ketrew_edsl.mli).
- To configure Ketrew use the configuration file
[documentation](src/doc/The_Configuration_File.md).
- You may want to “extend” Ketrew with new ways of running “long-running"
computations:  see the documentation on
[plugins](src/doc/Long-Running_Plugins.md),
and the examples in the library:
like [`Ketrew_lsf`](src/lib/ketrew_lsf.mli) or in the tests:
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
like [`Ketrew_engine`](src/lib/ketrew_engine.mli).

License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).



