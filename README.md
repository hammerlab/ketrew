Ketrew: Keep Track of Experimental Workflows
============================================

**Ketrew** is:

- an OCaml library providing an EDSL API to define complex and convoluted
workflows (interdependent steps/programs using a lot of data, with many
parameter variations, running on different *hosts* with various schedulers).
- an engine taking care of orchestrating the run of those workflows, 
and keeping track everything that succeeds, fails, or gets lost.
Ketrew can be a standalone application, or use a client-server architecture.

This documentation is available at <http://hammerlab.github.io/ketrew/>,
the source is at <https://github.com/hammerlab/ketrew/>.

This is **Work in Progress**, not ready general for use.

Build & Install
---------------

### From Opam

If you have `opam` up and running:

    opam remote add smondet git@github.com:smondet/dev-opam-repo
    opam install ketrew

Then you need at runtime `ssh` and `git` in the `$PATH`.

This gets you the `ketrew` executable and the `ketrew` library.

The EDSL
--------

### Overview

The EDSL is an OCaml library where all the functions are used to build a
workflow data-structure *except* one: `Ketrew.EDSL.run` which is used to submit
workflows to the engine.

A workflow is a Graph of “**targets**”.

The links between targets are “dependencies” (targets that need to be ensured or
run before a target can start) or “fallbacks” (targets that will be activated
if the target fails).

Any OCaml program can use the EDSL (script, compiled, or even inside the
toplevel), see the [documentation of the EDSL API](src/lib/ketrew_edsl.mli).

<small><blockquote>Please, also feel free to
[propose](https://github.com/hammerlab/ketrew/issues)
better names for things in the API.</blockquote></small>

### Example

This example is a “single-target” workflow that runs an arbitrary shell command on an
[LSF-based](http://en.wikipedia.org/wiki/Platform_LSF) cluster:

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

You can also explore [examples of more and more complicated
workflows](src/test/Workflow_Examples.ml) (*work-in-progress*).

The Engine
----------

### For The Impatient

If you want to run any of the
[workflow examples](src/test/Workflow_Examples.ml):

    ketrew init
    _obuild/ketrew-cli-test/ketrew-cli-test.asm  <arguments>
    ketrew run loop
    # Type 'q' anytime or when it's done
    ketrew explore
    # And follow the navigation menu to check the status of your workflow

### Initialization

To create a configuration file, run:

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

- To display the current status: `ketrew status`.
- To run as many steps as possible until a “fix-point” is reached:
`ketrew run fix` (see `ketrew run-engine --help`).
- To kill running jobs use `ketrew kill` + the target Identifier,<br/>
or do an interactive murder: `ketrew kill --interactive`
- Once targets are dead, one can “archive” them, i.e. put them in a
less-visible list: `ketrew archive --help`.

See also `ketrew interact` or `ketrew explore` for fun
*one-key-based* navigation.

Ketrew can try to be clever about killing an archiving;
see `ketrew autoclean --help`.

### Client-Server Mode

In this mode, the Ketrew engine runs a proper server which is
accessed over an HTTP API.

See the commands `ketrew start-server` and `ketrew stop-server`.

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
- You may want to call out directly to the [HTTP API](src/doc/The_HTTP_API.md)
(i.e. without `ketrew` as a client).
- If you want to help or simply to understand Ketrew
see the [development](src/doc/Developer_Documentation.md)
documentation, and have a look at the modules
like [`Ketrew_engine`](src/lib/ketrew_engine.mli).

License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).



