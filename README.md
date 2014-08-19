Ketrew: Keep Track of Experimental Workflows
============================================

Ketrew is:

- an OCaml library providing an EDSL API to define complex and convoluted
workflows (interdependent steps/programs using a lot of data, with many
parameter variations, running on different *hosts* with various schedulers).
- an engine taking care of orchestrating the run of those workflows, 
and keeping track everything that succeeds, fails, or gets lost.

This documentation is available at <http://hammerlab.github.io/ketrew/>,
the source is at <https://github.com/hammerlab/ketrew/>.

This is **Work in Progress**, not ready general for use.
[![Build Status](https://travis-ci.org/hammerlab/ketrew.svg?branch=master)](https://travis-ci.org/hammerlab/ketrew)


Build & Install
---------------

### From Opam

If you have `opam` up and running:

    opam remote add smondet git@github.com:smondet/dev-opam-repo
    opam install ketrew


Usage
-----

### Initialization

To create a configuration file, run:

    ketrew-client init

This creates `$HOME/.ketrew/client.toml` (see `ketrew-client init --help` to
choose another path).

### Workflow Scripts

A workflow script is an OCaml file (script, compiled, or even inside the
toplevel).

One can add functions that create and `run` workflows:

```ocaml
let run_command_with_lsf cmd =
  let open Ketrew.EDSL in
  let host = 
    parse_host "ssh://user42@MyLSFCluster/home/user42/ketrew-playground/?shell=bash" in
  let queue = "normalpeople" in
  let program = Program.sh cmd in
  run (
    target "run_command_with_lsf"
      ~make:(lsf ~queue ~wall_limit:"1:30" ~processors:(`Min_max (1,1)) ~host program)
  )

let () = (* Extremely basic main of the workflow script *)
  run_command_with_lsf Sys.argv.(1)
```

then one should be able to add and activate mini-workflow with:

    <script.ml> "<some shell command to run on the cluster>"


See the file `src/test/cli.ml` for more examples (*work-in-progress*),
and the [documentation of the EDSL API](src/lib/ketrew_edsl.mli).


### Ketrew-client

Ketrew is driven from the command-line client.  See:

    ketrew-client --help

To display the current status:

    ketrew-client status

To run as many steps as possible until a “fix-point” is reached:

    ketrew-client run fix

To kill running jobs use

    ketrew-client kill <target-ID>

or do an interactive murder:

    ketrew-client kill --interactive

Once targets are dead, one can “archive” them, i.e. put them in a less-visible
list.

    ketrew-client archive [--interactive] [<target-ID>]*

See also `ketrew-client interact` or `ketrew-client explore` for fun
*one-key-based* navigation.

### The Configuration File

The general format used to the configuration file is
[Toml](https://github.com/toml-lang/toml).
Here is a complete and commented example:

```toml
# Ketrew configuration file

# How much noise do you want on your terminal:
debug-level = 2

# When an SSH or system call fails it may not mean that the command in your
# workflow is wrong (could be an SSH config or tunneling problem), by default,
# Ketrew tries to be clever and does not make targets fail. To change this
# behavior put the following option to `true`:
turn-unix-ssh-failure-into-target-failure = false

# Unless explicitly told, all SSH/system calls are bounded by a timeout.  A
# given Host can have a specific timeout, but Ketrew has an upper-bound of all
# timeouts allowed  (default 60 seconds) here is how to set it:
host-timeout-upper-bound = 120

[client]
  # One can remove the colors in the output of the client with:
  color = false


[database]
  # The path to the database (mandatory):
  path = "/path/to/some/file"
  # What is the 'master-key' of the state inside the database
  # (normal users should not change that):
  state-key = "some-string"
```

One can test their configuration with:

    ketrew-client print-configuration

License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).



