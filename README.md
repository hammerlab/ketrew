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

    ketrew init

This creates `$HOME/.ketrew/configuration.toml` (see `ketrew init --help` to
choose another path, and see the [documentation](src/doc/configuration_file.md)
on the configuration file to tweak it).

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


See the file [`src/test/cli.ml`](src/test/cli.ml) for more examples (*work-in-progress*),
and the [documentation of the EDSL API](src/lib/ketrew_edsl.mli).


### Ketrew

Ketrew is driven from the command-line client.  See first:
`ketrew --help`; then:

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

There is an HTTP API (work in progress); with the commands
`ketrew start-server` and `ketrew stop-server`.

License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).



