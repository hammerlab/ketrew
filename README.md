Ketrew: Keep Track of Experimental Workflows
============================================

This is **Work in Progress**, not ready for use.

This documentation is available at <http://seb.mondet.org/ketrew/>.

Build & Install
---------------

### From Opam

If you have `opam` up and running:

    opam remote add smondet git@github.com:smondet/dev-opam-repo
    opam install type_conv
    opam install ketrew

(installing `type_conv` manually will be useless when
[`ocaml/opam-repository#2240`](https://github.com/ocaml/opam-repository/pull/2240)
is merged).

### Getting Just The Dependencies

Ketrew depends on

- `nonstd`: nano-library providing a portable extract of `Core_kernel`
- `pvem`: error monad
- `docout`: logging with `smart-print`
- `sosa`:  String module/functor
- `pvem_lwt_unix`: `Lwt_unix` wrapped in a `Pvem.t` (error monad) with more
precise error types.
- `uri`:
parse [RFC-3986](http://www.ietf.org/rfc/rfc3986.txt)-compliant URIs
(`uri` itself depends on `camlp4`).
- `cmdliner`: command line parsing
- `yojson`: JSON parsing/printing
- `atdgen/atd`: definition of serialization formats (used with `Yojson`).
- `toml`: config-file parsing

and uses the `ocp-build` build system.

The `please.sh` script can call `opam` itself:

    ./please.sh get-dependencies

### Build

Then you may setup and build everything:

    ./please.sh build

(for incremental compilation while developping please use: `ocp-build <target>`
directly)

### Install

For now, `ketrew` uses a custom install/uninstall procedure:

    ./please.sh install <prefix>

and uninstall with:

    ./please.sh uninstall <prefix>

### Build The Documentation

The documentation depends on [omd](https://github.com/ocaml/omd),
[higlo](http://zoggy.github.io/higlo/),
and Graphviz's `dot`:

    please.sh doc

and check-out `_doc/index.html`.

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
  run (
    target "run_command_with_lsf"
      ~make:(lsf ~queue ~wall_limit:"1:30" ~processors:(`Min_max (1,1)) ~host [cmd])
  )

let () = (* Extremely basic main of the workflow script *)
  run_command_with_lsf Sys.argv.(1)
```

then one should be able to add and activate mini-workflow with:

    <script.ml> "<some shell command to run on the cluster>"


See the file `src/test/cli.ml` for more examples (*work-in-progress*).


### Ketrew-client

Ketrew is driven from the command-line client.  See:

    ketrew-client --help

To display the current status:

    ketrew-client info

To run as many steps as possible until a “fix-point” is reached:

    ketrew-client run fix

To kill running jobs use

    ketrew-client kill <target-ID>

or do an interactive murder:

    ketrew-client kill --interactive


Tests
-----

Run the tests like this:

```bash
    ketrew_test_ssh=<Host> _obuild/ketrew-test/ketrew-test.asm [-no-color]
```

where `<Host>` is often an entry in your `.ssh/config` file.

The test will indeed look for the environment variable `ketrew_test_ssh`; if
not defined it will use `"localhost"`. But for the test to succeed it should be
an SSH host for which the user running the test does not need password.
The test will run some commands on that host and create files and directories
in its `/tmp` directory.



