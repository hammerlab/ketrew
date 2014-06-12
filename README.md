Ketrew: Keep Track of Experimental Workflows
============================================

This is **Work in Progress**, not ready for use.

This documentation is available at <http://seb.mondet.org/ketrew/>.

Build & Install
---------------

### Dependencies

Ketrew depends on

- `nonstd`: nano-library providing a portable extract of `Core_kernel`
- `pvem`: error monad
- `docout`: logging with `smart-print`
- `sosa`:  String module/functor
- `pvem_lwt_unix`: `Lwt_unix` wrapped in a `Pvem.t` (error monad) with more
precise error types.
- `uri`:
parse an [RFC-3986](http://www.ietf.org/rfc/rfc3986.txt)-compliant URIs
(`uri` itself depends on `camlp4`).
- `cmdliner`: command line parsing
- `yojson`: JSON parsing/printing
- `atdgen/atd`: definition of serialization formats (used with `Yojson`).

and uses the `ocp-build` build system.

The `please.sh` script can call `opam` itself:

    ./please.sh get-dependencies

### Build

Then you may setup and build everything:

    ./please.sh build

(for incremental compilation while developping please use: `ocp-build <target>`
directly)

You should not install `ketrew` yet, but you can always call at your own risk:

    ocp-build install

Tests
-----

Run the tests like this:

    ketrew_test_ssh=<Host> _obuild/ketrew-test/ketrew-test.asm [-no-color]

where `<Host>` is often an entry in your `.ssh/config` file.

The test will indeed look for the environment variable `ketrew_test_ssh`; if
not defined it will use `"localhost"`. But for the test to succeed it should be
an SSH host for which the user running the test does not need password.
The test will run some commands on that host and create files and directories
in its `/tmp` directory.

Documentation
-------------

The documentation depends on [omd](https://github.com/ocaml/omd), and
Graphviz's `dot`:

    please.sh doc

and check-out `_doc/index.html`.


