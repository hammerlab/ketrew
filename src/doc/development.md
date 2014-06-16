
Development Documentation
=========================

Build
-----

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

### Run a Toplevel

One can start an ocaml-toplevel (`ocaml` or `utop`) with Ketrew loaded-in:

    ./please.sh top

Tests
-----

Run the tests like this:

```bash
    ketrew_test_ssh=<Host> _obuild/ketrew-test/ketrew-test.asm [-no-color] ALL
```

where `<Host>` is often an entry in your `.ssh/config` file.

The test will indeed look for the environment variable `ketrew_test_ssh`; if
not defined it will use `"localhost"`. But for the test to succeed it should be
an SSH host for which the user running the test does not need password.
The test will run some commands on that host and create files and directories
in its `/tmp` directory.


