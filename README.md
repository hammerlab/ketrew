Ketrew: Keep Track of Experimental Workflows
============================================

This is **Work in Progress**, not ready for use.

Build & Install
---------------

Ketrew uses the `ocp-build` build system and depends on
`cmdliner`, `nonstd`, `pvem`, `docout`,  `sosa`,  `pvem_lwt_unix`, `cmdliner`,
and `atdgen`.

    ./please.sh build

You should *not* install `ketrew` yet, but you can always call at your own risk:

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

There is also a “command line” test, see:

    _obuild/ketrew-cli-test/ketrew-cli-test.asm --help

for now, `cli.ml` has only one workflow (using the library, no EDSL), that can
be added/activated with:

    _obuild/ketrew-cli-test/ketrew-cli-test.asm call website

and then run with:

    _obuild/ketrew-cli-test/ketrew-cli-test.asm run fix

It should build the documentation and commit it in the `gh-pages` branch (or
fail, for example, if the Git tree is not clean).

Documentation
-------------

The documentation depends on [omd](https://github.com/ocaml/omd), and
Graphviz's `dot`:

    please.sh doc


