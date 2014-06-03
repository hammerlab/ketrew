Ketrew: Keep Track of Experimental Workflows
============================================

This is **Work in Progress**, not ready for use.

Build & Install
---------------

Ketrew depends on
`nonstd`, `pvem`, `docout`,  `sosa`,  `pvem_lwt_unix`
and uses the `ocp-build` build system:

    ./please.sh build

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


