Ketrew: Keep Track of Experimental Workflows
============================================

Build & Install
---------------

Ketrew depends on
`nonstd`, `pvem`, `docout`,  `sosa`,  `pvem_lwt_unix`
and uses `ocp-build`:

    ./please.sh build

You should install within Opam, but you can always:

    ocp-build install

Tests
-----

Run the tests like this:

    ketrew_test_ssh=<Host> _obuild/ketrew-test/ketrew-test.asm [-no-color]

where `<Host>` is often an entry in your `.ssh/config` file.

The test will indeed look for the environment variable `ketrew_test_ssh`; if
not defined it will use `"localhost"`, but for the test to succeed it should be
a passwordless SSH host (the only command run on it is for now `ls /`).
