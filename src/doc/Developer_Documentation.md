
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
- `atd2cconv`: definition of serialization formats (used with `Yojson`).
- `toml`: config-file parsing
- `cohttp.lwt`, `ssl`, and `conduit`: HTTP server and client
- `findlib` + `dynlink`: dynamic loading of plugins 

and uses the `ocp-build` build system.

At runtime, Ketrew will use a reasonably recent `git` command, and an `ssh`
client (tested only with OpenSSH; but SSH calls are quite configurable).

The `please.sh` script can call `opam` itself:

    ./please.sh get-dependencies

### Build

Then you may setup and build everything:

    ./please.sh build

(for incremental compilation while developing please use: `ocp-build <target>`
directly)

### Install

For now, `ketrew` uses a custom install/uninstall procedure:

    ./please.sh install <prefix>

and uninstall with:

    ./please.sh uninstall <prefix>


If you use Opam ≥ *1.2.0~beta4*, you don't need a custom opam-repository:

    ./please.sh local-opam
    opam pin add -k path ketrew .


### Build The Documentation

The documentation depends on [oredoc](https://github.com/smondet/oredoc):

    please.sh doc

and check-out `_doc/<branch>/index.html` (unless branch is `master`, then
`_doc/index.html`).

### Run a Toplevel

One can start an ocaml-toplevel (`ocaml` or `utop`) with Ketrew loaded-in:

    ./please.sh top

Tests
-----

### Automated Tests

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

### The `cli` Test

The workflow [examples](../test/Workflow_Examples.ml) in the documentation
are actually interactive tests.

### The `integration` Test

The [integration](../test/integration.ml) test uses Ketrew to build
[Vagrant](https://github.com/mitchellh/vagrant) virtual machines and uses them
to test further features: we test the PBS and LSF long-running backends by
creating “one-node clusters”. For now, running the whole test as a single
workflow is a bit “shaky” (see progress of
[issue 62](https://github.com/hammerlab/ketrew/issues/62)), please use the
commands `prepare`, `go`, and `clean-up` separately.

### Dynamically Loaded Plugins

The build-system creates a plugin and a workflow which uses it:

- [`src/test/dummy_plugin.ml`](src/test/dummy_plugin.ml), and
- [`src/test/dummy_plugin_user.ml`](src/test/dummy_plugin_user.ml).

### Generating a Test Environment

In order to not impact a potential “global” installation of Ketrew, one can
use:

    ./please.sh test-env

```goodresult
Creating cert-key pair: _test_env/test-cert.pem, _test_env/test-key.pem
Creating _test_env/standalone-config-file.toml
Creating _test_env/server-config-file.toml
Creating _test_env/client-config-file.toml
Creating _test_env/test-authorized-tokens
Creating _test_env/env.env
```

The command creates the directory `_test_env/` with a preconfigured
test-environment (a self-signed SSL certificate/key pair,
client/server/standalone configuration files, an “authorization-tokens”
configuration, … which all work together harmoniously).

Sourcing `_test_env/test.env` will give a few aliases to run the tests.
Aliases which start with `ks` mean “with a *standalone-mode* configuration file;”
those which start with `kd` are in *client-server* mode (`'d'` for “distributed”).

- `kscli`: the standalone `ketrew` application.
- `kdserver`: the server `ketrew` application.
- `kdclient`: the client `ketrew` application.
- `kstest`: the [`cli` test](../test/Workflow_Examples.ml) with a “standalone-mode” configuration file.
- `kdtest`: the same `cli` test but as a client.
- `ksintegration`, and `kdintegration`: the
  [integration](../test/integration.ml) test in standalone and client modes.
- `ksplugin_user`, and `kdplugin_user`: the mini-workflow
  [using the plugin](src/test/dummy_plugin_user.ml).


