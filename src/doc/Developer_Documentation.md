
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
- `ppx_blob`, `ppx_deriving_yojson`, `ppx_deriving`, `yojson`: JSON
  parsing/printing and other code generation
- `cohttp.lwt`, `ssl`, and `conduit`: HTTP server and client
- `findlib` + `dynlink`: dynamic loading of plugins 
- `trakeva` with the `sqlite` backend

and uses the `oasis` as a build system.

At runtime, Ketrew may use an `ssh` client (tested only with OpenSSH; but SSH
calls are quite configurable).


### Build

Then you may setup and build everything:

     make configure # Generate the `_oasis`, call `oasis` and `configure`
     make           # The actual compilation of the library


### Install

Ketrew is installed by Oasis:

     ocaml setup.ml -install
     
(using the option `--prefix` at configure time).

There is an `opam` file in the repository:

     opam pin add ketrew .

will pick it (Opam ≥ *1.2.0*).


### Build The Documentation

The documentation depends on [oredoc](https://github.com/smondet/oredoc):

    make doc

and check-out `_doc/<branch>/index.html` (unless branch is `master`, then
`_doc/index.html`).

Tests
-----

### Automated Tests

Run the tests like this:

```bash
    ./ketrew-test [-no-color] <Test-names>
```

where a `Test-names` is one or more of

- `ALL` all of the following.
- `basic-test`
- `nohup-test`


### The `cli` Test

The workflow [examples](../test/Workflow_Examples.ml) in the documentation
are actually interactive tests (cf. `./ketrew-workflow-examples-test`).

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

    make test-env

```goodresult
Using package lwt.react as findlin-plugin
Creating cert-key pair: _test_env/test-cert.pem, _test_env/test-key.pem
Creating _test_env/configuration.ml
Creating _test_env/test-authorized-tokens
Creating _test_env/env.env
```

The command creates the directory `_test_env/` with a preconfigured
test-environment (a self-signed SSL certificate/key pair,
client/server/standalone configuration file, an “authorization-tokens”
configuration, … which all work together harmoniously).

Sourcing `_test_env/env.env` will give a few aliases to run the tests.
Aliases which start with `ks` mean “with a *standalone-mode* configuration file;”
those which start with `kd` are in *client-server* mode (`'d'` for “distributed”).

- `kscli`: the standalone `ketrew` application.
- `kdserver`: the server `ketrew` application.
- `kddaemon`: the same as `kdserver` but quiting the current terminal (`daemon`
  option).
- `kdclient`: the client `ketrew` application (talking to a `kdserver` or
  `kddaemon` instance).
- `kstest`: the [`cli` test](../test/Workflow_Examples.ml) with a “standalone-mode” configuration file.
- `kdtest`: the same `cli` test but as a client (again, talking to a `kdserver`
  or `kddaemon` instance).
- `ksintegration`, and `kdintegration`: the
  [integration](../test/integration.ml) test in standalone and client modes.
- `ksplugin_user`, and `kdplugin_user`: the mini-workflow
  [using the plugin](src/test/dummy_plugin_user.ml).

### Coverage

To generate coverage reports you need to instrument the code byt
reconfiguring/compiling from scratch using the environment variable
`WITH_BISECT` equal to `true`:

    WITH_BISECT=true make distclean configure all

Running the instrumented versions of the code will generate `bisect*.out` files
when run.

Then, `make report` will take the  latest such file
and generate an html file in `_report_dir/index.html`. `make clean_reports`
removes the reports and `_report_dir`.

To remove the instrumentation just use `make distclean configure all` withou
`WITH_BISECT` set to `true`.


How to Release
--------------

Once we are somewhat happy about the state of the `master` branch (tests,
documentation, issues that went into the “next release”
[milestone](https://github.com/hammerlab/ketrew/milestones)), this is the
release workflow:

- Release dependencies for which we are using unreleased features
(e.g. [`trakeva`](https://github.com/smondet/trakeva),
[`sosa`](https://github.com/smondet/sosa), etc.).
- Set version string in `tools/please.ml`.
- Update the introductory paragraph of the `README.md` file for the particular
version.
- Write a human-friendly change-log (go through git history and write important
changes).
- Create the release/tag `ketrew.x.y.z` (put the change-log there, see
releases [documentation](https://github.com/blog/1547-release-your-software)).
- Fork the
[mothership opam-repository](https://github.com/ocaml/opam-repository).
- Add a new package by modifying the repository's one (see `./opam`),
fix the URL and the MD5 sum (from release), test the package (in a new
opam-switch, or remove `ocamlfind`), create pull-request.
- Add the tag as an “interesting checkout” in
[`smondet/build-docs-workflow`](https://github.com/smondet/build-docs-workflow),
then build and push the documentation (which is for now part of
[`smondet/smondet.github.com`](https://github.com/smondet/smondet.github.com)).
- Once the opam PR is merged, brag about it, write a blog post, start
[hacking](https://github.com/hammerlab/ketrew/issues?q=is%3Aopen+is%3Aissue)
on the next version.

