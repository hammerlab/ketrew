
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
- `uri`: parse [RFC-3986](http://www.ietf.org/rfc/rfc3986.txt)-compliant URIs
  (`uri` itself depends on `camlp4`).
- `cmdliner`: command line parsing
- `ppx_deriving_yojson`, `ppx_deriving`, `yojson`: JSON
  parsing/printing and other code generation
- `cohttp.lwt`, `ssl`, and `conduit`: HTTP server and client
- `findlib` + `dynlink`: dynamic loading of plugins 
- `postgresql`: Ketrew uses a PostgreSQL database.
- `js_of_ocaml`, `tyxml` (with `reactiveData`)

and uses the `ocamlbuild` as a build system with the `solvuu-build` rule
library, and the tool `ocamlify`.

At runtime, Ketrew may use an `ssh` client (tested only with OpenSSH; but SSH
calls are quite configurable).


### Build

Then you may setup and build the libraries and `ketrew` the command line
application:

     make

to build also all the tests, use:

    WITH_TESTS=true make

### Install

There is also an `opam` directory in the repository:

     opam pin add ketrew .

will pick it (Opam ≥ *1.2.0*).


### Build The Documentation

The documentation depends on [oredoc](https://github.com/smondet/oredoc):

    make doc

and check-out `_build/doc/index.html`.

### Merlin

Simply,

    make merlin

Tests
-----

### Automated Tests

Run the tests like this:

```bash
export KETREW_TEST_DB="postgresql://example.com/?password=somepassword"
./ketrew-test.byte [-no-color] <Test-names>
```

where a `Test-names` is one or more of

- `ALL` all of the following.
- `basic-test`
- `automaton-graph`


### The `cli` Test

The workflow [examples](../test/Workflow_Examples.ml) in the documentation
are actually interactive tests (cf. `./ketrew-workflow-examples-test`).

### Dynamically Loaded Plugins

The build-system creates a plugin and a workflow which uses it:

- [`src/test/dummy-plugin/dummy_plugin.ml`](src/test/dummy-plugin/dummy_plugin.ml),
  and
- [`src/test/dummy_plugin_user.ml`](src/test/dummy_plugin_user.ml).

### Generating a Test Environment

In order to not impact a potential “global” installation of Ketrew, one can
use:

    make test-env

```goodresult
### Preparing Test Environment
Docker ketrew_postgres already running
Using package lwt.react as findlin-plugin
Creating cert-key pair: _test_env/test-cert.pem, _test_env/test-key.pem
Creating _test_env/configuration.ml
Creating _test_env/test-authorized-tokens
Creating _test_env/env.env
```

The command creates the directory `_test_env/` with a preconfigured
test-environment (a self-signed SSL certificate/key pair,
client/server configuration file, an “authorization-tokens”
configuration, … which all work together harmoniously).

It also uses `docker` to start a PostgreSQL server daemon unless it is already
running; to stop it use `docker kill ketrew_postgres`.

Sourcing `_test_env/env.env` will give a few aliases to run the tests.

- `kserver`: the server `ketrew` application.
- `rokserver`: the server `ketrew` application but running in “read-only” mode.
- `kclient`: the client `ketrew` application (talking to a `kdserver` instance).
- `ktest`: the [`cli` test](../test/Workflow_Examples.ml) with a client configuration file.
- See `_test_env/env.env` for more.

The URL to the postgres DB is also stored in `KETREW_TEST_DB`.

### Coverage

To generate coverage reports you need to instrument the code by
recompiling from scratch using the environment variable
`WITH_BISECT` equal to `true`:

    make clean
    WITH_BISECT=true make

Running the instrumented versions of the code will generate `bisect*.out` files
when run.

Then, `make bisect-report` will take these files
and generate an html file in `_report_dir/index.html`. `make bisect-clean`
removes the reports and `_report_dir`.

To remove the instrumentation just use `make clean; make` without
`WITH_BISECT` set to `true`.

**Note** that the Web-UI does not work when the code has been instrumented with
Bisect (You'll see errors in the JS-Console: “`caml_mutex_new` not
implemented”).

### Slow Down The Server

In order to test the WebUI in more adverse conditions, the server can be asked
to answer HTTP queries artificially slow:

    KETREW_DEBUG_SLOW_SERVER=1.,0.5 kdserver start

The 2 floating-point numbers `x,y` mean that, at each HTTP request, the server
will pause for `Random.float x +. y` seconds.

How to Release
--------------

Once we are somewhat happy about the state of the `master` branch (tests,
documentation, issues that went into the “next release”
[milestone](https://github.com/hammerlab/ketrew/milestones)), this is the
release workflow:

- Release dependencies for which we are using unreleased features
  (e.g. [`trakeva`](https://github.com/smondet/trakeva),
  [`sosa`](https://github.com/smondet/sosa), etc.).
- Set version string in `myocamlbuild.ml`
- Update the introductory paragraph of the `README.md` file for the particular
  version.
- Write a human-friendly change-log (go through git history and write important
  changes).
- Create the release/tag `ketrew.x.y.z` (put the change-log there, see
  releases [documentation](https://github.com/blog/1547-release-your-software)).
- Follow the instructions at
  [`OCamlPro/opam-publish`](https://github.com/OCamlPro/opam-publish)
- Make sure the documentation for the version is available at
  <http://hammerlab.org/docs/> (requires creating a `doc.x,y,z` long-living
  branch).
- Once the opam PR is merged, brag about it, write a blog post, start
  [hacking](https://github.com/hammerlab/ketrew/issues?q=is%3Aopen+is%3Aissue)
  on the next version.

