Build & Install
===============

Ketrew requires at least OCaml **4.03.0** and should be able to build & work on
any Unix platform.

From Opam
---------

If you have [opam](http://opam.ocaml.org/) up and running, just install Ketrew
while choose a database backend (you may pick both and choose later in the
config-file):

    opam install [ssl | tls]  ketrew

- if you want Ketrew to use HTTPS you need to get it linked
  with OpenSSL (package `ssl`) or [nqsb-TLS](https://nqsb.io/) (package `tls`).

This gets you

- a `ketrew` executable that can be used to schedule and run workflows,
- an OCaml library also called `ketrew` that handles the messy orchestration of
  those tasks and exports the `Ketrew.EDSL` module used to write workflows.

Remember that at runtime you'll need `ssh` in your `$PATH` if you need to
execute commands on distant hosts.

*Optional*: Ketrew, like any [Lwt](http://ocsigen.org/lwt/)-based piece of
software, will be much faster and scalable when `libev` is detected and used
as a backend. Use `opam install conf-libev` to tell opam that `libev` is
[installed](http://opam.ocaml.org/packages/conf-libev/conf-libev.4-11/), which
you can ensure with

  - `brew install libev` on MacOSX
  - `apt-get install libev-dev`on Debian/Ubuntu,
  - `yum install libev-devel` on CentOS (which requires
    `export C_INCLUDE_PATH=/usr/include/libev/` and `export LIBRARY_PATH=/usr/lib64/`

before `opam install conf-libev`.

Using Docker
------------

See the instructions at
[hub.docker.com](https://hub.docker.com):
[`hammerlab/ketrew-server`](https://hub.docker.com/r/hammerlab/ketrew-server/).

The project
[`hammerlab/secotrec`](https://github.com/hammerlab/secotrec)
can also be used to setup Ketrew environments.

Without Opam
------------

See the [development documentation](src/doc/Developer_Documentation.md) to find
out how to build Ketrew (and its dependencies) from source.

