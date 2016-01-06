Long-Running Process Plugins
============================

“Long-running plugins” are the name given to the implementations of Ketrew's
backends, e.g. `Daemonize`, `Yarn`, or `PBS`.

Implementation
--------------

The long-running plugins *must*:

- implement a module satisfying the `LONG_RUNNING` interface;
- provide a way to create values like
  `` `Long_running (name, serialized_state)``;
- register themselves with the function:
  `Ketrew.Plugin.register_long_running_plugin`.


Examples
--------

There are (for now) four long-running methods in the library, see
[`Ketrew.Lsf`](../lib/lsf.mli),
[`Ketrew.Yarn`](../lib/yarn.mli),
[`Ketrew.Pbs`](../lib/pbs.mli), and
[`Ketrew.Daemonize`](../lib/daemonize.mli).
They all use the module `Ketrew.Long_running_utilities` which provides a few
helpers for plugin writers.

Moreover, the tests contain a “dynamically linked plugin” that uses the
implementation of `Ketrew.Daemonize` and adds a (stupid) custom runtime-query:

- Implementation of the plugin:
  [`src/test/dummy-plugin/dummy_plugin.ml`](../test/dummy-plugin/dummy_plugin.ml).
- Workflow script that uses the plugin:
  [`src/test/dummy_plugin_user.ml`](../test/dummy_plugin_user.ml).

Setup
-----

By default, Ketrew expects long-running plugins to be compiled to native
dynamically loadable modules (`.cmxs` files).

Modules are then loaded from the configuration file thanks to the `~plugins`
optional argument of `Ketrew.Configuration.create`. Plugins can be direct
(preferably absolute) paths to `.cmxs` files *or* names of OCamlfind packages:

```ocaml
let plugins = [
  `OCamlfind "long_running_plugin_package";
  `Compiled "$PATH/to/custom_long_running_plugin.cmxs";
]
let () =
  output [
    profile "server-with-plugins"
      (create ~debug_level ~plugins
        (* ... *)
```

Plugins will be loaded in the order given in the list and can have
dependencies (that Ketrew will load).

Note that if Ketrew is compiled to bytecode one has to use `.cmo` or `.cma` 
files instead of `.cmxs`.


Alternative To Dyn-link
-----------------------

Dynamic linking can be annoying: for instance, some libraries do not install
`.cmxs` files, and some architectures do not support native dynlink, …

Luckily, there is an **alternative**, one can link plugins statically with
`src/app/main.ml` (or another
version of that), see the [documentation](./Alternative_CLI_Application.md).





