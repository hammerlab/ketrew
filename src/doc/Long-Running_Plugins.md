Long-Running Process Plugins
============================


Implementation
--------------

The long-running plugins *must*:

- implement a module satisfying the `LONG_RUNNING` interface;
- provide a way to create values like
<code>`Long_running (name, serialized_state)</code>;
- register themselves with the function:
`Ketrew_plugin.register_long_running_plugin`.


Examples
--------

There are (for now) three long-running methods in the library, see
[`Ketrew_lsf`](../lib/ketrew_lsf.mli),
[`Ketrew_pbs`](../lib/ketrew_pbs.mli), and
[`Ketrew_daemonize`](../lib/ketrew_daemonize.mli).
They all use the module `Ketrew_long_running_utilities` which provides a few
helpers for plugin writers.

Moreover, the tests contain a “dynamically linked plugin” that uses the
implementation of `Ketrew_daemonize` and adds a (stupid) custom runtime-query:

- Implementation of the plugin:
[`src/test/dummy_plugin.ml`](../test/dummy_plugin.ml).
- Workflow script that uses the plugin:
[`src/test/dummy_plugin_user.ml`](../test/dummy_plugin_user.ml).

Alternative To Dyn-link
-----------------------

Dynamic linking can be annoying: for instance, some libraries do not install
`.cmxs` files, and some architectures do not support native dynlink, …

Hopefully there is an **alternative**, one can link plugins statically with
`src/app/main.ml` (or another
version of that), see the [documentation](./Alternative_CLI_Application.md).





