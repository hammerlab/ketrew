Long-Running Process Plugins
============================


Implementation
--------------

The long-running plugins *must*:

- implement a module satisfying the `LONG_RUNNING` interface defined in
[`src/lib/ketrew_long_running.ml`](src/lib/ketrew_long_running.ml)
(with a **unique** `name`);
- provide a way to create values like
<code>`Long_running (name, serialized_state)</code>;
- register themselves with the function:
`Ketrew_state.register_long_running_plugin`
(see [`src/lib/ketrew_state.mli`](src/lib/ketrew_state.mli)).


Examples
--------

There are (for now) two long-running methods in the library, see
[`src/lib/ketrew_lsf.mli`](src/lib/ketrew_lsf.mli) and
[`src/lib/ketrew_daemonize.mli`](src/lib/ketrew_daemonize.mli).

And the tests contain a “dynamically linked plugin” that uses the
implementation of `Ketrew_daemonize` and adds a (stupid) custom runtime-query:

- Implementation of the plugin:
[`src/test/dummy_plugin.ml`](src/test/dummy_plugin.ml).
- Workflow script that uses the plugin:
[`src/test/dummy_plugin_user.ml`](src/test/dummy_plugin_user.ml).

Alternative
-----------

Dynamic linking can be annoying: some libraries do not install `.cmxs` files,
some architectures do not support native dynlink, … Hopefully there is an
alternative, one can link plugins statically with `src/app/mail.ml` (or another
version of that), see the
[documentation](src/doc/Alternative_CLI_Application.md).





