Avoiding Dynlink and Config Files
=================================

One may not want to use a configuration file (because lacking a file-system or
so) and/or dynamic linking of plugins (because dynamic-linking can be
problematic).

It is **possible**! This document shows how.


Plugins
-------

Plugins can be compiled to “good old-school” libraries:

    tmp_dir=/tmp/with_dummy_plugin
    mkdir -p $tmp_dir
    cp src/test/dummy_plugin.ml $tmp_dir
    ocamlfind opt  -package ketrew -thread -c  $tmp_dir/dummy_plugin.ml -o $tmp_dir/dummy_plugin.cmx

And can be linked-in directly with `src/app/main.ml`:

    cp src/app/main.ml $tmp_dir
    ocamlfind opt -package ketrew -thread -linkpkg -I $tmp_dir $tmp_dir/dummy_plugin.cmx $tmp_dir/main.ml -o $tmp_dir/new_ketrew_app
    $tmp_dir/new_ketrew_app --help=plain

We can run the plugin user to add a workflow:

    cp src/test/dummy_plugin_user.ml $tmp_dir
    ocamlfind opt -package ketrew -thread -linkpkg -I $tmp_dir $tmp_dir/dummy_plugin.cmx $tmp_dir/dummy_plugin_user.ml -o $tmp_dir/my_workflow
    $tmp_dir/my_workflow "du -sh $HOME"

We can run the workflow (type `'q'` when done):

    $tmp_dir/new_ketrew_app run loop

And *explore* to the target, to call the custom query:

    $tmp_dir/new_ketrew_app interact

(choose the target, `'s'` for “show-status”, `'1'` for “display date”).


Configuration
-------------

The function `Ketrew_command_line.run_main` can take an `~override_configuration` parameter, which can be created with
`Ketrew_configuration.{create,create_server}`
(c.f. 
[`Ketrew_command_line`](../lib/ketrew_command_line.mli) and
[`Ketrew_configuration`](../lib/ketrew_configuration.mli)).

See the example in
[`src/test/preconfigured_main.ml`](../test/preconfigured_main.ml), here is how
to compile it:

    tmp_dir=/tmp/with_preconfiguration
    mkdir -p $tmp_dir
    cp src/test/preconfigured_main.ml $tmp_dir
    ocamlfind opt -package ketrew -thread -linkpkg $tmp_dir/preconfigured_main.ml -o $tmp_dir/new_ketrew_app
    $tmp_dir/new_ketrew_app print-configuration

We can check that no configuration file is read with

    export KETREW_CONFIGURATION=/some/unknown/path/that/really/does/not/exist

The “normal” ketrew fails:

    ketrew pc

```badresult
[ketrew: ERROR]
    Error: Exception while reading
    "/some/unknown/path/that/really/does/not/exist":
    Unix.Unix_error(Unix.ENOENT, "open",
    "/some/unknown/path/that/really/does/not/exist")
```

The new one succeeds:

    $tmp_dir/new_ketrew_app print-configuration

```goodresult
[ketrew]
    From user-overriden:
    * Database: "/tmp/somepath"
    * State-key: "ketrew_persistent_state"
    * Unix-failure does not turn into target failure
    * Debug-level: 42
    * Client with colors
    * Timeout-upper-bound: 60. seconds
    * Plugins:
        * OCamlfind package: "lwt.react"
        * OCamlfind package: "lwt.unix"
    Server:
        Authorized tokens: Some "/tmp/tokens"
        Listen: TLS:4242 (Certificate: "somecert.pem",
            Key: "somekey.pem")
        Return-error-messages: false
```

