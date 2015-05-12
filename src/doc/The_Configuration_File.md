Configuration File
==================

Ketrew applications (ie. server or client), use a JSON format
configuration file (it is possible to avoid configuration files by using
the library and/or creating your own
[ad-hoc command-line application](./Alternative_CLI_Application.md)).

The configuration file can contain one or more named "profiles".

Location
--------

Ketrew finds it's configuration file by successively checking:

1. the command line option `--configuration-file` (alias: `-C`)
2. the environment variables `KETREW_CONFIGURATION` and `KETREW_CONFIG`,
3. given a “root” directory (either by the variable `KETREW_ROOT` or the default
`$HOME/.ketrew/`), Ketrew will try to access the files:
    - `configuration.json`,
    - `configuration.ml`, and
    - `configuration.sh`

Given the extension of the filename Ketrew will read the configuration
differently:

- `.json` → will read and parse the file;
- `.ml` → will execute `ocaml <file>`, collect `stdout`, and parse it (as JSON);
- `.sh` → will execute `./<file>`, collect `stdout`, and parse it (as JSON).

Other file extension are considered undefined behavior for future use.

After parsing the configuration file, Ketrew will select a profile by name:

1. using the command line option `-P`/`--configuration-profile`,
2. checking the environment variable `KETREW_PROFILE`,
3. using `"default"`.

Generating From Command Line
----------------------------

The command `ketrew initialize` can generate a configuration file (limited to
standalone mode for now); see `ketrew init --help`.


Example
-------

A configuration file `configuration.ml` (that Ketrew will execute through
`OCaml`) would look like:

```ocaml
#use "topfind"
#thread
#require "ketrew"

open Ketrew_configuration

let debug_level = 2
(* `debug-level`: integer specifying the amount of verbose messages:
    `0`: none,
    `1`: verbose,
    `2`: very verbose.
*)

(* Plugins to load: *)
let plugins = [
  `OCamlfind "lwt.react";
  `Compiled "/path/to/some/ketrew_plugin.cmxs";
]

(* User-Interface preferences: *)
let explorer =
  explorer
    ~request_targets_ids:(`Younger_than (`Days 1.5))
    ~targets_per_page:5
    ~targets_to_prefetch:10 ()
let ui = ui ~with_color:true ~explorer ~with_cbreak:true ()

(* A function that given a boolean value creates a “server
  configuration” that detaches or not from the shell. *)
let my_servers daemon =
  server ~ui
    ~engine:(engine ~database_parameters:"/path/to/database-client-server" ())
    ~authorized_tokens:[
       authorized_tokens_path "/path/to/authorized-tokens";
       authorized_token ~name:"The-inline-one" "inlinetoken";
     ]
    ~return_error_messages:true
    ~log_path:"/path/to/logs-of-server.txt"
    ~daemon
    ~command_pipe:"/path/to/command.pipe"
    (`Tls ("/path/to/cert.pem", "/path/to/key.pem", 8443))

(* We put together 4 profiles in this configuration and “output” them
   (literally, as Json, to `stdout`).

   `debug_level`, `plugins`, and `ui` are shared between configurations.
*)
let () =
  output [
    profile "standalone"
      (create ~debug_level ~plugins
         (standalone ~ui ()
            ~engine:(engine ~database_parameters:"/path/to/database-standalone" ())));
    profile "daemon"
      (create ~debug_level ~plugins (my_servers true));
    profile "server"
      (create ~debug_level ~plugins (my_servers false));
    profile "client"
      (create ~debug_level ~plugins (
          client ~ui ~token:"nekot" "https://127.0.0.1:8443"
          ));
  ]
```

You may run `ocaml configuration.ml` to see the equivalent Json.

Creating a test environment (`make test-env`, cf. developer
[docs](./Developer_Documentation.md)) generates a very similar configuration
file.

Details on the Options
----------------------

To build configurations refer to the [API](src/lib/ketrew_configuration.mli) of
the `Ketrew_configuration` module.

As shown above, the idea is to call the function `Ketrew_configuration.output`
with results of the function `Ketrew_configuration.profile`, themselves created
thanks to the function `Ketrew_configuration.create`, etc.

Print The Configuration
-----------------------

One can always test their configuration with:

    ketrew print-configuration

