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
standalone for now); see `ketrew init --help`.


Examples
--------

A configuration file `config.ml` (that Ketrew will execute through `OCaml`)
would look like:

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

You may run `ocaml config.ml` to see the equivalent Json.

Creating a test environment (`make test-env`, cf. developer
[docs](./Developer_Documentation.md)) generates a very similar configuration
file.

Explanation of the Options
--------------------------

To build configurations refer to the [API](src/lib/ketrew_configuration.mli) of
the `Ketrew_configuration` module.

### The `engine` Options

In standalone and server modes, the `engine` configures how to run the
workflows.

- `database_parameters`: the path to the database file/directory (the
default is `~/.ketrew/database`).
- `persistent_state_key`: the name of a key to the “root of the tree”; if more
than one application is using the same database, this can be useful to avoid
conflicts (“normal” users should *never* need to set this).
- `turn_unix_ssh_failure_into_target_failure`: boolean;
when an SSH or system call fails it may not mean that the command in your
workflow is wrong (could be an SSH configuration or tunneling problem). By
default (i.e. `false`), Ketrew tries to be clever and does not make targets
fail. To change this behavior set the option to `true`.
- `host_timeout_upper_bound`: float (seconds, default is `60.`); every
connection/command time-out will be `≤ upper-bound`.

### The `ui` Options

The `ui` function configures the behavior of the User Interface.

- `color`: boolean (default `true`); tell Ketrew to display *f-ANSI* colors.
- The Interactive Explorer's configuration:
    - `request_targets_ids` how to request target IDs from the server.
    - `targets_per_page`: the number of targets to display in a single “page.”
    - `targets_to_prefetch`: how many targets to download at once while
      prefetching (the client-side cache fetches targets in advance to improve
      latency).
- `with_cbreak`: should the interactive UI use “`cbreak`” or not:
    - when `false`: it reads from `stdin` classically (i.e. it waits for the
      `return` key to be pressed),
    - when `true`: it gets the key-presses directly (it's the default but
      requires a compliant terminal).

### The `plugins` Option

This optional argument asks Ketrew to dynamically load plugins:

- `` `Ocamlfind``: a package name or a list of package names to find and load
with `Findlib`.
- `` `Compiled``: a path or a list of paths to load.

### The `client` Options

The `client` function configures Ketrew in client-mode:

- the `connection`: URL for connecting to the server
  (e.g. `"https://example.com:8443"`).
- `token`: API authentication token.

### The `server` Options

The `server` function configures the HTTP server:

- The value `` `Tls (certificate, private_key, port)`` configures the connection
  settings:
    - `certificate`: path to the SSL certificate (*mandatory*).
    - `private_key`: path to the SSL private-key (*mandatory*).
    - `port`: port to listen on (*mandatory*).
- `authorized_tokens`: list of values representing authorized tokens, either:
    - `authorized_token ~name value`: an *inline* token definition, or
    - `authorized_token_path path`: a path to a file containing authentication
      tokens; it uses the SSH
      [`authorized_keys`](http://en.wikibooks.org/wiki/OpenSSH/Client_Configuration_Files#.7E.2F.ssh.2Fauthorized_keys)
      format (whitespace-separated lines of the form:
      `<name> <token> <optional comments ...>`).
- `command_pipe`: if set this asks the server to listen on a named pipe for
  control commands (*highly recommended*).
- `daemonize`: if `true`, ask the server to detach from the current terminal; if
  you use this option it is required to provide absolute paths for all other
  parameters requiring paths (daemonization changes the process directory to
  `/`).
- `log_path`: if set together with `daemonize`, ask the server to redirect logs
  to this path (if not set logs go to `/dev/null`).
- `return_error_messages`: if `true`, the server will return real error messages
  (in the *body* of the response) to the client; if `false` (the default), any
  kind of error will result in the same uninformative message.

Print The Configuration
-----------------------

One can always test their configuration with:

    ketrew print-configuration

