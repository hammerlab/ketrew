Configuration File
==================

The Format
----------

The general format used to the configuration file is
[Toml](https://github.com/toml-lang/toml).

Top-level options:

- `debug-level`: integer specifying the amount of verbose messages: `0`: none,
`1`: verbose, `2`: very verbose.

Then the configuration is divided into “sections”:
`engine`, `client`, `ui`, and `server`.
The presence of the `client` or `server` sections, defines the running mode
(if none of both are defined, Ketrew will be in standalone mode, if both are
defined this will be an error).


### The `engine` Section

In standalone and server modes, the `engine` section configures how to run the
workflows (ignored in `client` mode).

- `database-path`: the path to the database directory; it's a
currently a Git-repository that one can inspect with usual `git` commands (the
default is `~/.ketrew/database/`).
- `state-key`: the name of the key to the “root of the tree”; if more than one
application are using the same database, this can be useful to avoid conflicts
(“normal” users should *never* need to set this).
- `turn-unix-ssh-failure-into-target-failure`: boolean;
when an SSH or system call fails it may not mean that the command in your
workflow is wrong (could be an SSH configuration or tunneling problem), by
default (i.e. `false`), Ketrew tries to be clever and does not make targets
fail. To change this behavior put the option to `true`.
- `host-timeout-upper-bound`: float (seconds, default: `60.`); every
connection/command time-out will be `≤ upper-bound`.

### The `ui` Section

This section configures the behavior of the User Interface.

- `color`: boolean (default `true`); tell Ketrew to display *f-ANSI* colors.

### The `plugins` Section

This optional section asks Ketrew to dynamically load plugins:

- `ocamlfind`: a package name or a list of package names to find with `Findlib`
and to load.
- `compiled`: a path or a list of paths to load.

### The `client` Section

This table configures Ketrew in client-mode: 

- `connection`: URL to connect to server (e.g. `"https://example.com:8443"`).
- `token`: API authentication token.

### The `server` Section

This section configures the HTTP server:

- `certificate`: path to the SSL certificate (*mandatory*).
- `private-key`: path to the SSL private-key (*mandatory*).
- `port`: port to listen on (*mandatory*).
- `authorized-tokens-path`: path to the file giving the authentication tokens
that the server accepts (*mandatory-ish* since
authentication-less server-side is not implemented so far). The format of the
file is the SSH `authorized_keys` style.
- `command-pipe-path`: if set this asks the server to listen on a named pipe
for control commands (*highly recommended*).
- `daemonize`: if `true`, ask the server to detach from current terminal.
- `log-path`: if set together with `daemonize`, ask the server to redirect logs
to this path (if not set logs go to `/dev/null`).
- `return-error-messages`: if `true`, the server will return real error
messages (in the *body* of the response) to the client; if `false` (the
default), any kind of error will result in the same uninformative message.


Examples
--------

Note the `please.sh` script can generate configuration files for testing
(`please.sh test-env`).

### Standalone

```toml
# How much noise do you want on your terminal:
debug-level = 2

[plugins]
  ocamlfind =["lwt.unix", "findlib"]
  compiled = "/path/to/some_plugin.cmxs"

[engine]
  database-path = "/path/to/database-standalone"
  host-timeout-upper-bound = 120

[ui]
  color = true
```

### Server

```toml
debug-level = 1

[engine]
  database-path = "/path/to/database-client-server"
  turn-unix-ssh-failure-into-target-failure = false

[ui]
  color = true

[server]
  port = 8443
  certificate = "/path/to/test-cert.pem"
  private-key = "/path/to/test-key.pem"
  authorized-tokens-path = "/path/to/ketrew-authorized-tokens"
  return-error-messages = true
  log-path = "/var/log/logs-of-ketrew-server.txt"
  daemonize = true
  command-pipe-path = "/var/run/ketrew-command.pipe"
```

### Client

```toml
debug-level = 2

[client]
  connection = "https://example.com:8443"
  token = "aldskfjdalksjfiilfldksaj"
```

Print The Configuration
-----------------------

One can always test their configuration with:

    ketrew print-configuration

