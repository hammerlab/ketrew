Configuration File
==================

The Format
----------

The general format used to the configuration file is
[Toml](https://github.com/toml-lang/toml).

Top-level options:

- `debug-level`: integer specifying the amount of verbose messages: `0`: none,
`1`: verbose, `2`: very verbose.
- `turn-unix-ssh-failure-into-target-failure`: boolean;
when an SSH or system call fails it may not mean that the command in your
workflow is wrong (could be an SSH configuration or tunneling problem), by
default (i.e. `false`), Ketrew tries to be clever and does not make targets
fail. To change this behavior put the option to `true`.
- `host-timeout-upper-bound`: float (seconds, default: `60.`); every
connection/command time-out will be `≤ upper-bound`.

### The `client` Section

This optional table configures the client-side/command-line application: 

- `color`: boolean (default `true`); tell Ketrew to display *f-ANSI* colors.

### The `database` Section

Configure Ketrew's persistence (mandatory section):

- `path`: the path to the database file; it's a
[Dbmᵂ](http://en.wikipedia.org/wiki/Dbm) database for now, this parameter is
mandatory.
- `state-key`: the name of the key to the “root of the tree”; if more than one
application are using the same database, this can be useful to avoid conflicts
(“normal” users should need to set this).

### The `server` Section

This optional section configures the HTTP server:

- `certificate`: path to the SSL certificate (*mandatory*).
- `private-key`: path to the SSL private-key (*mandatory*).
- `port`: port to listen on (*mandatory*).
- `authorized-tokens-path`: path to the file giving in an SSH `authorized_keys`
style, to authentication tokens that the server accepts (*mandatory-ish* since
authentication-less server-side is not implemented so far).
- `command-pipe-path`: if set this asks the server to listen on a named pipe
for control commands (*recommended*).
- `daemonize`: if `true`, ask the server to detach from current terminal.
- `log-path`: if set, ask the server to redirect logs to this path (if not set
logs go to `/dev/null`).
- `return-error-messages`: if `true`, the server will return real error
messages (in the *body* of the response) to the client; if `false` (the
default), any kind of error will result in the same uninformative message.

### The `plugins` Section

This optional section asks Ketrew to dynamically load plugins:

- `ocamlfind`: a package name or a list of package names to find with `Findlib`
and to load.
- `compiled`: a path or a list of paths to load.


Example
-------

Here is a somewhat complete example:

```toml
# How much noise do you want on your terminal:
debug-level = 2

turn-unix-ssh-failure-into-target-failure = false

# Unless explicitly told, all SSH/system calls are bounded by a timeout.
host-timeout-upper-bound = 120

[client]
  # One can remove the colors in the output of the client with:
  color = false


[database]
  # The path to the database (mandatory):
  path = "/path/to/some/file"
  # Normal users should not change that:
  state-key = "some-string"

[server]
  certificate = "/etc/ketrew/cert.pem"
  private-key = "/etc/ketrew/key.pem"
  port = 8443
  authorized-tokens-path = "/etc/ketrew/authorized-tokens"
  return-error-messages = true
  log-path = "/var/log/ketrew/server.log"
  daemonize = true
  command-pipe-path = "/var/run/ketrew/command.pipe"

[plugins]
  ocamlfind =["lwt.unix", "lwt.react"]
  compiled = "_build/some_plugin/some_plugin.cmxs"
```

Print The Configuration
-----------------------

One can always test their configuration with:

    ketrew print-configuration

