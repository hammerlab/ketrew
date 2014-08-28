Configuration File
==================

One can always test their configuration with:

    ketrew print-configuration

The general format used to the configuration file is
[Toml](https://github.com/toml-lang/toml).
Here is a complete and commented example:

```toml
# Ketrew configuration file

# How much noise do you want on your terminal:
debug-level = 2

# When an SSH or system call fails it may not mean that the command in your
# workflow is wrong (could be an SSH config or tunneling problem), by default,
# Ketrew tries to be clever and does not make targets fail. To change this
# behavior put the following option to `true`:
turn-unix-ssh-failure-into-target-failure = false

# Unless explicitly told, all SSH/system calls are bounded by a timeout.  A
# given Host can have a specific timeout, but Ketrew has an upper-bound of all
# timeouts allowed  (default 60 seconds) here is how to set it:
host-timeout-upper-bound = 120

[client]
  # One can remove the colors in the output of the client with:
  color = false


[database]
  # The path to the database (mandatory):
  path = "/path/to/some/file"
  # What is the 'master-key' of the state inside the database
  # (normal users should not change that):
  state-key = "some-string"
```

