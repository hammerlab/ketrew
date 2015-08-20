Server Commands
---------------

The server listens on a command-pipe defined in the
[config-file](./The_Configuration_File.md).

The commands are based on lines, that are “sent” to the pipe.

In the examples below, we use `_test_env/test-command.pipe` because it is the
path defined by `make test-env`.

To ask the server to die nicely:

    echo 'die' > _test_env/test-command.pipe

To ask the server to reload the authentication module, hence re-reading the
authentication token files:

    echo 'reload-auth' > _test_env/test-command.pipe

To dump the table of HTTP connections ever senn by the server:

    echo 'dump-all-connections' > _test_env/test-command.pipe 

This will create a file in the server's current directory
`all-connections-<unique-id>` useful for debug purposes.

