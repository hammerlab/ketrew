Server Commands
---------------

The server listens on a command-pipe defined in the
[config-file](./The_Configuration_File.md).

The commands are based on lines, that are “sent” to the pipe.

In the examples below, we use `_test_env/test-command.pipe` because it is the
path defined by `./please.sh test-env`.

To ask the server to die nicely:

    echo 'die' > _test_env/test-command.pipe

To ask the server to reload the authentication file (authorized tokens):

    echo 'reload-auth' > _test_env/test-command.pipe

To add an arbitrary string to the measurements/logs maintained by the server:

    echo 'tag hello some random string' > _test_env/test-command.pipe

it will appear in the logs as:

    [2014-09-30-21h16m39s020ms-UTC]  hello some random string

To ask the server to write the measurements/logs to the database (and forget
them afterwards):

    echo 'flush-measurements' > _test_env/test-command.pipe

