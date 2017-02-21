Ketrew's Database Backend
=========================

Ketrew uses a Postgresql database.

Configuring The Database
------------------------

The database is configured through the `~database_parameters` argument of the
function `Ketrew.Configuration.engine`.

One has to use a “conninfo” URI
(cf. [documentation](http://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING)).

Synchronization & Backups
-------------------------

Ketrew comes with a synchronization tool, see `ketrew synchronize --help`.

One can copy the data between databases:

    ketrew sync --from postresql://127.0.0.1:5432/db1?password=foobar \
                --to postresql://pg@example.com/databasename?password=blabla

Or use “backup” directory trees:

    ketrew sync --from postresql://pg@example.com/databasename?password=blabla \
                --to backup://path/to/backup

or

    ketrew sync --from backup://path/to/backup \
                --to  postresql://pg@example.com/databasename?password=blabla

The backup directory contains a few directories, containing each a number of
JSON files.

