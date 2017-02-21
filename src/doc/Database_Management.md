Database Backends
=================

Using the [Trakeva](http://www.hammerlab.org/docs/trakeva/master/index.html)
library, Ketrew can happily work with both Sqlite or Postgresql database
backends.

Configuring Databases
---------------------

The database is configured through the `~database_parameters` argument of the
function `Ketrew.Configuration.engine`.

The default is to configure a Sqlite backend (which has then to be available),
but one can easily use a Postgresql database by passing a “conninfo” URI
(cf. [documentation](http://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING)).

Synchronization & Backups
-------------------------

Ketrew comes with a synchronization tool, see `ketrew synchronize --help`.

One can copy the data between databases:

    ketrew sync --from sqlite:///path/to/the/db --to postresql://pg@example.com/databasename?password=blabla

Or use “backup” directory trees:

    ketrew sync --from sqlite:///path/to/the/db --to backup://path/to/backup

or

    ketrew sync --from backup://path/to/backup --to  postresql://pg@example.com/databasename?password=blabla

The backup directory contains a few directories, containing each a number of
JSON files.

