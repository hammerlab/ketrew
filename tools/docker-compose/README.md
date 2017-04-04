Ketrew/Coclobas/PostgresQL Docker-Compose
-----------------------------------------

We use [`hammerlab/secotrec`](https://github.com/hammerlab/secotrec), to generate a
[`docker-compose`](https://docs.docker.com/compose/)
configuration file:

    secotrec-local compose-configuration ./tools/docker-compose/secotrec-local.yml

It gets us a practical local setup (Ketrew, a PostgreSQL server, and
a [Coclobas](https://github.com/hammerlab/coclobas) server in “local docker”
mode).

The script `kdc.sh` just calls `docker-compose` after (re)setting open
access rights on the `/tmp/secotrec-local-shared-temp` directory.

