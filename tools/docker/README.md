Docker Stuff
============

This a Docker image/build for testing Ketrew server. It uses a self-signed TLS
certificate with any PostgreSQL database (through the `$DB_URI` variable).

This image is kept for the time being, but more up-to-date and flexible images
are available at [`hammerlab/keredofi`](https://github.com/hammerlab/keredofi).

The instructions below are still valid ☺.


Usage
-----

### Using Docker Hub

There is a public build of the image:
<https://hub.docker.com/r/hammerlab/ketrew-server/>.

    DOCKER_TAG=latest  # Or any other tag
    docker pull hammerlab/ketrew-server:$DOCKER_TAG

Running the server (replace `-i` with `-d` to put it properly in the
background):

    docker run -ti -p 443:8443 \
        --env PORT=8443 --env AUTH_TOKEN=blablabla \
        --env "DB_URI=postgresql://pg.example.com/?user=uuuuuserr&password=passswwoooord" \
        hammerlab/ketrew-server:$DOCKER_TAG \
        ketrew start

And the server is there: <https://127.0.0.1/gui?token=blablabla>

Of course, just run `bash` instead of `ketrew start` to get a configured
environment.

See also `docker ps`, `docker kill <id-prefix>`, etc.

**Note:**
There is an easy way of providing a PostgreSQL database server:

    docker run --name ketrew-postgres -p 5432:5432 \
        -e POSTGRES_PASSWORD=kpass -d postgres

creates a (daemonized) container with a fresh DB server. Then one can use the
`--link` option to make it visible from the Ketrew container:

    docker run -ti -p 443:8443 \
        --env PORT=8443 --env AUTH_TOKEN=blablabla \
        --link ketrew-postgres:the_db \
        --env "DB_URI=postgresql://the_db/?user=postgres&password=kpass" \
        hammerlab/ketrew-server:$DOCKER_TAG ketrew start

One can also inspect the database with `psql`:

    docker run -it --rm --link ketrew-postgres:postgres \
       -e PGPASSWORD=kpass postgres psql -x -h postgres -U postgres

To stop/destroy the DB server:

    docker kill ketrew-postgres
    docker rm ketrew-postgres


### Building The Image Locally

In the repository:

    export DNAME=ketrew-dev-server
    cd tools/docker/
    docker build -t $DNAME .

Interactive exploration:

    docker run -ti $DNAME bash

Run the server as in the previous section using `$DNAME` instead of
`hammerlab/ketrew-server`.
