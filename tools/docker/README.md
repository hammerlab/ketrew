Docker Stuff
============

This a Docker image/build for testing Ketrew server. It uses a self-signed TLS
certificate and a local ephemeral Sqlite database.

Usage
-----

### Using Docker Hub

There is a public build of the image:
<https://hub.docker.com/r/hammerlab/ketrew-server/>.

    docker pull hammerlab/ketrew-server

Running the server (replace `-i` with `-d` to put it properly in the
background):

    docker run -ti -p 443:8443 \
        --env PORT=8443 --env AUTH_TOKEN=blablabla \
        hammerlab/ketrew-server ketrew start

And the server is there: <https://127.0.0.1/gui?token=blablabla>

Of course, just run `bash` instead of `ketrew start` to get a configured
environment.

See also `docker ps`, `docker kill <id-prefix>`, etc.

### Building The Image Locally

In the repository:

    export DNAME=ketrew-dev-server
    cd tools/docker/
    docker build -t $DNAME .

Interactive exploration:

    docker run -ti $DNAME bash

Run the server as in the previous section using `$DNAME` instead of
`hammerlab/ketrew-server`.
