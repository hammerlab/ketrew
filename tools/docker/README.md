Docker Stuff
============

Usage
-----

    export DNAME=ketrew-dev-server
    cd tools/docker/
    docker build -t $DNAME .

Interactive exploration:

    docker run -ti -p 80:8756 $DNAME bash

Running the server (add `-d` to put it in the background):

    docker run -t -p 443:8443 --env PORT=8443 --env AUTH_TOKEN=blablabla $DNAME ketrew start

And the server is there: <https://127.0.0.1/gui?token=blablabla>

See also `docker ps`, `docker kill <id-prefix>`, etc.
