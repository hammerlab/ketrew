FROM ocaml/opam:ubuntu-16.04_ocaml-4.03.0
RUN sudo apt-get install --yes libpq-dev libev-dev libgmp-dev
RUN opam install -y tls conf-libev
RUN opam pin --yes add ketrew https://github.com/hammerlab/ketrew.git

# We create a config directory:
RUN eval `opam config env` ; ketrew init --use-database=postgresql://example.com --conf /tmp/ketrew/ --self-signed

# But we use a custom config file (That still points to "/tmp/ketrew"):
COPY configuration.ml .
RUN sudo chmod 777 configuration.ml
ENV KETREW_CONFIGURATION ./configuration.ml
