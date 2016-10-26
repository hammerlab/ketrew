#!/usr/bin/env bash



travis_install_on_linux () {
    # Install OPAM PPA
    export ppa=avsm/ocaml42+opam12

    echo "yes" | sudo add-apt-repository ppa:$ppa
    sudo apt-get update -qq

    sudo killall postgres
    sudo apt-get install libpq-dev postgresql-9.4

    export PATH=$PATH:/usr/lib/postgresql/9.4/bin
    echo "PATH: $PATH"

    export opam_init_options="--comp=$OCAML_VERSION"
    sudo apt-get install -qq  opam time git
}

travis_install_on_osx () {
    curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
    sudo hdiutil attach XQuartz-2.7.6.dmg
    sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /

    brew update
    brew install opam

    export opam_init_options="--comp=$OCAML_VERSION"
}


case $TRAVIS_OS_NAME in
  osx) travis_install_on_osx ;;
  linux) travis_install_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# configure and view settings
export OPAMYES=1
echo "opam --version"
opam --version
echo "git --version"
git --version

# install OCaml packages
opam init $opam_init_options
eval `opam config env`

echo "ocaml -version"
ocaml -version

# Bypass opam bug #1747
git config --global user.email "you@example.com"
git config --global user.name "Your Name"

opam update

opam pin add ketrew .

opam install --yes ketrew

echo 'ocamlfind list | grep lwt'
ocamlfind list | grep lwt
echo 'ocamlfind list | grep cohttp'
ocamlfind list | grep cohttp

echo "Setting Warn-Error for the Travis test"
export OCAMLPARAM="warn-error=Ad-58,_"
# we disabled warning 58: https://gitlab.camlcity.org/gerd/lib-findlib/issues/3

echo "Now build Ketrew + tests"

omake build-all

echo "Mini-test:"
./ketrew-test config-file

echo "Graph-test:"
./ketrew-test automaton-graph

echo "Test-env:"
omake test-env

shopt -s expand_aliases
. _test_env/env.env

echo "Kdserver"
kdserver --help=plain

## ksplugin_user "sleep 2"
## kscli run loop > kscli_01.out
## 
## 
## echo "Output file kscli_01.out"
## tail -n 50 kscli_01.out
## 
## 
## echo "### Re-do the Getting Started:"
## echo "* init"
## ketrew init
## echo "* start daemon"
## ketrew start -P daemon
## echo "* submit mini-workflow"
## ketrew submit --wet-run --tag 1st-workflow --tag command-line --daemonize /tmp/KT,"ls -la"
## 
## echo "* submit lsf-example"
## ocaml src/example_scripts/lsf_example.ml 'ls -la'
## echo "* submit daemonize-example"
## ocaml src/example_scripts/daemonize_workflow.ml 'ls -la' 'hello@example.com'
## 
## echo "* loop until nothing moves"
## ketrew status --loop
