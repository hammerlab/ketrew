#!/usr/bin/env bash



export opam_pin_add=""
travis_install_on_linux () {
    # Install OCaml and OPAM PPAs
    case "$OCAML_VERSION,$OPAM_VERSION" in
        3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
        3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
        4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
        4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
        4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
        4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
        4.01.0,1.2.0) ppa=avsm/ocaml41+opam12; export opam_pin_add="add" ;;
        4.02.0,1.1.0) ppa=avsm/ocaml42+opam11 ;;
        4.02.0,1.2.0) ppa=avsm/ocaml42+opam12; export opam_pin_add="add" ;;
        4.02.2,1.1.0) ppa=avsm/ocaml42+opam11 ;;
        4.02.2,1.2.0) ppa=avsm/ocaml42+opam12; export opam_pin_add="add" ;;
        4.02.3,1.2.1) ppa=avsm/ocaml42+opam12; export opam_pin_add="add" ;;
      *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
    esac

    echo "yes" | sudo add-apt-repository ppa:$ppa
    sudo apt-get update -qq

    # Hack around Sqlite: https://github.com/cmu-is-projects/ferry/blob/master/.travis.yml
    sudo apt-get autoremove sqlite3
    sudo apt-add-repository -y ppa:travis-ci/sqlite3
    sudo apt-get -y update
    sudo apt-cache show sqlite3
    sudo apt-get install sqlite3=3.7.15.1-1~travis1
    sudo sqlite3 -version

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
    brew install sqlite
    # This path contains the version number which may change *often*
    export PKG_CONFIG_PATH=`find /usr/local/Cellar/sqlite -depth 1 | tail -n 1`/lib/pkgconfig
    echo "PKG_CONFIG_PATH: $PKG_CONFIG_PATH"
    echo "/usr/local/Cellar/sqlite/:"
    ls -l /usr/local/Cellar/sqlite/
    export opam_init_options="--comp=$OCAML_VERSION"
    export opam_pin_add="add"
}


case $TRAVIS_OS_NAME in
  osx) travis_install_on_osx ;;
  linux) travis_install_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# configure and view settings
export OPAMYES=1
echo "ocaml -version"
ocaml -version
echo "opam --version"
opam --version
echo "git --version"
git --version

# install OCaml packages
opam init $opam_init_options
eval `opam config env`

# Bypass opam bug #1747
git config --global user.email "you@example.com"
git config --global user.name "Your Name"

opam update

opam install sqlite3 postgresql

opam pin $opam_pin_add ketrew .

opam install --yes ketrew

echo 'ocamlfind list | grep lwt'
ocamlfind list | grep lwt
echo 'ocamlfind list | grep cohttp'
ocamlfind list | grep cohttp

echo "Sqlite version"
sqlite3 -version

echo "Setting Warn-Error for the Travis test"
export OCAMLPARAM="warn-error=Ad,_"

echo "Test-trakeva"
git clone  git://github.com/smondet/trakeva
cd trakeva
make configure
make
# On OSX we start our own postgresql server, on linux we don't run the tests
case $TRAVIS_OS_NAME in
    osx) ./trakeva_tests ;;
    linux) echo "No trakeva tests" ;;
    *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac
cd ..

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

ksplugin_user "sleep 2"
kscli run loop > kscli_01.out


echo "Output file kscli_01.out"
tail -n 50 kscli_01.out


echo "### Re-do the Getting Started:"
echo "* init"
ketrew init
echo "* start daemon"
ketrew start -P daemon
echo "* submit mini-workflow"
ketrew submit --wet-run --tag 1st-workflow --tag command-line --daemonize /tmp/KT,"ls -la"

echo "* submit lsf-example"
ocaml src/example_scripts/lsf_example.ml 'ls -la'
echo "* submit daemonize-example"
ocaml src/example_scripts/daemonize_workflow.ml 'ls -la' 'hello@example.com'

echo "* loop until nothing moves"
ketrew status --loop
