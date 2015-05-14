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
        4.02.1,1.1.0) ppa=avsm/ocaml42+opam11 ;;
        4.02.1,1.2.0) ppa=avsm/ocaml42+opam12; export opam_pin_add="add" ;;
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

    sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time git
}

travis_install_on_osx () {
    curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
    sudo hdiutil attach XQuartz-2.7.6.dmg
    sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /

    brew update
    brew install opam
    brew install pkg-config sqlite
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

opam remote add smondet git://github.com/smondet/dev-opam-repo

opam update

# We add the react dependency for lwt.react in the tests
opam install react

# We need a not-yet released bug in ppx_deriving_yojson
opam pin add ppx_deriving_yojson --yes -k git https://github.com/whitequark/ppx_deriving_yojson.git


opam pin $opam_pin_add ketrew .

opam install --yes ketrew

echo 'ocamlfind list | grep lwt'
ocamlfind list | grep lwt
echo 'ocamlfind list | grep cohttp'
ocamlfind list | grep cohttp

echo "Sqlite version"
sqlite3 -version

echo "Setting Warn-Error for the Travis test"
export OCAMLPARAM="warn-error=A,_"

echo "Test-trakeva"
git clone  git://github.com/smondet/trakeva
cd trakeva
make configure
make
./trakeva_tests
cd ..

echo "Now build Ketrew + tests"

make gen
make configure
make

echo "Mini-test:"
./ketrew-test config-file

echo "Graph-test:"
./ketrew-test automaton-graph


echo "Test-env:"
make test-env

shopt -s expand_aliases
. _test_env/env.env

echo "Kdserver"
kdserver --help=plain

ksplugin_user "sleep 2"
kscli run loop > kscli_01.out


echo "Output file kscli_01.out"
tail -n 50 kscli_01.out

echo "Try to compile Biokepi"
opam pin add biokepi -k git --yes https://github.com/hammerlab/biokepi.git
opam install --yes biokepi

