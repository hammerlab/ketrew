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
      *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
    esac

    echo "yes" | sudo add-apt-repository ppa:$ppa
    sudo apt-get update -qq
    sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time git
}

travis_install_on_osx () {
    curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
    sudo hdiutil attach XQuartz-2.7.6.dmg
    sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
    if [ "$OCAML_VERSION" != "4.01.0" ]; then
      export opam_init_options="--comp=$OCAML_VERSION"
    fi
    if [ "$OPAM_VERSION" = "1.2.0" ]; then
      exit 0
      brew install opam --HEAD
      export opam_pin_add="add"
    else
      brew install opam
    fi
}


case $TRAVIS_OS_NAME in
  osx) travis_install_on_osx ;;
  linux) travis_install_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# configure and view settings
export OPAMYES=1
ocaml -version
echo "ocaml -version"
opam --version
echo "opam --version"
opam --git-version
echo "opam --git-version"
git --version
echo "git --version"

# install OCaml packages
opam init $opam_init_options
eval `opam config env`

# Bypass opam bug #1747
git config --global user.email "you@example.com"
git config --global user.name "Your Name"

opam remote add smondet git://github.com/smondet/dev-opam-repo
opam update
git clone  git://github.com/smondet/atd2cconv
cd atd2cconv 
git checkout -t origin/assembled
opam pin $opam_pin_add atd2cconv .
cd ..

# We add the react dependency for lwt.react in the tests
opam install react
./please.sh get-dependencies

echo 'ocamlfind list | grep lwt'
ocamlfind list | grep lwt
echo 'ocamlfind list | grep cohttp'
ocamlfind list | grep cohttp

echo 'eval `opam config env`'
eval `opam config env`

echo 'ocamlfind list | grep lwt'
ocamlfind list | grep lwt
echo 'ocamlfind list | grep cohttp'
ocamlfind list | grep cohttp

echo "Clean"
./please.sh clean
echo "Build"
./please.sh build

echo "Mini-test:"
_obuild/ketrew-test/ketrew-test.asm db-test config-file


