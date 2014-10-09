#!/usr/bin/env bash

version_string="`git describe --tags --always --dirty || echo '0.0.0-alpha'`"
findlib_packages="sosa nonstd docout pvem pvem_lwt_unix cmdliner \
  atd cconv.yojson \
  yojson uri toml cohttp.lwt lwt ssl conduit dynlink findlib"
license_name="Apache-2.0"
seb=( "Sebastien Mondet" "seb@mondet.org" "http://seb.mondet.org" )
authors=( "seb" )
homepage="http://hammerlab.github.io/ketrew/"

ocp_build_version=1.99.6-beta

run_top () {
  local toplevel="ocaml"
  if utop -version ; then
    toplevel="utop"
  else
    if rlwrap --version ; then
      toplevel="rlwrap ocaml"
    fi 
  fi
  local ocamlfind_packages=`for p in $findlib_packages ; do echo -n ",$p" ; done`
  cat << EOF_ML > /tmp/ketrew_ocamlinit
#use "topfind"
#thread
#require "nonstd$ocamlfind_packages"
#directory "_obuild/ketrew/"
#load "ketrew.cma"
EOF_ML
  $toplevel -init /tmp/ketrew_ocamlinit

}

signature () {
  local ml_file=$1
  if [ "$1" = "" ]; then
    echo "missing ML file"
    exit 2
  fi
  local packages=""
  for p in $findlib_packages; do packages="$packages,$p" ; done
  ocamlfind ocamlc -thread -I _obuild/ketrew/ -package $packages -i -c $ml_file
}

print_opam_depedencies () {
  echo $findlib_packages | sed 's/\.[a-z]*/ /g' | sed 's/dynlink//g' | sed 's/findlib//g'
}

get_dependencies () {
  local opam_version=`opam --version`
  if [[ $opam_version =~ ^1.2 ]] ; then
    opam pin add ocp-build $ocp_build_version
    opam pin add atdgen 1.3.1
  else
    opam pin ocp-build $ocp_build_version
    opam pin atdgen 1.3.1
  fi
  opam install atd2cconv ocp-build type_conv `print_opam_depedencies`
}

opam_file () {
    local out_file=$1
#    local ocp_build_install_options=' "-install-destdir" prefix '
    local quoted_opam_packages=$(for f in $(print_opam_depedencies) ; do echo -n "\"$f\" " ; done)
    cat << END_OPAM > $out_file
opam-version: "1"
maintainer: "seb@mondet.org"
homepage: "$homepage"
ocaml-version: [ >= "4.01.0" ]
build: [
  ["./please.sh" "setup"]
  ["./please.sh" "build"]
  ["./please.sh" "install" prefix ]
]
remove: [
  ["./please.sh" "uninstall" prefix ]
]
depends: [ "ocp-build" {= "$ocp_build_version" } "atd2cconv" "ocamlfind" $quoted_opam_packages ]

END_OPAM
}

opam_package () {

    local destination=$1
    if [ -d "$destination" ] ; then
        echo "OK: $destination!"
    else
        echo "usage: opam_package [OPAM_REPO_DIR]"
        return 4
    fi
    local package=$destination/packages/ketrew/ketrew.$version_string/
    mkdir -p  $package
    opam_file $package/opam
    echo "Ketrew: Keep Track of Experimental Workflows" > $package/descr
    echo "git: \"git@github.com:hammerlab/ketrew\"" > $package/url

}
usage () {
  cat << EOBLOB
usage:
    $0 {setup,build,install,uninstall,clean,
        doc,top,
        sig,get-dependencies,
        local-opam,opam,meta-file,
        put-license,remove-license,
        test-env,
        help}"
EOBLOB
}

. ./tools/test_environment.env
. ./tools/ocp-build-hacks.env

while [ "$1" != "" ]; do
  case $1 in
    "setup" ) setup ;;
    "build" ) build ;;
    "build-no-color" ) build -no-color ;;
    "clean" ) rm -fr _prebuild _obuild build.ocp .merlin ocp-build.root* ;;
    "doc" )
      . ./tools/build_documentation.env
      make_doc ;;
    "top" ) run_top ;;
    "help" )  usage ;;
    "sig" ) signature $2; shift ;;
    "get-dependencies" ) get_dependencies ;;
    "meta-file" ) meta_file $2; shift ;;
    "install" ) install $2 ; shift;;
    "uninstall" ) uninstall $2 ; shift;;
    "local-opam" ) opam_file "./opam" ;;
    "opam" ) opam_package $2; shift ;;
    "put-license" )
      . tools/headache_licenses.env
      put_license ;;
    "remove-license" )
      . tools/headache_licenses.env
      remove_license ;;
    "test-env" )
      . tools/test_environment.env
      ssl_cert_key
      test_config_file
      test_environment ;;
    * ) echo "Unknown command \"$1\"" ; usage ; exit 1 ;;
  esac
  shift
done

