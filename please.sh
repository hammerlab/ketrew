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

lib_ml_files=$(find src/lib/ -type f -name '*.ml')
lib_mli_files=$(find src/lib/ -type f -name '*.mli')
lib_files="$lib_mli_files $lib_ml_files"

setup() {
  set -e
  local quoted_authors_list=""
  for idx in "${authors[@]}" ; do
    eval name=\${$idx[0]}
    eval email=\${$idx[1]}
    quoted_authors_list="$quoted_authors_list \"$name <$email>\""
  done


  mkdir -p _obuild/gen/
  local lib_atd_files=$(find src/atd/ -type f -name '*.atd')
  for atd in $lib_atd_files ; do
    name=`basename $atd`
    atd2cconv -inline-inherit-variants true -i $atd -o _obuild/gen/ketrew_gen_${name%.atd}.ml
  done

  local ocaml_findlib_packages_list=$(for f in $findlib_packages ; do echo "\"$f\"; " ; done)
  cat <<EOBLOB > _obuild/gen/ketrew_metadata.ml
  let version = "$version_string"
  let findlib_packages = [$ocaml_findlib_packages_list]
  let homepage = "$homepage"
EOBLOB
  local lib_gen_files=$(find _obuild/gen/ -type f -name '*.ml')

  local quoted_lib_files=$(for f in $lib_files ; do echo "\"$f\" " ; done)
  local quoted_gen_files=$(for f in $lib_gen_files ; do echo "\"$f\" " ; done)

  local yojson_hack_dir=$PWD/_prebuild/yojson
  mkdir -p $yojson_hack_dir
  cp -r `ocamlfind query yojson`/* $yojson_hack_dir
  cd $yojson_hack_dir 
  ocamlc -a -o yojson.cma `ocamlfind query easy-format`/easy_format.cmo yojson.cmo
  ocamlopt -a -o yojson.cmxa `ocamlfind query easy-format`/easy_format.cmx yojson.cmx
  cd -

  local quoted_findlib_packages=$(for f in $findlib_packages ; do echo "\"$f\" " ; done)
cat << OCP_END > build.ocp
version = "$version_string"
license = "$license_name"
authors = [ $quoted_authors_list ]
begin library "threads"
  generated = true
  dirname = [ "%{OCAMLLIB}%/threads" ]
  has_byte = false
end
begin library "yojson"
  generated = true
  dirname = [ "$yojson_hack_dir" ]
  requires = [ "easy-format" "biniou"  ]
end
begin  library "ketrew"
  sort = true
  files = [
    $quoted_lib_files
    $quoted_gen_files
  ]
  requires = [ "easy-format" "biniou" $quoted_findlib_packages ]
  comp = [ "-thread" ]
  link = [ "-thread" ]
end
begin program "ketrew-test"
  files = [ "src/test/main.ml" ]
  requires = [ "ketrew" "threads" ]
  link = [ "-thread" ]
  comp = ["-thread" ]
  install = false
end
begin program "ketrew-app"
  files = [ "src/app/main.ml" ]
  requires = [ "ketrew" "threads" ]
  link = [ "-thread" ]
  comp = ["-thread" ]
end
begin program "ketrew-cli-test"
  files = [ "src/test/Workflow_Examples.ml" ]
  requires = [ "ketrew" "threads" ]
  link = [ "-thread" ]
  comp = ["-thread" ]
  install = false
end
begin program "ketrew-integration-test"
  files = [ "src/test/integration.ml" ]
  requires = [ "ketrew" "threads" ]
  link = [ "-thread" ]
  comp = ["-thread" ]
  install = false
end
OCP_END

cat << MERLIN_END > .merlin
S ./src/lib/
S ./src/test/
B _obuild/ketrew
B _obuild/ketrew-test
MERLIN_END
for p in $findlib_packages ; do echo "PKG $p" >> .merlin ; done

ocp-build root

}

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
  else
    opam pin ocp-build $ocp_build_version
  fi
  opam install atd2cconv ocp-build type_conv `print_opam_depedencies`
}

#
# ocp-build install seems broken
# so here is a dirty implementation of META/install/uninstall
#
meta_file () {
  local meta=$1
  cat << EOF_META > $meta
version = "$version_string"
description = "The Ketrew workflow engine"
requires = "$findlib_packages"
archive(byte) = "ketrew.cma"
archive(native) = "ketrew.cmxa"
exists_if = "ketrew.cma"
EOF_META
}
install () {
    local prefix=$1
    meta_file _obuild/ketrew/META
    ocamlfind install ketrew _obuild/ketrew/META _obuild/ketrew/*.*
    local kclient=_obuild/ketrew-app/ketrew-app
    if [ -f $kclient.asm ] ; then
        cp $kclient.asm $prefix/bin/ketrew
    else
        cp $kclient.byte $prefix/bin/ketrew
    fi
}
uninstall () {
    local prefix=$1
    ocamlfind remove ketrew
    rm -f $prefix/bin/ketrew
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
build () {
  set -e
  if [ -f build.ocp ]; then
    echo "Not redoing setup"
  else
    echo "Calling setup"
    setup
  fi
  ocp-build $* ketrew  ketrew-app ketrew-cli-test ketrew-test ketrew-integration-test
  echo "Compiling also dummy-plugins and stuff"
  compile_dummy_plugin
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

