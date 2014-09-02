#!/usr/bin/env bash

version_string="0.0.1-prealpha"
findlib_packages="sosa nonstd docout pvem pvem_lwt_unix cmdliner atdgen atd \
  yojson uri toml dbm cohttp.lwt lwt ssl conduit dynlink findlib"
license_name="ISC"
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
    atdgen -t -o _obuild/gen/ketrew_gen_${name%.atd} $atd
    atdgen -j -j-std -o _obuild/gen/ketrew_gen_${name%.atd} $atd
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
  files = [ "src/test/cli.ml" ]
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

make_doc () {
  local git_branch=`git symbolic-ref --short HEAD`
  local outdir=_doc/
  if [ "$git_branch" != "master" ]; then
    outdir=_doc/$git_branch
  fi
  if [ "$KAPI_DOC" != "no" ] ; then
    local apidoc=$outdir/api/
    local ocamlfind_package_options=`for p in $findlib_packages ; do echo -n "-package $p " ; done`
    mkdir -p $apidoc
    ocamlfind ocamldoc -html -d $apidoc $ocamlfind_package_options  -thread \
      -charset UTF-8 -t "Ketrew API" -keep-code -colorize-code -sort \
      -I _obuild/ketrew/ $lib_mli_files $lib_ml_files 
    local dot_file=$outdir/modules.dot
    local image_file=modules.svg
    ocamlfind ocamldoc -dot -o $dot_file $ocamlfind_package_options  -thread \
      -t "Ketrew $version_string" \
      -I _obuild/ketrew/ $lib_mli_files $lib_ml_files 
    grep -v rotat $dot_file | dot -Tsvg  -o$outdir/$image_file
    rm $dot_file
  fi
  local markdown_authors_list=""
  for idx in "${authors[@]}" ; do
    eval name=\${$idx[0]}
    eval email=\${$idx[1]}
    eval web=\${$idx[2]}
    markdown_authors_list="$markdown_authors_list
- [$name]($web) (\`$email\`)"
  done

  local index_markdown=/tmp/ketrew_index_markdown
  local dev_markdown=/tmp/ketrew_devdoc_markdown

  cp README.md $index_markdown
  cat << END_MD >> $index_markdown

Authors
-------

$markdown_authors_list

END_MD

  local index=$outdir/index.html
  cp src/doc/* $outdir/
  ocaml_to_markdown src/test/cli.ml $outdir/test_cli.md
  ocaml_to_markdown src/test/dummy_plugin.ml $outdir/test_dummy_plugin.md
  ocaml_to_markdown src/test/dummy_plugin_user.ml $outdir/test_dummy_plugin_user.md
  ocaml_to_markdown src/test/preconfigured_main.ml $outdir/test_preconfigured_main.md
  ketrew_help_to_html init $outdir
  ketrew_help_to_html "--help" $outdir
  ketrew_help_to_html status $outdir
  ketrew_help_to_html run-engine $outdir
  ketrew_help_to_html kill $outdir
  ketrew_help_to_html archive $outdir
  ketrew_help_to_html interact $outdir
  ketrew_help_to_html explore $outdir
  ketrew_help_to_html autoclean $outdir
  ketrew_help_to_html start-server $outdir
  ketrew_help_to_html stop-server $outdir
  markdown_to_html $index_markdown $index "Ketrew: Home"
  for md in $outdir/*.md ; do
    local name=`basename ${md%.md} | sed 's/_/ /g'`
    markdown_to_html $md ${md%.md}.html "Ketrew: $name"
  done
}
ketrew_help_to_html () {
  local cmd=$1
  local outdir=$2
  local output=$outdir/ketrew_${cmd}_help.html
  #echo "Creating $output"
  if [ "$cmd" = "--help" ]; then
      _obuild/ketrew-app/ketrew-app.asm --help=groff | groff -Thtml -mandoc >  $output
  else
      _obuild/ketrew-app/ketrew-app.asm  $cmd --help=groff | groff -Thtml -mandoc >  $output
  fi

}
ocaml_to_markdown () {
  local input=$1
  local output=$2
  cat <<EOBLOB > $output
# File \`$input\`

\`\`\`ocaml
EOBLOB
  cat $input | sed 's/^(\*M/```/' | sed 's/^M\*)/```ocaml/' >> $output
  cat <<EOBLOB >> $output
\`\`\`
EOBLOB

}

markdown_transform_links () {
    local input=$1
    local output=$2
    sed 's:([^()]*/\([^/]*\)\.md):(\1.html):g' $input | \
        sed 's:([^()]*/test/\(.*\)\.ml):(test_\1.html):g' | \
        sed 's:([^()]*/lib/ketrew_long_running.ml):(api/Ketrew_long_running.html):g' | \
        sed 's:([a-z\.]*/lib/ketrew_\(.*\)\.mli):(api/Ketrew_\1.html):g' | \
        sed 's:`\(ketrew \([a-z\-]*\) *\(--help\)*\)`:[`\1`](ketrew_\2_help.html):g' > $output
}
markdown_to_html () {
    local input=$1
    local output=$2
    local title=$3
  cat << END_HTML > $output
<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" href="http://cdn.jsdelivr.net/bootstrap/3.1.1/css/bootstrap.min.css" type="text/css">
  <link rel="stylesheet" href="http://cdn.jsdelivr.net/bootstrap/3.1.1/css/bootstrap-theme.min.css" type="text/css">
  <link rel="stylesheet" href="code_style.css" type="text/css">
  <meta charset="utf-8">
  <title>$title</title>
</head>
  <body><div class="container">
  <h1>$title</h1>
  <div class="row">
  <div class="col-md-3">
  <h2>Contents</h2>
END_HTML
  local tmp=/tmp/kmd2html_$(basename input)
  #sed 's:src/lib/ketrew_edsl\.mli:api/Ketrew_edsl\.html:g' README.md > $index_markdown
  markdown_transform_links $input $tmp
  omd -otoc -ts 1 -td 4 $tmp >> $output
  echo "<h2>Menu</h2>" >> $output
  local menu_md=/tmp/ketrrew_doc_menu.md
  printf -- "- [Home](./index.html)\n" > $menu_md
  for md in src/doc/*.md ; do
    local name=`basename ${md%.md} | sed 's/_/ /g'`
    printf -- "- [$name]($md)\n" >> $menu_md
  done
  cat << END_MD >> $menu_md
- [Generated Documentaiton For The API](api/index.html) ([Modules Overview (SVG)](modules.svg))
END_MD
  markdown_transform_links $menu_md $menu_md.transofrm.md
  omd $menu_md.transofrm.md >> $output
  echo "</div><div class=\"col-md-9\">" >> $output
  omd -r ocaml='higlo' $tmp | grep -v '<h1' >> $output
  echo "</div></div></div></body><html>" >> $output
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
  opam install ocp-build type_conv `print_opam_depedencies`
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
  ["./please.sh" "build"]
  ["./please.sh" "install" prefix ]
]
remove: [
  ["./please.sh" "uninstall" prefix ]
]
depends: [ "ocp-build" {= "$ocp_build_version" } "ocamlfind" $quoted_opam_packages ]

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

travis_install_on_linux () {
    # Install OCaml and OPAM PPAs
    case "$OCAML_VERSION,$OPAM_VERSION" in
        3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
        3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
        4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
        4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
        4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
        4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
      *) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
    esac

    echo "yes" | sudo add-apt-repository ppa:$ppa
    sudo apt-get update -qq
    sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time
}

travis_install_on_osx () {
    curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
    sudo hdiutil attach XQuartz-2.7.6.dmg
    sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
    brew install opam
}

do_travis() {

  case $TRAVIS_OS_NAME in
    osx) travis_install_on_osx ;;
    linux) travis_install_on_linux ;;
    *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
  esac

  # configure and view settings
  export OPAMYES=1
  ocaml -version
  opam --version
  opam --git-version

  # install OCaml packages
  opam init
  eval `opam config env`

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


  echo "Test environment:"
  ./please.sh test-env
  . _obuild/test.env

  echo "Do some command line tests, with the server and all"
  export KETREW_CONFIGURATION=_obuild/test-config-file.toml
  ktapp="_obuild/ketrew-app/ketrew-app.asm"
  $ktapp start-server
  $ktapp status
  $ktapp stop-server
  $ktapp status
}

headache_config () {
  local config_file=$1
  cat << EOC > $config_file
# OCaml
| ".*\\.ml[il]?" -> frame open:"(*" line:"*" close:"*)"
| ".*\\.atd" -> frame open:"(*" line:"*" close:"*)"
EOC
}
export headache_files="src/lib/*.ml src/lib/*.mli src/atd/*.atd src/test/*.ml"
put_license () {

  local blob=/tmp/ketrew_license_blob
  local year=2014 
  local author="${seb[0]} <${seb[1]}>"
  local config=/tmp/ketrew_headache_config
  headache_config $config
  cat << EOBLOB > $blob
Copyright $year, $author

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.
EOBLOB
  headache -c $config  -h $blob $headache_files

}
remove_license () {
  local config=/tmp/ketrew_headache_config
  headache_config $config
  headache -c $config -r $headache_files 

}


test_config_file=_obuild/test-config-file.toml
test_authorized_tokens=_obuild/test-authorized-tokens
test_database_prefix=_obuild/test-database
test_certificate=_obuild/test-cert.pem
test_privkey=_obuild/test-key.pem
test_server_log=_obuild/test-server.log
test_command_pipe=_obuild/test-command.pipe
test_shell_env=_obuild/test.env

test_additional_findlib_plugin="findlib"
test_additional_findlib_plugin_code="let f () = Findlib.init ()"
set_test_additional_findlib_plugin () {
  if  ocamlfind query lwt.react > /dev/null  ; then
    export test_additional_findlib_plugin="lwt.react"
    export test_additional_findlib_plugin_code="let f () = Lwt_react.E.app"
  else
    export test_additional_findlib_plugin="graphics"
    export test_additional_findlib_plugin_code="let f () = Graphics.create_image 42 42"
  fi
  echo "Using package $test_additional_findlib_plugin add findlin-plugin"
}
ssl_cert_key () {
  mkdir -p _obuild/
  echo "Creating cert-key pair: $test_certificate, $test_privkey"
  openssl req -x509 -newkey rsa:2048 \
    -keyout $test_privkey -out $test_certificate \
    -days 10 -nodes -subj "/CN=test_ketrew" 2> /dev/null
}
test_config_file () {
  echo "Creating $test_config_file"
  cat <<EOBLOB > $test_config_file
# Ketrew test configuration file
debug-level = 2
[client]
  color = true
[database]
  path = "$test_database_prefix"
[server]
  certificate = "$test_certificate"
  private-key = "$test_privkey"
  port = 8443
  authorized-tokens-path = "$test_authorized_tokens"
  return-error-messages = true
  log-path = "$test_server_log"
  daemonize = true
  command-pipe-path = "$test_command_pipe"
[plugins]
  ocamlfind =["lwt.unix", "$test_additional_findlib_plugin"]
  compiled = "$PWD/_obuild/dummy_plugin_stuff/test_dummy_plugin.cmxs"
EOBLOB
  echo "Creating $test_authorized_tokens"
  cat << EOBLOB  >> $test_authorized_tokens
test1 dsafkdjshh4383497hfvfnfdsfli some comments
test2 dsaifdksafhkd8437189437tfodslcjdsacfaeo some more comments for test2
easy_auth nekot easy authentication
# commented line
weird-line-that-makes-a-warning
EOBLOB
}
compile_dummy_plugin () {
  echo "Compiling the Dummy-plugin and its user"
  local ocamlfind_package_options=`for p in $findlib_packages ; do echo -n "-package $p " ; done`
  local additional_package="-package $test_additional_findlib_plugin"
  local compile="ocamlfind opt -thread $ocamlfind_package_options $additional_package  -I _obuild/ketrew/ "
  set -e
  local compile_dir=_obuild/dummy_plugin_stuff/
  rm -fr $compile_dir
  mkdir -p $compile_dir
  cp src/test/dummy_plugin.ml $compile_dir
  echo "(* Code that really depends on $test_additional_findlib_plugin *)" >> \
    $compile_dir/dummy_plugin.ml
  echo $test_additional_findlib_plugin_code >> $compile_dir/dummy_plugin.ml
  cp src/test/dummy_plugin_user.ml $compile_dir
  $compile -shared $compile_dir/dummy_plugin.ml \
    -o  $compile_dir/test_dummy_plugin.cmxs
  $compile -linkpkg _obuild/ketrew/ketrew.cmxa \
    -I $compile_dir $compile_dir/dummy_plugin.cmx \
    $compile_dir/dummy_plugin_user.ml \
    -o $compile_dir/test_dummy_plugin_user.asm
   # just for the sake of it we compile the `preconfigured_main.ml` example:  
  cp src/test/preconfigured_main.ml $compile_dir
  $compile -linkpkg ketrew.cmxa $compile_dir/preconfigured_main.ml -o $compile_dir/preconfigured.asm
}
test_environment () {
  echo "Creating $test_shell_env"
  local confvar="KETREW_CONFIGURATION=$test_config_file"
  cat << EOBLOB > $test_shell_env
export ktest_url=https://localhost:8443
alias ktapp="$confvar _obuild/ketrew-app/ketrew-app.asm"
alias kttest="$confvar _obuild/ketrew-cli-test/ketrew-cli-test.asm"
alias ktkillserver='echo "die" > $test_command_pipe'
alias ktplugin_user="$confvar _obuild/dummy_plugin_stuff/test_dummy_plugin_user.asm"
EOBLOB
}

usage () {
  cat << EOBLOB
usage:
    $0 {setup,build,install,uninstall,clean,
        doc,top,
        sig,get-dependencies,
        local-opam,opam,meta-file,
        travis,
        put-license,remove-license,
        test-env, test-ssl-ck,test-config-file,test-shell-env
        help}"
EOBLOB
}

while [ "$1" != "" ]; do
  case $1 in
    "setup" ) setup ;;
    "build" ) setup; ocp-build build  ;;
    "build-no-color" ) setup; ocp-build -no-color ketrew-test ketrew-cli-test ;;
    "clean" ) rm -fr _prebuild _obuild build.ocp .merlin ocp-build.root* ;;
    "doc" ) make_doc ;;
    "top" ) run_top ;;
    "help" )  usage ;;
    "sig" ) signature $2; shift ;;
    "get-dependencies" ) get_dependencies ;;
    "meta-file" ) meta_file $2; shift ;;
    "install" ) install $2 ; shift;;
    "uninstall" ) uninstall $2 ; shift;;
    "local-opam" ) opam_file "./opam" ;;
    "opam" ) opam_package $2; shift ;;
    "travis" ) do_travis ;;
    "put-license" ) put_license ;;
    "remove-license" ) remove_license ;;
    "test-ssl-ck" ) ssl_cert_key ;;
    "test-config-file" ) test_config_file ;;
    "test-shell-env" )
      test_environment ;;
    "test-env" )
      set_test_additional_findlib_plugin
      compile_dummy_plugin
      ssl_cert_key
      test_config_file
      test_environment ;;
    * ) echo "Unknown command \"$1\"" ; usage ; exit 1 ;;
  esac
  shift
done

