#!/usr/bin/env bash

version_string="0.0.1-prealpha"
findlib_packages="sosa nonstd docout pvem pvem_lwt_unix cmdliner atdgen atd yojson uri toml"
license_name="ISC"
seb=( "Sebastien Mondet" "seb@mondet.org" "http://seb.mondet.org" )
authors=( "seb" )


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

  mkdir -p _obuild/atdgen/
  local lib_atd_files=$(find src/atd/ -type f -name '*.atd')
  for atd in $lib_atd_files ; do
    name=`basename $atd`
    atdgen -t -o _obuild/atdgen/ketrew_gen_${name%.atd} $atd
    atdgen -j -j-std -o _obuild/atdgen/ketrew_gen_${name%.atd} $atd
  done
  local lib_gen_files=$(find _obuild/atdgen/ -type f -name '*.ml')

  local quoted_lib_files=$(for f in $lib_files ; do echo "\"$f\" " ; done)
  local quoted_findlib_packages=$(for f in $findlib_packages ; do echo "\"$f\" " ; done)
  local quoted_gen_files=$(for f in $lib_gen_files ; do echo "\"$f\" " ; done)

  local yojson_hack_dir=$PWD/_prebuild/yojson
  mkdir -p $yojson_hack_dir
  cp -r `ocamlfind query yojson`/* $yojson_hack_dir
  cd $yojson_hack_dir 
  ocamlc -a -o yojson.cma `ocamlfind query easy-format`/easy_format.cmo yojson.cmo
  ocamlopt -a -o yojson.cmxa `ocamlfind query easy-format`/easy_format.cmx yojson.cmx
  cd -

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
    "ketrew_version.ml" (ocp2ml)
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
begin program "ketrew-cli-test"
  files = [ "src/test/cli.ml" ]
  requires = [ "ketrew" "threads" ]
  link = [ "-thread" ]
  comp = ["-thread" ]
  install = false
end
begin program "ketrew-client"
  files = [ "src/app/main.ml" ]
  requires = [ "ketrew" "threads" ]
  link = [ "-thread" ]
  comp = ["-thread" ]
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
  local outdir=_doc/
  local apidoc=$outdir/api/
  local ocamlfind_package_options=`for p in $findlib_packages ; do echo -n "-package $p " ; done`
  mkdir -p $apidoc
  ocamlfind ocamldoc -html -d $apidoc $ocamlfind_package_options  -thread \
    -charset UTF-8 -t "Ketrew API" -keep-code -colorize-code -sort \
    -I _obuild/ketrew/ $lib_mli_files $lib_ml_files 
  local dot_file=_doc/modules.dot
  local image_file=modules.svg
  ocamlfind ocamldoc -dot -o $dot_file $ocamlfind_package_options  -thread \
    -t "Ketrew $version_string" \
    -I _obuild/ketrew/ $lib_mli_files $lib_ml_files 
  dot -Tsvg $dot_file -o_doc/$image_file
  rm $dot_file

  local markdown_authors_list=""
  for idx in "${authors[@]}" ; do
    eval name=\${$idx[0]}
    eval email=\${$idx[1]}
    eval web=\${$idx[2]}
    markdown_authors_list="$markdown_authors_list
- [$name]($web) (\`$email\`)"
  done

  local index_markdown=/tmp/ketrew_index_markdown

  cp README.md $index_markdown
  cat << END_MD >> $index_markdown

Authors
-------

$markdown_authors_list

Code Documentation
------------------

- [ocaml-doc for the API](api/index.html)

<object class="img-rounded"  type="image/svg+xml" data="$image_file"
style="transform-origin: 0% 100% 0;
       transform: translateY(-100%) rotate(90deg);"
  >Your browser does not support SVG</object>
END_MD

  local index=_doc/index.html
  cp src/doc/code_style.css _doc/
  cat << END_HTML > $index
<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" href="http://cdn.jsdelivr.net/bootstrap/3.1.1/css/bootstrap.min.css" type="text/css">
  <link rel="stylesheet" href="http://cdn.jsdelivr.net/bootstrap/3.1.1/css/bootstrap-theme.min.css" type="text/css">
  <link rel="stylesheet" href="code_style.css" type="text/css">
  <meta charset="utf-8">
  <title>Ketrew $version_string</title>
</head>
  <body><div class="container">
  <h1>Ketrew: Keep Track of Experimental Workflows</h1>
  <h2>Contents</h2>
END_HTML
  omd -otoc -ts 1 -td 4 $index_markdown >> $index
  omd -r ocaml='higlo' $index_markdown | grep -v '<h1' >> $index
  echo "</div></body><html>" >> $index
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

get_dependencies () {
  opam remote add smondet git@github.com:smondet/opam-repository || \
    echo 'Already an opam repo called `smondet`'
  opam install ocp-build $findlib_packages
}

usage () {
  echo "usage: $0"
  echo "       $0 {setup,build,clean,doc,top,help}"
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
    * ) echo "Unknown command \"$1\"" ; usage ; exit 1 ;;
  esac
  shift
done

