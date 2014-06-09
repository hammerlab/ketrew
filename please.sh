#!/usr/bin/env bash

version_string="0.0.1-prealpha"
findlib_packages="sosa nonstd docout pvem pvem_lwt_unix cmdliner"
license_name="ISC"
seb=( "Sebastien Mondet" "seb@mondet.org" "http://seb.mondet.org" )
authors=( "seb" )


lib_ml_files=$(find src/lib/ -type f -name '*.ml')
lib_mli_files=$(find src/lib/ -type f -name '*.mli')
lib_files="$lib_mli_files $lib_ml_files"

setup() {
  local quoted_lib_files=$(for f in $lib_files ; do echo "\"$f\" " ; done)
  local quoted_findlib_packages=$(for f in $findlib_packages ; do echo "\"$f\" " ; done)

  local quoted_authors_list=""
  for idx in "${authors[@]}" ; do
    eval name=\${$idx[0]}
    eval email=\${$idx[1]}
    quoted_authors_list="$quoted_authors_list \"$name <$email>\""
  done

cat << OCP_END > build.ocp
version = "$version_string"
license = "$license_name"
authors = [ $quoted_authors_list ]
begin library "threads"
  generated = true
  dirname = [ "%{OCAMLLIB}%/threads" ]
  has_byte = false
end
begin  library "ketrew"
  sort = true
  files = [
  "ketrew_version.ml" (ocp2ml)
    $quoted_lib_files
  ]
  requires = [ $quoted_findlib_packages ]
  link = [ "-thread" ]
  comp = ["-thread" ]
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
  local index=_doc/index.html

  local markdown_authors_list=""
  for idx in "${authors[@]}" ; do
    eval name=\${$idx[0]}
    eval email=\${$idx[1]}
    eval web=\${$idx[2]}
    markdown_authors_list="$markdown_authors_list
- [$name]($web) (\`$email\`)"
  done

  cat << END_HTML > $index
<!DOCTYPE html>
<html>
<head>
  <link rel="stylesheet" href="api/style.css" type="text/css">
  <meta charset="utf-8">
  <title>Ketrew $version_string</title>
</head>
  <body>
END_HTML
  omd README.md >> $index
  omd << END_MD >> $index
Code Documentation
------------------

- [ocaml-doc for the API](api/index.html)

<a href="$image_file">
<object type="image/svg+xml" data="$image_file"
style="transform-origin: 0% 100% 0;
       transform: translateY(-100%) rotate(90deg);"
  >Your browser does not support SVG</object>
</a>

Authors
-------

$markdown_authors_list

END_MD
  echo "</body><html>" >> $index
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

usage () {
  echo "usage: $0"
  echo "       $0 {setup,build,clean,doc,top,help}"
}
for i in $* ; do
  case $i in
    "setup" ) setup ;;
    "build" | "" ) setup; ocp-build ketrew-test ketrew-cli-test ;;
    "clean" ) rm -fr _obuild build.ocp .merlin ocp-build.root* ;;
    "doc" ) make_doc ;;
    "top" ) run_top ;;
    "help" )  usage ;;
    * ) echo "Unknown command \"$1\"" ; usage ; exit 1 ;;
  esac
done

