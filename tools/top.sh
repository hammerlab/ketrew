#!/usr/bin/env bash

findlib_packages="sosa nonstd docout pvem pvem_lwt_unix cmdliner \
  ppx_deriving_yojson \
  yojson uri cohttp.lwt lwt ssl conduit dynlink findlib trakeva trakeva_sqlite"

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
#directory "_build/src/pure"
#load "ketrew_pure.cma"
#directory "_build/src/lib"
#load "ketrew.cma"
EOF_ML
  $toplevel -init /tmp/ketrew_ocamlinit

}

run_top
