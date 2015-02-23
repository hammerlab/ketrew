#! /usr/bin/env bash
#
# This file defines a `make_doc` bash function.
# 
# It expects 3 things in the environments:
#
# - `findlib_packages` (space separated)
# - optionally `KAPI_DOC`: if `no` then do not build the OCamlDoc part

ocamlfind_package_options=`ocaml ./tools/please.ml say ocamlfind-package-options`
lib_mli_files=`ocaml ./tools/please.ml say lib-mli-files`
lib_ml_files=`ocaml ./tools/please.ml say lib-ml-files`

make_doc () {
    local git_branch=`git symbolic-ref --short HEAD`
    local outdir=_doc/
    if [ "$git_branch" != "master" ]; then
        outdir=_doc/$git_branch
    fi
    local apidoc=_apidoc/$git_branch
    if [ "$KAPI_DOC" != "no" ] ; then
        mkdir -p $apidoc
        ocamlfind ocamldoc -html -d $apidoc $ocamlfind_package_options  -thread \
                  -charset UTF-8 -t "Ketrew API" -keep-code -colorize-code -sort \
                  -I _build/src/lib/ -I _build/gen/ $lib_mli_files $lib_ml_files 
    fi

    INPUT=src/doc/,src/test/ \
         INDEX=README.md \
         TITLE_PREFIX="Ketrew: " \
         API=$apidoc/ \
         COMMAND_SUBSTITUTIONS=ketrew:./ketrew \
         CATCH_MODULE_PATHS='^Ketrew[_a-z]*:,LONG_RUNNING:Ketrew_long_running.' \
         OUTPUT_DIR=$outdir \
         TITLE_SUBSTITUTIONS="dummy_plugin:Plugin Example, \
                         dummy_plugin_user:Plugin-Usage Example, \
                         integration:Integration Test, \
                         preconfigured_main.ml:Preconfigured Main Example, \
                         main:Main Test" \
         oredoc

}

make_doc $*
