#! /usr/bin/env bash
#
# This file defines a `make_doc` bash function.
# 
# It expects 3 things in the environments:
#
# - `findlib_packages` (comma-separated)
# - optionally `KAPI_DOC`: if `no` then do not build the OCamlDoc part


ocamlfind_packages=$1

make_doc () {
    local outdir=doc/
    local apidoc=apidoc/
    if [ "$KAPI_DOC" != "no" ] ; then
        mkdir -p $apidoc/src/
        #pure_files=`find src/pure -type f -name '*.mli'`
        pure_mlis=`ocamldep -one-line -modules src/pure/*.mli | cut -d : -f 1`
        lib_mlis=`ocamldep -one-line -modules src/lib/*.mli | cut -d : -f 1`
        local out_pure=$apidoc/src/ketrew_pure.mli
        echo "(** The “pure” common library (mostly data) *)" > $out_pure
        for i in $pure_mlis ; do
            base=`basename $i`
            basebase=${base%.mli}
            echo "module ${basebase^}"
            echo "module ${basebase^} : sig" >> $out_pure
            cat $i >> $out_pure
            echo "end" >> $out_pure
        done
        local out_lib=$apidoc/src/ketrew.mli
        echo "(** The library that actually does things in a UNIX environment (contains the engine and the server) *)" > $out_lib
        for i in $lib_mlis ; do
            base=`basename $i`
            basebase=${base%.mli}
            echo "module ${basebase^}"
            echo "module ${basebase^} : sig" >> $out_lib
            cat $i >> $out_lib
            echo "end" >> $out_lib
        done
        echo "module Long_running : sig (* manual one *)" >> $out_lib
        cat src/lib/long_running.ml >> $out_lib
        echo "end" >> $out_lib
        ocamlfind ocamldoc -html -d $apidoc -package $ocamlfind_packages  -thread \
                  -charset UTF-8 -t "Ketrew API" -keep-code -colorize-code \
                  -I ./src/lib/  -I ./src/pure/ -I src/ \
                  $out_pure $out_lib
    fi
    if [ "$CSS" = "" ]; then
        export CSS="https://maxcdn.bootstrapcdn.com/bootswatch/3.2.0/readable/bootstrap.min.css,https://cdn.rawgit.com/hammerlab/ketrew/2d1c430cca52caa71e363a765ff8775a6ae14ba9/src/doc/code_style.css";
    fi

    INPUT=src/doc/,src/test/Workflow_Examples.ml,src/test/dummy_plugin_user.ml,src/test/dummy-plugin \
         INDEX=README.md \
         TITLE_PREFIX="Ketrew: " \
         API=$apidoc/ \
         COMMAND_SUBSTITUTIONS=ketrew:./ketrew \
         CATCH_MODULE_PATHS='^Ketrew.:,LONG_RUNNING:Ketrew.Long_running.' \
         OUTPUT_DIR=$outdir \
         TITLE_SUBSTITUTIONS="dummy_plugin:Plugin Example, \
                         dummy_plugin_user:Plugin-Usage Example, \
                         integration:Integration Test, \
                         preconfigured_main.ml:Preconfigured Main Example, \
                         main:Main Test" \
         oredoc

}

make_doc $*
