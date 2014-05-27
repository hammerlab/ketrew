#!/usr/bin/env bash

lib_files=$(find src/lib/ -type f)
findlib_packages="sosa nonstd docout pvem pvem_lwt_unix"

setup() {
  quoted_lib_files=$(for f in $lib_files ; do echo "\"$f\" " ; done)
  quoted_findlib_packages=$(for f in $findlib_packages ; do echo "\"$f\" " ; done)

cat << OCP_END > build.ocp
begin  library "ketrew"
  files = [
    $quoted_lib_files
  ]
  requires = [ $quoted_findlib_packages ]
  asmcomp = [ "-g" ]
end
begin program "ketrew-test"
  files = [ "src/test/main.ml" ]
  requires = [ "ketrew" ]
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

case $1 in
  "setup" ) setup ;;
  "build" | "" ) ocp-build ketrew-test ;;
  "clean" ) rm -fr _obuild build.ocp .merlin ocp-build.root* ;;
esac

