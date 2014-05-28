#!/usr/bin/env bash

lib_files=$(find src/lib/ -type f)
findlib_packages="sosa nonstd docout pvem pvem_lwt_unix"

setup() {
  quoted_lib_files=$(for f in $lib_files ; do echo "\"$f\" " ; done)
  quoted_findlib_packages=$(for f in $findlib_packages ; do echo "\"$f\" " ; done)

cat << OCP_END > build.ocp
begin library "threads"
  generated = true
  dirname = [ "%{OCAMLLIB}%/threads" ]
  has_byte = false
end
begin  library "ketrew"
  sort = true
  files = [
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
usage () {
  echo "usage: $0"
  echo "       $0 {setup,build,clean,help}"
}

case $1 in
  "setup" ) setup ;;
  "build" | "" ) setup; ocp-build ketrew-test ;;
  "clean" ) rm -fr _obuild build.ocp .merlin ocp-build.root* ;;
  "help" )  usage ;;
  * ) echo "Unknown command \"$1\"" ; usage ; exit 1 ;;
esac

