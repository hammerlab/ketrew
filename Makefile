
JOBS=1
OCAMLBUILD=ocamlbuild -j $(JOBS) -use-ocamlfind -plugin-tag "package(solvuu-build,nonstd)"
include _build/project.mk
_build/project.mk:
	$(OCAMLBUILD) $(notdir $@)

.PHONY: merlin
merlin:
	rm -f .merlin _build/.merlin && $(MAKE) .merlin && cat .merlin

.PHONY: doc
doc:
	$(OCAMLBUILD) doc/index.html

.PHONY: test-env
test-env:
	. ./tools/test_environment.env ; prepare_test_environment

.PHONY: bisect-clean bisect-report
bisect-clean:
	rm -rf _report_dir bisect*.out
_report_dir:
	mkdir _report_dir

bisect-report: _report_dir
	bisect-ppx-report -I _build/src/pure/ -I _build/src/lib/ \
                      -I _build/src/app/ -I _build/src/test/  \
                      -I _build/ \
                      -I _build/ketrew-test \
                      -I _build/ketrew-workflow-examples \
                      -verbose -html _report_dir  bisect*.out
