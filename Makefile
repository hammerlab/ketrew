
.PHONY: all clean build configure distclean doc apidoc gen test-env

PLEASE=ocaml tools/please.ml

all: build

_oasis: tools/_oasis.in tools/please.ml
	$(PLEASE) make _oasis

setup.data: _oasis
	oasis setup -setup-update dynamic

configure: setup.data
	ocaml setup.ml -configure --enable-all $(EN) && \
	echo 'Configured'

# This call also generates "_build/FINDLIB_PACKAGES"
_build/VERSION:
	$(PLEASE) generate metadata

OCAMLBUILD_ANNOYING_LINKS=main.byte main.native Workflow_Examples.native integration.native dummy_plugin_user.native
OWN_BINARIES= ketrew-test ketrew ketrew-pure ketrew-workflow-examples-test ketrew-integration-test

# Files "_build/VERSION" and "_build/FINDLIB_PACKAGES" are imported as strings into 
# Ketrew via ppx_blob during compilation.
build: _build/VERSION
	ocaml setup.ml -build && \
	    rm -f $(OCAMLBUILD_ANNOYING_LINKS) && \
	    cp _build/src/app/main.native ketrew && \
	    cp _build/src/test/main.native ketrew-test && \
	    cp _build/src/test/Workflow_Examples.native ketrew-workflow-examples-test && \
	    cp _build/src/test/integration.native ketrew-integration-test && \
            echo "Done"

doc:
	./tools/build-documentation.sh

test-env:
	. ./tools/test_environment.env   ; \
             set_test_additional_findlib_plugin ; \
             ssl_cert_key ; test_config_file; test_environment

clean:
	rm -fr _build $(OWN_BINARIES)

distclean: clean clean_reports
	ocaml setup.ml -distclean || echo OK ; \
	    rm -fr setup.ml _tags gen src/*/META src/*/*.mldylib src/*/*.mllib _oasis

clean_reports:
	rm -rf _report_dir bisect*.out

_report_dir:
	mkdir _report_dir

report: _report_dir
	bisect-report -I _build -html _report_dir $(shell ls -t bisect*.out | head -1)
