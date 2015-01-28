
.PHONY: all clean build configure distclean doc apidoc gen test-env

PLEASE=ocaml tools/please.ml 

all: build

configure: setup.data

_oasis: tools/_oasis.in tools/please.ml
	$(PLEASE) make _oasis

setup.data: _oasis
	oasis setup -setup-update dynamic && \
	    ocaml setup.ml -configure --enable-all && \
	    echo 'Configured'

gen:
	$(PLEASE) generate ketrew_data

OCAMLBUILD_ANNOYING_LINKS=main.byte main.native Workflow_Examples.native integration.native dummy_plugin_user.native
OWN_BINARIES= ketrew-test ketrew ketrew-pure ketrew-workflow-examples-test ketrew-integration-test

build: gen
	ocaml setup.ml -build && \
	    rm -f $(OCAMLBUILD_ANNOYING_LINKS) && \
	    cp _build/src/test/main.native ketrew-test && \
	    cp _build/src/app/main.native ketrew && \
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

distclean: clean
	ocaml setup.ml -distclean || echo OK ; \
	    rm -fr setup.ml _tags myocamlbuild.ml gen src/*/META src/*/*.mldylib src/*/*.mllib _oasis
