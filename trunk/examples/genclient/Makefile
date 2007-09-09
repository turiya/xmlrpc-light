OCAMLMAKEFILE = OCamlMakefile

RESULT = genclient
SOURCES = genclient.ml
PACKS = xmlrpc-light

test: debug-code
	OCAMLRUNPARAM=b ./genclient http://admin:asdasd@gaia.inre.asu.edu/ixrtest/
all: test

include $(OCAMLMAKEFILE)
