OCAMLMAKEFILE = OCamlMakefile

RESULT = xmlrpc-light
SOURCES = XmlRpc.mli XmlRpc.ml
PACKS = extlib,xml-light,netclient
LIBINSTALL_FILES = XmlRpc.mli XmlRpc.cmi xmlrpc-light.cma xmlrpc-light.cmxa xmlrpc-light.a
OCAMLDOC = ocamlfind ocamldoc -package xml-light

all: native-code-library byte-code-library
install: libinstall

include $(OCAMLMAKEFILE)
