OCAMLMAKEFILE = OCamlMakefile

RESULT = xmlrpc-light
SOURCES = XmlRpcBase64.mli XmlRpcBase64.ml XmlRpc.mli XmlRpc.ml
PACKS = xml-light,netclient
LIBINSTALL_FILES = XmlRpcBase64.mli XmlRpcBase64.cmi XmlRpc.mli XmlRpc.cmi xmlrpc-light.cma xmlrpc-light.cmxa xmlrpc-light.a
OCAMLDOC = ocamlfind ocamldoc -package xml-light
DOC_FILES = XmlRpc.mli XmlRpcBase64.mli

all: native-code-library byte-code-library
install: libinstall

include $(OCAMLMAKEFILE)
