OCAMLMAKEFILE = OCamlMakefile

RESULT = xmlrpc-light
SOURCES = XmlRpcBase64.mli XmlRpcBase64.ml XmlRpcDateTime.mli XmlRpcDateTime.ml XmlRpc.mli XmlRpc.ml XmlRpcServer.mli XmlRpcServer.ml
PACKS = xml-light,netclient,nethttpd-for-netcgi2
LIBINSTALL_FILES = XmlRpcBase64.mli XmlRpcBase64.cmi XmlRpcDateTime.mli XmlRpcDateTime.cmi XmlRpc.mli XmlRpc.cmi XmlRpcServer.mli XmlRpcServer.cmi xmlrpc-light.cma xmlrpc-light.cmxa xmlrpc-light.a
OCAMLDOC = ocamlfind ocamldoc -package xml-light,nethttpd-for-netcgi2
DOC_FILES = XmlRpc.mli XmlRpcServer.mli XmlRpcDateTime.mli XmlRpcBase64.mli

all: native-code-library byte-code-library
install: libinstall
uninstall: libuninstall

.PHONY: test
test: byte-code-library
	ocaml test/test.ml

include $(OCAMLMAKEFILE)
