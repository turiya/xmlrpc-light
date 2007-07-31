OCAMLMAKEFILE = OCamlMakefile

RESULT = xmlrpc-light
SOURCES = XmlRpcBase64.mli XmlRpcBase64.ml XmlRpc.mli XmlRpc.ml XmlRpcServer.mli XmlRpcServer.ml
PACKS = xml-light,netclient,nethttpd-for-netcgi2
LIBINSTALL_FILES = XmlRpcBase64.mli XmlRpcBase64.cmi XmlRpc.mli XmlRpc.cmi XmlRpcServer.mli XmlRpcServer.cmi xmlrpc-light.cma xmlrpc-light.cmxa xmlrpc-light.a
OCAMLDOC = ocamlfind ocamldoc -package xml-light,nethttpd-for-netcgi2
DOC_FILES = XmlRpc.mli XmlRpcServer.mli XmlRpcBase64.mli

all: native-code-library byte-code-library
install: libinstall

include $(OCAMLMAKEFILE)
