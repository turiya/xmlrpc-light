# [XmlRpc-Light](http://code.google.com/p/xmlrpc-light/) #

**[XmlRpc-Light](http://code.google.com/p/xmlrpc-light/)** is an [XML-RPC](http://en.wikipedia.org/wiki/XML-RPC) client and server library written in OCaml. It requires [Xml-Light](http://tech.motion-twin.com/xmllight) and [Ocamlnet 2](http://projects.camlcity.org/projects/ocamlnet.html).

### New in version 0.6.1 ###

  * [WordPress](http://wordpress.org/) example updated to support all new RPC methods in [WordPress 2.7](http://codex.wordpress.org/Version_2.7), including reading and writing of comments and options, timezone-safe date-time handling, and page and post status lists
  * New example code: [complete set of bindings](http://xmlrpc-light.googlecode.com/svn/trunk/examples/ubigraph/) to [UbiGraph](http://ubietylab.net/ubigraph/), an interactive 3D graph rendering and animation engine
  * Support for int32 type and lazy multicall clients with "genclient" code-generation tool
  * New utility function, [XmlRpc.serve\_message](http://xmlrpc-light.googlecode.com/svn/trunk/doc/xmlrpc-light/html/XmlRpc.html#VALserve_message), enables building custom servers that efficiently map between other structured serialization formats

### New in version 0.6 ###

  * New XmlRpcDateTime module
  * Added support for "nil" data type
  * WordPress example updated to support WordPress 2.3
  * oUnit test suite

### New in version 0.5 ###

  * client: configurable socket timeouts
  * client: basic authentication
  * client: custom HTTP headers
  * client: SSL support (requires command-line "curl" program)
  * client: multicall class with optional lazy call behavior
  * client: better interoperability with Apache XMLRPC
  * client: code generation tool based on introspection functions
  * server: methodHelp and methodSignature introspection functions
  * server: shallow type checking of method signatures
  * server: multiple signatures per method (overloading)
  * both: proper text/xml Content-Type header and xml preamble
  * both: 32-bit integer support

### Client Example ###

```
    let rpc = new XmlRpc.client "http://localhost:8000" in
    let result = rpc#call "echo" [`String "hello!"] in
    print_endline (XmlRpc.dump result) 
```

### Server Example ###

```
    let server = new XmlRpcServer.cgi () in
    server#register "demo.sayHello"
      (fun _ -> `String "Hello!");
    server#run () 
```

### Documentation ###

[The latest HTML documentation is available here.](http://xmlrpc-light.googlecode.com/svn/trunk/doc/xmlrpc-light/html/index.html)

[The latest PDF documentation is available here.](http://xmlrpc-light.googlecode.com/svn/trunk/doc/xmlrpc-light/latex/doc.pdf)