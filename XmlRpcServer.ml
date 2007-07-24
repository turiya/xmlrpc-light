let invalid_method () =
  raise (XmlRpc.Error (-32601, "server error. requested method not found"))

let invalid_params () =
  raise (XmlRpc.Error (-32602, "server error. invalid method parameters"))

class netplex ?(parallelizer=Netplex_mp.mp()) ?(handler="xmlrpc") () =
object (self)
  val methods = Hashtbl.create 0

  val mutable base64_encode = fun s -> XmlRpcBase64.str_encode s
  val mutable base64_decode = fun s -> XmlRpcBase64.str_decode s

  val mutable datetime_encode = XmlRpc.iso8601_of_datetime
  val mutable datetime_decode = XmlRpc.datetime_of_iso8601

  method set_base64_encode f = base64_encode <- f
  method set_base64_decode f = base64_decode <- f

  method set_datetime_encode f = datetime_encode <- f
  method set_datetime_decode f = datetime_decode <- f

  method register name f =
    Hashtbl.replace methods name f

  method unregister name =
    Hashtbl.remove methods name

  initializer
    self#register "system.getCapabilities"
      (fun _ ->
         `Struct
           [
             "system.multicall",
             `Struct ["specUrl", `String "http://www.xmlrpc.com/discuss/msgReader$1208";
             "specVersion", `Int 1];
             "xmlrpc",
             `Struct ["specUrl", `String "http://www.xmlrpc.com/spec"; "specVersion", `Int 1];
             "faults_interop",
             `Struct ["specUrl", `String "http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php";
                      "specVersion", `Int 20010516];
           ]);

    self#register "system.listMethods"
      (fun _ ->
         let names = ref [] in
         Hashtbl.iter
           (fun n _ -> names := `String n :: !names)
           methods;
         `Array (List.sort compare !names));

    self#register "system.multicall"
      (function
         | [`Array calls] ->
             `Array
               (List.map
                  (function
                     | `Struct ["methodName", `String name;
                                "params", `Array params]
                     | `Struct ["params", `Array params;
                                "methodName", `String name] ->
                         (try `Array [(try Hashtbl.find methods name
                                       with Not_found -> invalid_method ())
                                        params]
                          with
                            | XmlRpc.Error (code, string) ->
                                `Struct ["faultCode", `Int code;
                                         "faultString", `String string]
                            | e ->
                                `Struct ["faultCode", `Int (-32500);
                                         "faultString",
                                         `String
                                           ("application error. "
                                            ^ Printexc.to_string e)])
                     | _ -> invalid_params ())
                  calls)
         | _ -> invalid_params ());

  method process (cgi : Netcgi_types.cgi_activation) =
    let input = cgi#argument_value "BODY" in
    let output =
      XmlRpc.serve
        ~base64_encode ~base64_decode
        ~datetime_encode ~datetime_decode
        (fun name ->
           try Hashtbl.find methods name
           with Not_found -> invalid_method ())
        input in
    cgi#output#output_string output;
    cgi#output#commit_work ()

  method run () =
    let (opt_list, cmdline_cfg) = Netplex_main.args () in
    Arg.parse
      opt_list
      (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
      "usage: netplex [options]";

    let xmlrpc =
      { Nethttpd_services.dyn_handler = (fun _ -> self#process);
        dyn_activation =
          Nethttpd_services.std_activation `Std_activation_buffered;
        dyn_uri = None;
        dyn_translator = (fun _ -> "");
        dyn_accept_all_conditionals = false
      } in

    let config_cgi =
      { Netcgi_env.default_config with
          Netcgi_env.permitted_input_content_types = [ "text/xml" ]
      } in

    let handlers = [handler, xmlrpc] in

    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

    Netplex_main.startup
      parallelizer
      Netplex_log.logger_factories
      Netplex_workload.workload_manager_factories
      [ Nethttpd_plex.nethttpd_factory ~config_cgi ~handlers () ]
      cmdline_cfg
end
