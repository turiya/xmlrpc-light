(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2007 Dave Benjamin (dave@ramenlabs.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let invalid_method name =
  raise
    (XmlRpc.Error
       (-32601, "server error. requested method " ^ name ^ " not found"))

let invalid_params () =
  raise
    (XmlRpc.Error
       (-32602, "server error. invalid method parameters"))

let system_get_capabilities _ =
  `Struct
    [
      "system.multicall",
      `Struct
        [
          "specUrl", `String "http://www.xmlrpc.com/discuss/msgReader$1208";
          "specVersion", `Int 1;
        ];

      "xmlrpc",
      `Struct
        [
          "specUrl", `String "http://www.xmlrpc.com/spec";
          "specVersion", `Int 1;
        ];

      "faults_interop",
      `Struct
        [
          "specUrl", `String "http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php";
          "specVersion", `Int 20010516;
        ];
    ]

let system_list_methods methods _ =
  let names = ref [] in
  Hashtbl.iter
    (fun n _ -> names := `String n :: !names)
    methods;
  `Array (List.sort compare !names)

let system_multicall methods = function
  | [`Array calls] ->
      `Array
        (List.map
           (function
              | `Struct ["methodName", `String name;
                         "params", `Array params]
              | `Struct ["params", `Array params;
                         "methodName", `String name] ->
                  (try `Array [(try Hashtbl.find methods name
                                with Not_found -> invalid_method name)
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
  | _ -> invalid_params ()

let parse_version version =
  let rec loop nums rest =
    try
      let i = String.index rest '.' in
      loop
        ((String.sub rest 0 i) :: nums)
        (String.sub rest (i + 1) (String.length rest - i - 1))
    with Not_found ->
      rest :: nums in
  List.rev (List.map int_of_string (loop [] version))

let ocamlnet_version = parse_version Netconst.ocamlnet_version

class virtual base =
object (self)
  val methods =
    (Hashtbl.create 0 : (string, XmlRpc.value list -> XmlRpc.value) Hashtbl.t)

  val mutable base64_encode = fun s -> XmlRpcBase64.str_encode s
  val mutable base64_decode = fun s -> XmlRpcBase64.str_decode s

  val mutable datetime_encode = XmlRpc.iso8601_of_datetime
  val mutable datetime_decode = XmlRpc.datetime_of_iso8601

  val mutable error_handler = XmlRpc.default_error_handler

  method set_base64_encode f = base64_encode <- f
  method set_base64_decode f = base64_decode <- f

  method set_datetime_encode f = datetime_encode <- f
  method set_datetime_decode f = datetime_decode <- f

  method set_error_handler f = error_handler <- f

  method register name f =
    Hashtbl.replace methods name f

  method unregister name =
    Hashtbl.remove methods name

  method virtual run : unit -> unit

  initializer
    self#register "system.getCapabilities" system_get_capabilities;
    self#register "system.listMethods" (system_list_methods methods);
    self#register "system.multicall" (system_multicall methods);
end

class type server =
object
  inherit base
  method run : unit -> unit
end

class cgi () =
object (self)
  inherit base

  method private process (cgi : Netcgi.cgi) =
    match cgi#request_method with
      | `POST ->
          let input = cgi#argument_value "BODY" in
          let output =
            XmlRpc.serve
              ~base64_encode ~base64_decode
              ~datetime_encode ~datetime_decode
              ~error_handler
              (fun name ->
                 try Hashtbl.find methods name
                 with Not_found -> invalid_method name)
              input in
          cgi#set_header ~content_type:"text/xml" ();
          cgi#output#output_string output;
          cgi#output#commit_work ()
      | _ ->
          cgi#output#output_string
            "XML-RPC server accepts POST requests only.\n";
          cgi#output#commit_work ()

  method run () =
    let config =
      { Netcgi.default_config with
          Netcgi_common.permitted_input_content_types = [ "text/xml" ]
      } in

    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    Netcgi_cgi.run ~config ~output_type:(`Transactional buffered) self#process
end

open Netcgi1_compat

class netplex ?(parallelizer=Netplex_mp.mp()) ?(handler="xmlrpc") () =
object (self)
  inherit base

  method private process env (cgi : Netcgi_types.cgi_activation) =
    match cgi#request_method with
      | `POST ->
          let input = cgi#argument_value "BODY" in
          let output =
            XmlRpc.serve
              ~base64_encode ~base64_decode
              ~datetime_encode ~datetime_decode
              ~error_handler
              (fun name ->
                 try Hashtbl.find methods name
                 with Not_found -> invalid_method name)
              input in
          if ocamlnet_version < [2; 2; 8]
          then env#send_output_header ()
          else cgi#set_header ~content_type:"text/xml" ();
          cgi#output#output_string output;
          cgi#output#commit_work ()
      | _ ->
          if ocamlnet_version < [2; 2; 8]
          then env#send_output_header ();
          cgi#output#output_string
            "XML-RPC server accepts POST requests only.\n";
          cgi#output#commit_work ()

  method run () =
    let (opt_list, cmdline_cfg) = Netplex_main.args () in
    Arg.parse
      opt_list
      (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
      ("usage: " ^ Sys.executable_name ^ " [options]");

    let xmlrpc =
      { Nethttpd_services.dyn_handler = self#process;
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
