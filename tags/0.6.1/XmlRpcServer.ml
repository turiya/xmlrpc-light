(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2007-2009 Dave Benjamin (dave@ramenlabs.com)
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

type param_type =
    [ `Array
    | `Binary
    | `Boolean
    | `DateTime
    | `Double
    | `Int
    | `String
    | `Struct
    | `Undefined ]

let invalid_method name =
  raise
    (XmlRpc.Error
       (-32601, "server error. requested method " ^ name ^ " not found"))

let invalid_params () =
  raise
    (XmlRpc.Error
       (-32602, "server error. invalid method parameters"))

let wrong_num_params () =
  raise
    (XmlRpc.Error
       (-32602, "server error. wrong number of method parameters"))

let system_get_capabilities introspection _ =
  let capabilities =
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

      "nil",
      `Struct
        [
          "specUrl", `String "http://www.ontosys.com/xml-rpc/extensions.php";
          "specVersion", `Int 20010518;
        ];
    ] in
  let capabilities =
    if introspection
    then capabilities @
      [
        "introspection",
        `Struct
          [
            "specUrl", `String "http://xmlrpc.usefulinc.com/doc/reserved.html";
            "specVersion", `Int 1
          ]
      ]
    else capabilities in
  `Struct capabilities

let system_list_methods methods _ =
  let names = ref [] in
  Hashtbl.iter
    (fun n _ -> names := `String n :: !names)
    methods;
  `Array (List.sort compare !names)

let system_method_help method_help = function
  | [`String name] ->
      (try `String (Hashtbl.find method_help name)
       with Not_found -> invalid_method name)
  | _ -> invalid_params ()

let system_method_signature method_signatures = function
  | [`String name] ->
      (try `Array (List.map
                     (fun signature ->
                        `Array
                          (List.map
                             (function
                                | `Array -> `String "array"
                                | `Binary -> `String "base64"
                                | `Boolean -> `String "boolean"
                                | `DateTime -> `String "dateTime.iso8601"
                                | `Double -> `String "double"
                                | `Int -> `String "int"
                                | `String -> `String "string"
                                | `Struct -> `String "struct"
                                | `Undefined -> `String "undefined")
                             signature))
                     (Hashtbl.find method_signatures name))
       with Not_found -> invalid_method name)
  | _ -> invalid_params ()

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

let check_signatures signatures f params =
  let num_params = List.length params in

  let valid_num_params signature =
    List.length signature - 1 = num_params in

  let valid_params signature =
    let passed = ref true in
    begin
      match signature with
        | [] -> ()
        | _ :: param_types ->
            List.iter2
              (fun expected actual ->
                 match (expected, actual) with
                   | (`Array, `Array _)
                   | (`Binary, `Binary _)
                   | (`Boolean, `Boolean _)
                   | (`DateTime, `DateTime _)
                   | (`Double, `Double _)
                   | (`Int, `Int _)
                   | (`Int, `Int32 _)
                   | (`String, `String _)
                   | (`Struct, `Struct _)
                   | (`Undefined, _)
                   | (_, `Nil)
                     -> ()
                   | _ -> passed := false)
              param_types
              params
    end;
    !passed in

  if signatures = []
  then ()
  else if not (List.exists valid_num_params signatures)
  then wrong_num_params ()
  else if not (List.exists valid_params signatures)
  then invalid_params ();

  f params

let rec parse_version ver =
  try let i = String.index ver '.' in
      int_of_string (String.sub ver 0 i) ::
        parse_version (String.sub ver (i + 1) (String.length ver - i - 1))
  with Not_found -> [int_of_string ver]

let ocamlnet_version = parse_version Netconst.ocamlnet_version

class virtual base =
object (self)
  val methods =
    (Hashtbl.create 0 : (string, XmlRpc.value list -> XmlRpc.value) Hashtbl.t)
  val method_help =
    (Hashtbl.create 0 : (string, string) Hashtbl.t)
  val method_signatures =
    (Hashtbl.create 0 : (string, param_type list list) Hashtbl.t)

  val mutable base64_encoder = fun s -> XmlRpcBase64.str_encode s
  val mutable base64_decoder = fun s -> XmlRpcBase64.str_decode s

  val mutable datetime_encoder = XmlRpcDateTime.to_string
  val mutable datetime_decoder = XmlRpcDateTime.of_string

  val mutable error_handler = XmlRpc.default_error_handler

  method set_base64_encoder f = base64_encoder <- f
  method set_base64_decoder f = base64_decoder <- f

  method set_datetime_encoder f = datetime_encoder <- f
  method set_datetime_decoder f = datetime_decoder <- f

  method set_error_handler f = error_handler <- f

  method serve f input =
    XmlRpc.serve
      ~base64_encoder ~base64_decoder
      ~datetime_encoder ~datetime_decoder
      ~error_handler
      f input

  method serve_message f input =
    XmlRpc.serve_message
      ~error_handler
      f input

  method register name ?(help="") ?(signature=[]) ?(signatures=[]) f =
    if help <> ""
    then (Hashtbl.replace method_help name help;
          self#enable_introspection ());
    let signatures =
      if signature <> []
      then signature :: signatures
      else signatures in
    if signatures <> []
    then (Hashtbl.replace method_signatures name signatures;
          self#enable_introspection ());
    Hashtbl.replace methods name (if signatures <> []
                                  then check_signatures signatures f
                                  else f)

  method unregister name =
    Hashtbl.remove methods name;
    Hashtbl.remove method_help name;
    Hashtbl.remove method_signatures name

  method virtual run : unit -> unit

  val mutable introspection = false

  method private enable_introspection () =
    if not introspection
    then
      begin
        introspection <- true;
        self#register "system.getCapabilities"
          ~help:"Returns a struct describing the XML-RPC specifications supported by this server"
          ~signature:[`Struct]
          (system_get_capabilities true);
        self#register "system.listMethods"
          ~help:"Returns an array of available methods on this server"
          ~signature:[`Array]
          (system_list_methods methods);
        self#register "system.methodHelp"
          ~help:"Returns a documentation string for the specified method"
          ~signature:[`String; `String]
          (system_method_help method_help);
        self#register "system.methodSignature"
          ~help:"Returns an array describing the return type and required parameters of a method"
          ~signature:[`Array; `String]
          (system_method_signature method_signatures);
        self#register "system.multicall"
          ~help:"Boxcar multiple RPC calls in one request"
          ~signature:[`Array; `Array]
          (system_multicall methods);
      end

  initializer
    self#register "system.getCapabilities" (system_get_capabilities false);
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
            self#serve
              (fun name ->
                 try Hashtbl.find methods name
                 with Not_found -> invalid_method name)
              input in
          cgi#set_header ~content_type:"text/xml" ();
          cgi#output#output_string "<?xml version=\"1.0\"?>\n";
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
            self#serve
              (fun name ->
                 try Hashtbl.find methods name
                 with Not_found -> invalid_method name)
              input in
          if ocamlnet_version < [2; 2; 8]
          then env#send_output_header ()
          else (cgi#set_header ~content_type:"text/xml" ();
                cgi#output#output_string "<?xml version=\"1.0\"?>\n");
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
