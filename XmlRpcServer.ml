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

type param_type =
    [ `Array
    | `Binary
    | `Boolean
    | `DateTime
    | `Double
    | `Int
    | `String
    | `Struct ]

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
                     (function
                        | `Array -> `Array [`String "array"]
                        | `Binary -> `Binary "base64"
                        | `Boolean -> `Boolean true
                        | `DateTime -> `DateTime (2000,1,2,12,1,2,0)
                        | `Double -> `Double 3.1415
                        | `Int -> `Int 42
                        | `String -> `String "string"
                        | `Struct -> `Struct ["struct",
                                              `String "struct"])
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

let rec parse_version ver =
  try let i = String.index ver '.' in
      int_of_string (String.sub ver 0 i) ::
        parse_version (String.sub ver (i + 1) (String.length ver - i - 1))
  with Not_found -> [int_of_string ver]

let ocamlnet_version = parse_version Netconst.ocamlnet_version

let check_signature signature f params =
  match signature with
    | [] -> f params
    | _ :: param_types ->
        if List.length param_types <> List.length params
        then wrong_num_params ();
        List.iter2
          (fun expected actual ->
               match (expected, actual) with
                 | (`Array, `Array _)
                 | (`Binary, `Binary _)
                 | (`Boolean, `Boolean _)
                 | (`DateTime, `DateTime _)
                 | (`Double, `Double _)
                 | (`Int, `Int _)
                 | (`String, `String _)
                 | (`Struct, `Struct _)
                     -> ()
                 | _ -> invalid_params ())
          param_types
          params;
        f params

class virtual base =
object (self)
  val methods =
    (Hashtbl.create 0 : (string, XmlRpc.value list -> XmlRpc.value) Hashtbl.t)
  val method_help =
    (Hashtbl.create 0 : (string, string) Hashtbl.t)
  val method_signatures =
    (Hashtbl.create 0 : (string, param_type list) Hashtbl.t)

  val mutable base64_encoder = fun s -> XmlRpcBase64.str_encode s
  val mutable base64_decoder = fun s -> XmlRpcBase64.str_decode s

  val mutable datetime_encoder = XmlRpc.iso8601_of_datetime
  val mutable datetime_decoder = XmlRpc.datetime_of_iso8601

  val mutable error_handler = XmlRpc.default_error_handler

  method set_base64_encoder f = base64_encoder <- f
  method set_base64_decoder f = base64_decoder <- f

  method set_datetime_encoder f = datetime_encoder <- f
  method set_datetime_decoder f = datetime_decoder <- f

  method set_error_handler f = error_handler <- f

  method register name ?(help="") ?(signature=[]) f =
    if help <> ""
    then (Hashtbl.replace method_help name help;
          self#enable_introspection ());
    if signature <> []
    then (Hashtbl.replace method_signatures name signature;
          self#enable_introspection ());
    Hashtbl.replace methods name (if signature <> []
                                  then check_signature signature f
                                  else f)

  method unregister name =
    Hashtbl.remove methods name

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
            XmlRpc.serve
              ~base64_encoder ~base64_decoder
              ~datetime_encoder ~datetime_decoder
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
              ~base64_encoder ~base64_decoder
              ~datetime_encoder ~datetime_decoder
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
