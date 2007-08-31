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

let version = "0.5"

exception Error of (int * string)

type value =
    [ `Array of value list
    | `Binary of string
    | `Boolean of bool
    | `DateTime of int * int * int * int * int * int * int
    | `Double of float
    | `Int of int
    | `String of string
    | `Struct of (string * value) list ]

type message =
    | MethodCall of (string * value list)
    | MethodResponse of value
    | Fault of (int * string)

let safe_map f xs =
  List.rev (List.rev_map f xs)

let invalid_xml () =
  raise (Error (-32700, "parse error. not well formed"))

let invalid_xmlrpc () =
  raise (Error (-32600,
                "server error. invalid xml-rpc. not conforming to spec"))

let string_of_tz_offset offset =
  Printf.sprintf "%c%02d:%02d"
    (if offset >= 0 then '+' else '-')
    (abs (offset / 60))
    (abs (offset mod 60))

let tz_offset_of_string = function
  | "" | "Z" -> 0
  | string ->
      Scanf.sscanf string "%c%02d:%02d"
        (fun sign hour min ->
           min + hour * (if sign = '-' then -60 else 60))

let iso8601_of_datetime (y, m, d, h, m', s, tz_offset) =
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02d%s"
    y m d h m' s (string_of_tz_offset tz_offset)

let datetime_of_iso8601 string =
  try
    Scanf.sscanf string "%04d%02d%02dT%02d:%02d:%02d%s"
      (fun y m d h m' s tz ->
         (y, m, d, h, m', s, (tz_offset_of_string tz)))
  with
    | Scanf.Scan_failure _
    | End_of_file ->
        raise (Error (-32600,
                      "server error. unable to parse dateTime value"))

let rec dump = function
  | `String data -> data
  | `Int data -> string_of_int data
  | `Boolean data -> if data then "true" else "false"
  | `Double data -> string_of_float data
  | `Binary data -> data
  | `Array data -> "[" ^ (String.concat ", "
                            (safe_map dump data)) ^ "]"
  | `Struct data -> "{" ^ (String.concat ", "
                             (safe_map
                                (fun (n, v) ->
                                   n ^ ": " ^ (dump v))
                                data)) ^ "}"
  | `DateTime data -> iso8601_of_datetime data

let rec xml_element_of_value
    ?(base64_encoder=fun s -> XmlRpcBase64.str_encode s)
    ?(datetime_encoder=iso8601_of_datetime)
    value =
  Xml.Element
    (match value with
       | `String data -> ("string", [], [Xml.PCData data])
       | `Int data -> ("int", [], [Xml.PCData (string_of_int data)])
       | `Boolean data -> ("boolean", [], [Xml.PCData
                                             (if data then "1" else "0")])
       | `Double data -> ("double", [], [Xml.PCData (string_of_float data)])
       | `Binary data -> ("base64", [], [Xml.PCData (base64_encoder data)])
       | `Array data ->
           ("array", [], [Xml.Element
                            ("data", [],
                             safe_map
                               (fun item ->
                                  Xml.Element ("value", [],
                                               [xml_element_of_value
                                                  ~base64_encoder
                                                  ~datetime_encoder
                                                  item]))
                               data)])
       | `Struct data ->
           ("struct", [],
            safe_map
              (fun (name, value) ->
                 Xml.Element
                   ("member", [],
                    [Xml.Element ("name", [], [Xml.PCData name]);
                     Xml.Element ("value", [],
                                  [xml_element_of_value
                                     ~base64_encoder
                                     ~datetime_encoder
                                     value])]))
              data)
       | `DateTime data ->
           ("dateTime.iso8601", [],
            [Xml.PCData (datetime_encoder data)]))

let rec value_of_xml_element
    ?(base64_decoder=fun s -> XmlRpcBase64.str_decode s)
    ?(datetime_decoder=datetime_of_iso8601)
    = function
      | Xml.Element ("string", [], []) -> `String ""
      | Xml.Element ("string", [], [Xml.PCData data]) -> `String data
      | Xml.Element ("int", [], [Xml.PCData data])
      | Xml.Element ("i4", [], [Xml.PCData data]) ->
          `Int (int_of_string data)
      | Xml.Element ("boolean", [], [Xml.PCData data]) ->
          `Boolean (data <> "0")
      | Xml.Element ("double", [], [Xml.PCData data]) ->
          `Double (float_of_string data)
      | Xml.Element ("base64", [], []) -> `Binary ""
      | Xml.Element ("base64", [], [Xml.PCData data]) ->
          `Binary (base64_decoder data)
      | Xml.Element ("array", [], [Xml.Element ("data", [], data)]) ->
          `Array
            (safe_map
               (function
                  | Xml.Element ("value", [], []) ->
                      (* Empty value is assumed to be an empty string. *)
                      `String ""
                  | Xml.Element ("value", [], [value]) ->
                      value_of_xml_element
                        ~base64_decoder
                        ~datetime_decoder
                        value
                  | _ -> invalid_xmlrpc ())
               data)
      | Xml.Element ("struct", [], members) ->
          `Struct
            (safe_map
               (function
                  | Xml.Element ("member", [],
                                 [Xml.Element ("name", [], [Xml.PCData name]);
                                  Xml.Element ("value", [], [])]) ->
                      (* Empty value is assumed to be an empty string. *)
                      (name, `String "")
                  | Xml.Element ("member", [],
                                 [Xml.Element ("name", [], [Xml.PCData name]);
                                  Xml.Element ("value", [], [value])]) ->
                      (name,
                       value_of_xml_element
                         ~base64_decoder
                         ~datetime_decoder
                         value)
                  | _ -> invalid_xmlrpc ())
               members)
      | Xml.Element ("dateTime:iso8601", [], [Xml.PCData data]) ->
          (* The colon above is intentional. (See fix_dotted_tags.) *)
          `DateTime (datetime_decoder data)
      | Xml.PCData data ->
          (* Untyped data is assumed to be a string. *)
          `String data
      | _ -> invalid_xmlrpc ()

let xml_element_of_message
    ?(base64_encoder=fun s -> XmlRpcBase64.str_encode s)
    ?(datetime_encoder=iso8601_of_datetime)
    message =
  let make_param param =
    Xml.Element ("param", [],
                 [Xml.Element ("value", [],
                               [xml_element_of_value
                                  ~base64_encoder
                                  ~datetime_encoder
                                  param])]) in
  let make_fault code string =
    Xml.Element ("value", [],
                 [xml_element_of_value ~base64_encoder ~datetime_encoder
                    (`Struct ["faultCode", `Int code;
                              "faultString", `String string])]) in
  match message with
    | MethodCall (name, params) ->
        Xml.Element
          ("methodCall", [],
           [Xml.Element ("methodName", [], [Xml.PCData name]);
            Xml.Element ("params", [], safe_map make_param params)])
    | MethodResponse value ->
        Xml.Element
          ("methodResponse", [],
           [Xml.Element ("params", [], [make_param value])])
    | Fault (code, string) ->
        Xml.Element ("methodResponse", [],
                     [Xml.Element ("fault", [], [make_fault code string])])

let message_of_xml_element
    ?(base64_decoder=fun s -> XmlRpcBase64.str_decode s)
    ?(datetime_decoder=datetime_of_iso8601)
    xml_element =
  let parse_params params =
    safe_map
      (function
         | Xml.Element ("param", [], [Xml.Element ("value", [], [])]) ->
             (* Empty value is assumed to be an empty string. *)
             `String ""
         | Xml.Element ("param", [], 
                        [Xml.Element ("value", [], [element])]) ->
             value_of_xml_element ~base64_decoder ~datetime_decoder element
         | _ -> invalid_xmlrpc ())
      params in
  let parse_fault = function
    | [Xml.Element ("value", [], [element])] ->
        (match value_of_xml_element ~base64_decoder ~datetime_decoder element
         with
           | `Struct ["faultCode", `Int code;
                      "faultString", `String string]
           | `Struct ["faultString", `String string;
                      "faultCode", `Int code] ->
               (code, string)
           | _ -> invalid_xmlrpc ())
    | _ -> invalid_xmlrpc () in
  match xml_element with
    | Xml.Element ("methodCall", [],
                   [Xml.Element ("methodName", [], [Xml.PCData name]);
                    Xml.Element ("params", [], params)]) ->
        MethodCall (name, parse_params params)
    | Xml.Element ("methodResponse", [],
                   [Xml.Element ("params", [], params)]) ->
        MethodResponse (List.hd (parse_params params))
    | Xml.Element (_, [], [Xml.Element ("fault", [], fault)]) ->
        Fault (parse_fault fault)
    | _ -> invalid_xmlrpc ()

(* Workaround for Xml-Light, which doesn't like dots in tag names. *)
let fix_dotted_tags s =
  let len = String.length s in
  let in_tag = ref false in
  for i = 0 to len - 1 do
    match s.[i] with
      | '<' -> in_tag := true
      | '>' -> in_tag := false
      | '.' when !in_tag -> s.[i] <- ':'
      | _ -> ()
  done

class client
  ?(debug=false)
  ?(timeout=300.0)
  ?(useragent="XmlRpc-Light/" ^ version)
  url =
object (self)
  val url = url

  val mutable debug = debug
  val mutable timeout = timeout
  val mutable useragent = useragent

  val mutable base64_encoder = fun s -> XmlRpcBase64.str_encode s
  val mutable base64_decoder = fun s -> XmlRpcBase64.str_decode s

  val mutable datetime_encoder = iso8601_of_datetime
  val mutable datetime_decoder = datetime_of_iso8601

  method url = url

  method debug = debug
  method set_debug debug' = debug <- debug'
  method timeout = timeout
  method set_timeout timeout' = timeout <- timeout'
  method useragent = useragent
  method set_useragent useragent' = useragent <- useragent'

  method set_base64_encoder f = base64_encoder <- f
  method set_base64_decoder f = base64_decoder <- f

  method set_datetime_encoder f = datetime_encoder <- f
  method set_datetime_decoder f = datetime_decoder <- f

  method call name params =
    let xml_element =
      xml_element_of_message
        ~base64_encoder
        ~datetime_encoder
        (MethodCall (name, params)) in

    let xml =
      "<?xml version=\"1.0\"?>\n"
      ^ Xml.to_string_fmt xml_element in

    let parsed_url = Neturl.parse_url url in
    let basic_auth =
      try
        Some (Neturl.url_user parsed_url,
              Neturl.url_password parsed_url)
      with Not_found ->
        None in
    let url =
      Neturl.string_of_url
        (Neturl.remove_from_url ~user:true ~password:true parsed_url) in

    let call = new Http_client.post_raw url xml in
    call#set_req_header "User-Agent" useragent;
    call#set_req_header "Content-Type" "text/xml";

    begin
      match basic_auth with
        | Some (user, password) ->
            call#set_req_header "Authorization"
              ("Basic " ^
                 Netencoding.Base64.encode (user ^ ":" ^ password))
        | None -> ()
    end;

    let pipeline = new Http_client.pipeline in
    pipeline#set_proxy_from_environment ();

    let opt = pipeline#get_options in
    pipeline#set_options
      {opt with Http_client.
         connection_timeout = timeout;
      };

    if debug then
      begin
        let opt = pipeline#get_options in
        pipeline#set_options
          {opt with Http_client.
             verbose_status = true;
             verbose_request_header = true;
             verbose_response_header = true;
             verbose_request_contents = true;
             verbose_response_contents = true;
             verbose_connection = true;
          }
      end;

    pipeline#add call;
    pipeline#run ();

    match call#status with
      | `Successful ->
          let contents = call#get_resp_body () in
          fix_dotted_tags contents;
          (match (message_of_xml_element
                    ~base64_decoder
                    ~datetime_decoder
                    (Xml.parse_string contents))
           with
             | MethodResponse value -> value
             | Fault (code, string) -> raise (Error (code, string))
             | _ -> invalid_xmlrpc ())
      | `Client_error ->
          raise (Error (-32300, "transport error. client error"))
      | `Http_protocol_error e ->
          raise (Error (-32300, "transport error. protocol error"))
      | `Redirection ->
          raise (Error (-32300, "transport error. redirected"))
      | `Server_error ->
          raise (Error (-32300, "transport error. server error"))
      | `Unserved ->
          assert false
end

class multicall (client : client) =
object (self)
  val client = client
  val mutable queue = []
  val mutable executed = false
  val mutable results = None
  val counter = ref 0

  method call name params =
    if self#executed then failwith "multicall#call: already executed";
    let num = !counter in
    incr counter;
    queue <- (name, params) :: queue;
    lazy (self#result num)

  method execute () =
    if self#completed then failwith "multicall#execute: already completed";
    executed <- true;
    let calls = List.rev queue in
    let args = [`Array
                  (safe_map
                     (fun (name, params) ->
                        `Struct ["methodName", `String name;
                                 "params", `Array params])
                     calls)] in
    match client#call "system.multicall" args with
      | `Array values -> results <- Some (Array.of_list values)
      | _ -> invalid_xmlrpc ()

  method result num =
    if not self#completed then self#execute ();
    match results with
      | Some values ->
          (match values.(num) with
             | `Array [v] -> v
             | `Struct ["faultCode", `Int code;
                        "faultString", `String string]
             | `Struct ["faultString", `String string;
                        "faultCode", `Int code] ->
                 raise (Error (code, string))
             | _ -> invalid_xmlrpc ())
      | None -> assert false

  method executed = executed
  method completed = results <> None
end

let default_error_handler e =
  raise (Error (-32500, "application error. " ^ Printexc.to_string e))

let quiet_error_handler e =
  raise e

let serve
    ?(base64_encoder=fun s -> XmlRpcBase64.str_encode s)
    ?(base64_decoder=fun s -> XmlRpcBase64.str_decode s)
    ?(datetime_encoder=iso8601_of_datetime)
    ?(datetime_decoder=datetime_of_iso8601)
    ?(error_handler=default_error_handler)
    f s =
  try
    begin
      try
        begin
          fix_dotted_tags s;
          match (message_of_xml_element
                   ~base64_decoder
                   ~datetime_decoder
                   (Xml.parse_string s))
          with
            | MethodCall (name, params) ->
                Xml.to_string_fmt
                  (xml_element_of_message
                     ~base64_encoder
                     ~datetime_encoder
                     (try MethodResponse (f name params) with
                        | Error _ as e -> raise e
                        | e -> error_handler e))
            | _ -> invalid_xmlrpc ()
        end
      with Xml.Error _ -> invalid_xml ()
    end
  with Error (code, string) ->
    Xml.to_string_fmt (xml_element_of_message (Fault (code, string)))
