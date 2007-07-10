(*
 * XmlRpc Light, a small XmlRpc client based on Xml Light and Ocamlnet
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

let string_of_tz_offset offset =
  Printf.sprintf "%c%02d%02d"
    (if offset >= 0 then '+' else '-')
    (abs (offset / 60))
    (abs (offset mod 60))

let tz_offset_of_string = function
  | "" | "Z" -> 0
  | string ->
      Scanf.sscanf string "%c%02d%02d"
        (fun sign hour min ->
           min + hour * (if sign = '-' then -60 else 60))

let datetime_of_iso8601 string =
  Scanf.sscanf string "%04d%02d%02d%c%02d:%02d:%02d%s"
    (fun y m d _ h m' s tz ->
       (y, m, d, h, m', s, (tz_offset_of_string tz)))

let iso8601_of_datetime (y, m, d, h, m', s, tz_offset) =
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02d%s"
    y m d h m' s (string_of_tz_offset tz_offset)

let rec dump = function
  | `String data -> data
  | `Int data -> string_of_int data
  | `Boolean data -> if data then "true" else "false"
  | `Double data -> string_of_float data
  | `Binary data -> data
  | `Array data -> "[" ^ (String.concat ", "
                            (List.map dump data)) ^ "]"
  | `Struct data -> "{" ^ (String.concat ", "
                             (List.map
                                (fun (n, v) ->
                                   n ^ ": " ^ (dump v))
                                data)) ^ "}"
  | `DateTime data -> iso8601_of_datetime data

let rec xml_element_of_value value =
  Xml.Element
    (match value with
       | `String data -> ("string", [], [Xml.PCData data])
       | `Int data -> ("int", [], [Xml.PCData (string_of_int data)])
       | `Boolean data -> ("boolean", [], [Xml.PCData
                                             (if data then "1" else "0")])
       | `Double data -> ("double", [], [Xml.PCData (string_of_float data)])
       | `Binary data -> ("base64", [], [Xml.PCData (XmlRpcBase64.str_encode data)])
       | `Array data ->
           ("array", [], [Xml.Element
                            ("data", [],
                             List.map
                               (fun item ->
                                  Xml.Element ("value", [],
                                               [xml_element_of_value item]))
                               data)])
       | `Struct data ->
           ("struct", [],
            List.map
              (fun (name, value) ->
                 Xml.Element
                   ("member", [],
                    [Xml.Element ("name", [], [Xml.PCData name]);
                     Xml.Element ("value", [],
                                  [xml_element_of_value value])]))
              data)
       | `DateTime data ->
           ("dateTime.iso8601", [],
            [Xml.PCData (iso8601_of_datetime data)]))

let rec value_of_xml_element = function
  | Xml.Element ("string", [], []) -> `String ""
  | Xml.Element ("string", [], [Xml.PCData data]) -> `String data
  | Xml.Element ("int", [], [Xml.PCData data])
  | Xml.Element ("i4", [], [Xml.PCData data]) -> `Int (int_of_string data)
  | Xml.Element ("boolean", [], [Xml.PCData data]) ->
      `Boolean (data <> "0")
  | Xml.Element ("double", [], [Xml.PCData data]) ->
      `Double (float_of_string data)
  | Xml.Element ("base64", [], []) -> `Binary ""
  | Xml.Element ("base64", [], [Xml.PCData data]) ->
      `Binary (XmlRpcBase64.str_decode data)
  | Xml.Element ("array", [], [Xml.Element ("data", [], data)]) ->
      `Array
        (List.map
           (function
              | Xml.Element ("value", [], [value]) ->
                  value_of_xml_element value
              | _ -> raise (Error (-32700, "parse error")))
           data)
  | Xml.Element ("struct", [], members) ->
      `Struct
        (List.map
           (function
              | Xml.Element ("member", [],
                             [Xml.Element ("name", [], [Xml.PCData name]);
                              Xml.Element ("value", [], [value])]) ->
                  (name, value_of_xml_element value)
              | _ -> raise (Error (-32700, "parse error")))
           members)
  | Xml.Element ("dateTime:iso8601", [], [Xml.PCData data]) ->
      (* The colon above is intentional. (See fix_dotted_tags.) *)
      `DateTime (datetime_of_iso8601 data)
  | _ -> raise (Error (-32700, "parse error"))

let message_of_xml_element xml_element =
  let parse_params params =
    List.map
      (function
         | Xml.Element ("param", [], 
                        [Xml.Element ("value", [], [element])]) ->
             value_of_xml_element element
         | _ -> raise (Error (-32700, "parse error")))
      params in
  let parse_fault = function
    | [Xml.Element ("value", [], [element])] ->
        (match value_of_xml_element element with
           | `Struct ["faultCode", `Int code;
                      "faultString", `String string] ->
               (code, string)
           | _ -> raise (Error (-32700, "parse error")))
    | _ -> raise (Error (-32700, "parse error")) in
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
    | _ -> raise (Error (-32700, "parse error"))

let xml_element_of_message =
  let make_param param =
    Xml.Element ("param", [],
                 [Xml.Element ("value", [],
                               [xml_element_of_value param])]) in
  let make_fault code string =
    Xml.Element ("value", [],
                 [xml_element_of_value
                    (`Struct ["faultCode", `Int code;
                              "faultString", `String string])]) in
  function
    | MethodCall (name, params) ->
        Xml.Element
          ("methodCall", [],
           [Xml.Element ("methodName", [], [Xml.PCData name]);
            Xml.Element ("params", [], List.map make_param params)])
    | MethodResponse value ->
        Xml.Element
          ("methodResponse", [],
           [Xml.Element ("params", [], [make_param value])])
    | Fault (code, string) ->
        Xml.Element ("methodResponse", [],
                     [Xml.Element ("fault", [], [make_fault code string])])

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

class client url =
object (self)
  val url = url
  val mutable useragent = "OCaml " ^ Sys.ocaml_version
  val mutable debug = false

  method url = url
  method useragent = useragent
  method set_useragent useragent' = useragent <- useragent'
  method debug = debug
  method set_debug debug' = debug <- debug'

  method call name params =
    let xml_element = xml_element_of_message (MethodCall (name, params)) in
    let xml = Xml.to_string_fmt xml_element in
    if debug then print_endline xml;
    let call = new Http_client.post_raw url xml in
    call#set_req_header "User-agent" useragent;
    let pipeline = new Http_client.pipeline in
    pipeline#add call;
    pipeline#run ();
    match call#status with
      | `Successful ->
          let contents = call#get_resp_body () in
          if debug then print_endline contents;
          fix_dotted_tags contents;
          (match message_of_xml_element (Xml.parse_string contents) with
             | MethodResponse value -> value
             | Fault (code, string) -> raise (Error (code, string))
             | _ -> raise (Error (-32700, "parse error")))
      | `Client_error -> raise (Error (-32300, "client error"))
      | `Http_protocol_error e -> raise (Error (-32300, "protocol error"))
      | `Redirection -> raise (Error (-32300, "redirected"))
      | `Server_error -> raise (Error (-32300, "server error"))
      | `Unserved -> assert false
end
