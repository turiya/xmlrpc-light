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

let test = "test_value" >:::
  [
    "of_int" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("int", [], [Xml.PCData "42"]))
           (XmlRpc.xml_element_of_value (`Int 42)));

    "to_int" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Int 42)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("int", [], [Xml.PCData "42"]))));

    "to_int_i4" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Int 42)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("i4", [], [Xml.PCData "42"]))));

    "of_int32" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("int", [], [Xml.PCData "2000000000"]))
           (XmlRpc.xml_element_of_value (`Int32 2000000000l)));

    "to_int32" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Int32 2000000000l)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("int", [], [Xml.PCData "2000000000"]))));

    "of_double" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("double", [], [Xml.PCData "42.000000001"]))
           (XmlRpc.xml_element_of_value (`Double 42.000000001)));

    "to_double" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Double 42.000000001)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("double", [], [Xml.PCData "42.000000001"]))));

    "of_nil" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("nil", [], []))
           (XmlRpc.xml_element_of_value `Nil));

    "to_nil" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           `Nil
           (XmlRpc.value_of_xml_element
              (Xml.Element ("nil", [], []))));

    "of_boolean" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("boolean", [], [Xml.PCData "1"]))
           (XmlRpc.xml_element_of_value (`Boolean true)));

    "to_boolean" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Boolean false)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("boolean", [], [Xml.PCData "0"]))));

    "of_string" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("string", [],
                         [Xml.PCData " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n "]))
           (XmlRpc.xml_element_of_value
              (`String " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")));

    "to_string" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`String " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")
           (XmlRpc.value_of_xml_element
              (Xml.Element ("string", [],
                            [Xml.PCData " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n "]))));

    "of_binary" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("base64", [],
                         [Xml.PCData "IDwmPkFCQ0RFRkcwMTIzNDUgDX4hQCMkJV4mKigpCQ0KIA=="]))
           (XmlRpc.xml_element_of_value
              (`Binary " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")));

    "to_binary" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Binary " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")
           (XmlRpc.value_of_xml_element
              (Xml.Element ("base64", [],
                            [Xml.PCData "IDwmPkFCQ0\r\nRFRkcwMTIzN\nDUgDX4hQCMkJV4mKigpCQ0KIA=="]))));

    "of_array" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element
              ("array", [],
               [Xml.Element
                  ("data", [],
                   [Xml.Element ("value", [],
                                 [Xml.Element ("int", [], [Xml.PCData "5"])]);
                    Xml.Element
                      ("value", [],
                       [Xml.Element ("string", [], [Xml.PCData "six"])]);
                    Xml.Element
                      ("value", [],
                       [Xml.Element
                          ("array", [],
                           [Xml.Element
                              ("data", [],
                               [Xml.Element
                                  ("value", [],
                                   [Xml.Element
                                      ("boolean", [], [Xml.PCData "0"])]);
                                Xml.Element
                                  ("value", [],
                                   [Xml.Element
                                      ("double", [],
                                       [Xml.PCData "-1."])])])])])])]))
           (XmlRpc.xml_element_of_value
              (`Array
                 [`Int 5; `String "six";
                  `Array [`Boolean false; `Double (-1.0)]])));

    "to_array" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Array
              [`Int 5; `String "six";
               `Array [`Boolean false; `Double (-1.0)]])
           (XmlRpc.value_of_xml_element
              (Xml.Element
                 ("array", [],
                  [Xml.Element
                     ("data", [],
                      [Xml.Element ("value", [],
                                    [Xml.Element ("int", [],
                                                  [Xml.PCData "5"])]);
                       (* test untyped value - should be treated as string *)
                       Xml.Element ("value", [], [Xml.PCData "six"]);
                       Xml.Element
                         ("value", [],
                          [Xml.Element
                             ("array", [],
                              [Xml.Element
                                 ("data", [],
                                  [Xml.Element
                                     ("value", [],
                                      [Xml.Element
                                         ("boolean", [], [Xml.PCData "0"])]);
                                   Xml.Element
                                     ("value", [],
                                      [Xml.Element
                                         ("double", [],
                                          [Xml.PCData "-1."])])])])])])]))));

    "of_struct" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element
              ("struct", [],
               [Xml.Element
                  ("member", [],
                   [Xml.Element ("name", [], [Xml.PCData "foo"]);
                    Xml.Element ("value", [],
                                 [Xml.Element ("int", [],
                                               [Xml.PCData "42"])])]);
                Xml.Element
                  ("member", [],
                   [Xml.Element ("name", [], [Xml.PCData "bar"]);
                    Xml.Element
                      ("value", [],
                       [Xml.Element
                          ("struct", [],
                           [Xml.Element
                              ("member", [],
                               [Xml.Element ("name", [], [Xml.PCData "baz"]);
                                Xml.Element
                                  ("value", [],
                                   [Xml.Element ("string", [],
                                                 [Xml.PCData
                                                    "rutabega"])])])])])])]))
           (XmlRpc.xml_element_of_value
              (`Struct
                 ["foo", `Int 42;
                  "bar", `Struct ["baz", `String "rutabega"]])));

    "to_struct" >::
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Struct
              ["foo", `Int 42;
               "bar", `Struct ["baz", `String "rutabega"]])
           (XmlRpc.value_of_xml_element
              (Xml.Element
                 ("struct", [],
                  [Xml.Element
                     ("member", [],
                      [Xml.Element ("name", [], [Xml.PCData "foo"]);
                       Xml.Element ("value", [],
                                    [Xml.Element ("int", [],
                                                  [Xml.PCData "42"])])]);
                   Xml.Element
                     ("member", [],
                      [Xml.Element ("name", [], [Xml.PCData "bar"]);
                       Xml.Element
                         ("value", [],
                          [Xml.Element
                             ("struct", [],
                              [Xml.Element
                                 ("member", [],
                                  [Xml.Element ("name", [], [Xml.PCData "baz"]);
                                   (* test untyped value *)
                                   Xml.Element
                                     ("value", [],
                                      [Xml.PCData "rutabega"])])])])])]))));
   "to_struct_empty_name" >::
     (fun () ->
        assert_equal
          ~printer:XmlRpc.dump
          (`Struct ["", `String ""; "", `Int 42])
          (XmlRpc.value_of_xml_element
             (Xml.parse_string
                "<struct>
                   <member><name></name><value></value></member>
                   <member><name/><value><int>42</int></value></member>
                 </struct>")));
  ]

let tests = test :: tests
