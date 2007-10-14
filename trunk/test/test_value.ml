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
    "of_int" >: TestCase
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("int", [], [Xml.PCData "42"]))
           (XmlRpc.xml_element_of_value (`Int 42)));

    "to_int" >: TestCase
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Int 42)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("int", [], [Xml.PCData "42"]))));

    "to_int_i4" >: TestCase
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Int 42)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("i4", [], [Xml.PCData "42"]))));

    "of_int32" >: TestCase
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("int", [], [Xml.PCData "2000000000"]))
           (XmlRpc.xml_element_of_value (`Int32 2000000000l)));

    "to_int32" >: TestCase
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Int32 2000000000l)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("int", [], [Xml.PCData "2000000000"]))));

    "of_double" >: TestCase
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("double", [], [Xml.PCData "42.000000001"]))
           (XmlRpc.xml_element_of_value (`Double 42.000000001)));

    "to_double" >: TestCase
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Double 42.000000001)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("double", [], [Xml.PCData "42.000000001"]))));

    "of_boolean" >: TestCase
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("boolean", [], [Xml.PCData "1"]))
           (XmlRpc.xml_element_of_value (`Boolean true)));

    "to_boolean" >: TestCase
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Boolean false)
           (XmlRpc.value_of_xml_element
              (Xml.Element ("boolean", [], [Xml.PCData "0"]))));

    "of_string" >: TestCase
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("string", [],
                         [Xml.PCData " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n "]))
           (XmlRpc.xml_element_of_value
              (`String " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")));

    "to_string" >: TestCase
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`String " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")
           (XmlRpc.value_of_xml_element
              (Xml.Element ("string", [],
                            [Xml.PCData " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n "]))));

    "of_binary" >: TestCase
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element ("base64", [],
                         [Xml.PCData "IDwmPkFCQ0RFRkcwMTIzNDUgDX4hQCMkJV4mKigpCQ0KIA=="]))
           (XmlRpc.xml_element_of_value
              (`Binary " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")));

    "to_binary" >: TestCase
      (fun () ->
         assert_equal
           ~printer:XmlRpc.dump
           (`Binary " <&>ABCDEFG012345 \r~!@#$%^&*()\t\r\n ")
           (XmlRpc.value_of_xml_element
              (Xml.Element ("base64", [],
                            [Xml.PCData "IDwmPkFCQ0\r\nRFRkcwMTIzN\nDUgDX4hQCMkJV4mKigpCQ0KIA=="]))));
  ]

let tests = test :: tests
