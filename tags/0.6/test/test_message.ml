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

let string_of_message = function
  | XmlRpc.MethodCall (name, params) ->
      sprintf "MethodCall (%s, [%s])"
        name (String.concat "; " (List.map XmlRpc.dump params))
  | XmlRpc.MethodResponse value ->
      sprintf "MethodResponse (%s)" (XmlRpc.dump value)
  | XmlRpc.Fault (code, string) ->
      sprintf "Fault (%d, %s)" code string

let test = "test_message" >:::
  [
    "of_method_call" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element
              ("methodCall", [],
               [Xml.Element ("methodName", [],
                             [Xml.PCData "demo.addTwoNumbers"]);
                Xml.Element
                  ("params", [],
                   [Xml.Element
                      ("param", [],
                       [Xml.Element
                          ("value", [],
                           [Xml.Element ("int", [], [Xml.PCData "4"])])]);
                    Xml.Element
                      ("param", [],
                       [Xml.Element
                          ("value", [],
                           [Xml.Element ("int", [], [Xml.PCData "5"])])])])]))
           (XmlRpc.xml_element_of_message
              (XmlRpc.MethodCall ("demo.addTwoNumbers", [`Int 4; `Int 5]))));

    "to_method_call" >::
      (fun () ->
         assert_equal
           ~printer:string_of_message
           (XmlRpc.MethodCall ("demo.addTwoNumbers",
                               [`String "4"; `String "5"]))
           (XmlRpc.message_of_xml_element
              (Xml.Element
                 ("methodCall", [],
                  [Xml.Element ("methodName", [],
                                [Xml.PCData "demo.addTwoNumbers"]);
                   Xml.Element
                     ("params", [],
                      [Xml.Element
                         ("param", [],
                          [Xml.Element
                             ("value", [],
                              [Xml.Element ("string", [],
                                            [Xml.PCData "4"])])]);
                       (* test untyped value - should be treated as string *)
                       Xml.Element
                         ("param", [],
                          [Xml.Element
                             ("value", [], [Xml.PCData "5"])])])]))));

    "to_method_call_no_params" >::
      (fun () ->
         assert_equal
           ~printer:string_of_message
           (XmlRpc.MethodCall ("demo.helloWorld", []))
           (XmlRpc.message_of_xml_element
              (Xml.Element
                 ("methodCall", [],
                  [Xml.Element ("methodName", [],
                                [Xml.PCData "demo.helloWorld"])]))));

    "of_method_response" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element
              ("methodResponse", [],
               [Xml.Element
                  ("params", [],
                   [Xml.Element
                      ("param", [],
                       [Xml.Element
                          ("value", [],
                           [Xml.Element ("string", [],
                                         [Xml.PCData "test"])])])])]))
           (XmlRpc.xml_element_of_message
              (XmlRpc.MethodResponse (`String "test"))));

    "to_method_response" >::
      (fun () ->
         assert_equal
           ~printer:string_of_message
           (XmlRpc.MethodResponse (`String "test"))
           (XmlRpc.message_of_xml_element
              (Xml.Element
                 ("methodResponse", [],
                  [Xml.Element
                     ("params", [],
                      [Xml.Element
                         ("param", [],
                          [Xml.Element
                             ("value", [],
                              [Xml.Element ("string", [],
                                            [Xml.PCData "test"])])])])])));
         assert_equal
           ~printer:string_of_message
           (XmlRpc.MethodResponse (`String "test"))
           (XmlRpc.message_of_xml_element
              (Xml.Element
                 ("methodResponse", [],
                  [Xml.Element
                     ("params", [],
                      [Xml.Element
                         ("param", [],
                          (* test untyped value *)
                          [Xml.Element
                             ("value", [], [Xml.PCData "test"])])])]))));

    "of_fault" >::
      (fun () ->
         assert_equal
           ~printer:Xml.to_string
           (Xml.Element
              ("methodResponse", [],
               [Xml.Element
                  ("fault", [],
                   [Xml.Element
                      ("value", [],
                       [Xml.Element
                          ("struct", [],
                           [Xml.Element
                              ("member", [],
                               [Xml.Element ("name", [],
                                             [Xml.PCData "faultCode"]);
                                Xml.Element
                                  ("value", [],
                                   [Xml.Element ("int", [],
                                                 [Xml.PCData "12345"])])]);
                            Xml.Element
                              ("member", [],
                               [Xml.Element ("name", [],
                                             [Xml.PCData "faultString"]);
                                Xml.Element
                                  ("value", [],
                                   [Xml.Element
                                      ("string", [],
                                       [Xml.PCData
                                          "something terrible happened"
                                       ])])])])])])]))
           (XmlRpc.xml_element_of_message
              (XmlRpc.Fault (12345, "something terrible happened"))));

    "to_fault" >::
      (fun () ->
         assert_equal
           ~printer:string_of_message
           (XmlRpc.Fault (12345, "something terrible happened"))
           (XmlRpc.message_of_xml_element
              (Xml.Element
                 ("methodResponse", [],
                  [Xml.Element
                     ("fault", [],
                      [Xml.Element
                         ("value", [],
                          [Xml.Element
                             ("struct", [],
                              [Xml.Element
                                 ("member", [],
                                  [Xml.Element ("name", [],
                                                [Xml.PCData "faultCode"]);
                                   Xml.Element
                                     ("value", [],
                                      [Xml.Element ("int", [],
                                                    [Xml.PCData "12345"])])]);
                               Xml.Element
                                 ("member", [],
                                  [Xml.Element ("name", [],
                                                [Xml.PCData "faultString"]);
                                   Xml.Element
                                     ("value", [],
                                      [Xml.Element
                                         ("string", [],
                                          [Xml.PCData
                                             "something terrible happened"
                                          ])])])])])])])));
         assert_equal
           ~printer:string_of_message
           (XmlRpc.Fault (12345, "something terrible happened"))
           (XmlRpc.message_of_xml_element
              (Xml.Element
                 ("methodResponse", [],
                  [Xml.Element
                     ("fault", [],
                      [Xml.Element
                         ("value", [],
                          [Xml.Element
                             ("struct", [],
                              [Xml.Element
                                 ("member", [],
                                  [Xml.Element ("name", [],
                                                [Xml.PCData "faultCode"]);
                                   Xml.Element
                                     ("value", [],
                                      [Xml.Element ("int", [],
                                                    [Xml.PCData "12345"])])]);
                               Xml.Element
                                 ("member", [],
                                  [Xml.Element ("name", [],
                                                [Xml.PCData "faultString"]);
                                   (* test untyped value *)
                                   Xml.Element
                                     ("value", [],
                                      [Xml.PCData
                                         "something terrible happened"
                                      ])])])])])]))));
  ]

let tests = test :: tests
