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

let test = "test_serve" >:::
  [
    "reverse" >::
      (fun () ->
         let data = [`Int 1; `Int 2; `Int 3] in
         assert_equal
           ~printer:string_of_message
           (XmlRpc.MethodResponse (`Array (List.rev data)))
           (XmlRpc.message_of_xml_element
              (Xml.parse_string
                 (XmlRpc.serve
                    (function
                       | "reverse" ->
                           (function
                              | [`Array items] ->
                                  `Array (List.rev items)
                              | _ -> failwith "wrong parameters")
                       | _ -> failwith "unknown method")
                    (Xml.to_string
                       (XmlRpc.xml_element_of_message
                          (XmlRpc.MethodCall
                             ("reverse", [`Array data]))))))));
  ]

let tests = test :: tests
