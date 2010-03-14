(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2007-2010 Dave Benjamin (dave@ramenlabs.com)
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

let () =
  let server = new XmlRpcServer.netplex () in
  server#register "demo.addTwoNumbers"
    ~help:"Add two numbers together"
    ~signatures:[[`Int; `Int; `Int];
                 [`Double; `Int; `Double];
                 [`Double; `Double; `Int];
                 [`Double; `Double; `Double];
                 [`Double; `String; `String]]
    (function
       | [`Int x; `Int y] -> `Int (x + y)
       | [`Int x; `Double y] -> `Double (float_of_int x +. y)
       | [`Double x; `Int y] -> `Double (x +. float_of_int y)
       | [`Double x; `Double y] -> `Double (x +. y)
       | [`String x; `String y] -> `Double (float_of_string x
                                            +. float_of_string y)
       | _ -> XmlRpcServer.invalid_params ());
  server#run ()
