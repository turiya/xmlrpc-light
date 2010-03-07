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

#use "topfind";;

#require "oUnit";;
#require "netclient";;
#require "nethttpd-for-netcgi2";;
#require "xml-light";;

#load "xmlrpc-light.cma";;

open OUnit;;
open Printf;;

let tests = [];;

#use "test/test_base64.ml";;
#use "test/test_datetime.ml";;
#use "test/test_value.ml";;
#use "test/test_message.ml";;
#use "test/test_serve.ml";;

OUnit.run_test_tt_main (TestList tests);;
