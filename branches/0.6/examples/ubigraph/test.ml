(*
 * XmlRpc Light, a small XmlRpc library based on Xml Light and Ocamlnet
 * Copyright (C) 2008 Dave Benjamin (dave@ramenlabs.com)
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

(* Based on Python example, "examples/Python/ubigraph_example.py",
   distributed with Ubigraph. For details, see the Ubigraph website:
   http://ubietylab.net/ubigraph/
*)

let client = new Ubigraph.client "http://localhost:20738/RPC2"
let u = client#ubigraph
let _ = u#clear ()

let x = u#new_vertex ()
let _ = u#set_vertex_attribute x "shape" "sphere"
let _ = u#set_vertex_attribute x "color" "#ffff00"

let small_red = u#new_vertex_style 0l
let _ = u#set_vertex_style_attribute small_red "shape" "sphere"
let _ = u#set_vertex_style_attribute small_red "color" "#ff0000"
let _ = u#set_vertex_style_attribute small_red "size" "0.2"

let previous_r = ref None
let () =
  for i = 0 to 9 do
    let r = u#new_vertex () in
    let _ = u#change_vertex_style r small_red in
    let _ = u#set_vertex_attribute r "label" (string_of_int i) in
    let e = u#new_edge x r in
    let _ = u#set_edge_attribute e "arrow" "true" in
    match !previous_r with
      | None ->
          previous_r := Some r
      | Some r' ->
          let e = u#new_edge r r' in
          let _ = u#set_edge_attribute e "spline" "true" in
          let _ = u#set_edge_attribute e "stroke" "dashed" in
          previous_r := Some r
  done
