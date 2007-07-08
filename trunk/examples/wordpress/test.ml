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

let wp =
  new WordPress.api
    "http://localhost/wordpress/xmlrpc.php" 1 "admin" "secret"

let () =
  List.iter
    (fun {WordPress.Category.category_id=category_id;
          category_name=name;
          description=description} ->
       Printf.printf "Category #%d\n  Name: %s\n  Description: %s\n\n"
         category_id name description)
    (wp#get_categories ())
