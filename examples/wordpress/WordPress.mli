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

exception Type_error of string
exception Unknown_field of string

val strict : bool ref

module Blog :
sig
  type t = {
             mutable is_admin : bool;
             mutable url : string;
             mutable blog_id : int;
             mutable blog_name : string;
             mutable xmlrpc : string;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
end

module Category :
sig
  type t = {
             mutable category_id : int;
             mutable parent_id : int;
             mutable description : string;
             mutable category_name : string;
             mutable html_url : string;
             mutable rss_url : string;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
end

module CommentCount :
sig
  type t = {
             mutable approved : int;
             mutable awaiting_moderation : int;
             mutable spam : int;
             mutable total_comments : int;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
end

module CustomField :
sig
  type t = {
             mutable id : int option;
             mutable key : string option;
             mutable value : string;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
  val to_xmlrpc : t -> XmlRpc.value
end

module Option :
sig
  type t = {
             mutable desc : string;
             mutable readonly : bool;
             mutable value : string;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
end

module User :
sig
  type t = {
             mutable user_id : int;
             mutable user_login : string;
             mutable display_name : string;
             mutable user_email : string;
             mutable meta_value : string;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
end

module PageListItem :
sig
  type t = {
             mutable page_id : int;
             mutable page_title : string;
             mutable page_parent_id : int;
             mutable date_created : XmlRpcDateTime.t;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
end

module Page :
sig
  type t = {
             mutable date_created : XmlRpcDateTime.t;
             mutable user_id : int;
             mutable page_id : int;
             mutable page_status : string;
             mutable description : string;
             mutable title : string;
             mutable link : string;
             mutable permalink : string;
             mutable categories : string list;
             mutable excerpt : string;
             mutable text_more : string;
             mutable mt_allow_comments : bool;
             mutable mt_allow_pings : bool;
             mutable wp_slug : string;
             mutable wp_password : string;
             mutable wp_author : string;
             mutable wp_page_parent_id : int;
             mutable wp_page_parent_title : string;
             mutable wp_page_order : int;
             mutable wp_author_id : int;
             mutable wp_author_display_name : string;
             mutable custom_fields : CustomField.t list;
             mutable wp_page_template : string;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
  val to_xmlrpc : t -> XmlRpc.value
end

module Post :
sig
  type t = {
             mutable user_id : int;
             mutable post_id : int;
             mutable post_status : string;
             mutable date_created : XmlRpcDateTime.t;
             mutable description : string;
             mutable title : string;
             mutable link : string;
             mutable permalink : string;
             mutable categories : string list;
             mutable excerpt : string;
             mutable text_more : string;
             mutable mt_allow_comments : bool;
             mutable mt_allow_pings : bool;
             mutable mt_keywords : string;
             mutable wp_slug : string;
             mutable wp_password : string;
             mutable wp_author_id : int;
             mutable wp_author_display_name : string;
             mutable custom_fields : CustomField.t list;
           }
  val make : unit -> t
  val of_xmlrpc : XmlRpc.value -> t
  val to_xmlrpc : t -> XmlRpc.value
end

class api :
  url:string ->
  blog_id:int ->
  username:string ->
  password:string ->
object
  val blog_id : int
  val password : string
  val rpc : XmlRpc.client
  val std_args : XmlRpc.value list
  val username : string
  method rpc : XmlRpc.client
  method delete_page : int -> unit
  method delete_post : int -> unit
  method edit_page : int -> Page.t -> bool -> unit
  method edit_post : int -> Post.t -> bool -> unit
  method get_authors : unit -> User.t list
  method get_blogs : unit -> Blog.t list
  method get_categories : unit -> Category.t list
  method get_comment_count : int -> CommentCount.t
  method get_options : string list -> (string * Option.t) list
  method get_page : int -> Page.t
  method get_page_list : unit -> PageListItem.t list
  method get_page_status_list : unit -> (string * string) list
  method get_page_templates : unit -> (string * string) list
  method get_pages : unit -> Page.t list
  method get_post : int -> Post.t
  method get_post_status_list : unit -> (string * string) list
  method get_recent_posts : int -> Post.t list
  method new_category :
    name:string ->
    slug:string -> parent_id:int -> description:string -> int
  method new_page : Page.t -> bool -> int
  method new_post : Post.t -> bool -> int
  method set_options : (string * string) list -> (string * Option.t) list
  method suggest_categories : string -> int -> XmlRpc.value
  method upload_file :
    name:string ->
    typ:string -> bits:string -> overwrite:bool -> string * string * string
end
