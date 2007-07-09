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

exception Type_error of string
exception Unknown_field of string

type datetime = int * int * int * int * int * int * int

let map_array f = function
  | `Array items -> List.map f items
  | other -> raise (Type_error (XmlRpc.dump other))

let iter_struct f = function
  | `Struct pairs -> List.iter f pairs
  | other -> raise (Type_error (XmlRpc.dump other))

let int_value = function
  | `Int n -> n
  | `String s -> int_of_string s
  | other -> raise (Type_error (XmlRpc.dump other))

module Category = struct
  type t = { mutable category_id : int;
             mutable parent_id : int;
             mutable description : string;
             mutable category_name : string;
             mutable html_url : string;
             mutable rss_url : string; }

  let make () =
    {category_id=0;
     parent_id=0;
     description="";
     category_name="";
     html_url="";
     rss_url=""} 

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("categoryId", `String v) -> result.category_id <- int_of_string v
         | ("parentId", `String v) -> result.parent_id <- int_of_string v
         | ("description", `String v) -> result.description <- v
         | ("categoryName", `String v) -> result.category_name <- v
         | ("htmlUrl", `String v) -> result.html_url <- v
         | ("rssUrl", `String v) -> result.rss_url <- v
         | (field, _) -> raise (Unknown_field field))
      value;
    result
end

module CategorySearchResult = struct
  type t = { mutable category_id : int;
             mutable category_name : string; }

  let make () =
    {category_id=0; category_name=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("category_id", `String v) ->
             result.category_id <- int_of_string v
         | ("category_name", `String v) -> result.category_name <- v
         | (field, _) -> raise (Unknown_field field))
      value;
    result
end

module User = struct
  type t = { mutable user_id : int;
             mutable user_login : string;
             mutable display_name : string;
             mutable user_email : string;
             mutable meta_value : string; }

  let make () =
    {user_id=0;
     user_login="";
     display_name="";
     user_email="";
     meta_value=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("user_id", `String v) -> result.user_id <- int_of_string v
         | ("user_login", `String v) -> result.user_login <- v
         | ("display_name", `String v) -> result.display_name <- v
         | ("user_email", `String v) -> result.user_email <- v
         | ("meta_value", `String v) -> result.meta_value <- v
         | (field, _) -> raise (Unknown_field field))
      value;
    result
end

module PageListItem = struct
  type t = { mutable page_id : int;
             mutable page_title : string;
             mutable page_parent_id : int;
             mutable date_created : datetime;
           }

  let make () =
    {page_id=0;
     page_title="";
     page_parent_id=0;
     date_created=(0,0,0,0,0,0,0)}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("page_id", `String v) -> result.page_id <- int_of_string v
         | ("page_title", `String v) -> result.page_title <- v
         | ("page_parent_id", `String v) -> result.page_id <- int_of_string v
         | ("dateCreated", `DateTime v) -> result.date_created <- v
         | (field, _) -> raise (Unknown_field field))
      value;
    result
end

module Page = struct
  type t = { mutable date_created : datetime;
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
             mutable wp_author_display_name : string; }

  let make () =
    {date_created=(0,0,0,0,0,0,0);
     user_id=0;
     page_id=0;
     page_status="";
     description="";
     title="";
     link="";
     permalink="";
     categories=[];
     excerpt="";
     text_more="";
     mt_allow_comments=false;
     mt_allow_pings=false;
     wp_slug="";
     wp_password="";
     wp_author="";
     wp_page_parent_id=0;
     wp_page_parent_title="";
     wp_page_order=0;
     wp_author_id=0;
     wp_author_display_name=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("dateCreated", `DateTime v) -> result.date_created <- v
         | ("userid", `String v) -> result.user_id <- int_of_string v
         | ("page_id", `String v) -> result.page_id <- int_of_string v
         | ("page_status", `String v) -> result.page_status <- v
         | ("description", `String v) -> result.description <- v
         | ("title", `String v) -> result.title <- v
         | ("link", `String v) -> result.link <- v
         | ("permaLink", `String v) -> result.permalink <- v
         | ("categories", `Array v) ->
             result.categories <- List.map XmlRpc.dump v
         | ("excerpt", `String v) -> result.excerpt <- v
         | ("text_more", `String v) -> result.text_more <- v
         | ("mt_allow_comments", `Int v) -> result.mt_allow_comments <- v<>0
         | ("mt_allow_pings", `Int v) -> result.mt_allow_pings <- v<>0
         | ("wp_slug", `String v) -> result.wp_slug <- v
         | ("wp_password", `String v) -> result.wp_password <- v
         | ("wp_author", `String v) -> result.wp_author <- v
         | ("wp_page_parent_id", `Int v) ->
             result.wp_page_parent_id <- v
         | ("wp_page_parent_id", `String v) ->
             result.wp_page_parent_id <- int_of_string v
         | ("wp_page_parent_title", `String v) ->
             result.wp_page_parent_title <- v
         | ("wp_page_order", `String v) ->
             result.wp_page_order <-  int_of_string v
         | ("wp_author_id", `String v) ->
             result.wp_author_id <- int_of_string v
         | ("wp_author_display_name", `String v) ->
             result.wp_author_display_name <- v
         | (field, _) -> raise (Unknown_field field))
      value;
    result

  let to_xmlrpc page =
    `Struct ["wp_slug", `String page.wp_slug;
             "wp_password", `String page.wp_password;
             "wp_page_parent_id", `Int page.wp_page_parent_id;
             "wp_page_order", `Int page.wp_page_order;
             "wp_author_id", `Int page.wp_author_id;
             "title", `String page.title;
             "description", `String page.description;
             "mt_excerpt", `String page.excerpt;
             "mt_text_more", `String page.text_more;
             "mt_allow_comments", `Boolean page.mt_allow_comments;
             "mt_allow_pings", `Boolean page.mt_allow_pings;
             "dateCreated", `DateTime page.date_created;
             "categories", `Array (List.map
                                     (fun s -> `String s)
                                     page.categories)]
end

module Post = struct
  type t = { mutable user_id : int;
             mutable post_id : int;
             mutable date_created : datetime;
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
             mutable wp_author_id : int;
             mutable wp_author_display_name : string; }

  let make () =
    {date_created=(0,0,0,0,0,0,0);
     user_id=0;
     post_id=0;
     description="";
     title="";
     link="";
     permalink="";
     categories=[];
     excerpt="";
     text_more="";
     mt_allow_comments=false;
     mt_allow_pings=false;
     wp_slug="";
     wp_password="";
     wp_author_id=0;
     wp_author_display_name=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("dateCreated", `DateTime v) -> result.date_created <- v
         | ("userid", `String v) -> result.user_id <- int_of_string v
         | ("postid", `String v) -> result.post_id <- int_of_string v
         | ("description", `String v) -> result.description <- v
         | ("title", `String v) -> result.title <- v
         | ("link", `String v) -> result.link <- v
         | ("permaLink", `String v) -> result.permalink <- v
         | ("categories", `Array v) ->
             result.categories <- List.map XmlRpc.dump v
         | ("mt_excerpt", `String v) -> result.excerpt <- v
         | ("mt_text_more", `String v) -> result.text_more <- v
         | ("mt_allow_comments", `Int v) -> result.mt_allow_comments <- v<>0
         | ("mt_allow_pings", `Int v) -> result.mt_allow_pings <- v<>0
         | ("wp_slug", `String v) -> result.wp_slug <- v
         | ("wp_password", `String v) -> result.wp_password <- v
         | ("wp_author_id", `String v) -> result.wp_author_id <- int_of_string v
         | ("wp_author_display_name", `String v) -> result.wp_author_display_name <- v
         | (field, _) -> raise (Unknown_field field))
      value;
    result

  let to_xmlrpc post =
    `Struct ["dateCreated", `DateTime post.date_created;
             "description", `String post.description;
             "title", `String post.title;
             "categories", `Array (List.map
                                     (fun s -> `String s)
                                     post.categories);
             "mt_excerpt", `String post.excerpt;
             "mt_text_more", `String post.text_more;
             "mt_allow_comments", `Boolean post.mt_allow_comments;
             "mt_allow_pings", `Boolean post.mt_allow_pings;
             "wp_slug", `String post.wp_slug;
             "wp_password", `String post.wp_password;
             "wp_author_id", `Int post.wp_author_id]
end

class api ~url ~blog_id ~username ~password =
object (self)
  val rpc = new XmlRpc.client url
  val std_args = [`Int blog_id; `String username; `String password]
  val blog_id = blog_id
  val username = username
  val password = password

  method rpc = rpc

  method get_page page_id =
    Page.of_xmlrpc (rpc#call "wp.getPage"
                      [`Int blog_id;
                       `Int page_id;
                       `String username;
                       `String password])

  method get_pages () =
    map_array Page.of_xmlrpc (rpc#call "wp.getPages" std_args)

  method get_page_list () =
    map_array PageListItem.of_xmlrpc (rpc#call "wp.getPageList" std_args)

  method new_page content publish =
    int_value
      (rpc#call "wp.newPage"
         (std_args @ [Page.to_xmlrpc content; `Boolean publish]))

  method delete_page page_id =
    ignore
      (rpc#call "wp.deletePage" (std_args @ [`Int page_id]))

  method edit_page page_id content publish =
    ignore
      (rpc#call "wp.editPage" [`Int blog_id;
                               `Int page_id;
                               `String username;
                               `String password;
                               Page.to_xmlrpc content;
                               `Boolean publish])

  method get_post post_id =
    Post.of_xmlrpc (rpc#call "metaWeblog.getPost"
                      [`Int post_id;
                       `String username;
                       `String password])

  method get_recent_posts num_posts =
    map_array Post.of_xmlrpc (rpc#call "metaWeblog.getRecentPosts"
                                (std_args @ [`Int num_posts]))

  method new_post content publish =
    int_value
      (rpc#call "metaWeblog.newPost"
         (std_args @ [Post.to_xmlrpc content; `Boolean publish]))

  method edit_post post_id content publish =
    ignore
      (rpc#call "metaWeblog.editPost" [`Int post_id;
                                       `String username;
                                       `String password;
                                       Post.to_xmlrpc content;
                                       `Boolean publish])

  method get_authors () =
    map_array User.of_xmlrpc (rpc#call "wp.getAuthors" std_args)

  method get_categories () =
    map_array Category.of_xmlrpc (rpc#call "wp.getCategories" std_args)

  method new_category ~name ~slug ~parent_id ~description =
    int_value
      (rpc#call "wp.newCategory"
         (std_args @ [`Struct ["name", `String name;
                               "slug", `String slug;
                               "parent_id", `Int parent_id;
                               "description", `String description]]))

  method suggest_categories category max_results =
    map_array CategorySearchResult.of_xmlrpc
      (rpc#call "wp.suggestCategories"
         (std_args @ [`String category; `Int max_results]))

  method upload_file ~name ~typ ~bits ~overwrite =
    let value =
      rpc#call "wp.uploadFile"
        (std_args @ [`Struct ["name", `String name;
                              "type", `String typ;
                              "bits", `Binary bits;
                              "overwrite", `Boolean overwrite]]) in
    let file, url, typ = ref "", ref "", ref "" in
    iter_struct
      (function
         | ("file", `String v) -> file := v
         | ("url", `String v) -> url := v
         | ("type", `String v) -> typ := v
         | (field, _) -> raise (Unknown_field field))
      value;
    (!file, !url, !typ)
end
