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

exception Type_error of string
exception Unknown_field of string

let strict = ref false

let warn exn =
  if !strict
  then raise exn
  else prerr_endline (Printexc.to_string exn)

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

module Blog = struct
  type t = { mutable is_admin : bool;
             mutable url : string;
             mutable blog_id : int;
             mutable blog_name : string;
             mutable xmlrpc : string; }

  let make () =
    {is_admin=false;
     url="";
     blog_id=0;
     blog_name="";
     xmlrpc=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("isAdmin", `Boolean v) -> result.is_admin <- v
         | ("url", `String v) -> result.url <- v
         | ("blogid", `String v) -> result.blog_id <- int_of_string v
         | ("blogid", `Int v) -> result.blog_id <- v
         | ("blogName", `String v) -> result.blog_name <- v
         | ("xmlrpc", `String v) -> result.xmlrpc <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result
end

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
         | ("categoryId", `Int v) -> result.category_id <- v
         | ("parentId", `String v) -> result.parent_id <- int_of_string v
         | ("parentId", `Int v) -> result.parent_id <- v
         | ("description", `String v) -> result.description <- v
         | ("categoryName", `String v) -> result.category_name <- v
         | ("htmlUrl", `String v) -> result.html_url <- v
         | ("rssUrl", `String v) -> result.rss_url <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result
end

module CommentCount = struct
  type t = { mutable approved : int;
             mutable awaiting_moderation : int;
             mutable spam : int;
             mutable total_comments : int; }

  let make () =
    {approved=0;
     awaiting_moderation=0;
     spam=0;
     total_comments=0}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("approved", `String v) -> result.approved <- int_of_string v
         | ("approved", `Int v) -> result.approved <- v
         | ("awaiting_moderation", `Int v) -> result.awaiting_moderation <- v
         | ("spam", `Int v) -> result.spam <- v
         | ("total_comments", `Int v) -> result.total_comments <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result
end

module Comment = struct
  type t = { mutable date_created : XmlRpcDateTime.t;
             mutable user_id : int;
             mutable comment_id : int;
             mutable parent : int;
             mutable status : string;
             mutable content : string;
             mutable link : string;
             mutable post_id : int;
             mutable post_title : string;
             mutable author : string;
             mutable author_url : string;
             mutable author_email : string;
             mutable author_ip : string;
             mutable typ : string }

  let make () =
    {date_created=(0,0,0,0,0,0,0);
     user_id=0;
     comment_id=0;
     parent=0;
     status="";
     content="";
     link="";
     post_id=0;
     post_title="";
     author="";
     author_url="";
     author_email="";
     author_ip="";
     typ=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("date_created_gmt", `DateTime v) -> result.date_created <- v
         | ("user_id", `String v) -> result.user_id <- int_of_string v
         | ("user_id", `Int v) -> result.user_id <- v
         | ("comment_id", `String v) -> result.comment_id <- int_of_string v
         | ("comment_id", `Int v) -> result.comment_id <- v
         | ("parent", `String v) -> result.parent <- int_of_string v
         | ("parent", `Int v) -> result.parent <- v
         | ("status", `String v) -> result.status <- v
         | ("content", `String v) -> result.content <- v
         | ("link", `String v) -> result.link <- v
         | ("post_id", `String v) -> result.post_id <- int_of_string v
         | ("post_id", `Int v) -> result.post_id <- v
         | ("post_title", `String v) -> result.post_title <- v
         | ("author", `String v) -> result.author <- v
         | ("author_url", `String v) -> result.author_url <- v
         | ("author_email", `String v) -> result.author_email <- v
         | ("author_ip", `String v) -> result.author_ip <- v
         | ("type", `String v) -> result.typ <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result

  let to_xmlrpc comment =
    `Struct ["date_created_gmt", `DateTime comment.date_created;
             "user_id", `Int comment.user_id;
             "comment_id", `Int comment.comment_id;
             "parent", `Int comment.parent;
             "status", `String comment.status;
             "content", `String comment.content;
             "link", `String comment.link;
             "post_id", `Int comment.post_id;
             "post_title", `String comment.post_title;
             "author", `String comment.author;
             "author_url", `String comment.author_url;
             "author_email", `String comment.author_email;
             "author_ip", `String comment.author_ip;
             "type", `String comment.typ]
end

module CustomField = struct
  type t = { mutable id : int option;
             mutable key : string option;
             mutable value : string }

  let make () =
    {id=None;
     key=None;
     value=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("id", `String v) -> result.id <- Some (int_of_string v)
         | ("id", `Int v) -> result.id <- Some v
         | ("key", `String v) -> result.key <- Some v
         | ("value", `String v) -> result.value <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result

  let to_xmlrpc field =
    match field with
      | {id=None; key=None; value=value} ->
          `Struct ["value", `String value]
      | {id=Some id; key=None; value=value} ->
          `Struct ["id", `Int id; "value", `String value]
      | {id=None; key=Some key; value=value} ->
          `Struct ["key", `String key; "value", `String value]
      | {id=Some id; key=Some key; value=value} ->
          `Struct ["id", `Int id; "key", `String key; "value", `String value]
end

module Option = struct
  type t = { mutable desc : string;
             mutable readonly : bool;
             mutable value : string; }

  let make () =
    {desc="";
     readonly=false;
     value=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("desc", `String v) -> result.desc <- v
         | ("readonly", `Boolean v) -> result.readonly <- v
         | ("value", `String v) -> result.value <- v
         | (field, _) -> warn (Unknown_field field))
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
         | ("user_id", `Int v) -> result.user_id <- v
         | ("user_login", `String v) -> result.user_login <- v
         | ("display_name", `String v) -> result.display_name <- v
         | ("user_email", `String v) -> result.user_email <- v
         | ("meta_value", `String v) -> result.meta_value <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result
end

module PageListItem = struct
  type t = { mutable page_id : int;
             mutable page_title : string;
             mutable page_parent_id : int;
             mutable date_created : XmlRpcDateTime.t;
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
         | ("page_id", `Int v) -> result.page_id <- v
         | ("page_title", `String v) -> result.page_title <- v
         | ("page_parent_id", `String v) -> result.page_parent_id <- int_of_string v
         | ("page_parent_id", `Int v) -> result.page_parent_id <- v
         | ("dateCreated", `DateTime v) -> result.date_created <- v
         | ("date_created_gmt", `DateTime v) -> result.date_created <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result
end

module Page = struct
  type t = { mutable date_created : XmlRpcDateTime.t;
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
             mutable wp_page_template : string; }

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
     wp_author_display_name="";
     custom_fields=[];
     wp_page_template=""}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("dateCreated", `DateTime v) -> result.date_created <- v
         | ("date_created_gmt", `DateTime v) -> result.date_created <- v
         | ("userid", `String v) -> result.user_id <- int_of_string v
         | ("userid", `Int v) -> result.user_id <- v
         | ("page_id", `String v) -> result.page_id <- int_of_string v
         | ("page_id", `Int v) -> result.page_id <- v
         | ("page_status", `String v) -> result.page_status <- v
         | ("description", `String v) -> result.description <- v
         | ("title", `String v) -> result.title <- v
         | ("link", `String v) -> result.link <- v
         | ("permaLink", `String v) -> result.permalink <- v
         | ("categories", `Array v) ->
             result.categories <- List.map XmlRpc.dump v
         | ("excerpt", `String v) -> result.excerpt <- v
         | ("text_more", `String v) -> result.text_more <- v
         | ("mt_excerpt", `String v) -> result.excerpt <- v
         | ("mt_text_more", `String v) -> result.text_more <- v
         | ("mt_allow_comments", `Int v) -> result.mt_allow_comments <- v<>0
         | ("mt_allow_comments", `Boolean v) -> result.mt_allow_comments <- v
         | ("mt_allow_pings", `Int v) -> result.mt_allow_pings <- v<>0
         | ("mt_allow_pings", `Boolean v) -> result.mt_allow_pings <- v
         | ("wp_slug", `String v) -> result.wp_slug <- v
         | ("wp_password", `String v) -> result.wp_password <- v
         | ("wp_author", `String v) -> result.wp_author <- v
         | ("wp_author_display_name", `String v) ->
             result.wp_author_display_name <- v
         | ("wp_page_parent_id", `String v) ->
             result.wp_page_parent_id <- int_of_string v
         | ("wp_page_parent_id", `Int v) ->
             result.wp_page_parent_id <- v
         | ("wp_page_parent_title", `String v) ->
             result.wp_page_parent_title <- v
         | ("wp_page_order", `String v) ->
             result.wp_page_order <- int_of_string v
         | ("wp_page_order", `Int v) ->
             result.wp_page_order <- v
         | ("wp_author_id", `String v) ->
             result.wp_author_id <- int_of_string v
         | ("wp_author_id", `Int v) ->
             result.wp_author_id <- v
         | ("custom_fields", `Array v) ->
             result.custom_fields <- List.map CustomField.of_xmlrpc v
         | ("wp_page_template", `String v) ->
             result.wp_page_template <- v
         | (field, _) -> warn (Unknown_field field))
      value;
    result

  let to_xmlrpc page =
    `Struct ["userid", `Int page.user_id;
             "page_id", `Int page.page_id;
             "page_status", `String page.page_status;
             "wp_slug", `String page.wp_slug;
             "wp_password", `String page.wp_password;
             "wp_author", `String page.wp_author;
             "wp_author_display_name", `String page.wp_author_display_name;
             "wp_page_parent_id", `Int page.wp_page_parent_id;
             "wp_page_parent_title", `String page.wp_page_parent_title;
             "wp_page_order", `Int page.wp_page_order;
             "wp_author_id", `Int page.wp_author_id;
             "title", `String page.title;
             "description", `String page.description;
             "link", `String page.link;
             "permaLink", `String page.permalink;
             "mt_excerpt", `String page.excerpt;
             "mt_text_more", `String page.text_more;
             "mt_allow_comments", `Boolean page.mt_allow_comments;
             "mt_allow_pings", `Boolean page.mt_allow_pings;
             "dateCreated", `DateTime page.date_created;
             "date_created_gmt", `DateTime page.date_created;
             "categories", `Array (List.map
                                     (fun s -> `String s)
                                     page.categories);
             "custom_fields", `Array (List.map
                                        CustomField.to_xmlrpc
                                        page.custom_fields);
             "wp_page_template", `String page.wp_page_template]
end

module Post = struct
  type t = { mutable user_id : int;
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
             mutable custom_fields : CustomField.t list; }

  let make () =
    {date_created=(0,0,0,0,0,0,0);
     user_id=0;
     post_id=0;
     post_status="";
     description="";
     title="";
     link="";
     permalink="";
     categories=[];
     excerpt="";
     text_more="";
     mt_allow_comments=false;
     mt_allow_pings=false;
     mt_keywords="";
     wp_slug="";
     wp_password="";
     wp_author_id=0;
     wp_author_display_name="";
     custom_fields=[]}

  let of_xmlrpc value =
    let result = make () in
    iter_struct
      (function
         | ("dateCreated", `DateTime v) -> result.date_created <- v
         | ("date_created_gmt", `DateTime v) -> result.date_created <- v
         | ("userid", `String v) -> result.user_id <- int_of_string v
         | ("userid", `Int v) -> result.user_id <- v
         | ("postid", `String v) -> result.post_id <- int_of_string v
         | ("postid", `Int v) -> result.post_id <- v
         | ("post_status", `String v) -> result.post_status <- v
         | ("description", `String v) -> result.description <- v
         | ("title", `String v) -> result.title <- v
         | ("link", `String v) -> result.link <- v
         | ("permaLink", `String v) -> result.permalink <- v
         | ("categories", `Array v) ->
             result.categories <- List.map XmlRpc.dump v
         | ("mt_excerpt", `String v) -> result.excerpt <- v
         | ("mt_text_more", `String v) -> result.text_more <- v
         | ("mt_allow_comments", `Int v) -> result.mt_allow_comments <- v<>0
         | ("mt_allow_comments", `Boolean v) -> result.mt_allow_comments <- v
         | ("mt_allow_pings", `Int v) -> result.mt_allow_pings <- v<>0
         | ("mt_allow_pings", `Boolean v) -> result.mt_allow_pings <- v
         | ("mt_keywords", `String v) -> result.mt_keywords <- v
         | ("wp_slug", `String v) -> result.wp_slug <- v
         | ("wp_password", `String v) -> result.wp_password <- v
         | ("wp_author_id", `String v) -> result.wp_author_id <- int_of_string v
         | ("wp_author_id", `Int v) -> result.wp_author_id <- v
         | ("wp_author_display_name", `String v) ->
             result.wp_author_display_name <- v
         | ("custom_fields", `Array v) ->
             result.custom_fields <- List.map CustomField.of_xmlrpc v
         | (field, _) -> warn (Unknown_field field))
      value;
    result

  let to_xmlrpc post =
    `Struct ["dateCreated", `DateTime post.date_created;
             "date_created_gmt", `DateTime post.date_created;
             "userid", `Int post.user_id;
             "postid", `Int post.post_id;
             "post_status", `String post.post_status;
             "description", `String post.description;
             "title", `String post.title;
             "link", `String post.link;
             "permaLink", `String post.permalink;
             "categories", `Array (List.map
                                     (fun s -> `String s)
                                     post.categories);
             "mt_excerpt", `String post.excerpt;
             "mt_text_more", `String post.text_more;
             "mt_allow_comments", `Boolean post.mt_allow_comments;
             "mt_allow_pings", `Boolean post.mt_allow_pings;
             "mt_keywords", `String post.mt_keywords;
             "wp_slug", `String post.wp_slug;
             "wp_password", `String post.wp_password;
             "wp_author_id", `Int post.wp_author_id;
             "wp_author_display_name", `String post.wp_author_display_name;
             "custom_fields", `Array (List.map
                                        CustomField.to_xmlrpc
                                        post.custom_fields)];
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

  method get_page_status_list () =
    match rpc#call "wp.getPageStatusList" std_args with
      | `Struct pairs -> List.map (fun (k, v) -> (k, XmlRpc.dump v)) pairs
      | other -> raise (Type_error (XmlRpc.dump other))

  method get_page_templates () =
    match rpc#call "wp.getPageTemplates" std_args with
      | `Struct pairs -> List.map (fun (k, v) -> (k, XmlRpc.dump v)) pairs
      | other -> raise (Type_error (XmlRpc.dump other))

  method new_page content publish =
    int_value
      (rpc#call "wp.newPage"
         (std_args @ [Page.to_xmlrpc content; `Boolean publish]))

  method edit_page page_id content publish =
    ignore
      (rpc#call "wp.editPage" [`Int blog_id;
                               `Int page_id;
                               `String username;
                               `String password;
                               Page.to_xmlrpc content;
                               `Boolean publish])

  method delete_page page_id =
    ignore
      (rpc#call "wp.deletePage" (std_args @ [`Int page_id]))

  method get_post post_id =
    Post.of_xmlrpc (rpc#call "metaWeblog.getPost"
                      [`Int post_id;
                       `String username;
                       `String password])

  method get_recent_posts num_posts =
    map_array Post.of_xmlrpc (rpc#call "metaWeblog.getRecentPosts"
                                (std_args @ [`Int num_posts]))

  method get_post_status_list () =
    match rpc#call "wp.getPostStatusList" std_args with
      | `Struct pairs -> List.map (fun (k, v) -> (k, XmlRpc.dump v)) pairs
      | other -> raise (Type_error (XmlRpc.dump other))

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

  method delete_post post_id =
    ignore
      (rpc#call "metaWeblog.deletePost" [`Int blog_id;
                                         `Int post_id;
                                         `String username;
                                         `String password;
                                         `Boolean false])

  method get_authors () =
    map_array User.of_xmlrpc (rpc#call "wp.getAuthors" std_args)

  method get_blogs () =
    map_array Blog.of_xmlrpc (rpc#call "wp.getUsersBlogs" [`String username;
                                                           `String password])

  method get_comment_count post_id =
    CommentCount.of_xmlrpc
      (rpc#call "wp.getCommentCount" (std_args @ [`Int post_id]))

  method get_comment_status_list () =
    match rpc#call "wp.getCommentStatusList" std_args with
      | `Struct pairs -> List.map (fun (k, v) -> (k, XmlRpc.dump v)) pairs
      | other -> raise (Type_error (XmlRpc.dump other))

  method get_comment comment_id =
    Comment.of_xmlrpc
      (rpc#call "wp.getComment" (std_args @ [`Int comment_id]))

  method get_comments ?(status="") ?(post_id=0) ?(offset=0) ?(number=10) () =
    map_array Comment.of_xmlrpc
      (rpc#call "wp.getComments"
         (std_args @ [`Struct ["status", `String status;
                               "post_id", `Int post_id;
                               "offset", `Int offset;
                               "number", `Int number]]))

  method new_comment comment =
    int_value
      (rpc#call "wp.newComment"
         (std_args @ [`Int comment.Comment.post_id;
                      Comment.to_xmlrpc comment]))

  method edit_comment comment_id comment =
    ignore
      (rpc#call "wp.editComment"
         (std_args @ [`Int comment_id;
                      Comment.to_xmlrpc comment]))

  method delete_comment comment_id =
    ignore
      (rpc#call "wp.deleteComment" (std_args @ [`Int comment_id]))

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
    rpc#call "wp.suggestCategories"
      (std_args @ [`String category; `Int max_results])

  method get_options names =
    let result =
      rpc#call
        "wp.getOptions"
        (std_args @ [`Array (List.map (fun s -> `String s) names)]) in
    match result with
      | `Struct pairs ->
          List.map (fun (name, opt) -> (name, Option.of_xmlrpc opt)) pairs
      | `Array [] -> []
      | other -> raise (Type_error (XmlRpc.dump other))

  method set_options options =
    let result =
      rpc#call
        "wp.setOptions"
        (std_args @ [`Struct (List.map (fun (name, value) ->
                                          (name, `String value)) options)]) in
    match result with
      | `Struct pairs ->
          List.map (fun (name, opt) -> (name, Option.of_xmlrpc opt)) pairs
      | `Array [] -> []
      | other -> raise (Type_error (XmlRpc.dump other))

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
         | (field, _) -> warn (Unknown_field field))
      value;
    (!file, !url, !typ)
end
