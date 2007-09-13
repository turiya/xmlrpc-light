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

(** XmlRpc Light server. *)

(** Example: {[
    let server = new XmlRpcServer.cgi () in
    server#register "demo.sayHello"
      (fun _ -> `String "Hello!");
    server#run () ]}

    By inheriting from {!XmlRpcServer.base}, all servers provide
    the following introspection functions by default: [system.listMethods],
    [system.getCapabilities]. To prevent their use, use [server#unregister].
*)

(** Type of parameters used in method signatures. *)
type param_type =
    [ `Array | `Binary | `Boolean | `DateTime
    | `Double | `Int | `String | `Struct ]

(** {6 Base classes} *)

(** Abstract base class for XmlRpc servers. *)
class virtual base :
object
  (** Hashtable mapping method names to implementation functions. *)
  val methods : (string, XmlRpc.value list -> XmlRpc.value) Hashtbl.t

  (** Sets an alternate Base-64 binary encoding function. *)
  method set_base64_encoder : (string -> string) -> unit

  (** Sets an alternate Base-64 binary decoding function. *)
  method set_base64_decoder : (string -> string) -> unit

  (** Sets an alternate ISO-8601 date/time encoding function. *)
  method set_datetime_encoder :
    (int * int * int * int * int * int * int -> string) -> unit

  (** Sets an alternate ISO-8601 date/time decoding function. *)
  method set_datetime_decoder :
    (string -> int * int * int * int * int * int * int) -> unit

  (** Sets an alternate handler for unhandled exceptions.
      See {!XmlRpc.default_error_handler} and
      {!XmlRpc.quiet_error_handler} for examples. *)
  method set_error_handler : (exn -> XmlRpc.message) -> unit

  (** Registers a method with the server. *)
  method register :
    string ->
    ?help:string ->
    ?signature:param_type list ->
    (XmlRpc.value list -> XmlRpc.value) -> unit

  (** Removes a method from the server. *)
  method unregister : string -> unit

  (** Starts the main server process. *)
  method virtual run : unit -> unit
end

(** Type of concrete XmlRpc server classes. *)
class type server =
object
  inherit base

  (** Starts the main server process. *)
  method run : unit -> unit
end

(** {6 Server implementations} *)

(** CGI XmlRpc server based on Netcgi2. *)
class cgi : unit -> server

(** Stand-alone XmlRpc server based on Netplex. *)
class netplex :
  ?parallelizer:Netplex_types.parallelizer ->
  ?handler:string ->
  unit -> server

(** {6 Utility functions} *)

(** Raise an {!XmlRpc.Error} indicating a method name not found. *)
val invalid_method : string -> 'a

(** Raise an {!XmlRpc.Error} indicating invalid method parameters. *)
val invalid_params : unit -> 'a