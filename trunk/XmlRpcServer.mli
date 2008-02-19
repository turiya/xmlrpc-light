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

    Additionally, the methods [system.methodHelp] and [system.methodSignature]
    will be made available if at least one method help or method signature is
    provided.
*)

(** Type of parameters used in method signatures. *)
type param_type =
    [ `Array | `Binary | `Boolean | `DateTime | `Double
    | `Int | `String | `Struct | `Undefined ]

(** {2 Base classes} *)

(** Abstract base class for XmlRpc servers. *)
class virtual base :
object
  (** Hashtable mapping method names to implementation functions. *)
  val methods : (string, XmlRpc.value list -> XmlRpc.value) Hashtbl.t

  (** Base-64 binary encoding function. *)
  val mutable base64_encoder : string -> string

  (** Base-64 binary decoding function. *)
  val mutable base64_decoder : string -> string

  (** ISO-8601 date/time encoding function. *)
  val mutable datetime_encoder : XmlRpcDateTime.t -> string

  (** ISO-8601 date/time decoding function. *)
  val mutable datetime_decoder : string -> XmlRpcDateTime.t

  (** Handler for unhandled exceptions. *)
  val mutable error_handler : exn -> XmlRpc.message

  (** Sets an alternate Base-64 binary encoding function. *)
  method set_base64_encoder : (string -> string) -> unit

  (** Sets an alternate Base-64 binary decoding function. *)
  method set_base64_decoder : (string -> string) -> unit

  (** Sets an alternate ISO-8601 date/time encoding function. *)
  method set_datetime_encoder : (XmlRpcDateTime.t -> string) -> unit

  (** Sets an alternate ISO-8601 date/time decoding function. *)
  method set_datetime_decoder : (string -> XmlRpcDateTime.t) -> unit

  (** Sets an alternate handler for unhandled exceptions.
      See {!XmlRpc.default_error_handler} and
      {!XmlRpc.quiet_error_handler} for examples. *)
  method set_error_handler : (exn -> XmlRpc.message) -> unit

  (** For use in subclasses; calls {!XmlRpc.serve} with the current
      encoders, decoders, and error handler. *)
  method serve :
    (string -> XmlRpc.value list -> XmlRpc.value) -> string -> string

  (** Like [serve], but operates on messages instead of strings. *)
  method serve_message :
    (string -> XmlRpc.value list -> XmlRpc.value) ->
    XmlRpc.message -> XmlRpc.message

  (** Registers a method with the server.

      If a [help] string is specified, its contents will be returned for
      calls to [system.methodHelp] for this method.

      If [signature] is specified, this method's signature will be published
      by [system.methodSignature] and (shallow) type-checking will be enabled
      for parameters passed into this method.

      Multiple signatures can be supplied via [signatures] if desired to
      provide for overloaded methods.

      Signatures are of the form [return-type; param1-type; param2-type; ...]
      where each type is an instance of the {!param_type} variant. *)
  method register :
    string ->
    ?help:string ->
    ?signature:param_type list ->
    ?signatures:param_type list list ->
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

(** {2 Server implementations} *)

(** CGI XmlRpc server based on Netcgi2. *)
class cgi : unit -> server

(** Stand-alone XmlRpc server based on Netplex. *)
class netplex :
  ?parallelizer:Netplex_types.parallelizer ->
  ?handler:string ->
  unit -> server

(** {2 Utility functions} *)

(** Raise an {!XmlRpc.Error} indicating a method name not found. *)
val invalid_method : string -> 'a

(** Raise an {!XmlRpc.Error} indicating invalid method parameters. *)
val invalid_params : unit -> 'a
