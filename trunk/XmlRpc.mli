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

(** XmlRpc Light.

    XmlRpc Light is a minimal XmlRpc client based on Xml Light and Ocamlnet.

    It provides a type for values, a client class with a simple calling
    interface, and low-level tools that can be used to implement a server.

    {i (c) 2007 Dave Benjamin}
*)

(** {6 High-level interface} *)

(** Example: {[
    let rpc = new XmlRpc.client "http://localhost:8000" in
    let result = rpc#call "echo" [`String "hello!"] in
    print_endline (XmlRpc.dump result) ]}
*)

(** Raised for all errors including XmlRpc faults (code, string). *)
exception Error of (int * string)

(** Polymorphic variant type for XmlRpc values:
    - [`Array]: An ordered list of values
    - [`Binary]: A string containing binary data
    - [`Boolean]: A boolean
    - [`DateTime]: A date-time value
      (year, month, day, hour, minute, second, timezone offset in minutes)
    - [`Double]: A floating-point value
    - [`Int]: An integer
    - [`String]: A string
    - [`Struct]: An association list of (name, value) pairs

    Note that base64-encoding of [`Binary] values is done automatically.
    You do not need to do the encoding yourself.
 *)
type value =
    [ `Array of value list
    | `Binary of string
    | `Boolean of bool
    | `DateTime of int * int * int * int * int * int * int
    | `Double of float
    | `Int of int
    | `String of string
    | `Struct of (string * value) list ]

(** Class for XmlRpc clients. Takes a single argument, the Url. *)
class client :
  string ->
object
  (** Url of the remote XmlRpc server. *)
  val url : string

  (** User-agent to send in request headers. *)
  val mutable useragent : string

  (** If true, Xml messages will be printed to standard output. *)
  val mutable debug : bool

  (** Gets [url]. *)
  method url : string

  (** Gets [useragent]. *)
  method useragent : string

  (** Sets [useragent]. *)
  method set_useragent : string -> unit

  (** Gets [debug]. *)
  method debug : bool

  (** Sets [debug]. *)
  method set_debug : bool -> unit

  (** Sets an alternate Base-64 binary encoding function. *)
  method set_base64_encode : (string -> string) -> unit

  (** Sets an alternate Base-64 binary decoding function. *)
  method set_base64_decode : (string -> string) -> unit

  (** Sets an alternate ISO-8601 date/time encoding function. *)
  method set_datetime_encode : (int * int * int * int * int * int * int -> string) -> unit

  (** Sets an alternate ISO-8601 date/time decoding function. *)
  method set_datetime_decode : (string -> int * int * int * int * int * int * int) -> unit

  (** [call name params] invokes an XmlRpc method and returns the result,
      or raises {!XmlRpc.Error} on error. *)
  method call : string -> value list -> value
end

(** {6 Utility functions} *)

(** Converts an XmlRpc value to a human-readable string. *)
val dump : value -> string

(** Converts a date/time tuple to an ISO-8601 string. *)
val iso8601_of_datetime : int * int * int * int * int * int * int -> string

(** Converts an ISO-8601 string to a date/time tuple. *)
val datetime_of_iso8601 : string -> int * int * int * int * int * int * int

(** {6 Low-level interface} *)

(** Type for XmlRpc messages. *)
type message =
    | MethodCall of (string * value list)
    | MethodResponse of value
    | Fault of (int * string)

(** Converts an Xml Light element to an XmlRpc message. *)
val message_of_xml_element :
  ?base64_decode:(string -> string) ->
  ?datetime_decode:(string -> int * int * int * int * int * int * int) ->
  Xml.xml -> message

(** Converts an XmlRpc message to an Xml Light element. *)
val xml_element_of_message :
  ?base64_encode:(string -> string) ->
  ?datetime_encode:(int * int * int * int * int * int * int -> string) ->
  message -> Xml.xml

(** Converts an Xml Light element to an XmlRpc value. *)
val value_of_xml_element :
  ?base64_decode:(string -> string) ->
  ?datetime_decode:(string -> int * int * int * int * int * int * int) ->
  Xml.xml -> value

(** Converts an XmlRpc value to an Xml Light element. *)
val xml_element_of_value :
  ?base64_encode:(string -> string) ->
  ?datetime_encode:(int * int * int * int * int * int * int -> string) ->
  value -> Xml.xml

(** {6 Server tools} *)

(** Given a handler function, parses an XmlRpc method call and returns the
    result as a string. This function can be used to build many different
    kinds of XmlRpc servers since it makes no assumptions about the network
    library (or other communications method) used.

    If an exception other than {!XmlRpc.Error} occurs, the exception is
    passed to [error_handler]. If [error_handler] returns a message,
    the message will be used as the result. If an {!XmlRpc.Error} is
    raised by either the main function or [error_handler], it will be
    converted to an XmlRpc [Fault]. Any other exception raised by
    [error_handler] is allowed to escape.

    For a full-featured, network-capable server implementation, see the
    {!XmlRpcServer} module. *)
val serve :
  ?base64_encode:(string -> string) ->
  ?base64_decode:(string -> string) ->
  ?datetime_encode:(int * int * int * int * int * int * int -> string) ->
  ?datetime_decode:(string -> int * int * int * int * int * int * int) ->
  ?error_handler:(exn -> message) ->
  (string -> value list -> value) -> string -> string

(** The default error handler for [serve].

    This error handler catches all exceptions and converts them into
    faults by wrapping them in [XmlRpc.Error]. *)
val default_error_handler : exn -> message

(** A "quiet" error handler for [serve].

    This error handler simply re-raises the exception. Use this if you
    want exceptions to remain unhandled so that they will escape to the
    error log. The client will receive a generic "transport error",
    which is more secure since it does not reveal any information about
    the specific exception that occurred. *)
val quiet_error_handler : exn -> message
