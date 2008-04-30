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

(** XmlRpc Light.

    XmlRpc Light is a minimal XmlRpc library based on Xml Light and Ocamlnet.

    It provides a type for values, a client class with a simple calling
    interface, and low-level tools that can be used to implement a server.

    {i (c) 2007 Dave Benjamin}
*)

(** Version of XmlRpc-Light as a string. *)
val version : string

(** {2 High-level interface} *)

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
    - [`DateTime]: A date/time value
    - [`Double]: A floating-point value
    - [`Int]: An integer
    - [`Int32]: A 32-bit integer
    - [`Nil]: A null value
    - [`String]: A string
    - [`Struct]: An association list of (name, value) pairs

    Note that base64-encoding of [`Binary] values is done automatically.
    You do not need to do the encoding yourself.
*)
type value =
    [ `Array of value list
    | `Binary of string
    | `Boolean of bool
    | `DateTime of XmlRpcDateTime.t
    | `Double of float
    | `Int of int
    | `Int32 of int32
    | `Nil
    | `String of string
    | `Struct of (string * value) list ]

(** Class for XmlRpc clients. Takes a single mandatory argument, [url].

    If [url] is of the form "http://username:password@...", basic
    authentication will be used.

    If [url] starts with "https", Curl will be used instead of Ocamlnet.
    The "curl" command-line program must be in your path for this to work.
    You can use the [insecure_ssl] setting to allow connections to servers
    with self-signed certificates; by default this is false and certificates
    must be valid.

    [timeout] can be used to specify the maximum amount of time
    elapsed before a connection is cancelled. It defaults to 300.0 seconds.

    [headers] may contain an array of (name, value) pairs of additional
    headers to send with each request.

    The [useragent] setting provides a convenient way to change the
    User-Agent header, which defaults to "XmlRpc-Light/<version>".

    The [debug] setting, if true, will enable verbose debugging output to
    the standard error strem.
*)
class client :
  ?debug:bool ->
  ?headers:(string * string) list ->
  ?insecure_ssl:bool ->
  ?timeout:float ->
  ?useragent:string ->
  string ->
object
  (** Url of the remote XmlRpc server. *)
  val url : string

  (** If true, Xml messages will be printed to standard error. *)
  val mutable debug : bool

  (** List of custom HTTP headers to send with each request. *)
  val mutable headers : (string * string) list

  (** If true, SSL will be allowed even if the certificate is self-signed. *)
  val mutable insecure_ssl : bool

  (** Maximum time to wait for a request to complete, in seconds. *)
  val mutable timeout : float

  (** User-agent to send in request headers. *)
  val mutable useragent : string

  (** Gets [url]. *)
  method url : string

  (** Gets [debug]. *)
  method debug : bool

  (** Sets [debug]. *)
  method set_debug : bool -> unit

  (** Gets [headers]. *)
  method headers : (string * string) list

  (** Sets [headers]. *)
  method set_headers : (string * string) list -> unit

  (** Gets [insecure_ssl]. *)
  method insecure_ssl : bool

  (** Sets [insecure_ssl]. *)
  method set_insecure_ssl : bool -> unit

  (** Gets [timeout]. *)
  method timeout : float

  (** Sets [timeout]. *)
  method set_timeout : float -> unit

  (** Gets [useragent]. *)
  method useragent : string

  (** Sets [useragent]. *)
  method set_useragent : string -> unit

  (** Sets an alternate Base-64 binary encoding function. *)
  method set_base64_encoder : (string -> string) -> unit

  (** Sets an alternate Base-64 binary decoding function. *)
  method set_base64_decoder : (string -> string) -> unit

  (** Sets an alternate ISO-8601 date/time encoding function. *)
  method set_datetime_encoder : (XmlRpcDateTime.t -> string) -> unit

  (** Sets an alternate ISO-8601 date/time decoding function. *)
  method set_datetime_decoder : (string -> XmlRpcDateTime.t) -> unit

  (** [call name params] invokes an XmlRpc method and returns the result,
      or raises {!XmlRpc.Error} on error. *)
  method call : string -> value list -> value
end

(** Convenience class for [system.multicall] calls.

    Instances take an {!XmlRpc.client} as an argument: {[
        # let mc = new XmlRpc.multicall client;;
        val mc : XmlRpc.multicall = <obj>
    ]}
    The "call" method works like [client#call], but it returns a lazy
    value: {[
        # let a = mc#call "demo.addTwoNumbers" [`Int 3; `Int 4];;
        val a : XmlRpc.value Lazy.t = <lazy>
        # let b = mc#call "demo.addTwoNumbers" [`Int 42; `String "oh noes!"];;
        val b : XmlRpc.value Lazy.t = <lazy>
        # let c = mc#call "demo.addTwoNumbers" [`Double 3.0; `Double 4.0];;
        val c : XmlRpc.value Lazy.t = <lazy>
    ]}
    At this point, the call has not been executed yet: {[
        # mc#executed;;
        -- : bool = false
    ]}
    As soon as one of the return values is forced, the call is executed: {[
        # Lazy.force a;;
        -- : XmlRpc.value = `Int 7
        # mc#executed;;
        -- : bool = true
    ]}
    Once a call has been executed, this instance cannot be used to make any
    further calls; instead, a new [multicall] instance must be created: {[
        # mc#call "demo.addTwoNumbers" [`Int 2; `Int 2];;
        Exception: Failure "multicall#call: already executed".
    ]}
    If an XmlRpc fault occurred, the exception will be thrown when the lazy
    value is forced: {[
        # Lazy.force b;;
        Exception: XmlRpc.Error (-32602, "server error. invalid method parameters").
    ]}
    This will not prevent further methods from executing successfully: {[
        # Lazy.force c;;
        -- : XmlRpc.value = `Double 7.
    ]}
    It is possible for a [multicall] to be executed but not completed, for
    example if a transport error occurs. Aside from catching the exception,
    the [completed] property indicates if the call actually went through
    or not: {[
        # mc#completed;;
        -- : bool = true
    ]}
    It is not necessary to use lazy values. Instead, the call can be
    executed explicitly, and the results can be retrieved by number: {[
        # let mc = new XmlRpc.multicall client;;
        val mc : XmlRpc.multicall = <obj>
        # ignore (mc#call "demo.addTwoNumbers" [`Int 2; `Int 2]);;
        -- : unit = ()
        # ignore (mc#call "demo.addTwoNumbers" [`Int 3; `Int 3]);;
        -- : unit = ()
        # mc#result 1;;
        -- : XmlRpc.value = `Int 6
    ]}
*)
class multicall : client ->
object
  (** Adds a call to this [multicall] instance.
      If the call has already executed, the following exception will
      be raised:
      Failure "multicall#call: already executed". *)
  method call : string -> value list -> value Lazy.t

  (** Forces the call to execute immediately.
      If the call has already executed and completed successfully, the
      following exception will be raised:
      Failure "multicall#execute: already completed". *)
  method execute : unit -> unit

  (** Returns a [multicall] result, executing the call if necessary.
      The results are numbered starting with zero. *)
  method result : int -> value

  (** True if the call has executed, whether or not it succeeded. *)
  method executed : bool

  (** True of the call has executed and completed successfully. *)
  method completed : bool
end

(** {2 Utility functions} *)

(** Converts an XmlRpc value to a human-readable string. *)
val dump : value -> string

(** {2 Low-level interface} *)

(** Type for XmlRpc messages. *)
type message =
    | MethodCall of (string * value list)
    | MethodResponse of value
    | Fault of (int * string)

(** Converts an Xml Light element to an XmlRpc message. *)
val message_of_xml_element :
  ?base64_decoder:(string -> string) ->
  ?datetime_decoder:(string -> XmlRpcDateTime.t) ->
  Xml.xml -> message

(** Converts an XmlRpc message to an Xml Light element. *)
val xml_element_of_message :
  ?base64_encoder:(string -> string) ->
  ?datetime_encoder:(XmlRpcDateTime.t -> string) ->
  message -> Xml.xml

(** Converts an Xml Light element to an XmlRpc value. *)
val value_of_xml_element :
  ?base64_decoder:(string -> string) ->
  ?datetime_decoder:(string -> XmlRpcDateTime.t) ->
  Xml.xml -> value

(** Converts an XmlRpc value to an Xml Light element. *)
val xml_element_of_value :
  ?base64_encoder:(string -> string) ->
  ?datetime_encoder:(XmlRpcDateTime.t -> string) ->
  value -> Xml.xml

(** {2 Server tools} *)

(** Creates a function from string (Xml representing a [MethodCall]) to
    string (Xml representing a [MethodResult] or [Fault]) given a function
    of the form: ([name] -> [params] -> [result]), where [name] is the
    name of the method, [params] is a list of parameter values, and
    [result] is the result value.

    This function can be used to build many different kinds of XmlRpc
    servers since it makes no assumptions about the network library
    or other communications method used.

    If an exception other than {!XmlRpc.Error} occurs, the exception is
    passed to [error_handler]. If [error_handler] returns a message,
    the message will be used as the result. If an {!XmlRpc.Error} is
    raised by either the main function or [error_handler], it will be
    converted to an XmlRpc [Fault]. Any other exception raised by
    [error_handler] is allowed to escape.

    For a full-featured, easy-to-use, network-capable server implementation,
    see the {!XmlRpcServer} module. *)
val serve :
  ?base64_encoder:(string -> string) ->
  ?base64_decoder:(string -> string) ->
  ?datetime_encoder:(XmlRpcDateTime.t -> string) ->
  ?datetime_decoder:(string -> XmlRpcDateTime.t) ->
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
