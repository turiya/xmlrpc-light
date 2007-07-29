(** XmlRpc Light server. *)

(** Abstract base class for XmlRpc servers. *)
class virtual base :
object
  val methods : (string, XmlRpc.value list -> XmlRpc.value) Hashtbl.t

  val mutable base64_encode : string -> string
  val mutable base64_decode : string -> string

  val mutable datetime_encode :
      int * int * int * int * int * int * int -> string
  val mutable datetime_decode :
      string -> int * int * int * int * int * int * int

  method set_base64_encode : (string -> string) -> unit
  method set_base64_decode : (string -> string) -> unit

  method set_datetime_encode :
    (int * int * int * int * int * int * int -> string) -> unit
  method set_datetime_decode :
    (string -> int * int * int * int * int * int * int) -> unit

  method set_error_handler : (exn -> XmlRpc.message) -> unit

  method register : string -> (XmlRpc.value list -> XmlRpc.value) -> unit
  method unregister : string -> unit
  method virtual run : unit -> unit
end

(** Type of concrete XmlRpc server classes. *)
class type server =
object
  inherit base
  method run : unit -> unit
end

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
