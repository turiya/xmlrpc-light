(** XmlRpc Light server. *)

class base :
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

  method register : string -> (XmlRpc.value list -> XmlRpc.value) -> unit
  method unregister : string -> unit
end

class cgi :
  unit ->
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

  method register : string -> (XmlRpc.value list -> XmlRpc.value) -> unit
  method unregister : string -> unit

  method run : unit -> unit
end

class netplex :
  ?parallelizer:Netplex_types.parallelizer ->
  ?handler:string ->
  unit ->
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

  method register : string -> (XmlRpc.value list -> XmlRpc.value) -> unit
  method unregister : string -> unit

  method run : unit -> unit
end

val invalid_method : string -> 'a
val invalid_params : unit -> 'a
