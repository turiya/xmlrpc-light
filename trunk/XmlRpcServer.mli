class netplex :
  ?parallelizer:Netplex_types.parallelizer ->
  ?handler:string ->
  unit ->
object
  val mutable base64_decode : string -> string
  val mutable base64_encode : string -> string
  val mutable datetime_decode :
      string -> int * int * int * int * int * int * int
  val mutable datetime_encode :
      int * int * int * int * int * int * int -> string
  val methods : (string, XmlRpc.value list -> XmlRpc.value) Hashtbl.t
  method process : Netcgi_types.cgi_activation -> unit
  method register : string -> (XmlRpc.value list -> XmlRpc.value) -> unit
  method run : unit -> unit
  method set_base64_decode : (string -> string) -> unit
  method set_base64_encode : (string -> string) -> unit
  method set_datetime_decode :
    (string -> int * int * int * int * int * int * int) -> unit
  method set_datetime_encode :
    (int * int * int * int * int * int * int -> string) -> unit
  method unregister : string -> unit
end

val invalid_params : unit -> 'a
