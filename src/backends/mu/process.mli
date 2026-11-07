type response = { status : int; stdout : string }
type request = private {
  id : int; argv : string list;
  cancelled : bool Atomic.t; response : response option Atomic.t;
}
type t
type event = Start of request | Cancel of int
exception Timeout
val create : notify:(unit -> unit) -> unit -> t
val run : t -> now:(unit -> float) -> sleep:(float -> unit) ->
  ?timeout:float -> string list -> response
val drain : t -> event list
val complete : t -> int -> response -> unit
