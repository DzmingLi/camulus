type response = { status : int; stdout : string }
type request = private {
  id : int; argv : string list; input_path : string; limit : int;
  cancelled : bool Atomic.t; response : response option Atomic.t;
}
type t
type event = Start of request | Cancel of int
exception Failed of int
exception Timeout
val create : notify:(unit -> unit) -> unit -> t
val run : t -> now:(unit -> float) -> sleep:(float -> unit) ->
  ?timeout:float -> ?limit:int -> ?input:string -> string list -> response
val drain : t -> event list
val complete : t -> int -> response -> unit
val cancel_all : t -> event list
