(** Shared transport with explicit per-backend URL policy. No credential lookup,
    automatic redirects, or retries occur here. Cancellation releases handles. *)
type meth = Get | Post | Delete | Patch
type request = { meth:meth; url:string; headers:(string * string) list; body:string; limit:int }
type response = { status:int; headers:(string * string) list; body:string }
exception Transport_error of string
module type S = sig val request : request -> response end
type url_policy = Http_and_https | Https_only | Https_or_loopback
val validate : ?policy:url_policy -> string -> string
val resolve : string -> string -> string
val same_origin : string -> string -> bool
val get : ?headers:(string * string) list -> ?limit:int -> string -> request
val request : ?policy:url_policy -> ?user_agent:string -> ?connect_timeout_ms:int -> ?timeout_ms:int -> request -> response
