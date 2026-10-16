type meth = Get | Post
type request = { meth : meth; url : string; headers : (string * string) list; body : string }
type response = { status : int; headers : (string * string) list; body : string }
exception Transport_error of string
module type S = sig val request : request -> response end
