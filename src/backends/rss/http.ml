exception Transport_error of string
type request = { url : string; headers : (string * string) list; limit : int }
type response = { status : int; headers : (string * string) list; body : string }
module type S = sig val request : request -> response end
let validate url =
  let u = Uri.of_string url in
  if not (List.mem (Uri.scheme u) [Some "https"; Some "http"]) ||
     List.mem (Uri.host u) [None; Some ""] || Uri.userinfo u <> None ||
     String.exists (fun c -> Char.code c < 32 || c = '\127') url then
    invalid_arg "Feed URL must use HTTP(S), with a host and no URL credentials";
  url
let resolve base path = Uri.resolve "" (Uri.of_string base) (Uri.of_string path) |> Uri.to_string
