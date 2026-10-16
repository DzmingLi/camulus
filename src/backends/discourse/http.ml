type meth = Get | Post | Delete
type request = { meth:meth; url:string; headers:(string * string) list; body:string; limit:int }
type response = { status:int; headers:(string * string) list; body:string }
exception Transport_error of string
module type S = sig val request : request -> response end
let validate url =
  let u=Uri.of_string url in
  let loopback=List.mem (Uri.host u) [Some "127.0.0.1";Some "localhost";Some "::1"] in
  if (Uri.scheme u<>Some "https" && not(Uri.scheme u=Some "http" && loopback)) ||
     List.mem (Uri.host u) [None;Some ""] || Uri.userinfo u<>None ||
     String.exists(fun c -> Char.code c<32 || c='\127') url then
    invalid_arg "Discourse requires HTTPS without URL credentials (loopback HTTP is allowed for tests)";
  url
let resolve base path = Uri.resolve "" (Uri.of_string base) (Uri.of_string path) |> Uri.to_string
let same_origin a b =
  let a=Uri.of_string a and b=Uri.of_string b in
  let port u=Option.value (Uri.port u) ~default:(if Uri.scheme u=Some "https" then 443 else 80) in
  Uri.scheme a=Uri.scheme b && Uri.host a=Uri.host b && port a=port b
