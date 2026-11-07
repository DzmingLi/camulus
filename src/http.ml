(** Shared HTTP data and Eio-aware ocurl transport. Backends own authentication,
    URL policy and redirect decisions; this transport never retries or follows
    redirects, and sends only the headers explicitly supplied by the caller. *)
type meth = Get | Post | Delete | Patch
type request = { meth:meth; url:string; headers:(string * string) list; body:string; limit:int }
type response = { status:int; headers:(string * string) list; body:string }
exception Transport_error of string
module type S = sig val request : request -> response end
type url_policy = Http_and_https | Https_only | Https_or_loopback
let validate ?(policy=Http_and_https) url =
  let u=Uri.of_string url in
  let loopback=List.mem (Uri.host u) [Some "127.0.0.1";Some "localhost";Some "::1"] in
  let scheme=Uri.scheme u in
  let allowed=scheme=Some "https" || (scheme=Some "http" &&
    match policy with Http_and_https->true | Https_only->false | Https_or_loopback->loopback) in
  if not allowed || List.mem (Uri.host u) [None;Some ""] || Uri.userinfo u<>None ||
     String.exists(fun c->Char.code c<32 || c='\127') url then
    invalid_arg "HTTP URL violates the caller's scheme/host policy or contains URL credentials";
  url
let resolve base path = Uri.resolve "" (Uri.of_string base) (Uri.of_string path) |> Uri.to_string
let same_origin a b =
  let a=Uri.of_string a and b=Uri.of_string b in
  let port u=Option.value (Uri.port u) ~default:(if Uri.scheme u=Some "https" then 443 else 80) in
  Uri.scheme a=Uri.scheme b && Uri.host a=Uri.host b && port a=port b
let get ?(headers=[]) ?(limit=16*1024*1024) url = {meth=Get;url;headers;body="";limit}
(* Initialize before spawning the worker. Never clean up libcurl globally in
   Emacs, which can host other modules using it. Each transfer owns its handles. *)
let () = Curl.global_init Curl.CURLINIT_GLOBALALL
let request ?(policy=Http_and_https) ?(user_agent="camulus/0.1")
    ?(connect_timeout_ms=10000) ?(timeout_ms=30000) (r:request) =
  ignore(validate ~policy r.url);
  if r.limit<0 || connect_timeout_ms<=0 || timeout_ms<=0 then invalid_arg "Invalid HTTP transfer limits";
  List.iter(fun(k,v)->if k="" || String.exists(fun c->c='\r'||c='\n'||c='\000') (k^v) then
    invalid_arg "Invalid HTTP header")r.headers;
  if not(List.mem "asynchdns" (Curl.version_info()).features) then
    raise(Transport_error "libcurl must support asynchronous DNS for the Eio transport");
  let easy=Curl.init() and multi=Curl.Multi.create() in
  let attached=ref false in
  Fun.protect ~finally:(fun()->
    if !attached then Curl.Multi.remove multi easy;
    Curl.Multi.cleanup multi;Curl.cleanup easy) @@ fun()->
  let body=Buffer.create 4096 and headers=ref [] and header_size=ref 0 and failure=ref None in
  Curl.set_url easy r.url;Curl.set_nosignal easy true;
  Curl.set_encoding easy Curl.CURL_ENCODING_ANY;Curl.set_useragent easy user_agent;
  Curl.set_followlocation easy false;
  Curl.set_protocols easy (match policy with Https_only->[Curl.CURLPROTO_HTTPS]
    | Http_and_https | Https_or_loopback->[Curl.CURLPROTO_HTTP;Curl.CURLPROTO_HTTPS]);
  Curl.set_sslverifypeer easy true;Curl.set_sslverifyhost easy Curl.SSLVERIFYHOST_HOSTNAME;
  Option.iter(Curl.set_cainfo easy)(Sys.getenv_opt "NIX_SSL_CERT_FILE");
  Curl.set_connecttimeoutms easy connect_timeout_ms;Curl.set_timeoutms easy timeout_ms;
  Curl.set_httpheader easy(List.map(fun(k,v)->k^": "^v)r.headers);
  Curl.set_writefunction easy(fun chunk->
    if String.length chunk > r.limit - Buffer.length body then
      (failure:=Some "HTTP response exceeds the requested size limit";0)
    else(Buffer.add_string body chunk;String.length chunk));
  Curl.set_headerfunction easy(fun line->
    header_size:= !header_size+String.length line;
    if !header_size>65536 then(failure:=Some "HTTP response headers exceed 64 KiB";0)
    else(
      if String.starts_with ~prefix:"HTTP/" line then headers:=[]
      else (match String.index_opt line ':' with None->()|Some n->
        headers:=(String.sub line 0 n |> String.lowercase_ascii,
          String.sub line (n+1)(String.length line-n-1) |> String.trim)::!headers);
      String.length line));
  (match r.meth with Get->()|Post|Delete|Patch->
    Curl.set_postfields easy r.body;Curl.set_postfieldsize easy(String.length r.body);
    Curl.set_customrequest easy(match r.meth with Post->"POST"|Delete->"DELETE"|Patch->"PATCH"|Get->assert false));
  Curl.Multi.add multi easy;attached:=true;
  let rec poll()=
    ignore(Curl.Multi.perform multi);
    match Curl.Multi.remove_finished multi with
    |Some(_,code)->attached:=false;if code<>Curl.CURLE_OK then
      raise(Transport_error(Option.value !failure ~default:"HTTP transfer failed or timed out"))
    |None->Eio_unix.sleep 0.01;poll() in
  (try poll() with Curl.CurlException _|Curl.Multi.Error _|Curl.Multi.CError _->
    raise(Transport_error(Option.value !failure ~default:"HTTP transport failure")));
  {status=Curl.get_responsecode easy;headers= !headers;body=Buffer.contents body}
