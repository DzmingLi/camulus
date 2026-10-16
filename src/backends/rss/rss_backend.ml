module Article = Article
module Http = Http
module Curl_transport = Curl_transport
module Decode = Decode
module C = Reader.Content
let source = "rss"
let source_of_url url = "rss:" ^ Http.validate url
let url_of_source source =
  if not(String.starts_with ~prefix:"rss:" source) then invalid_arg "Not an RSS source";
  String.sub source 4 (String.length source-4) |> Http.validate
let is_source = String.starts_with ~prefix:"rss:"
type group = string
type cursor = string
type article = Decode.article
let article_json a = `Assoc["kind",`String "article";"message",Reader.Content_state.message_json a.Decode.message;"attachments",Decode.attachments a]
type operation = List of group | Search of group * string | Read of group * string | Attachment of group * string * int * string
let operation_of_json j =
  let open Yojson.Safe.Util in
  let str k=j |> member k |> to_string in
  let url=Http.validate(str "url") in
  match str "op" with
  | "list" -> List url | "search" -> Search(url,str "query") | "read" -> Read(url,str "id")
  | "attachment" -> let part=j |> member "part" |> to_int in
    if part<0 then invalid_arg "Invalid attachment part";
    Attachment(url,str "id",part,str "destination")
  | _ -> invalid_arg "Unknown RSS operation"
let operation_url = function List u | Search(u,_) | Read(u,_) | Attachment(u,_,_,_) -> u
module Make (H:Http.S) = struct
  type cached = { endpoint:string; etag:string option; modified:string option; articles:article list }
  type t = { cache:(string,cached) Hashtbl.t; lock:Eio.Mutex.t; subscriptions_file:string option; decode:url:string -> string -> article list }
  type nonrec group=group
  type nonrec cursor=cursor
  let source=source
  let create ?subscriptions_file ?(decode=Decode.parse) () = {cache=Hashtbl.create 16;lock=Eio.Mutex.create ();subscriptions_file;decode}
  let locked t f = Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.lock) f
  module Subscription = struct
    type nonrec t = t
    type target = string
    type settings = bool
    let access t f =
      let path=match t.subscriptions_file with Some p when not(Filename.is_relative p) -> p
        | _ -> failwith "Configure an absolute RSS subscriptions file" in
      Eio_unix.run_in_systhread(fun () ->
        let rec mkdir p = if not(Sys.file_exists p) then
          (mkdir(Filename.dirname p);try Unix.mkdir p 0o700 with Unix.Unix_error(Unix.EEXIST,_,_)->()) in
        mkdir(Filename.dirname path);
        let fd=Unix.openfile (path^".lock") [Unix.O_CREAT;Unix.O_RDWR;Unix.O_CLOEXEC] 0o600 in
        Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
          Unix.lockf fd Unix.F_LOCK 0;
          let urls=if not(Sys.file_exists path) then [] else
            match Yojson.Safe.from_file path with
            | `List xs -> List.map(function `String u -> Http.validate u | _ -> failwith "Invalid RSS subscriptions file") xs
            | _ -> failwith "Invalid RSS subscriptions file" in
          f path urls))
    let followed t = locked t (fun () -> access t (fun _ urls -> urls))
    let get t url = let url=Http.validate url in
      locked t (fun () -> access t (fun _ urls -> List.mem url urls))
    let set t url subscribed = let url=Http.validate url in
      locked t (fun () -> access t (fun path urls ->
        let urls=List.filter((<>)url) urls in
        let urls=List.sort_uniq String.compare (if subscribed then url::urls else urls) in
        let temp,ch=Filename.open_temp_file ~temp_dir:(Filename.dirname path) ".rss-subscriptions-" ".json" in
        Fun.protect ~finally:(fun () -> close_out_noerr ch;try Sys.remove temp with Sys_error _ -> ()) (fun () ->
          Yojson.Safe.to_channel ch (`List(List.map(fun u -> `String u) urls));
          output_char ch '\n';flush ch;Unix.fsync(Unix.descr_of_out_channel ch);close_out ch;
          Unix.rename temp path)))
  end
  let followed = Subscription.followed
  let header k r=List.assoc_opt k r.Http.headers
  let fetch ?cached ?(limit=16*1024*1024) url =
    let rec go visited url =
      ignore(Http.validate url);
      if List.length visited>=6 || List.mem url visited then failwith "Feed redirect loop or limit exceeded";
      let headers=match cached with
        | Some c when c.endpoint=url -> List.filter_map Fun.id [Option.map(fun v -> "If-None-Match",v)c.etag;Option.map(fun v -> "If-Modified-Since",v)c.modified]
        | _ -> [] in
      let r=H.request {url;headers;limit} in
      if List.mem r.status [301;302;303;307;308] then
        match header "location" r with None -> failwith "Redirect has no Location"
        | Some next -> go (url::visited) (Http.resolve url next)
      else url,r in
    go [] url
  let download_url ?(limit=128*1024*1024) url =
    let _,r=fetch ~limit url in
    if r.status<>200 then failwith(Printf.sprintf "Publication HTTP %d" r.status);
    r.body
  let load t url =
    let old=Hashtbl.find_opt t.cache url in
    let endpoint,r=fetch ?cached:old url in
    if r.status=304 then match old with
      | Some c when c.endpoint=endpoint -> c.articles
      | _ -> failwith "Unexpected 304 without cached feed"
    else if r.status<>200 then failwith(Printf.sprintf "Feed HTTP %d" r.status)
    else begin
      let articles=t.decode ~url:endpoint r.body in
      Hashtbl.replace t.cache url {endpoint;etag=header "etag" r;modified=header "last-modified" r;articles};
      articles
    end
  let fetch_articles t url = locked t (fun () ->
    let articles=load t url in (Hashtbl.find t.cache url).endpoint,articles)
  let page articles = {C.messages=List.map(fun a -> a.Decode.message) articles;notifications=[];after=None}
  let list t url ~after =
    if after<>None then invalid_arg "RSS has no pagination cursor";
    locked t (fun () -> page(load t url))
  let search t url ~query ~after =
    if after<>None then invalid_arg "RSS has no pagination cursor";
    let contains haystack needle =
      let h=String.lowercase_ascii haystack and n=String.lowercase_ascii needle in
      let rec loop i = i+String.length n<=String.length h &&
        (String.sub h i (String.length n)=n || loop(i+1)) in loop 0 in
    locked t (fun () -> load t url |> List.filter(fun a -> let m=a.Decode.message in
      contains (m.title^" "^m.author^" "^m.body^" "^Option.value m.html ~default:"") query) |> page)
  let read t url id = locked t (fun () ->
    let articles=match Hashtbl.find_opt t.cache url with Some c -> c.articles | None -> load t url in
    match List.find_opt(fun a -> a.Decode.message.id=id) articles with
    | Some a -> a
    | None -> failwith "Article is no longer in this feed; open its saved copy if archived")
  let download ?(limit=128*1024*1024) enclosure =
    let _,r=fetch ~limit enclosure.Decode.url in
    if r.status<>200 then failwith(Printf.sprintf "Enclosure HTTP %d" r.status);
    r.body
  let archive t url id =
    let a=read t url id in
    let total=ref 0 in
    let files=List.map(fun e -> let data=download ~limit:(128*1024*1024 - !total) e in
      total:= !total+String.length data;
      if !total>128*1024*1024 then failwith "Enclosures exceed 128 MiB";
      e.Decode.part,data) a.enclosures in
    a,files
  let attachment t url id part destination =
    let a=read t url id in
    let enclosure=match List.find_opt(fun e -> e.Decode.part=part) a.enclosures with
      | Some e -> e | None -> invalid_arg "Unknown enclosure" in
    let data=download enclosure in
    Eio_unix.run_in_systhread(fun () ->
      let fd=Unix.openfile destination [Unix.O_CREAT;Unix.O_EXCL;Unix.O_WRONLY;Unix.O_CLOEXEC] 0o600 in
      let ch=Unix.out_channel_of_descr fd in
      try output_string ch data;flush ch;Unix.fsync fd;close_out ch
      with ex -> close_out_noerr ch;(try Unix.unlink destination with _ -> ());raise ex)
end
module Live = Make(Curl_transport)

module Subscription : Reader.Backend.Subscription with type t = Live.t and type target = string and type settings = bool = Live.Subscription

let followed = Live.Subscription.followed
