module Article = Article
module Http = Reader.Http
module Transport = struct let request r = Http.request ~policy:Http.Http_and_https ~user_agent:"camulus/0.1 (RSS/Atom)" r end
module Decode = Decode
module Feed_xml = Feed_xml
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
type operation = List of group | Read of group * string | Attachment of group * string * int * string
let operation_of_json j =
  let open Yojson.Safe.Util in
  let str k=j |> member k |> to_string in
  let url=Http.validate(str "url") in
  match str "op" with
  | "list" -> List url | "read" -> Read(url,str "id")
  | "attachment" -> let part=j |> member "part" |> to_int in
    if part<0 then invalid_arg "Invalid attachment part";
    Attachment(url,str "id",part,str "destination")
  | _ -> invalid_arg "Unknown RSS operation"
let operation_url = function List u | Read(u,_) | Attachment(u,_,_,_) -> u
module Make (H:Http.S) = struct
  type cached = { endpoint:string; etag:string option; modified:string option; articles:article list }
  type t = { cache:(string,cached) Hashtbl.t; lock:Eio.Mutex.t; decode:url:string -> string -> article list }
  type nonrec group=group
  type nonrec cursor=cursor
  let source=source
  let create ?(decode=Decode.parse) () = {cache=Hashtbl.create 16;lock=Eio.Mutex.create ();decode}
  let locked t f = Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.lock) f
  let header k r=List.assoc_opt k r.Http.headers
  let fetch ?cached url =
    let rec go visited url =
      ignore(Http.validate url);
      if List.length visited>=6 || List.mem url visited then failwith "Feed redirect loop or limit exceeded";
      let headers=match cached with
        | Some c when c.endpoint=url -> List.filter_map Fun.id [Option.map(fun v -> "If-None-Match",v)c.etag;Option.map(fun v -> "If-Modified-Since",v)c.modified]
        | _ -> [] in
      let r=H.request (Http.get ~headers url) in
      if List.mem r.status [301;302;303;307;308] then
        match header "location" r with None -> failwith "Redirect has no Location"
        | Some next -> go (url::visited) (Http.resolve url next)
      else url,r in
    go [] url
  let download_url url =
    let _,r=fetch url in
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
  let page articles = {C.messages=List.map(fun a -> a.Decode.message) articles;after=None}
  let list t url ~after =
    if after<>None then invalid_arg "RSS has no pagination cursor";
    locked t (fun () -> page(load t url))
  let read t url id = locked t (fun () ->
    let articles=match Hashtbl.find_opt t.cache url with Some c -> c.articles | None -> load t url in
    match List.find_opt(fun a -> a.Decode.message.id=id) articles with
    | Some a -> a
    | None -> failwith "Article is no longer in this feed; open its saved copy if archived")
  let download enclosure =
    let _,r=fetch enclosure.Decode.url in
    if r.status<>200 then failwith(Printf.sprintf "Enclosure HTTP %d" r.status);
    r.body
  let archive t url id =
    let a=read t url id in
    let files=List.map(fun e -> e.Decode.part,download e) a.enclosures in
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
module Live = Make(Transport)
