module C=Reader.Content
module R=Rss_backend
module D=R.Decode
module Html=Reader.Html
let source="netease"
type group=string
type cursor=string
let field k=function `Assoc xs->Option.value(List.assoc_opt k xs)~default:`Null|_->`Null
let str k j=match field k j with `String s->s|`Int n->string_of_int n|_->""
let num k j=match field k j with `Int n->n|_->0
let array k j=match field k j with `List xs->xs|_->[]
let numeric s=if s="" || not(String.for_all(function '0'..'9'->true|_->false)s) then invalid_arg "Expected numeric NetEase ID" else s
let user=function group when String.starts_with ~prefix:"events." group ->numeric(String.sub group 7 (String.length group-7))|_->invalid_arg "NetEase expects events.USER-ID"
let https url=if String.starts_with ~prefix:"//" url then "https:"^url else
 if String.starts_with ~prefix:"http://" url then "https://"^String.sub url 7 (String.length url-7) else url
let decode event=
 let raw=numeric(str "id" event) and owner=field "user" event in
 let uid=numeric(str "userId" owner) in
 let payload=match field "json" event with `String s->Yojson.Safe.from_string s|`Assoc _ as j->j|_->`Assoc [] in
 let text=str "msg" payload and song=field "song" payload in
 let name=str "name" song and author=str "nickname" owner in
 let title=match str "resourceTitle" (field "commentThread" (field "info" event)) with
 | ""->if text<>"" then text else if name<>"" then name else author^" 的云村动态"|s->s in
 let song_html=if song=`Null then "" else
  let song_id=numeric(str "id" song) in
  let artists=array "artists" song |> List.map(str "name") |> String.concat " / " in
  "<blockquote><strong>"^Html.escape name^"</strong><p>"^Html.escape artists^
  "</p><a href=\"https://music.163.com/#/song?id="^song_id^"\">▶ 在网易云音乐播放</a></blockquote>" in
 let html="<p>"^(String.split_on_char '\n' text|>List.map Html.escape|>String.concat "<br>")^"</p>"^song_html^
  String.concat ""(List.filter_map(fun p->match str "originUrl" p with ""->None|url->Some("<img src=\""^Html.escape(https url)^"\">"))(array "pics" event)) in
 let message={C.id="event:"^raw;thread_id=None;parent_id=None;references=[];title;author;body="";html=None;
 url="https://music.163.com/#/event?id="^raw^"&uid="^uid;link_url=None;created=float(num "eventTime" event)/.1000.;
 unavailable=false;score=0;my_vote=C.No_vote;activity=Some(Digest.to_hex(Digest.string(title^html)))} in
 R.Article.of_html message html
module Make(H:R.Http.S)=struct
 module Download=R.Make(H)
 type t={cache:(string,D.article)Hashtbl.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ()={cache=Hashtbl.create 64}
 let list t group ~after=
  let uid=user group in
  let time=match after with None->None|Some s->(match Yojson.Safe.from_string s with
   | `List[`String g;`String time] when g=group->Some(numeric time)|_->invalid_arg "Invalid NetEase cursor") in
  let url="https://music.163.com/api/event/get/"^uid^(match time with None->""|Some s->"?lasttime="^s^"&limit=20") in
  let r=H.request{R.Http.url;headers=["Referer","https://music.163.com/"];limit=16*1024*1024} in
  if r.status<>200 then failwith(Printf.sprintf "NetEase HTTP %d" r.status);
  let j=Yojson.Safe.from_string r.body in
  let events=match num "code" j,field "events" j with 200,`List xs->xs|_->failwith "NetEase rejected event listing" in
  let articles=List.map decode events in
  let more=field "more" j=`Bool true in let next=str "lasttime" j in
  if more && (events=[] || next="" || Some next=time) then failwith "NetEase invalid continuation";
  List.iter(fun(a:D.article)->Hashtbl.replace t.cache a.message.id a)articles;
  {C.messages=List.map(fun(a:D.article)->a.message)articles;notifications=[];
   after=if more then Some(Yojson.Safe.to_string(`List[`String group;`String(numeric next)]))else None}
 let search _ _ ~query:_ ~after:_=failwith "NetEase user events do not offer scoped remote search"
 let read t group id=
  match Hashtbl.find_opt t.cache id with Some a->a|None->
  ignore(list t group ~after:None);
  match Hashtbl.find_opt t.cache id with Some a->a|None->failwith "This event is no longer in the current page; open its original URL or archived copy"
 let download ~limit url=Download.download_url ~limit url
 let archive t group id=R.Article.archive ~download(read t group id)
 let attachment t group id part destination=R.Article.attachment ~download(read t group id)part destination
end
module Live=Make(R.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
