module C=Reader.Content
module R=Rss_backend
module D=R.Decode
module Html=Reader.Html
module Sign=Sign
let source="zhihu"
type group=string
type cursor=string
let field k = function `Assoc pairs->Option.value(List.assoc_opt k pairs)~default:`Null|_->`Null
let str key j=match field key j with `String s->s|`Int n->string_of_int n|_->""
let number key j=match field key j with `Int n->float n|`Float n->n|_->0.
let numeric s=if s="" || not(String.for_all(function '0'..'9'->true|_->false)s) then invalid_arg "Expected numeric Zhihu ID" else s
let parts group=
 let strip p=String.sub group (String.length p)(String.length group-String.length p) in
 let kind,user_type,user=
  if String.starts_with ~prefix:"articles.people." group then "articles","people",strip "articles.people."
  else if String.starts_with ~prefix:"articles.org." group then "articles","org",strip "articles.org."
  else if String.starts_with ~prefix:"answers." group then "answers","people",strip "answers."
  else invalid_arg "Zhihu expects articles.people.USER, articles.org.USER or answers.USER" in
 if user="" || not(String.for_all(function 'a'..'z'|'A'..'Z'|'0'..'9'|'.'|'_'|'-'->true|_->false)user) then invalid_arg "Invalid Zhihu user";
 kind,user_type,user
let rec images=function
 | Html.Text _ as n->n
 | Html.Element(tag,attrs,xs)->
   let actual=List.find_map(fun k->List.assoc_opt k attrs)["data-actualsrc";"data-original"] in
   let attrs=match actual with None->attrs|Some src->("src",src)::List.remove_assoc "src" attrs in
   Html.Element(tag,attrs,List.map images xs)
let decode kind j =
 let raw=numeric(str "id" j) and author=field "author" j in
 let article=kind="articles" in
 let id=(if article then "article:" else "answer:")^raw in
 let question=field "question" j in
 let url=if article then "https://zhuanlan.zhihu.com/p/"^raw else
  "https://www.zhihu.com/question/"^numeric(str "id" question)^"/answer/"^raw in
 let title=if article then str "title" j else str "title" question in
 let html=Html.parse(str "content" j)|>images|>Html.render in
 let message={C.id;thread_id=None;parent_id=None;references=[];title;author=str "name" author;
 body="";html=None;url;link_url=None;created=number (if article then "created" else "created_time") j;
 unavailable=false;score=0;my_vote=C.No_vote;activity=Some(Digest.to_hex(Digest.string(title^html)))} in
 R.Article.of_html message html
module Make(H:R.Http.S)=struct
 module Download=R.Make(H)
 type t={cookies:string->(string*string)list;cache:(string,D.article)Hashtbl.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ~cookies ()={cookies;cache=Hashtbl.create 128}
 let request t ~referer url=
  let cookies=t.cookies url in
  let dc0=match List.assoc_opt "d_c0" cookies with Some s when s<>""->s|_->failwith "Log in to Zhihu in the configured Firefox profile (d_c0 missing)" in
  let headers=["User-Agent","Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36";"Cookie",Reader.Browser_cookies.header cookies;"Referer",referer;"x-api-version","3.0.91";
   "x-app-za","OS=Web";"x-requested-with","fetch";"x-zse-93",Sign.version;"x-zse-96",Sign.header ~url ~dc0] in
  let r=H.request {R.Http.url;headers;limit=16*1024*1024} in
  if r.status<>200 then failwith(Printf.sprintf "Zhihu HTTP %d; check browser login or access challenge" r.status);
  Yojson.Safe.from_string r.body
 let list t group ~after=
  let kind,user_type,user=parts group in
  let offset=match after with None->0|Some s->(match Yojson.Safe.from_string s with
   | `List[`String g;`Int n] when g=group && n>0 && n<=1000000->n|_->invalid_arg "Invalid Zhihu cursor") in
  let url=Uri.of_string("https://www.zhihu.com/api/v4/members/"^user^"/"^kind) |> fun u->
   Uri.add_query_params' u (if kind="articles" then
    ["include","data[*].comment_count,content,voteup_count,created,updated;data[*].author.vip_info";
     "offset",string_of_int offset;"limit","20";"sort_by","created"]
    else ["limit","20";"include","data[*].is_normal,content"] @
      (if offset=0 then [] else ["offset",string_of_int offset])) |> Uri.to_string in
  let j=request t ~referer:("https://www.zhihu.com/"^user_type^"/"^user^"/") url in
  let data=match field "data" j with `List xs->xs|_->failwith "Zhihu returned no data list" in
  let articles=List.map(decode kind)data in
  let more=match field "is_end" (field "paging" j) with `Bool b->not b|_->List.length data=20 in
  if more && data=[] then failwith "Zhihu returned empty non-final page";
  List.iter(fun (a:D.article)->Hashtbl.replace t.cache a.message.id a)articles;
  {C.messages=List.map(fun(a:D.article)->a.message)articles;notifications=[];
   after=if more then Some(Yojson.Safe.to_string(`List[`String group;`Int(offset+List.length data)])) else None}
 let search _ _ ~query:_ ~after:_=failwith "Zhihu author entries do not provide scoped remote search"
 let read t _group id=match Hashtbl.find_opt t.cache id with Some a->a|None->
  let kind,raw=match String.split_on_char ':' id with ["article";n]->"articles",numeric n|["answer";n]->"answers",numeric n|_->invalid_arg "Invalid Zhihu identity" in
  let j=request t ~referer:"https://www.zhihu.com/" ("https://www.zhihu.com/api/v4/"^kind^"/"^raw) in
  let a=decode kind j in if a.message.id<>id then failwith "Zhihu returned another article";
  Hashtbl.replace t.cache id a;a
 let invalidate t id=Hashtbl.remove t.cache id
 let download ~limit url=Download.download_url ~limit url
 let archive t group id=R.Article.archive ~download (read t group id)
 let attachment t group id part destination=R.Article.attachment ~download(read t group id)part destination
end
module Live=Make(R.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
