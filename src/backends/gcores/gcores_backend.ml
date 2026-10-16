module R=Rss_backend
module D=R.Decode
module C=Reader.Content
module Html=Reader.Html
let source="gcores"
type group=string
type cursor=string
let field k=function `Assoc xs->Option.value(List.assoc_opt k xs)~default:`Null|_->`Null
let str k j=match field k j with `String s->s|`Int n->string_of_int n|_->""
let array k j=match field k j with `List xs->xs|_->[]
let numeric s=if s="" || not(String.for_all(function '0'..'9'->true|_->false)s)then invalid_arg "Expected numeric Gcores ID" else s
let user group=if String.starts_with ~prefix:"talks." group then numeric(String.sub group 6 (String.length group-6)) else invalid_arg "Gcores expects talks.USER-ID"
let identity item=
 let kind=str "type" item and id=numeric(str "id" item) in
 if not(List.mem kind ["talks";"articles";"radios";"videos"])then invalid_arg "Unsupported Gcores resource";
 kind^":"^id
let resource id=match String.split_on_char ':' id with
 |[kind;id] when List.mem kind ["talks";"articles"]->kind,numeric id|_->invalid_arg "Invalid Gcores article identity"
let image path=if path="" then "" else R.Http.validate(if String.starts_with ~prefix:"//" path then "https:"^path
 else if String.starts_with ~prefix:"http" path then path else "https://image.gcores.com/"^(if path.[0]='/' then String.sub path 1 (String.length path-1)else path))
let draft value=match value with
 |`String ""|`Null->`Assoc["blocks",`List[];"entityMap",`Assoc[]]
 |`String s->Yojson.Safe.from_string s|`Assoc _ as j->j|_->failwith "Invalid Gcores content"
let draft_html content=
 let j=draft content in
 let blocks=match field "blocks" j with `List xs when List.length xs<=10000->xs|_->failwith "Gcores content has no bounded Draft.js blocks" in
 let entity range=
  let e=field(str "key" range)(field "entityMap" j) in let data=field "data" e in
  let img path=let url=image path in if url="" then ""else "<img src=\""^Html.escape url^"\">" in
  match str "type" e with
  |"GALLERY"->String.concat ""(List.map(fun x->img(str "path" x))(array "images" data))^"<p>"^Html.escape(str "caption" data)^"</p>"
  |"IMAGE"->img(match str "src" data with ""->str "path" data|s->s)
  |_->let url=match str "url" data with ""->str "href" data|s->s in
    if url="" then ""else "<a href=\""^Html.escape url^"\">"^Html.escape url^"</a>" in
 String.concat ""(List.map(fun b->
  let kind=str "type" b and text=Html.escape(str "text" b) in
  let tag=match kind with "header-one"->"h1"|"header-two"->"h2"|"header-three"->"h3"|"blockquote"->"blockquote"|"code-block"->"pre"|_->"p" in
  (if text="" || kind="atomic" then ""else "<"^tag^">"^text^"</"^tag^">")^
  String.concat ""(List.map entity(array "entityRanges" b)))blocks)
let decode item included =
 let id=identity item and a=field "attributes" item in
 let relation=field "data"(field "user"(field "relationships" item))in
 let author=List.find_opt(fun x->str "id" x=str "id" relation && str "type" x=str "type" relation)included in
 let author=Option.fold ~none:"机核用户" ~some:(fun x->str "nickname"(field "attributes" x))author in
 let raw=field "content" a in let text=String.concat " "(List.map(str "text")(array "blocks"(draft raw)))in
 let title=match str "title" a with ""->(match String.split_on_char '\n' text with s::_ when s<>""->s|_->"机核动态")|s->s in
 let kind,rawid=resource id in
 {C.id;thread_id=None;parent_id=None;references=[];title;author;body="";html=None;
  url="https://www.gcores.com/"^kind^"/"^rawid;link_url=None;
  created=D.date(match str "published-at" a with ""->str "created-at" a|s->s);
  unavailable=false;score=0;my_vote=C.No_vote;
  activity=Some(Digest.to_hex(Digest.string(Yojson.Safe.to_string (`List(List.map(fun k->field k a)["content";"title";"updated-at";"cover";"thumb";"desc";"excerpt"])))))}
let full item included=
 let message=decode item included and a=field "attributes" item in
 let cover=match str "cover" a with ""->str "thumb" a|s->s in
 let cover=if cover=""then ""else "<img src=\""^Html.escape(image cover)^"\">" in
 let intro=match str "desc" a with ""->str "excerpt" a|s->s in
 let body=draft_html(field "content" a) in
 if body="" && cover=""then failwith "Gcores returned no article body";
 R.Article.of_html message (cover^"<p>"^Html.escape intro^"</p>"^body)
module Make(H:R.Http.S)=struct
 module Download=R.Make(H)
 type t={full:(string,D.article)Hashtbl.t; revisions:(string,string option)Hashtbl.t;lock:Eio.Mutex.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ()={full=Hashtbl.create 64;revisions=Hashtbl.create 64;lock=Eio.Mutex.create ()}
 let get url=
  let r=H.request{R.Http.url;headers=["Accept","application/vnd.api+json";"Referer","https://www.gcores.com/"];limit=16*1024*1024}in
  if r.status<>200 then failwith(Printf.sprintf "Gcores HTTP %d" r.status);
  Yojson.Safe.from_string r.body
 let page t group query after=
  let uid=user group in let scope=match query with None->`Null|Some s->`String s in
  let offset,previous=match after with None->0,""|Some c->match Yojson.Safe.from_string c with
   |`List[`String g;q;`Int n;`String previous]when g=group && q=scope && n>0 && n<=1000000->n,previous
   |_->invalid_arg "Gcores cursor belongs to another group or search" in
  let endpoint="https://www.gcores.com/gapi/v1/users/"^uid^"/talks" in
  let url=Uri.with_query(Uri.of_string endpoint)["page[limit]",["30"];"page[offset]",[string_of_int offset];"sort",["-created-at"];"include",["user"]]|>Uri.to_string in
  let j=get url in let data=match field "data" j with `List xs->xs|_->failwith "Gcores returned no item list" in
  let fingerprint=Digest.to_hex(Digest.string(String.concat ","(List.map identity data)))in
  if data<>[] && fingerprint=previous then failwith "Gcores pagination repeated a page";
  let messages=List.filter(fun x->List.mem(str "type" x)["talks";"articles"])data |> List.map(fun x->decode x (array "included" j)) in
  List.iter(fun(m:C.message)->Hashtbl.replace t.revisions m.id m.activity)messages;
  let contains text q=let text=String.lowercase_ascii text and q=String.lowercase_ascii q in
   let rec loop n=n+String.length q<=String.length text && (String.sub text n (String.length q)=q || loop(n+1))in loop 0 in
  let messages=match query with None->messages|Some q->List.filter(fun(m:C.message)->contains(m.title^" "^m.author)q)messages in
  let next=offset+List.length data in
  let more=match field "record-count"(field "meta" j)with `Int total->next<total|_->List.length data=30 in
  {C.messages;notifications=[];after=if data=[] || not more then None else Some(Yojson.Safe.to_string(`List[`String group;scope;`Int next;`String fingerprint]))}
 let list t group ~after=page t group None after
 let search t group ~query ~after=page t group (Some query) after
 let read t _group id=
  let kind,rawid=resource id in
  Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun()->Eio.Mutex.unlock t.lock)(fun()->
   match Hashtbl.find_opt t.full id,Hashtbl.find_opt t.revisions id with
   |Some a,Some revision when a.message.activity=revision->a
   |_->let j=get("https://www.gcores.com/gapi/v1/"^kind^"/"^rawid^"?include=user") in
     let a=full(field "data" j)(array "included" j)in
     if a.message.id<>id then failwith "Gcores returned the wrong article";
     Hashtbl.replace t.full id a;Hashtbl.replace t.revisions id a.message.activity;a)
 let invalidate t id=Hashtbl.remove t.revisions id
 let download ~limit url=Download.download_url ~limit url
 let archive t group id=R.Article.archive ~download(read t group id)
 let attachment t group id part path=R.Article.attachment ~download(read t group id)part path
end
module Live=Make(R.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
