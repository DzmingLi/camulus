module C=Reader.Content
module R=Rss_backend
module D=R.Decode
module Html=Reader.Html
let source="douban"
type group=string
type cursor=string
let field k = function `Assoc pairs->Option.value(List.assoc_opt k pairs)~default:`Null|_->`Null
let str k j=match field k j with `String s->s|`Int n->string_of_int n|_->""
let num k j=match field k j with `Int n->n|`String s->Option.value(int_of_string_opt s)~default:0|_->0
let array k j=match field k j with `List xs->xs|_->[]
let numeric s=if s="" || not(String.for_all(function '0'..'9'->true|_->false)s) then invalid_arg "Expected numeric Douban ID" else s
let parts group=match String.split_on_char '.' group with
 | [kind;n] when List.mem kind ["timeline";"replies";"topic";"group-topic"] -> kind,numeric n
 | _->invalid_arg "Douban expects timeline.USER, replies.ACCOUNT, topic.ID or group-topic.ID"
let root_id id=List.hd(String.split_on_char '/' id)
let root_parts id=match String.split_on_char ':' (root_id id) with
 | [kind;n] when List.mem kind ["status";"topic";"group-topic"]->kind,numeric n
 | _->invalid_arg "Invalid Douban content ID"
let root_url id=let kind,n=root_parts id in match kind with
 | "status"->"https://m.douban.com/status/"^n^"/"
 | "topic"->"https://www.douban.com/topic/"^n^"/"
 | _->"https://www.douban.com/group/topic/"^n^"/"
let target url=
 let u=Uri.of_string(R.Http.validate url) in
 if Uri.scheme u<>Some "https" || Uri.port u<>None then None else
 let xs=String.split_on_char '/' (Uri.path u) |> List.filter((<>)"") in
 let found=match Uri.host u,xs with
 | Some "www.douban.com",["topic";n]->Some("topic:"^numeric n)
 | Some "www.douban.com",["group";"topic";n]->Some("group-topic:"^numeric n)
 | Some "www.douban.com",["people";_;"status";n] | Some "m.douban.com",["status";n]->Some("status:"^numeric n)
 | _->None in found
let date s=
 if String.length s=19 && s.[10]=' ' then D.date(String.sub s 0 10^"T"^String.sub s 11 8^"+08:00") else D.date s
let message ~id ~title ~author ~created ~html =
 let root=root_id id in
 {C.id;thread_id=Some root;parent_id=None;references=[];title;author;body="";html=Some html;
 url=root_url root;link_url=None;created;unavailable=false;score=0;my_vote=C.No_vote;
 activity=Some(Digest.to_hex(Digest.string(title^html)))}
let image j=let url=str "url" (field "large" j) in if url="" then "" else "<img src=\""^Html.escape url^"\">"
let text s=String.split_on_char '\n' s|>List.map Html.escape|>String.concat "<br>"
let rec status_html depth j=
 if depth>8 then "" else
 let card=field "card" j in
 "<p>"^text(str "text" j)^"</p>"^String.concat ""(List.map image(array "images" j))^
 (if card=`Null then "" else "<blockquote><a href=\""^Html.escape(str "url" card)^"\">"^Html.escape(str "title" card)^"</a><p>"^text(str "subtitle" card)^"</p>"^image(field "image" card)^"</blockquote>")^
 (let shared=match field "reshared_status" j with `Null->field "parent_status" j|v->v in
  if shared=`Null then "" else "<blockquote>"^status_html(depth+1)shared^"</blockquote>")
let status j=
 let id="status:"^numeric(str "id" j) in
 let html=status_html 0 j in
 R.Article.of_html (message ~id ~title:"" ~author:(str "name" (field "author" j)) ~created:(date(str "create_time" j)) ~html) html
(* Douban's rich paragraphs use CSS pre-wrap; SHR otherwise collapses their
   literal newlines. Preserve text-node breaks only inside paragraphs. *)
let paragraph_breaks node =
 Html.select Soup.is_text node |> List.iter(fun text_node->
  let ancestors=Soup.ancestors text_node |> Soup.to_list |> List.map Soup.name in
  if List.mem "p" ancestors && not(List.exists(fun tag->List.mem tag ["pre";"code"])ancestors)then(
   let text=Soup.R.leaf_text text_node in
   if String.contains text '\n' || String.contains text '\r' then(
    let lines=String.split_on_char '\r' text |> String.concat "" |> String.split_on_char '\n' in
    let fragment=Soup.create_soup()in
    List.iteri(fun i line->
     if i>0 then Soup.append_root fragment (Soup.create_element "br");
     Soup.append_root fragment (Soup.create_text line))lines;
    Soup.replace text_node fragment)));
 node

type reaction = {liked:bool;count:int option}
let json_reaction j =
 let liked=match field "liked" j with `Bool value->Some value|_->
  match field "reaction_type" j with `Int n->Some(n=1)|_->None in
 Option.map(fun liked->{liked;count=List.find_map(fun key->match field key j with `Int n when n>=0->Some n|_->None)["like_count";"vote_count";"reactions_count"]})liked
let topic_reaction id html =
 let _,number=root_parts id in
 Html.find(fun n->Html.attr "data-object_id" n=number && Html.attr "data-reaction_type" n<>"")(Html.parse html)
 |> Option.map(fun n->{liked=Html.attr "data-reaction_type" n="1";count=None})
let reaction_fields = function
 | None->[]
 | Some r->["liked",`Bool r.liked;"like-count",(match r.count with None->`Null|Some n->`Int n)]
let like_target id liked =
 let kind,n=root_parts id in
 match String.split_on_char '/' id with
 | [_] when kind="status" -> "https://www.douban.com/j/status/"^(if liked then "like" else "unlike"),["sid",n],true
 | [_] -> "https://m.douban.com/rexxar/api/v2/group/topic/"^n^"/react",["reaction_type",if liked then "1" else "0"],false
 | [_;comment] when String.starts_with ~prefix:"comment:" comment ->
   let cid=numeric(String.sub comment 8 (String.length comment-8)) in
   let path=if kind="status" then "status/comment/"^cid else "group/topic/"^n^"/comment/"^cid in
   "https://m.douban.com/rexxar/api/v2/"^path^"/react",["reaction_type",if liked then "1" else "0"],false
 | _->invalid_arg "Invalid Douban like target"
let topic id html=
 let tree=Html.parse html in
 let body=match Html.find(Html.has_class "topic-richtext")tree with Some n->n|None->failwith "Douban topic body missing; check login or changed page layout" in
 let pick classes=List.find_map(fun c->Html.find(Html.has_class c)tree)classes |> Option.map(fun n->String.trim(Html.text n)) |> Option.value ~default:"" in
 let title=pick["topic-title"] and author=pick["author-name";"from"] and created=date(pick["create-time"]) in
 let html=Html.render(paragraph_breaks body) in R.Article.of_html(message ~id ~title ~author ~created ~html)html
module Make(H:R.Http.S)=struct
 module Download=R.Make(H)
 type t={cookies:string->(string*string)list;post:R.Http.request->body:string->R.Http.response;cache:(string,D.article)Hashtbl.t;reactions:(string,reaction)Hashtbl.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ?(post=(fun _ ~body:_->failwith "Douban write transport is unavailable")) ~cookies ()={cookies;post;cache=Hashtbl.create 128;reactions=Hashtbl.create 128}
 let reaction t id=Hashtbl.find_opt t.reactions id
 let remember_reaction t id value=match value with Some r->Hashtbl.replace t.reactions id r|None->Hashtbl.remove t.reactions id
 let message_json t (m:C.message)=match Reader.Content_state.message_json m with
  | `Assoc fields->`Assoc(reaction_fields(reaction t m.id)@fields)|_->assert false
 let article_json t (a:D.article)=`Assoc["kind",`String "article";"message",message_json t a.message;"attachments",D.attachments a]
 let allowed url=
  let u=Uri.of_string(R.Http.validate url) in
  Uri.scheme u=Some "https" && Uri.port u=None &&
  (match Uri.host u with
   | Some "m.douban.com"->String.starts_with ~prefix:"/rexxar/api/v2/" (Uri.path u) || Option.is_some(target url)
   | Some "www.douban.com"->Option.is_some(target url) || List.mem (Uri.path u)["/reply_notify/";"/notification/reply_notify"]
   | _->false)
 let request t url=
  let rec fetch depth url=
   if depth>3 || not(allowed url) then failwith "Unsupported Douban destination or redirect";
   let cookies=t.cookies url in
   let r=H.request {R.Http.meth=Get;body="";url;headers=["User-Agent","Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0";"Cookie",Reader.Credentials.Firefox.header cookies;
    "Referer",url]} in
   if List.mem r.status [301;302;303;307;308] then
    (match List.assoc_opt "location" r.headers with None->failwith "Douban redirect missing target"|Some loc->fetch(depth+1)(R.Http.resolve url loc))
   else if r.status<>200 then failwith(Printf.sprintf "Douban HTTP %d; check Firefox login or access challenge" r.status)
   else r.body,url in fetch 0 url
 let json t url=
  let j=Yojson.Safe.from_string(fst(request t url)) in
  if (field "code" j<>`Null && num "code" j<>0) || field "localized_message" j<>`Null then failwith "Douban rejected request";j
 let remember t (a:D.article)=Hashtbl.replace t.cache a.message.id a;a
 let read_root t id=
  let id=root_id id in let kind,n=root_parts id in
  let a=if kind="status" then
   let j=json t ("https://m.douban.com/rexxar/api/v2/status/"^n) in
   let j=match field "status" j with `Null->j|v->v in
   remember_reaction t id (json_reaction j);status j
  else let html=fst(request t (root_url id)) in
   remember_reaction t id (topic_reaction id html);topic id html in
  if a.message.id<>id then failwith "Douban returned another discussion";remember t a
 let list t group ~after=
  let kind,n=parts group in
  let offset=match after with None->0|Some s->(match Yojson.Safe.from_string s with
   | `List[`String g;`Int v] when g=group && v>0 && v<=100000->v|_->invalid_arg "Invalid Douban cursor") in
  let articles,more=match kind with
  | "timeline" ->
   let j=json t (Printf.sprintf "https://m.douban.com/rexxar/api/v2/status/user_timeline/%s?start=%d&count=20" n offset) in
   let items=match field "items" j with `List xs->xs|_->failwith "Douban timeline data missing" in
   let articles=List.filter_map(fun wrapper->
    if field "deleted" wrapper=`Bool true then None else match field "status" wrapper with
    | `Null->None | s->
      let card=field "card" s in
      (match (if str "type" card="topic" then target(str "url" card) else None) with
      | Some id->Some(read_root t id) | None->let a=status s in remember_reaction t a.message.id (json_reaction s);Some(remember t a)))items in
   articles,(if field "total" j<>`Null then offset+List.length items<num "total" j else List.length items=20)
  | "replies" ->
   let cookies=t.cookies "https://www.douban.com/reply_notify/" in
   let account=List.assoc_opt "dbcl2" cookies |> Option.value ~default:"" |> String.trim in
   let account=String.split_on_char ':' account |> List.hd |> String.split_on_char '"' |> String.concat "" in
   if account<>n then failwith "Firefox is not logged into the configured Douban reply account";
   let html,_=request t (Printf.sprintf "https://www.douban.com/reply_notify/?start=%d" offset) in
   let tree=Html.parse html in
   let nodes=Html.select(Html.has_class "new-reply-item")tree in
   let empty_inbox=match Html.find(Html.has_class "notification-list")tree with
    | Some container->Html.select(fun n->Html.tag n="li")container=[]
    | None->false in
   if nodes=[] && not empty_inbox then failwith "Douban reply page unavailable or its layout changed";
   let articles=List.map(fun node->
    let content=Html.find(Html.has_class "content")node |> Option.value ~default:node in
    let anchor=Html.find(fun n->Html.tag n="a" && Html.attr "href" n<>"")content |> Option.get in
    let url=R.Http.resolve "https://www.douban.com/reply_notify/" (Html.attr "href" anchor) in
    let page,final_url=request t url in
    let id=match target final_url with Some id->id|None->
     let doc=Html.parse page in
     (match Html.select(fun n->Html.tag n="a" || Html.tag n="link" || Html.tag n="meta")doc
      |> List.find_map(fun n->let u=if Html.tag n="meta" then Html.attr "content" n else Html.attr "href" n in
         if String.starts_with ~prefix:"https://" u then target u else None) with
      |Some id->id|None->failwith "Unsupported Douban reply target") in
    let a=read_root t id in
    {a with message={a.message with activity=Some(Html.attr "id" node)}})nodes in
   articles,List.length nodes>=20
  | _->if after<>None then invalid_arg "Single Douban topic has no next page";
    [read_root t ((if kind="topic" then "topic:" else "group-topic:")^n)],false in
  let unique=Hashtbl.create 32 in
  let articles=List.filter(fun(a:D.article)->if Hashtbl.mem unique a.message.id then false else (Hashtbl.add unique a.message.id ();true))articles in
  {C.messages=List.map(fun(a:D.article)->a.message)articles;after=if more then Some(Yojson.Safe.to_string(`List[`String group;`Int(offset+20)]))else None}
 let search _ _ ~query:_ ~after:_=failwith "Douban entries do not provide scoped remote search"
 let stream_thread t id ~emit_article ~emit=
  let root=root_id id in let kind,n=root_parts root in
  let a=read_root t root in emit_article a;
  emit{C.thread=root;messages=[a.message];gaps=[];complete=false};
  let base="https://m.douban.com/rexxar/api/v2/"^(if kind="status" then "status/" else "group/topic/") in
  let seen=Hashtbl.create 128 and scheduled=Hashtbl.create 128 and requests=ref 0 in
  let rec page path field_name start fallback=
   incr requests;if !requests>1000 then failwith "Douban discussion page limit exceeded";
   let j=json t (Printf.sprintf "%s%s?start=%d&count=20&nested=1&order_by=time" base path start) in
   let items=match field field_name j with `List xs->xs|_->failwith "Douban comment page missing" in
   if field "total" j=`Null then failwith "Douban comment total missing";
   let count=match field "count" j with `Null->20|_->num "count" j in
   if count<=0 || (field "start" j<>`Null && num "start" j<>start) then failwith "Invalid Douban pagination";
   let more=start+count<num "total" j in
   if more && items=[] then failwith "Empty Douban non-final page";
   let pending=ref [] in
   let rec collect depth fallback partial c=
    if depth>128 then failwith "Douban comment nesting limit exceeded";
    let cid=numeric(str "id" c) in let id=root^"/comment:"^cid in
    if not(partial && Hashtbl.mem seen id) then begin
     let ref_comment=field "ref_comment" c in
     let parent=if ref_comment<>`Null then root^"/comment:"^numeric(str "id" ref_comment)
      else match str "parent_comment_id" c with ""->fallback |p->root^"/comment:"^numeric p in
     if id=parent then failwith "Douban comment refers to itself";
     let html="<p>"^text(str "text" c)^"</p>" in
     let m=message ~id ~title:a.message.title ~author:(str "name"(field "author" c)) ~created:(date(str "create_time" c)) ~html in
     let m={m with parent_id=Some parent;url=a.message.url^"#comment_"^cid;unavailable=partial || field "is_deleted" c=`Bool true} in
     let article=R.Article.of_html m html in
     if not partial then (Hashtbl.replace seen id ();remember_reaction t id (json_reaction c));
     ignore(remember t article);emit_article article;
     emit{C.thread=root;messages=[m];gaps=[];complete=false};
     if ref_comment<>`Null then collect(depth+1)fallback true ref_comment;
     if not partial then begin
      let replies=array "replies" c in List.iter(collect(depth+1)id false)replies;
      let next=if field "next_reply_start" c=`Null then List.length replies else num "next_reply_start" c in
      if next<num "total_replies" c && not(Hashtbl.mem scheduled cid) then begin
       Hashtbl.add scheduled cid ();pending:=(cid,next,id)::!pending end
     end
    end in
   List.iter(collect 0 fallback false)items;
   List.iter(fun(cid,next,id)->page("comment/"^cid^"/replies")"replies" next id)(List.rev !pending);
   if more then page path field_name(start+count)fallback
  in page(n^"/comments")"comments" 0 root;
  emit{C.thread=root;messages=[];gaps=[];complete=true}
 let thread t id ~emit=stream_thread t id ~emit_article:(fun _->())~emit
 let read t _group id=match Hashtbl.find_opt t.cache id with Some a->a|None->
  if id=root_id id then read_root t id else begin stream_thread t id ~emit_article:(fun _->())~emit:(fun _->());
   match Hashtbl.find_opt t.cache id with Some a->a|None->failwith "Douban comment not found" end
 let set_like t ~id ~liked =
  let url,fields,is_status=like_target id liked in
  let cookies=t.cookies url in
  if not(List.mem_assoc "dbcl2" cookies) then failwith "Log in to Douban in the configured Firefox profile first";
  let token value =
   let value=String.trim value in
   let value=if String.length value>=2 && value.[0]='"' && value.[String.length value-1]='"' then String.sub value 1 (String.length value-2) else value in
   if value="" || String.length value>256 || String.exists(fun c->Char.code c<=32 || Char.code c>=127 || c=';' || c='"')value then None else Some value in
  let ck=match Option.bind(List.assoc_opt "ck" cookies)token with Some ck->ck|None->
   (* Firefox often keeps ck only in memory. Fetch a fresh CSRF token using
      the same browser account; do not create a persistent Camulus session. *)
   let bootstrap="https://www.douban.com/mine/" in
   let r=H.request {R.Http.meth=Get;body="";url=bootstrap;headers=["Cookie",Reader.Credentials.Firefox.header(t.cookies bootstrap);"Referer","https://www.douban.com/";
    "User-Agent","Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0"]} in
   if r.status<>200 then failwith "Douban CSRF bootstrap failed; check Firefox login";
   let ck=List.find_map(fun(k,v)->
    if String.lowercase_ascii k<>"set-cookie" then None else
    let first=List.hd(String.split_on_char ';' v) |> String.trim in
    if String.starts_with ~prefix:"ck=" first then token(String.sub first 3 (String.length first-3)) else None)r.headers in
   (match ck with Some ck->ck|None->failwith "Douban did not return a CSRF token; log in again in Firefox") in
  let headers=["Cookie",Reader.Credentials.Firefox.header(("ck",ck)::List.remove_assoc "ck" cookies);
   "User-Agent","Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0";
   "Referer","https://www.douban.com/";"Origin","https://www.douban.com";
   "Content-Type","application/x-www-form-urlencoded; charset=UTF-8";"X-CSRF-TOKEN",ck;"X-Requested-With","XMLHttpRequest"] in
  let body=Uri.encoded_of_query(List.map(fun(k,v)->k,[v])(("ck",ck)::fields)) in
  let r=try t.post {R.Http.meth=Get;body="";url;headers} ~body with R.Http.Transport_error _->
    failwith "Douban like transfer failed or timed out; refresh to check the remote state" in
  if r.status<>200 then failwith(Printf.sprintf "Douban like failed (HTTP %d); refresh to check the remote state" r.status);
  let j=try Yojson.Safe.from_string r.body with _->failwith "Invalid Douban like response; refresh to check the remote state" in
  if (field "code" j<>`Null && num "code" j<>0) || field "localized_message" j<>`Null || field "r" j=`Int 1 then
   failwith "Douban rejected the like; check Firefox login or account verification";
  let count=if is_status then match field "count" j with `Int n when n>=0->Some n|_->failwith "Douban did not confirm the like"
   else (if field "reaction_type" j<>`Int(if liked then 1 else 0) then failwith "Douban did not confirm the requested reaction";
     match json_reaction j with Some r->r.count|None->None) in
  Hashtbl.replace t.reactions id {liked;count}
 let invalidate t id=Hashtbl.remove t.cache id
 let download url=Download.download_url url
 let archive_article _ a=R.Article.archive ~download a
 let archive t group id=archive_article t (read t group id)
 let attachment t group id part destination=R.Article.attachment ~download (read t group id)part destination
end
module Live=Make(R.Transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
module Threading : Reader.Backend.Threading with type t=Live.t = Live

(* The host supplies its selected credential provider; the backend does not
   discover browser profiles or read browser databases. *)
let create_live ~cookies () =
 Live.create ~cookies ~post:(fun request ~body->
  R.Transport.request {request with meth=Reader.Http.Post;body}) ()
module Liking : Reader.Backend.Liking with type t=Live.t = Live
