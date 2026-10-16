module C=Reader.Content
module Html=Reader.Html
module R=Rss_backend
module D=R.Decode
module Http=Http
let source="csrin"
let numeric s=if s="" || String.length s>18 || not(String.for_all(function '0'..'9'->true|_->false)s) then invalid_arg "Expected numeric CS.RIN.RU ID" else s
let site value=
 let value=String.trim value in let value=if String.ends_with ~suffix:"/" value then value else value^"/" in
 let u=Uri.of_string value in
 let local=Uri.scheme u=Some "http" && List.mem(Uri.host u)[Some "127.0.0.1";Some "localhost";Some "::1"] in
 if not(local || List.mem value ["https://cs.rin.ru/forum/";"https://csrin.org/forum/"]) || Uri.userinfo u<>None || Uri.query u<>[] || Uri.fragment u<>None
 then invalid_arg "Use https://cs.rin.ru/forum/ or https://csrin.org/forum/";
 ignore(Http.validate value);value
let group s=match String.split_on_char '.' s with
 |[kind;id] when List.mem kind ["forum";"topic"]->kind,numeric id
 |_->invalid_arg "Use forum.ID or topic.ID"
let param url name=Option.map numeric(Uri.get_query_param(Uri.of_string url)name)
let post_id topic post="topic:"^numeric topic^"/post:"^numeric post
let identity id=match String.split_on_char '/' id with
 |[topic;post] when String.starts_with ~prefix:"topic:" topic && String.starts_with ~prefix:"post:" post ->
   numeric(String.sub topic 6 (String.length topic-6)),numeric(String.sub post 5 (String.length post-5))
 |_->invalid_arg "Expected topic:ID/post:ID"
let topic_of_id id=match String.split_on_char ':' id with
 |["topic";n]->numeric n|_->fst(identity id)
let contains s part=try ignore(Str.search_forward(Str.regexp_string part)s 0);true with Not_found->false
let tidy s=
 let s=List.fold_left(fun s space->Str.global_replace(Str.regexp_string space)" "s)s["\194\160";"\226\128\175"]in
 Str.global_replace(Str.regexp "[ \t\r\n]+")" " s |> String.trim
let text n=Html.text n |> tidy
let html_date s=
 let s=tidy s in
 let s=if String.starts_with ~prefix:"Posted:" s then String.trim(String.sub s 7 (String.length s-7))else s in
 let words=String.split_on_char ' '(String.map(fun c->if c=',' then ' 'else c)s) |> List.filter((<>)"") in
 let words=match words with _weekday::day::month::year::clock::[] ->[day;month;year;clock]|_->words in
 let date=D.date(String.concat " " words^" +0100")in
 if date=0. then failwith ("Unrecognized CS.RIN.RU post date: "^s);date
let make ~base ~topic ~post ~title ~author ~created ~body=
 let id=post_id topic post in
 let message={C.id;thread_id=Some("topic:"^topic);parent_id=None;references=[];title;author;body="";html=None;
  url=Http.resolve base ("viewtopic.php?t="^topic^"&p="^post^"#p"^post);link_url=None;created;
  unavailable=false;score=0;my_vote=C.No_vote;activity=Some(D.hash(title^"\000"^body))}in
 let rec attachments = function
  |Html.Element("a",attrs,xs) as n when contains (Html.attr "href" n) "download/file.php" ->
    Html.Element("a",("class","attachment")::List.remove_assoc "class" attrs,List.map attachments xs)
  |Html.Element(tag,attrs,xs)->Html.Element(tag,attrs,List.map attachments xs)|n->n in
 R.Article.of_html message (Html.render(attachments(Html.parse body)))
let feed ~base body=
 let root=D.parse_xml body in if not(D.is D.atom "feed" root)then failwith "Expected forum Atom feed; login may be required";
 let entries=D.elements D.atom "entry" root in if List.length entries>5000 then failwith "Forum feed too large";
 List.map(fun n->
  let url=D.value D.atom "id" n in
  let topic=Option.value(param url "t")~default:"" and post=Option.value(param url "p")~default:"" in
  let title=D.value D.atom "title" n |> Html.parse |> text in
  let author=Option.fold ~none:"" ~some:(D.value D.atom "name")(D.child D.atom "author" n)in
  let created=D.date(D.value D.atom "published" n)in
  let a=make ~base ~topic ~post ~title ~author ~created ~body:(D.value D.atom "content" n)in
  let updated=D.value D.atom "updated" n in
  {a with message={a.message with activity=Some(D.hash(Option.value a.message.activity ~default:""^updated))}})entries
let topic_page ~base ~topic ~start body=
 let root=Html.parse body in
 let title=Option.fold ~none:"" ~some:text(Html.find(Html.has_class "titles")root)in
 let posts=Html.select(Html.has_class "tablebg")root |> List.filter_map(fun table->
  let bodies=Html.select(Html.has_class "postbody")table and authors=Html.select(Html.has_class "postauthor")table in
  match bodies,authors with
  |[body],[author]->
    let anchor=Html.find(fun n->Html.tag n="a" && Str.string_match(Str.regexp "^p[0-9]+$")(Html.attr "name" n)0)table in
    let date=Html.find(fun n->Html.tag n="div" && String.starts_with ~prefix:"Posted:"(text n))table in
    let forum=Html.select(fun n->Html.tag n="a")table |> List.find_map(fun n->param(Html.attr "href" n)"f")in
    let post=match anchor with Some n->String.sub(Html.attr "name" n)1(String.length(Html.attr "name" n)-1)|None->failwith "Forum post has no identity" in
    let created=match date with Some n->html_date(text n)|None->failwith "Forum post has no timestamp" in
    let forum=Option.value forum ~default:""in ignore(numeric forum);
    Some(make ~base ~topic ~post ~title ~author:(text author) ~created ~body:(Html.render body),forum)
  |_->None)in
 if posts=[]then failwith "Topic has no readable posts; login or permissions may be required";
 let offsets=Html.select(fun n->Html.tag n="a")root |> List.filter_map(fun n->
  let href=Html.attr "href" n in
  if param href "t"=Some topic then match param href "start" with
   |Some s->let n=int_of_string s in if n>start then Some n else None|_->None else None)in
 let next=match List.sort_uniq Int.compare offsets with n::_->Some n|_->None in posts,next
let hidden form=Html.select(fun n->Html.tag n="input" && String.lowercase_ascii(Html.attr "type" n)="hidden" && Html.attr "name" n<>"")form
 |> List.fold_left(fun xs n->(Html.attr "name" n,Html.attr "value" n)::List.remove_assoc(Html.attr "name" n)xs)[]
let form field root=Html.find(fun n->Html.tag n="form" && Option.is_some(Html.find(fun c->Html.attr "name" c=field)n))root
let safe_action base form=
 let url=Http.resolve base(Html.attr "action" form)in
 if not(Http.same_origin base url) || not(String.starts_with ~prefix:(Uri.path(Uri.of_string base))(Uri.path(Uri.of_string url)))
 then failwith "Refusing an off-site forum form";
 ignore(Http.validate url);url
exception Submit_uncertain
module Make(H:Http.S)=struct
 type t={base:string;cookies:string->(string*string)list;jar:(string,(string*string*float))Hashtbl.t;posts:(string,D.article)Hashtbl.t;forums:(string,string)Hashtbl.t;max_pages:int;lock:Eio.Mutex.t;http_lock:Eio.Mutex.t}
 type group=string
 type cursor=string
 type draft={forum:string;topic:string option;subject:string;body:string}
 type receipt=string
 let source=source
 let create ?(base="https://cs.rin.ru/forum/") ?(cookies=fun _->[]) ?(max_pages=100) ()=
  if max_pages<1 || max_pages>1000 then invalid_arg "Invalid thread page limit";
  {base=site base;cookies;jar=Hashtbl.create 16;posts=Hashtbl.create 64;forums=Hashtbl.create 64;max_pages;lock=Eio.Mutex.create();http_lock=Eio.Mutex.create()}
 let locked t f=Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun()->Eio.Mutex.unlock t.lock)f
 let remember t url headers=
  List.iter(fun(k,v)->if String.lowercase_ascii k="set-cookie"then
   match String.split_on_char ';' v with
   |pair::attrs->(match String.index_opt pair '=' with None->()|Some i->
     let key=String.trim(String.sub pair 0 i)and value=String.sub pair(i+1)(String.length pair-i-1)in
     let attrs=List.filter_map(fun a->match String.split_on_char '=' (String.trim a)with k::v->Some(String.lowercase_ascii k,String.concat "=" v)|_->None)attrs in
     let path=Option.value(List.assoc_opt "path" attrs)~default:"/" in
     let host=Option.value(Uri.host(Uri.of_string url))~default:"" in
     let domain=Option.value(List.assoc_opt "domain" attrs)~default:host in
     let domain=if String.starts_with ~prefix:"." domain then String.sub domain 1(String.length domain-1)else domain in
     if domain=host && key<>""then (
      let expiry=match List.assoc_opt "max-age" attrs with Some n->Unix.gettimeofday()+.Option.value(float_of_string_opt n)~default:0.|None->
       (match List.assoc_opt "expires" attrs with Some s->max 1. (D.date s)|None->0.) in
      Hashtbl.replace t.jar key(value,path,expiry)))|_->())headers
 let request t ?fields ?(limit=16*1024*1024) url=
  Eio.Mutex.lock t.http_lock;Fun.protect ~finally:(fun()->Eio.Mutex.unlock t.http_lock)(fun()->
  let rec run checked redirects url=
   if redirects>5 || not(Http.same_origin t.base url)then failwith "Invalid forum redirect";
   let path=Uri.path(Uri.of_string url) in
   let cookies=t.cookies url |> List.filter(fun(k,_)->not(Hashtbl.mem t.jar k)) in
   let cookies=Hashtbl.fold(fun k(v,p,expiry) xs->
    let path_ok=path=p || (String.starts_with ~prefix:p path && (String.ends_with ~suffix:"/" p || (String.length path>String.length p && path.[String.length p]='/')))in
    if path_ok && (expiry=0. || expiry>Unix.gettimeofday())then(k,v)::xs else xs)t.jar cookies in
   let headers=["Accept","text/html,application/atom+xml";"Cookie",Reader.Browser_cookies.header cookies] in
   let headers=if fields=None then headers else ("Content-Type","application/x-www-form-urlencoded")::("Referer",url)::headers in
   let body=Option.fold ~none:"" ~some:(fun xs->Uri.encoded_of_query(List.map(fun(k,v)->k,[v])xs))fields in
   let r=H.request{Http.meth=(if fields=None then Get else Post);url;headers;body;limit}in
   remember t url r.headers;
   if r.status=401 && fields=None && not checked then (
    let extract key=try ignore(Str.search_forward(Str.regexp(key^"=\\([^;\"' \r\n]+\\)"))r.body 0);Str.matched_group 1 r.body with Not_found->failwith "Security check requires a browser" in
    let token=extract "securitytoken"and expiration=extract "securitytoken_expiration"in
    Hashtbl.replace t.jar "securitytoken"(token,"/",0.);Hashtbl.replace t.jar "securitytoken_expiration"(expiration,"/",0.);
    let u=Uri.of_string url in run true (redirects+1)(Uri.to_string(Uri.with_path u("/securitycheck"^Uri.path u))))
   else if fields=None && List.mem r.status[301;302;303;307;308]then
    let location=Option.value(List.assoc_opt "location" r.headers)~default:""in
    if location=""then failwith "Forum redirect has no location"else run checked(redirects+1)(Http.resolve url location)
   else r
  in run false 0 url)
 let get t path=let r=request t (Http.resolve t.base path)in if r.status<>200 then failwith(Printf.sprintf "CS.RIN.RU HTTP %d"r.status);r.body
 let link_root root (a:D.article)=let m=a.message in {a with message={m with parent_id=(if m.id=root then None else Some root);references=(if m.id=root then []else[root])}}
 let stream_unlocked t id ~emit_article ~emit=
  let topic=topic_of_id id in
  let seen=Hashtbl.create 64 and root=ref None in
  let rec pages count start=
   if count>=t.max_pages then failwith "CS.RIN.RU thread page limit exceeded; thread is incomplete";
   let posts,next=topic_page ~base:t.base ~topic ~start(get t(Printf.sprintf "viewtopic.php?t=%s&start=%d"topic start))in
   if !root=None then root:=Some(fst(List.hd posts)).message.id;
   let articles=List.map(fun(a,forum)->let a=link_root(Option.get !root)a in
    if Hashtbl.mem seen a.message.id then failwith "Forum pagination repeated a post";
    Hashtbl.add seen a.message.id();Hashtbl.replace t.posts a.message.id a;Hashtbl.replace t.forums topic forum;
    emit_article a;a)posts in
   emit{C.thread="topic:"^topic;messages=List.map(fun(a:D.article)->a.message)articles;gaps=[];complete=next=None};
   Option.iter(pages(count+1))next
  in pages 0 0
 let stream_thread t id ~emit_article ~emit=locked t(fun()->stream_unlocked t id ~emit_article ~emit)
 let thread t id ~emit=stream_thread t id ~emit_article:(fun _->())~emit
 let read_unlocked t id=
  let topic,post=identity id in
  let first,_=topic_page ~base:t.base ~topic ~start:0(get t("viewtopic.php?t="^topic^"&start=0"))in
  let root=(fst(List.hd first)).message.id in
  let posts=if List.exists(fun(a,_)->a.D.message.id=id)first then first else
   fst(topic_page ~base:t.base ~topic ~start:0(get t("viewtopic.php?t="^topic^"&p="^post)))in
  let found=ref None in
  List.iter(fun(a,forum)->let a=link_root root a in
   Hashtbl.replace t.posts a.message.id a;Hashtbl.replace t.forums topic forum;
   if a.message.id=id then found:=Some a)posts;
  match !found with Some a->a|None->failwith "Forum post is unavailable"
 let read t _group id=locked t(fun()->read_unlocked t id)
 let list t name ~after=locked t(fun()->
  if after<>None then invalid_arg "Forum feeds have no next page";
  let kind,id=group name in
  let entries=feed ~base:t.base(get t("feed.php?"^(if kind="forum"then "f"else "t")^"="^id))in
  (* Atom already supplies native topic identity. Do not fetch bodies or guess roots while listing. *)
  let messages=List.map(fun(a:D.article)->Hashtbl.replace t.posts a.message.id a;a.message)entries in
  {C.messages;notifications=[];after=None})
 let forums t=locked t(fun()->
  Html.parse(get t "index.php") |> Html.select(fun n->Html.tag n="a")
  |> List.filter_map(fun n->let href=Http.resolve t.base(Html.attr "href"n)in
    if Http.same_origin t.base href && Filename.basename(Uri.path(Uri.of_string href))="viewforum.php" && text n<>""then
      Option.map(fun id->"forum."^id,text n)(param href "f")else None)
  |> List.sort_uniq(fun(a,_)(b,_)->String.compare a b))
 let search _ _ ~query:_ ~after:_=failwith "CS.RIN.RU does not provide group search in Camulus yet"
 let download t ~limit url=
  if Http.same_origin t.base url then(
   let r=request t ~limit url in if r.status<>200 || String.length r.body>limit then failwith "Forum attachment unavailable or too large";r.body)
  else let module Media=struct
   let request (r:R.Http.request)=
    let response=H.request{Http.meth=Get;url=r.url;headers=r.headers;body="";limit=r.limit}in
    {R.Http.status=response.status;headers=response.headers;body=response.body}
  end in let module Download=R.Make(Media)in Download.download_url ~limit url
 let archive_article t a=R.Article.archive ~download:(download t)a
 let archive t group id=archive_article t(read t group id)
 let attachment t _group id part destination=let a=match Hashtbl.find_opt t.posts id with Some a->a|None->read t "" id in
  R.Article.attachment ~download:(download t)a part destination
 let prepare t name id=
  match id with
  |None->let kind,forum=group name in if kind<>"forum"then invalid_arg "Open a post before replying";
    {forum;topic=None;subject="";body=""}
  |Some id->locked t(fun()->let topic=topic_of_id id in
    let a=match Hashtbl.find_opt t.posts id with Some a when Hashtbl.mem t.forums topic->a|_->read_unlocked t id in
    {forum=Hashtbl.find t.forums topic;topic=Some topic;subject="Re: "^a.message.title;body=""})
 let login t ~username ~password=locked t(fun()->
  let root=Html.parse(get t "ucp.php?mode=login")in
  let f=match form "password"root with Some f->f|None->failwith "Forum login form unavailable"in
  let action=safe_action t.base f in
  let fields=["username",username;"password",password;"login","Login"]@List.filter(fun(k,_)->not(List.mem k["username";"password";"login"]))(hidden f)in
  let initial=request t ~fields action in
  let r=if List.mem initial.status[302;303]then
    match List.assoc_opt "location"initial.headers with Some path->request t (Http.resolve t.base path)|None->initial
    else initial in
  let confirmed= r.status=200 && Option.is_some(Html.find(fun n->Html.tag n="a" && contains(Html.attr "href" n)"mode=logout")(Html.parse r.body))in
  if not confirmed then failwith "Login not confirmed; complete any CAPTCHA in a browser";
  Hashtbl.clear t.posts)
 let send t draft=locked t(fun()->
  ignore(numeric draft.forum);Option.iter(fun n->ignore(numeric n))draft.topic;
  if String.trim draft.subject="" || String.trim draft.body="" || String.exists(fun c->c='\r'||c='\n'||c='\000')draft.subject || contains draft.body "<#part" || contains draft.body "<#multipart" then invalid_arg "Use a subject and plain text or BBCode; attachments are unsupported";
  let path="posting.php?mode="^(if draft.topic=None then "post"else "reply")^"&f="^draft.forum^Option.fold ~none:""~some:(fun n->"&t="^n)draft.topic in
  let root=Html.parse(get t path)in
  if form "password"root<>None then failwith "Log in before posting";
  let f=match form "message"root with Some f->f|None->failwith "Posting unavailable or topic locked"in
  if Option.is_some(Html.find(fun n->contains(Html.attr "name" n)"captcha" || contains(Html.attr "name" n)"confirm")f)then failwith "Complete the forum CAPTCHA in a browser";
  let action=safe_action t.base f and fields=hidden f in
  if not(List.for_all(fun k->List.mem_assoc k fields && List.assoc k fields<>"")["form_token";"creation_time"])then failwith "Posting form lacks fresh CSRF tokens";
  let fields=["subject",draft.subject;"message",draft.body;"post","Submit"]@List.filter(fun(k,_)->not(List.mem k["subject";"message";"post"]))fields in
  (* Once submission starts, cancellation, timeout and unconfirmed replies are uncertain. Never replay. *)
  try let r=request t ~fields action in
   let redirect=if List.mem r.status[302;303]then List.assoc_opt "location"r.headers else if r.status=200 then
    let dom=Html.parse r.body in
    if form "message"dom<>None || form "password"dom<>None then None else
    Html.select(fun n->Html.tag n="meta" && String.lowercase_ascii(Html.attr "http-equiv" n)="refresh")dom
    |>List.find_map(fun n->let content=Html.attr "content"n in
      try let i=Str.search_forward(Str.regexp_case_fold "url=")content 0 in Some(String.sub content(i+4)(String.length content-i-4)|>String.trim)with Not_found->None)
   else None in
   let url=match redirect with Some url->Http.resolve t.base url|None->raise Submit_uncertain in
   if not(Http.same_origin t.base url) || Filename.basename(Uri.path(Uri.of_string url))<>"viewtopic.php" || param url "t"=None || (draft.topic<>None && param url "t"<>draft.topic)then raise Submit_uncertain;
   Hashtbl.clear t.posts;url
  with _->raise Submit_uncertain)
end
module Live=Make(Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=string and type cursor=string = Live
module Threading : Reader.Backend.Threading with type t=Live.t = Live
module Writing : Reader.Backend.Writing with type t=Live.t and type draft=Live.draft and type receipt=Live.receipt = Live

type operation = Forums | Login of string * string | Prepare of string * string option | Send of Live.draft
