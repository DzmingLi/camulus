module R=Rss_backend
module D=R.Decode
module C=Reader.Content
module Html=Reader.Html
let source="blogger"
type group=string
type cursor=string
let host group =
 let prefix="posts." in
 if not(String.starts_with ~prefix group) then invalid_arg "Blogger expects posts.BLOG-HOST";
 let host=String.lowercase_ascii(String.sub group 6 (String.length group-6)) in
 let labels=String.split_on_char '.' host in
 if List.length labels<2 || not(List.for_all(fun label->String.length label>0 && String.length label<=63 &&
   label.[0]<>'-' && label.[String.length label-1]<>'-' &&
   String.for_all(function 'a'..'z'|'0'..'9'|'-'->true|_->false)label)labels) then invalid_arg "Invalid Blogger host";
 host
let feed_url group index =
 "https://"^host group^"/feeds/posts/default?max-results=25"^(if index=1 then "" else "&start-index="^string_of_int index)
let full_post (a:D.article) body =
 let tree=Html.parse body in
 let node=match Html.find(fun n->Html.has_class "post-body" n || Html.attr "itemprop" n="articleBody") tree with
 | Some node->node | None->failwith "Blogger page has no article body; summary was not archived as full text" in
 let cleaned=Html.clean ~base:a.message.url node |> Option.get in
 if String.trim(Html.text cleaned)="" && Html.media cleaned=[] then failwith "Blogger article body is empty";
 let full=R.Article.of_html a.message (Html.render cleaned) in
 let enclosures=List.sort_uniq(fun (a:D.enclosure) b->String.compare a.url b.url)(a.enclosures@full.enclosures)
   |> List.mapi(fun part e->{e with D.part}) in
 {full with enclosures}
module Make(H:R.Http.S)=struct
 module Feed=R.Make(H)
 type t={feed:Feed.t; next:(string,int option)Hashtbl.t; articles:((group*string),D.article)Hashtbl.t;
   full:((group*string),D.article)Hashtbl.t;lock:Eio.Mutex.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ()=
  let next=Hashtbl.create 16 in
  let decode ~url body=
   let root=D.parse_xml body in
   if not(D.is D.atom "feed" root)then failwith "Blogger did not return Atom";
   let articles=D.parse ~url body in
   let after=D.elements D.atom "link" root |> List.find_opt(fun n->D.attr "" "rel" n="next")
    |> Option.map(fun n->
      let u=Uri.of_string(D.attr "" "href" n) in
      match Option.bind(Uri.get_query_param u "start-index") int_of_string_opt with
      | Some n when n>0 && n<=1000000->n | _->failwith "Invalid Blogger next-page index") in
   Hashtbl.replace next url after;
   articles in
  {feed=Feed.create ~decode ();next;articles=Hashtbl.create 64;full=Hashtbl.create 64;lock=Eio.Mutex.create ()}
 let page t group query after =
  ignore(host group);
  let index=match after with None->1|Some cursor->match Yojson.Safe.from_string cursor with
    | `List[`String g;q;`Int n] when g=group && q=(match query with None->`Null|Some s->`String s) && n>1 && n<=1000000->n
    | _->invalid_arg "Blogger cursor belongs to another group or search" in
  Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun()->Eio.Mutex.unlock t.lock)(fun()->
   let endpoint,articles=Feed.fetch_articles t.feed (feed_url group index) in
   List.iter(fun(a:D.article)->Hashtbl.replace t.articles(group,a.message.id)a)articles;
   let next=Option.join(Hashtbl.find_opt t.next endpoint) in
   let after=Option.map(fun n->
     if n<=index || articles=[] then failwith "Blogger pagination did not advance";
     Yojson.Safe.to_string(`List[`String group;(match query with None->`Null|Some s->`String s);`Int n]))next in
   let contains text query=
    let text=String.lowercase_ascii text and query=String.lowercase_ascii query in
    let rec loop i=i+String.length query<=String.length text && (String.sub text i (String.length query)=query || loop(i+1)) in loop 0 in
   let articles=match query with None->articles|Some q->List.filter(fun(a:D.article)->
     contains(a.message.title^" "^a.message.author^" "^a.message.body^" "^Option.value a.message.html ~default:"")q)articles in
   {C.messages=List.map(fun(a:D.article)->a.message)articles;notifications=[];after})
 let list t group ~after=page t group None after
 let search t group ~query ~after=page t group (Some query) after
 let refresh t group=
  let page=list t group ~after:None in
  let fresh=List.map(fun(m:C.message)->m.id)page.messages in
  Hashtbl.filter_map_inplace(fun(g,id)a->if g=group && not(List.mem id fresh)then None else Some a)t.articles;
  page
 let read t group id =
  ignore(host group);
  let rec find after pages=
   if pages>=256 then failwith "Blogger lookup page limit exceeded";
   let p=list t group ~after in match Hashtbl.find_opt t.articles(group,id),p.after with
    | Some a,_->a |None,Some next->find(Some next)(pages+1)|None,None->failwith "Blogger article is no longer listed; open its archive" in
  let a=match Hashtbl.find_opt t.articles(group,id)with Some a->a|None->find None 0 in
  Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun()->Eio.Mutex.unlock t.lock)(fun()->
   match Hashtbl.find_opt t.full(group,id)with
   |Some cached when cached.message.activity=a.message.activity->cached
   |_->let body=Feed.download_url ~limit:(16*1024*1024)(R.Http.validate a.message.url) in
     let full=full_post a body in Hashtbl.replace t.full(group,id)full;full)
 let download ~limit url=Feed.download_url ~limit url
 let archive t group id=R.Article.archive ~download(read t group id)
 let attachment t group id part path=R.Article.attachment ~download(read t group id)part path
end
module Live=Make(R.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
