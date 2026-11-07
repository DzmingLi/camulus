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
 "https://"^host group^"/feeds/posts/default"^(if index=1 then "" else "?start-index="^string_of_int index)
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
 type t={feed:(D.article list * int option) Feed.cache; articles:((group*string),D.article)Hashtbl.t;
   full:((group*string),D.article)Hashtbl.t;lock:Eio.Mutex.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ()=
  let decode ~url body=
   let articles,links=D.atom_page ~url body in
   let after=List.find_opt(fun(link:Syndic.Atom.link)->link.rel=Syndic.Atom.Link(Uri.of_string "next"))links
    |> Option.map(fun link->
      match Option.bind(Uri.get_query_param link.Syndic.Atom.href "start-index") int_of_string_opt with
      | Some n when n>0->n | _->failwith "Invalid Blogger next-page index") in
   articles,after in
  {feed=Feed.create_cache ~decode ();articles=Hashtbl.create 64;full=Hashtbl.create 64;lock=Eio.Mutex.create ()}
 let list t group ~after =
  ignore(host group);
  let index=match after with None->1|Some cursor->match Yojson.Safe.from_string cursor with
    | `List[`String g;`Int n] when g=group && n>1->n
    | _->invalid_arg "Blogger cursor belongs to another group" in
  Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun()->Eio.Mutex.unlock t.lock)(fun()->
   let _,(articles,next)=Feed.fetch_cached t.feed (feed_url group index) in
   List.iter(fun(a:D.article)->Hashtbl.replace t.articles(group,a.message.id)a)articles;
   let after=Option.map(fun n->
     if n<=index || articles=[] then failwith "Blogger pagination did not advance";
     Yojson.Safe.to_string(`List[`String group;`Int n]))next in
   {C.messages=List.map(fun(a:D.article)->a.message)articles;after})
 let refresh t group=
  let page=list t group ~after:None in
  let fresh=List.map(fun(m:C.message)->m.id)page.messages in
  Hashtbl.filter_map_inplace(fun(g,id)a->if g=group && not(List.mem id fresh)then None else Some a)t.articles;
  page
 let read t group id =
  ignore(host group);
  let rec find after=
   let p=list t group ~after in match Hashtbl.find_opt t.articles(group,id),p.after with
    | Some a,_->a |None,Some next->find(Some next)|None,None->failwith "Blogger article is no longer listed; open its archive" in
  let a=match Hashtbl.find_opt t.articles(group,id)with Some a->a|None->find None in
  Eio.Mutex.lock t.lock;Fun.protect ~finally:(fun()->Eio.Mutex.unlock t.lock)(fun()->
   match Hashtbl.find_opt t.full(group,id)with
   |Some cached when cached.message.activity=a.message.activity->cached
   |_->let body=Feed.download_url (R.Http.validate a.message.url) in
     let full=full_post a body in Hashtbl.replace t.full(group,id)full;full)
 let download url=Feed.download_url url
 let archive t group id=R.Article.archive ~download(read t group id)
 let attachment t group id part path=R.Article.attachment ~download(read t group id)part path
end
module Live=Make(R.Transport)
