module C=Reader.Content
module Xml=Feed_xml
module A=Syndic.Atom
module R=Syndic.Rss2
type enclosure = {part:int;url:string;name:string;size:int;mime:string}
type article = {message:C.message;enclosures:enclosure list}
let enclosure_json a=`Assoc["part",`Int a.part;"url",`String a.url;"name",`String a.name;"size",`Int a.size;"mime",`String a.mime]
let attachments a=`List(List.map enclosure_json a.enclosures)
let parsed f=try f()with A.Error.Error((line,column),message)->
 failwith(Printf.sprintf "Invalid syndication feed at %d:%d: %s" line column message)
let hash s=Digest.to_hex(Digest.string s)
let date s=try Ptime.to_float_s(Syndic.Date.of_rfc3339 s)with _->
  try Ptime.to_float_s(Syndic.Date.of_rfc822 s)with _->0.
let enclosure part url size mime =
 let name=Uri.path url |> Filename.basename |> Uri.pct_decode in
 {part;url=Uri.to_string url;name=(if name=""||name="/"||name="."then "attachment"else name);size=max 0 size;mime}
let article ~id ~title ~author ~body ~html ~url ~created ~updated enclosures =
 let id=if id=""then(if url<>""then url else "fallback:"^hash(title^"\000"^body^Option.value html ~default:""))else id in
 let revision=hash(Yojson.Safe.to_string(`List[`String title;`String body;`String(Option.value html ~default:"");`String updated;`List(List.map enclosure_json enclosures)]))in
 {message={C.id;thread_id=None;parent_id=None;references=[];title;author;body;html;url;
   link_url=(if url=""then None else Some url);created;unavailable=false;score=0;my_vote=No_vote;activity=Some revision};enclosures}
let uri=Option.fold ~none:"" ~some:Uri.to_string
let text_construct : A.text_construct -> string * string option = function
 |A.Text s->s,None|A.Html(_,s)->"",Some s
 |A.Xhtml(base,nodes)->"",Some(Xml.xhtml ~base nodes)
let title construct=match text_construct construct with s,None->s|_,Some html->Reader.Html.parse html |> Reader.Html.text
let of_atom (feed:A.feed)=List.map(fun(e:A.entry)->
 let url=List.find_opt(fun(l:A.link)->l.rel=A.Alternate && List.mem l.type_media [None;Some "text/html";Some "application/xhtml+xml"])e.links
   |> Option.fold ~none:"" ~some:(fun l->Uri.to_string l.A.href)in
 let body,html=match e.content with
  |Some(A.Text s)->s,None|Some(A.Html(_,s))->"",Some s
  |Some(A.Xhtml(base,nodes))->"",Some(Xml.xhtml ~base:(Some(Option.value base ~default:(Uri.of_string url)))nodes)
  |_->Option.fold ~none:("",None)~some:text_construct e.summary in
 let enclosures=List.filter(fun(l:A.link)->l.rel=A.Enclosure)e.links |>List.mapi(fun part l->
   enclosure part l.A.href (Option.value l.length ~default:0)(Option.value l.type_media ~default:""))in
 let first,rest=e.authors in
 article ~id:(Uri.to_string e.id) ~title:(match e.title with A.Html(_,s)->s|other->title other)
   ~author:(String.concat ", "(List.map(fun(a:A.author)->a.name)(first::rest))) ~body ~html ~url
   ~created:(Ptime.to_float_s(Option.value e.published ~default:e.updated)) ~updated:(Syndic.Date.to_rfc3339 e.updated) enclosures)feed.entries
let atom_page ~url body =
 let feed=parsed(fun()->A.parse ~xmlbase:(Uri.of_string url)(Xmlm.make_input ~strip:false (`String(0,body))))in
 of_atom feed,feed.links
let parse ~url body =
 let root=Xml.read body in
 let input()=Xmlm.make_input ~strip:false (`String(0,body))in
 let articles=if Xml.is Xml.atom "feed" root then fst(atom_page ~url body)
 else if Xml.is "" "rss" root then(
  let feed=parsed(fun()->R.parse ~xmlbase:(Uri.of_string url)(input()))in
  let channel=Option.get(Xml.child "" "channel" root)in
  let nodes=Xml.elements "" "item" channel in
  List.map(fun(i:R.item)->
   let title,description=match i.story with R.Title t->t,""|R.Description(_,s)->"",s|R.All(t,_,s)->t,s in
   let id=Option.fold ~none:"" ~some:(fun(g:R.guid)->Uri.to_string g.data)i.guid in
   let node=List.find_opt(fun n->Xml.value "" "guid" n=id)nodes in
   let author=Option.value i.author ~default:(Option.fold ~none:""~some:(Xml.value Xml.dc "creator")node)in
   let content=snd i.content in
   let url=match i.link with Some u->Uri.to_string u|None->Option.fold ~none:""~some:(fun(g:R.guid)->if g.permalink then Uri.to_string g.data else "")i.guid in
   let created=Option.fold ~none:(Option.fold ~none:0. ~some:(fun n->date(Xml.value Xml.dc "date" n))node) ~some:Ptime.to_float_s i.pubDate in
   article ~id ~title ~author ~body:"" ~html:(Some(if content=""then description else content)) ~url ~created ~updated:""
    (Option.fold ~none:[]~some:(fun(e:R.enclosure)->[enclosure 0 e.url e.length e.mime])i.enclosure))feed.items)
 else if Xml.is Xml.rdf "RDF" root then(
  let feed=parsed(fun()->Syndic.Rss1.parse ~xmlbase:(Uri.of_string url)(input()))in
  let nodes=Xml.elements Xml.rss1 "item" root in
  List.map(fun(i:Syndic.Rss1.item)->
   let id=Uri.to_string i.about in
   let node=List.find_opt(fun n->Xml.attr Xml.rdf "about" n=id)nodes in
   let field name=Option.fold ~none:""~some:(Xml.value Xml.dc name)node in
   article ~id ~title:i.title ~author:(field "creator") ~body:"" ~html:(Some(Option.value i.description ~default:""))
     ~url:(Uri.to_string i.link) ~created:(date(field "date")) ~updated:"" [])feed.item)
 else failwith "Expected RSS or Atom feed" in
 let seen=Hashtbl.create 64 in
 List.filter(fun a->if Hashtbl.mem seen a.message.id then false else(Hashtbl.add seen a.message.id();true))articles
