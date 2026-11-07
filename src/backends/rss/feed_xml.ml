(** XML bounds and extension metadata. RSS/Atom fields are parsed by Syndic;
    these helpers are only for namespace extensions and XHTML rendering. *)
let atom="http://www.w3.org/2005/Atom"
let rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
let rss1="http://purl.org/rss/1.0/"
let dc="http://purl.org/dc/elements/1.1/"
let input body =
  if String.length body>16*1024*1024 then failwith "Feed exceeds 16 MiB";
  let input=Xmlm.make_input ~strip:false (`String(0,body)) in
  let depth=ref 0 and nodes=ref 0 and roots=ref 0 in
  while not(Xmlm.eoi input)do
    (match Xmlm.input input with
     |`Dtd None->()|`Dtd(Some _)->failwith "Feed DTDs are not supported"
     |`El_start _->incr nodes;if !depth=0 then incr roots;incr depth;
       if !depth>129 || !roots>1 then failwith "Feed XML nesting/document limit exceeded"
     |`El_end->decr depth|`Data _->incr nodes);
    if !nodes>200000 then failwith "Feed XML node limit exceeded";
  done;
  Xmlm.make_input ~strip:false (`String(0,body))
let read body = Syndic.XML.of_xmlm(input body) |> snd
let children = function Syndic.XML.Node(_,_,xs)->xs|_->[]
let is ns name = function Syndic.XML.Node(_,((u,n),_),_)->u=ns && n=name|_->false
let elements ns name n=List.filter(is ns name)(children n)
let child ns name n=List.find_opt(is ns name)(children n)
let attr ns name = function Syndic.XML.Node(_,(_,attrs),_)->Option.value(List.assoc_opt(ns,name)attrs)~default:""|_->""
let rec text = function Syndic.XML.Data(_,s)->s|n->String.concat ""(List.map text(children n))
let value ns name n=Option.fold ~none:"" ~some:(fun n->String.trim(text n))(child ns name n)
let rec html parent = function
 |Syndic.XML.Data(_,s)->Reader.Html.escape s
 |Syndic.XML.Node(_,((ns,name),attrs),xs)as n->
  let b=match attr Xmlm.ns_xml "base" n with ""->parent|b->Reader.Http.resolve parent b in
  if ns<>"http://www.w3.org/1999/xhtml"then ""else
  "<"^name^String.concat ""(List.filter_map(fun((ns,key),v)->if ns<>""then None else
    Some(" "^key^"=\""^Reader.Html.escape(if key="href"||key="src"then Reader.Http.resolve b v else v)^"\""))attrs)^">"^
    String.concat ""(List.map(html b)xs)^"</"^name^">"
