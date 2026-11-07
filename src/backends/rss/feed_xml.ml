(** XML bounds, extension metadata and XHTML adaptation. Syndic parses feeds
    and serializes XML; these helpers retain only reader-specific policy. *)
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

(* Resolve inherited XML bases and remove XML-only namespaces before giving the
   fragment to the HTML reader. Syndic handles serialization and escaping. *)
let xhtml ~base nodes =
  let rec adapt base = function
    | Syndic.XML.Data _ as node -> Some node
    | Syndic.XML.Node(pos,((ns,name),attrs),xs)as node ->
      if ns<>"http://www.w3.org/1999/xhtml"then None else
      let base=match attr Xmlm.ns_xml "base" node with
        | ""->base | value->Some(Syndic.XML.resolve ~xmlbase:base (Uri.of_string value))in
      let attrs=List.filter_map(fun((ns,key),value)->
        if ns<>"" || key="xmlns"then None else
        let value=if List.mem key ["href";"src";"poster"]then
          Uri.to_string(Syndic.XML.resolve ~xmlbase:base (Uri.of_string value))else value in
        Some(("",key),value))attrs in
      Some(Syndic.XML.Node(pos,(("",name),attrs),List.filter_map(adapt base)xs)) in
  Syndic.XML.to_string(Syndic.XML.Node((0,0),(("","div"),[]),List.filter_map(adapt base)nodes))
