module C = Reader.Content
let atom = "http://www.w3.org/2005/Atom"
let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
let rss1 = "http://purl.org/rss/1.0/"
let dc = "http://purl.org/dc/elements/1.1/"
let content = "http://purl.org/rss/1.0/modules/content/"
let xml = "http://www.w3.org/XML/1998/namespace"
type node = Text of string | Element of Xmlm.tag * node list
let children = function Element (_,xs) -> xs | _ -> []
let is ns name = function Element (((u,n),_),_) -> u=ns && n=name | _ -> false
let elements ns name n = List.filter (is ns name) (children n)
let child ns name n = List.find_opt (is ns name) (children n)
let attr ns name = function Element ((_,attrs),_) -> Option.value (List.assoc_opt (ns,name) attrs) ~default:"" | _ -> ""
let rec text = function Text s -> s | Element (_,xs) -> String.concat "" (List.map text xs)
let value ns name n = Option.fold ~none:"" ~some:(fun n -> String.trim(text n)) (child ns name n)
let first xs = Option.value (List.find_opt ((<>) "") xs) ~default:""
let base parent n = let b=attr xml "base" n in if b="" then parent else Http.resolve parent b
let link base s = if s="" then "" else Http.resolve base s
let hash s = Digest.to_hex (Digest.string s)
let escape s = let b=Buffer.create (String.length s) in String.iter (function
  | '&' -> Buffer.add_string b "&amp;" | '<' -> Buffer.add_string b "&lt;"
  | '>' -> Buffer.add_string b "&gt;" | '"' -> Buffer.add_string b "&quot;"
  | c -> Buffer.add_char b c) s;Buffer.contents b
(* XHTML is serialized as HTML for the editor's renderer; foreign namespaces
   are discarded rather than mistaken for HTML. Relative links inherit xml:base. *)
let rec html parent = function
  | Text s -> escape s
  | Element (((ns,name),attrs),xs) as n ->
    let b=base parent n in
    if ns<>"http://www.w3.org/1999/xhtml" then "" else
    "<"^name^String.concat "" (List.filter_map (fun ((ns,key),v) ->
      if ns<>"" then None else
      Some(" "^key^"=\""^escape(if key="href" || key="src" then link b v else v)^"\"")) attrs)^">"^
    String.concat "" (List.map (html b) xs)^"</"^name^">"
let date s =
  match Ptime.of_rfc3339 s with
  | Ok(t,_,_) -> Ptime.to_float_s t
  | Error _ ->
    (* RSS RFC 822 dates. Convert via Ptime, independent of the host timezone. *)
    try
      let s=match String.index_opt s ',' with Some i -> String.sub s (i+1) (String.length s-i-1) | None -> s in
      let words=String.split_on_char ' ' (String.map (function '\t'|'\n'|'\r' -> ' ' | c -> c) s) |> List.filter ((<>) "") in
      match words with
      | day::month::year::clock::zone::_ ->
        let month=String.lowercase_ascii month in
        let rec find i=function [] -> raise Not_found | x::xs -> if x=month then i else find(i+1)xs in
        let m=find 1 ["jan";"feb";"mar";"apr";"may";"jun";"jul";"aug";"sep";"oct";"nov";"dec"] in
        let y=int_of_string year in let y=if y<50 then y+2000 else if y<100 then y+1900 else y in
        let h,min,sec=match List.map int_of_string (String.split_on_char ':' clock) with
          | [h;m;s] -> h,m,s | [h;m] -> h,m,0 | _ -> raise Not_found in
        let zone=String.uppercase_ascii zone in
        let offset=match List.assoc_opt zone ["UT",0;"GMT",0;"Z",0;"EST",-18000;"EDT",-14400;"CST",-21600;"CDT",-18000;"MST",-25200;"MDT",-21600;"PST",-28800;"PDT",-25200] with
          | Some n -> n
          | None ->
            if String.length zone<>5 || not(List.mem zone.[0] ['+';'-']) then raise Not_found;
            let zh=int_of_string(String.sub zone 1 2) and zm=int_of_string(String.sub zone 3 2) in
            if zh>23 || zm>59 then raise Not_found;
            (if zone.[0]='-' then -1 else 1)*(zh*3600+zm*60) in
        Ptime.of_date_time ((y,m,int_of_string day),((h,min,sec),offset)) |> Option.get |> Ptime.to_float_s
      | _ -> 0.
    with _ -> 0.
type enclosure = { part:int; url:string; name:string; size:int; mime:string }
type article = { message:C.message; enclosures:enclosure list }
let enclosure_json a = `Assoc["part",`Int a.part;"url",`String a.url;"name",`String a.name;"size",`Int a.size;"mime",`String a.mime]
let attachments a = `List(List.map enclosure_json a.enclosures)
let parse_xml body =
  if String.length body > 16*1024*1024 then failwith "Feed exceeds 16 MiB";
  let input=Xmlm.make_input ~strip:false (`String(0,body)) in
  let count=ref 0 in
  let rec node depth =
    incr count;
    if depth>128 || !count>200000 then failwith "Feed XML nesting/node limit exceeded";
    match Xmlm.input input with
    | `Data s -> Text s
    | `El_start tag ->
      let rec items acc=match Xmlm.peek input with
        | `El_end -> ignore(Xmlm.input input);List.rev acc
        | _ -> items(node(depth+1)::acc) in
      Element(tag,items [])
    | _ -> failwith "Unexpected feed XML" in
  (match Xmlm.input input with `Dtd None -> () | _ -> failwith "Feed DTDs are not supported");
  let root=node 0 in
  if not(Xmlm.eoi input) then failwith "Trailing feed XML";
  root
let parse ~url body =
  let root=parse_xml body in
  let ns,container,entries,atomic =
    if is atom "feed" root then atom,root,elements atom "entry" root,true
    else if is "" "rss" root then
      let channel=Option.value (child "" "channel" root) ~default:(Text "") in
      if channel=Text "" then failwith "RSS channel is missing";
      "",channel,elements "" "item" channel,false
    else if is rdf "RDF" root then rss1,root,elements rss1 "item" root,false
    else failwith "Expected RSS or Atom feed" in
  if List.length entries>5000 then failwith "Feed has more than 5000 entries";
  let feed_base=if container=root then base url root else base (base url root) container in
  let author n=if atomic then Option.fold ~none:"" ~some:(value atom "name") (child atom "author" n)
    else first[value "" "author" n;value dc "creator" n] in
  let seen=Hashtbl.create 64 in
  List.filter_map(fun entry ->
    let b=base feed_base entry in
    let links=elements atom "link" entry in
    let permalink=if atomic then
        List.find_opt(fun n -> List.mem (attr "" "rel" n) ["";"alternate"] &&
          List.mem (attr "" "type" n) ["";"text/html";"application/xhtml+xml"]) links
        |> Option.fold ~none:"" ~some:(fun n -> link (base b n) (attr "" "href" n))
      else let l=value ns "link" entry in if l<>"" then link b l else
        Option.fold ~none:"" ~some:(fun n -> if attr "" "isPermaLink" n="false" then "" else link b (String.trim(text n))) (child ns "guid" entry) in
    let title=value ns "title" entry in
    let body,html_body=if atomic then
        let c=match child atom "content" entry with Some c when attr "" "src" c="" -> Some c | _ -> child atom "summary" entry in
        Option.fold ~none:("",None) ~some:(fun n -> match attr "" "type" n with
          | "html"|"text/html" -> "",Some(text n)
          | "xhtml" -> "",Some(String.concat "" (List.map(html(base b n))(children n)))
          | ""|"text"|"text/plain" -> text n,None
          | _ -> value atom "summary" entry,None) c
      else "",Some(first[value content "encoded" entry;value ns "description" entry]) in
    let raw_id=first[ value ns (if atomic then "id" else "guid") entry;attr rdf "about" entry;permalink] in
    let id=if raw_id="" then "fallback:"^hash(title^"\000"^body^Option.value html_body ~default:"") else raw_id in
    if Hashtbl.mem seen id then None else begin
      Hashtbl.add seen id ();
      let enclosure_nodes=if atomic then List.filter(fun n -> attr "" "rel" n="enclosure") links else elements ns "enclosure" entry in
      let enclosures=List.mapi(fun part n ->
        let url=link (base b n) (attr "" (if atomic then "href" else "url") n) in
        let name=Uri.path(Uri.of_string url) |> Filename.basename |> Uri.pct_decode in
        {part;url;name=(if name="" || name="/" || name="." then "attachment" else name);
         size=max 0 (Option.value(int_of_string_opt(attr "" "length" n)) ~default:0);mime=attr "" "type" n}) enclosure_nodes in
      let created=date(first[value ns (if atomic then "published" else "pubDate") entry;value atom "updated" entry;value dc "date" entry]) in
      let revision=hash(Yojson.Safe.to_string (`List[`String title;`String body;`String(Option.value html_body ~default:"");`String(value atom "updated" entry);`List(List.map enclosure_json enclosures)])) in
      Some {message={C.id;thread_id=None;parent_id=None;references=[];title;author=first[author entry;author container];body;html=html_body;
        url=permalink;link_url=(if permalink="" then None else Some permalink);created;unavailable=false;score=0;my_vote=No_vote;activity=Some revision};enclosures}
    end) entries
