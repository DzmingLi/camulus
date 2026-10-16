(** Small bounded HTML tree used by publication backends. *)
type node = Text of string | Element of string * (string * string) list * node list
let children = function Element(_,_,xs)->xs | Text _->[]
let attr name = function Element(_,attrs,_)->Option.value(List.assoc_opt name attrs) ~default:"" | _->""
let tag = function Element(name,_,_)->name | _->""
let has_class name node = String.split_on_char ' ' (attr "class" node) |> List.mem name
let rec text = function Text s->s | Element(_,_,xs)->String.concat "" (List.map text xs)
let rec find predicate node = if predicate node then Some node else List.find_map (find predicate) (children node)
let rec select predicate node = (if predicate node then [node] else []) @ List.concat_map (select predicate) (children node)
let escape s = let b=Buffer.create(String.length s) in String.iter(function
 | '&'->Buffer.add_string b "&amp;" | '<'->Buffer.add_string b "&lt;" | '>'->Buffer.add_string b "&gt;"
 | '"'->Buffer.add_string b "&quot;" | c->Buffer.add_char b c)s;Buffer.contents b
let parse input =
 if String.length input>16*1024*1024 then failwith "HTML exceeds 16 MiB";
 let stack=ref [("root",[],ref [])] and count=ref 0 in
 let add n=match !stack with (_,_,xs)::_ -> xs:=n::!xs | _->assert false in
 Markup.(string input |> parse_html |> signals) |> Markup.iter(fun signal ->
   incr count;if !count>200000 || List.length !stack>128 then failwith "HTML structure limit exceeded";
   match signal with
   | `Start_element ((_,name),attrs) -> stack:=(name,List.filter_map(fun ((ns,k),v)->if ns="" then Some(k,v) else None)attrs,ref [])::!stack
   | `Text ss -> add(Text(String.concat "" ss))
   | `End_element -> (match !stack with (name,attrs,xs)::rest when rest<>[] -> stack:=rest;add(Element(name,attrs,List.rev !xs)) | _->())
   | _->());
 match !stack with [(_,_,xs)]->Element("root",[],List.rev !xs) | _->failwith "Incomplete HTML tree"
let safe_link ~base name value =
 let u=Uri.resolve "" (Uri.of_string base) (Uri.of_string(String.trim value)) in
 if List.mem (Uri.scheme u) [Some "http";Some "https"] || (name="href" && Uri.scheme u=Some "mailto")
 then Some(Uri.to_string u) else None
let rec clean ?(drop=(fun _->false)) ~base node = match node with
 | Text _ -> Some node
 | Element(name,attrs,xs) ->
   if drop node || List.mem name ["script";"style";"iframe";"object";"embed";"noscript";"form"] then None else
   let attrs=List.filter_map(fun (k,v)->
     if String.starts_with ~prefix:"on" k || List.mem k ["style";"srcdoc";"srcset"] then None
     else if List.mem k ["src";"href";"poster"] then Option.map(fun v->k,v)(safe_link ~base k v)
     else Some(k,v)) attrs in
   let name=if name="span" && has_class "smallcaps" node then "strong" else name in
   Some(Element(name,attrs,List.filter_map(clean ~drop ~base)xs))
let rec render = function
 | Text s -> escape s
 | Element("root",_,xs) -> String.concat "" (List.map render xs)
 | Element(name,attrs,xs) ->
   "<"^name^String.concat "" (List.map(fun(k,v)->" "^k^"=\""^escape v^"\"")attrs)^">"^
   (if List.mem name ["img";"source";"br";"hr";"meta";"link";"input";"wbr";"area";"base";"col";"embed";"param";"track"] then ""
    else String.concat "" (List.map render xs)^"</"^name^">")
let media tree =
 select(fun n -> List.mem (tag n) ["img";"audio";"video";"source"] ||
   (tag n="a" && (attr "download" n<>"" || has_class "attachment" n))) tree
 |> List.concat_map(fun n -> List.filter ((<>) "") [attr (if tag n="a" then "href" else "src") n;attr "poster" n])
 |> List.sort_uniq String.compare
