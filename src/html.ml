(** Reader-specific HTML policy on Lambda Soup's DOM. Parsing, traversal and
    serialization belong to the library; cleaning mutates the supplied DOM. *)
type node = Soup.general Soup.node
let parse input = Soup.parse input |> Soup.coerce
let children node = Soup.children node |> Soup.to_list
let attr name node = Option.bind (Soup.element node) (Soup.attribute name) |> Option.value ~default:""
let set_attr name value node =
 Option.iter (Soup.set_attribute name value) (Soup.element node)
let tag node = Option.fold ~none:"" ~some:Soup.name (Soup.element node)
let has_class name node = Option.fold ~none:false ~some:(fun e->List.mem name(Soup.classes e)) (Soup.element node)
let text node = String.concat "" (Soup.texts node)
let find predicate node = if predicate node then Some node else Soup.descendants node |> Soup.filter predicate |> Soup.first
let select predicate node =
 (if predicate node then [node] else []) @ (Soup.descendants node |> Soup.filter predicate |> Soup.to_list)
(* Used in both text and quoted attributes by backend-generated HTML. *)
let escape s = let b=Buffer.create(String.length s) in String.iter(function
 | '&'->Buffer.add_string b "&amp;" | '<'->Buffer.add_string b "&lt;" | '>'->Buffer.add_string b "&gt;"
 | '"'->Buffer.add_string b "&quot;" | c->Buffer.add_char b c)s;Buffer.contents b
let safe_link ~base name value =
 let u=Uri.resolve "" (Uri.of_string base) (Uri.of_string(String.trim value)) in
 if List.mem (Uri.scheme u) [Some "http";Some "https"] || (name="href" && Uri.scheme u=Some "mailto")
 then Some(Uri.to_string u) else None
let clean ?(drop=(fun _->false)) ~base node =
 let remove node=drop node || List.mem(tag node)["script";"style";"iframe";"object";"embed";"noscript";"form"] in
 if remove node then None else (
  (* Snapshot the traversal before mutating nodes and attributes. *)
  List.iter(fun n->if remove n then Soup.delete n else Option.iter(fun element->
   let attrs=Soup.fold_attributes(fun acc k v->(k,v)::acc)[] element in
   List.iter(fun(k,v)->
    if String.starts_with ~prefix:"on" k || List.mem k ["style";"srcdoc";"srcset"] then Soup.delete_attribute k element
    else if List.mem k ["src";"href";"poster"] then match safe_link ~base k v with
      | None->Soup.delete_attribute k element | Some v->Soup.set_attribute k v element)attrs;
   if Soup.name element="span" && List.mem "smallcaps" (Soup.classes element)then Soup.set_name "strong" element
  )(Soup.element n)) (node::(Soup.descendants node |> Soup.to_list));
  Some node)
let render = Soup.to_string
let media tree =
 select(fun n -> List.mem (tag n) ["img";"audio";"video";"source"] ||
   (tag n="a" && (attr "download" n<>"" || has_class "attachment" n))) tree
 |> List.concat_map(fun n -> List.filter ((<>) "") [attr (if tag n="a" then "href" else "src") n;attr "poster" n])
 |> List.sort_uniq String.compare

let image_data_uri bytes =
 let starts prefix = String.starts_with ~prefix bytes in
 let at offset value =
  String.length bytes >= offset + String.length value &&
  String.sub bytes offset (String.length value) = value in
 let mime=
  if starts "\255\216\255" then Some "image/jpeg"
  else if starts "\137PNG\r\n\026\n" then Some "image/png"
  else if starts "GIF87a" || starts "GIF89a" then Some "image/gif"
  else if starts "RIFF" && at 8 "WEBP" then Some "image/webp"
  else if at 4 "ftypavif" || at 4 "ftypavis" then Some "image/avif"
  else None in
 Option.map (fun mime -> "data:" ^ mime ^ ";base64," ^ Base64.encode_string bytes) mime

(* Cached covers are already encoded for a quoted src attribute. Keep their
   binary-sized payload out of the HTML tokenizer and serializer. *)
let cached_image_src = Str.regexp " src=\"\\(data:image/[a-zA-Z0-9.+-]+;base64,[A-Za-z0-9+/=]*\\)\""
let embedded_image ~url html =
 let images=Hashtbl.create 1 in
 let compact=Str.global_substitute cached_image_src (fun input ->
   let data=Str.matched_group 1 input in
   let key="camulus-cover:" ^ string_of_int (Hashtbl.length images) in
   Hashtbl.add images key data;
   " src=\"" ^ key ^ "\"") html in
 if Hashtbl.length images=0 then None else
 find (fun node -> tag node="img" && attr "data-camulus-image-url" node=url &&
   Hashtbl.mem images (attr "src" node)) (parse compact)
 |> Option.map (fun node -> Hashtbl.find images (attr "src" node),attr "data-camulus-cover-revision" node)

let render_with_cover tree node data =
 let rec insert marker =
   set_attr "src" marker node;
   let html=render tree and attribute=Str.regexp_string (" src=\"" ^ marker ^ "\"") in
   let start=Str.search_forward attribute html 0 in
   let stop=Str.match_end () in
   if (try ignore (Str.search_forward attribute html stop);true with Not_found -> false)
   then insert (marker ^ "-")
   else String.concat "" [String.sub html 0 (start+6);data;String.sub html (stop-1) (String.length html-stop+1)] in
 insert "camulus-cover:embedded"

let embedded_image_bytes (src,_) =
 Option.bind (String.index_opt src ',') (fun comma ->
  Base64.decode ~off:(comma+1) src |> Result.to_option)
