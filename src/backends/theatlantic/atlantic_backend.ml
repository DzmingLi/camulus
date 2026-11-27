module C=Reader.Content
module R=Rss_backend
module D=R.Decode
module Xml=R.Feed_xml
module Html=Reader.Html
type group=string
type cursor=string
let feed_url group =
 let prefix="author." in
 if not(String.starts_with ~prefix group) then invalid_arg "Atlantic expects author.SLUG";
 let slug=String.sub group (String.length prefix) (String.length group-String.length prefix) in
 if slug="" || not(String.for_all(function 'a'..'z'|'A'..'Z'|'0'..'9'|'-'->true | _->false)slug) then invalid_arg "Invalid Atlantic author slug";
 "https://www.theatlantic.com/feed/author/"^slug^"/"
let decode ~url body =
 let root=Xml.read body in
 if not(Xml.is Xml.atom "feed" root) then failwith "Atlantic did not return Atom";
 let entries=Xml.elements Xml.atom "entry" root in
 let articles=D.parse ~url body in
 List.map(fun (a:D.article) ->
  let image=match List.find_opt(fun n->Xml.value Xml.atom "id" n=a.message.id) entries with
    | None->""
    | Some entry -> Xml.children entry |> List.find_opt(fun n -> Xml.attr "" "url" n<>"" &&
        (Xml.is "http://search.yahoo.com/mrss/" "content" n || Xml.attr "" "medium" n="image" || String.starts_with ~prefix:"image/" (Xml.attr "" "type" n)))
      |> Option.fold ~none:"" ~some:(Xml.attr "" "url") in
  let base=if a.message.url="" then url else a.message.url in
  let clean html=Html.parse html |> Html.clean ~base
    ~drop:(fun n -> Html.attr "data-id" n="injected-recirculation-link") |> Option.get in
  let html=Option.value a.message.html ~default:(Html.escape a.message.body) in
  let tree=clean html in
  let tree=match (if String.trim image="" then None else Html.safe_link ~base "src" image) with
    | Some image ->
      let is_cover node=Html.tag node="img" && Html.attr "src" node=image in
      if Html.find is_cover tree=None then
        clean ("<figure><img src=\"" ^ Html.escape image ^ "\"></figure>" ^ html)
      else tree
    | _ -> tree in
  let enclosures=(List.map(fun (e:D.enclosure)->e.url)a.enclosures @ Html.media tree) |> List.sort_uniq String.compare |> List.mapi(fun part url ->
    {D.part;url;name=Uri.path(Uri.of_string url)|>Filename.basename|>Uri.pct_decode;size=0;mime=""}) in
  {D.message={a.message with title=String.trim(Html.text(Html.parse a.message.title));html=Some(Html.render tree);body=""};enclosures}) articles
let preserve_images ~download ~previous (article:D.article) =
 let tree=Html.parse (Option.get article.message.html) in
 let images=Html.select (fun node -> Html.tag node="img" && Html.attr "src" node<>"") tree in
 if images=[] then article else
 let revision=Option.value article.message.activity ~default:"" in
 let saved=match previous article.message.id with
   | Some cached ->
     (match Yojson.Safe.Util.(cached |> member "message" |> member "html") with
      | `String html -> Html.embedded_images html
      | _ -> Hashtbl.create 0)
   | None -> Hashtbl.create 0 in
 let fetch url =
   let old=Hashtbl.find_opt saved url in
   match old with
   | Some (_,cached_revision) when cached_revision=revision -> old
   | _ ->
     (try match Html.image_data_uri (download url) with
      | Some data -> Some (data,revision)
      | None -> old
      with Failure _ | R.Http.Transport_error _ -> old) in
 let fetched=Hashtbl.create 8 in
 List.iter (fun node ->
   let url=Html.attr "src" node in
   let image=match Hashtbl.find_opt fetched url with
     | Some result -> result
     | None -> let result=fetch url in Hashtbl.add fetched url result;result in
   Option.iter (fun (data,revision) ->
     Html.set_attr "data-camulus-image-url" url node;
     Html.set_attr "data-camulus-image-revision" revision node;
     Html.set_attr "src" data node) image) images;
 {article with message={article.message with html=Some (Html.render tree)}}

module Make(H:R.Http.S)=struct
 module Feed=R.Make(H)
 type t=Feed.t
 type nonrec group=group
 type nonrec cursor=cursor
 let create ()=Feed.create ~decode ()
 let list t group ~after=Feed.list t (feed_url group) ~after
 let local_articles t group ~after ~previous =
  Feed.list_articles t (feed_url group) ~after
  |> Eio.Fiber.List.map ~max_fibers:4 (preserve_images ~download:Feed.download_url ~previous)
 let read t group id=Feed.read t (feed_url group) id
 let read_local t group id ~previous =
  read t group id |> preserve_images ~download:Feed.download_url ~previous
 let archive t group id=Feed.archive t (feed_url group) id
 let attachment t group id part path=Feed.attachment t (feed_url group) id part path

 let download_for (article:D.article) url =
  let saved=Option.bind article.message.html (fun html ->
    Option.bind (Html.embedded_image ~url html) Html.embedded_image_bytes) in
  match saved with Some bytes -> bytes | None -> Feed.download_url url
 let archive_local article=R.Article.archive ~download:(download_for article) article
 let attachment_local article=R.Article.attachment ~download:(download_for article) article
end
module Live=Make(R.Transport)
