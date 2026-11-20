module C=Reader.Content
module R=Rss_backend
module D=R.Decode
module Xml=R.Feed_xml
module Html=Reader.Html
let source="atlantic"
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
      let tree=if Html.find is_cover tree=None then
        clean ("<figure><img src=\"" ^ Html.escape image ^ "\"></figure>" ^ html)
        else tree in
      Option.iter (Html.set_attr "data-camulus-image-url" image) (Html.find is_cover tree);
      tree
    | _ -> tree in
  let enclosures=(List.map(fun (e:D.enclosure)->e.url)a.enclosures @ Html.media tree) |> List.sort_uniq String.compare |> List.mapi(fun part url ->
    {D.part;url;name=Uri.path(Uri.of_string url)|>Filename.basename|>Uri.pct_decode;size=0;mime=""}) in
  {D.message={a.message with title=String.trim(Html.text(Html.parse a.message.title));html=Some(Html.render tree);body=""};enclosures}) articles
let cover tree =
 Html.find (fun node -> Html.tag node="img" && Html.attr "data-camulus-image-url" node<>"") tree
let saved_image html url =
 Html.embedded_image ~url (Html.parse html) |> Option.map (fun node ->
   Html.attr "src" node,Html.attr "data-camulus-cover-revision" node)
let preserve_cover ~download ~previous (article:D.article) =
 let html=Option.get article.message.html in
 let tree=Html.parse html in
 match cover tree with
 | None -> article
 | Some node ->
   let url=Html.attr "data-camulus-image-url" node in
   let revision=Option.value article.message.activity ~default:"" in
   let saved=Option.bind (previous article.message.id) (fun cached ->
    let open Yojson.Safe.Util in
    match cached |> member "message" |> member "html" with
    | `String html -> saved_image html url
    | _ -> None) in
   let image=match saved with
    | Some (_,cached_revision) when cached_revision=revision -> saved
    | _ ->
      (try match Html.image_data_uri (download url) with
       | Some data -> Some(data,revision)
       | None -> saved
       with Failure _ | R.Http.Transport_error _ -> saved) in
   Option.iter (fun (data,revision) ->
     Html.set_attr "src" data node;
     Html.set_attr "data-camulus-cover-revision" revision node) image;
   {article with message={article.message with html=Some(Html.render tree)}}
module Make(H:R.Http.S)=struct
 module Feed=R.Make(H)
 type t=Feed.t
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ()=Feed.create ~decode ()
 let list t group ~after=Feed.list t (feed_url group) ~after
 let local_articles t group ~after ~previous =
  Feed.list_articles t (feed_url group) ~after
  |> Eio.Fiber.List.map ~max_fibers:4 (preserve_cover ~download:Feed.download_url ~previous)
 let read t group id=Feed.read t (feed_url group) id
 let read_local t group id ~previous =
  read t group id |> preserve_cover ~download:Feed.download_url ~previous
 let archive t group id=Feed.archive t (feed_url group) id
 let attachment t group id part path=Feed.attachment t (feed_url group) id part path

 let download_for (article:D.article) url =
  let saved=Option.bind article.message.html (fun html ->
    Option.bind (Html.embedded_image ~url (Html.parse html)) Html.embedded_image_bytes) in
  match saved with Some bytes -> bytes | None -> Feed.download_url url
 let archive_local article=R.Article.archive ~download:(download_for article) article
 let attachment_local article=R.Article.attachment ~download:(download_for article) article
end
module Live=Make(R.Transport)
