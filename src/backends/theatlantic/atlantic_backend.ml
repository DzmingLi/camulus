module C=Reader.Content
module R=Rss_backend
module D=R.Decode
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
 let root=D.parse_xml body in
 if not(D.is D.atom "feed" root) then failwith "Atlantic did not return Atom";
 let entries=D.elements D.atom "entry" root in
 let articles=D.parse ~url body in
 List.map(fun (a:D.article) ->
  let image=match List.find_opt(fun n->D.value D.atom "id" n=a.message.id) entries with
    | None->""
    | Some entry -> D.children entry |> List.find_opt(fun n -> D.attr "" "url" n<>"" &&
        (D.is "http://search.yahoo.com/mrss/" "content" n || D.attr "" "medium" n="image" || String.starts_with ~prefix:"image/" (D.attr "" "type" n)))
      |> Option.fold ~none:"" ~some:(D.attr "" "url") in
  let html=(if image="" then "" else "<figure><img src=\""^Html.escape image^"\"></figure>")^
    Option.value a.message.html ~default:a.message.body in
  let tree=Html.parse html |> Html.clean ~base:a.message.url
    ~drop:(fun n -> Html.attr "data-id" n="injected-recirculation-link") |> Option.get in
  let enclosures=(List.map(fun (e:D.enclosure)->e.url)a.enclosures @ Html.media tree) |> List.sort_uniq String.compare |> List.mapi(fun part url ->
    {D.part;url;name=Uri.path(Uri.of_string url)|>Filename.basename|>Uri.pct_decode;size=0;mime=""}) in
  {D.message={a.message with title=String.trim(Html.text(Html.parse a.message.title));html=Some(Html.render tree);body=""};enclosures}) articles
module Make(H:R.Http.S)=struct
 module Feed=R.Make(H)
 type t=Feed.t
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ()=Feed.create ~decode ()
 let list t group ~after=Feed.list t (feed_url group) ~after
 let search t group ~query ~after=Feed.search t (feed_url group) ~query ~after
 let read t group id=Feed.read t (feed_url group) id
 let archive t group id=
   let a,files=Feed.archive t (feed_url group) id in
   {Reader.Content_state.message=a.message;attachments=D.attachments a;files;raw=None}
 let attachment t group id part path=Feed.attachment t (feed_url group) id part path
end
module Live=Make(R.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
