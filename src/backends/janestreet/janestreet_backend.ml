module C=Reader.Content
module R=Rss_backend
module D=R.Decode
module Html=Reader.Html
let source="janestreet"
type group=string
type cursor=string
let feed_url="https://blog.janestreet.com/feed.xml"
let validate_group = function "posts"->() | _->invalid_arg "Jane Street expects posts"
let post_url url =
 ignore(R.Http.validate url);
 let u=Uri.of_string url in
 if Uri.scheme u<>Some "https" || Uri.host u<>Some "blog.janestreet.com" || Uri.port u<>None then
   invalid_arg "Jane Street post must belong to blog.janestreet.com";
 url
let decode ~url body =
 D.parse ~url body |> List.map(fun (a:D.article) ->
   let id=post_url a.message.url in
   {a with message={a.message with id;author=(if a.message.author="" then "Jane Street" else a.message.author)}})
let full_post (a:D.article) html =
 let document=Html.parse html in
 let article=Html.find(fun n -> Html.tag n="article" && Option.is_some(Html.find(Html.has_class "post-content")n))document
   |> Option.value ~default:(Html.Text "") in
 let post=match Html.find(Html.has_class "post-content")article with Some n->n | None->failwith "Jane Street page has no post-content" in
 let hero=Html.find(Html.has_class "featimg-wrapper")article in
 let authors=Html.select(Html.has_class "name")article |> List.concat_map(Html.select(fun n->Html.tag n="a"))
   |> List.map(fun n->String.trim(Html.text n)) |> List.filter((<>)"") |> List.sort_uniq String.compare in
 let nodes=Option.to_list hero @ [post] in
 let tree=Html.Element("root",[],nodes) |> Html.clean ~base:a.message.url |> Option.get in
 if String.trim(Html.text tree)="" then failwith "Jane Street returned an empty post";
 let enclosures=Html.media tree |> List.mapi(fun part url ->
   {D.part;url;name=Uri.path(Uri.of_string url)|>Filename.basename|>Uri.pct_decode;size=0;mime=""}) in
 {D.message={a.message with html=Some(Html.render tree);body="";
   author=(if authors=[] then a.message.author else String.concat ", " authors)};enclosures}
module Make(H:R.Http.S)=struct
 module Feed=R.Make(H)
 type t={feed:Feed.t;full:(string,D.article) Hashtbl.t;lock:Eio.Mutex.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ()={feed=Feed.create ~decode ();full=Hashtbl.create 64;lock=Eio.Mutex.create ()}
 let list t group ~after=validate_group group;Feed.list t.feed feed_url ~after
 let search t group ~query ~after=validate_group group;Feed.search t.feed feed_url ~query ~after
 let read t group id =
   validate_group group;ignore(post_url id);
   let a=Feed.read t.feed feed_url id in
   Eio.Mutex.lock t.lock;
   Fun.protect ~finally:(fun ()->Eio.Mutex.unlock t.lock) (fun () ->
     match Hashtbl.find_opt t.full id with
     | Some cached when cached.message.activity=a.message.activity -> cached
     | _ -> let html=Feed.download_url ~limit:(16*1024*1024) (post_url a.message.url) in
       let full=full_post a html in Hashtbl.replace t.full id full;full)
 let archive t group id =
   let a=read t group id in
   let total=ref 0 in
   let files=List.map(fun (e:D.enclosure)->
     let data=Feed.download_url ~limit:(128*1024*1024 - !total) e.url in
     total:= !total+String.length data;
     if !total>128*1024*1024 then failwith "Jane Street attachments exceed 128 MiB";
     e.part,data)a.enclosures in
   {Reader.Content_state.message=a.message;attachments=D.attachments a;raw=None;files}
 let attachment t group id part destination =
   let a=read t group id in
   let e=match List.find_opt(fun (e:D.enclosure)->e.part=part)a.enclosures with Some e->e | None->invalid_arg "Unknown attachment" in
   let data=Feed.download_url e.url in
   Eio_unix.run_in_systhread(fun ()->
     let fd=Unix.openfile destination [Unix.O_CREAT;Unix.O_EXCL;Unix.O_WRONLY;Unix.O_CLOEXEC] 0o600 in
     let ch=Unix.out_channel_of_descr fd in
     try output_string ch data;flush ch;Unix.fsync fd;close_out ch
     with ex->close_out_noerr ch;(try Sys.remove destination with _->());raise ex)
end
module Live=Make(R.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
