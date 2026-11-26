module C=Reader.Content
module Html=Reader.Html
let of_html (message:C.message) html =
 let tree=Html.parse html |> Html.clean ~base:message.url |> Option.get in
 {Decode.message={message with html=Some(Html.render tree);body=""};enclosures=[]}
let archive ~download (a:Decode.article) =
 let files=List.map(fun (e:Decode.enclosure)->e.part,download e.url)a.enclosures in
 {Reader.Content_state.message=a.message;attachments=Decode.attachments a;raw=None;files}
(* Standalone publication archives keep inline pictures in their HTML. *)
let archive_html ~download (article:Decode.article) =
 let message=article.message in
 let html=Option.map (fun source ->
   let tree=Html.parse source in
   let images=Hashtbl.create 8 in
   Html.select (fun node -> Html.tag node="img") tree |> List.iter (fun node ->
     let url=Html.attr "src" node in
     match Html.safe_link ~base:message.url "src" url with
     | Some url when url<>"" && Html.attr "src" node<>"" ->
       let data=match Hashtbl.find_opt images url with
         | Some cached -> cached
         | None -> let data=Html.image_data_uri (download url) in
           Hashtbl.add images url data;data in
       Option.iter (fun data -> Html.set_attr "src" data node) data
     | _ -> ());
   Html.render tree) message.html in
 archive ~download {article with message={message with html}}
let attachment ~download (a:Decode.article) part destination =
 let e=match List.find_opt(fun (e:Decode.enclosure)->e.part=part)a.enclosures with Some e->e|None->invalid_arg "Unknown attachment" in
 let bytes=download e.url in
 Eio_unix.run_in_systhread(fun()->
  let fd=Unix.openfile destination [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_EXCL;Unix.O_CLOEXEC] 0o600 in
  let ch=Unix.out_channel_of_descr fd in
  try output_string ch bytes;flush ch;Unix.fsync fd;close_out ch
  with ex->close_out_noerr ch;(try Sys.remove destination with _->());raise ex)
