module C=Reader.Content
module Html=Reader.Html
let of_html (message:C.message) html =
 let tree=Html.parse html |> Html.clean ~base:message.url |> Option.get in
 let urls=Html.media tree in
 if List.length urls>1000 then failwith "Article has too many attachments";
 let enclosures=List.mapi(fun part url ->
  {Decode.part;url;name=Uri.path(Uri.of_string url)|>Filename.basename|>Uri.pct_decode;size=0;mime=""})urls in
 {Decode.message={message with html=Some(Html.render tree);body=""};enclosures}
let archive ~download (a:Decode.article) =
 let total=ref 0 in
 let files=List.map(fun (e:Decode.enclosure)->let bytes=download ~limit:(128*1024*1024 - !total) e.url in
  total:= !total+String.length bytes;if !total>128*1024*1024 then failwith "Archive media exceed 128 MiB";e.part,bytes)a.enclosures in
 {Reader.Content_state.message=a.message;attachments=Decode.attachments a;raw=None;files}
let attachment ~download (a:Decode.article) part destination =
 let e=match List.find_opt(fun (e:Decode.enclosure)->e.part=part)a.enclosures with Some e->e|None->invalid_arg "Unknown attachment" in
 let bytes=download ~limit:(64*1024*1024) e.url in
 Eio_unix.run_in_systhread(fun()->
  let fd=Unix.openfile destination [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_EXCL;Unix.O_CLOEXEC] 0o600 in
  let ch=Unix.out_channel_of_descr fd in
  try output_string ch bytes;flush ch;Unix.fsync fd;close_out ch
  with ex->close_out_noerr ch;(try Sys.remove destination with _->());raise ex)
