module C=Reader.Content
module R=Rss_backend
module D=R.Decode
module Mime=Maildir_backend.Mime
let source="public-inbox"
type group=string
type cursor=string
let group url=
 let u=Uri.of_string(R.Http.validate url) in
 let parts=String.split_on_char '/' (Uri.path u) |> List.filter((<>)"") in
 match parts with
 | [list;message] when Uri.scheme u=Some "https" && Uri.query u=[] && Uri.fragment u=None &&
   String.for_all(function 'a'..'z'|'A'..'Z'|'0'..'9'|'_'|'-'->true|_->false)list ->
   let message=Uri.pct_decode message in
   if message="" || String.exists(fun c->Char.code c<=32 || List.mem c ['<';'>';'/';'\\'])message then invalid_arg "Invalid public-inbox Message-ID";
   let base=Uri.with_path u ("/"^list^"/") in
   Uri.to_string base, message
 | _->invalid_arg "Public-inbox expects an HTTPS archive/LIST/MESSAGE-ID URL"
let message_url base id=base^Uri.pct_encode id^"/"
let canonical url=let base,id=group url in message_url base id
let gunzip bytes=
 if not(String.starts_with ~prefix:"\031\139" bytes) then bytes else
 let path=Filename.temp_file "ecaml-public-inbox-" ".gz" in
 Fun.protect ~finally:(fun()->Sys.remove path)(fun()->
  let out=open_out_bin path in
  Fun.protect ~finally:(fun()->close_out_noerr out)(fun()->output_string out bytes);
  let input=Gzip.open_in path in
  Fun.protect ~finally:(fun()->Gzip.close_in input)(fun()->
   let result=Buffer.create 65536 and chunk=Bytes.create 65536 in
   let rec read()=let n=Gzip.input input chunk 0 (Bytes.length chunk) in
    if n>0 then begin if Buffer.length result+n>128*1024*1024 then failwith "Public-inbox expands beyond 128 MiB";
     Buffer.add_subbytes result chunk 0 n;read()end in
   read();Buffer.contents result))
type article={article:D.article;raw:string}
let parse_mbox ~url body=
 let base,_=group url in
 if String.length body>128*1024*1024 then failwith "Public-inbox mbox exceeds 128 MiB";
 let records=ref [] and current=Buffer.create 4096 and inside=ref false and header=ref true in
 let flush()=if !inside then (records:=Buffer.contents current::!records;Buffer.clear current) in
 String.split_on_char '\n' body |> List.iter(fun line->
  if String.starts_with ~prefix:"From mboxrd@z " line then begin
   flush();inside:=true;header:=true;
   if List.length !records>=10000 then failwith "Public-inbox thread exceeds 10000 messages"
  end else if !inside then begin
   let line=if !header then line else
    let rec quoted i=if i<String.length line && line.[i]='>' then quoted(i+1) else i in
    let i=quoted 0 in
    if i>0 && String.starts_with ~prefix:"From " (String.sub line i (String.length line-i)) then String.sub line 1 (String.length line-1) else line in
   if line="" || line="\r" then header:=false;
   Buffer.add_string current line;Buffer.add_char current '\n'
  end else if String.trim line<>"" then failwith "Public-inbox response is not mboxrd");
 flush();if !records=[] then failwith "Public-inbox returned no mail messages";
 let seen=Hashtbl.create 64 in
 List.rev !records |> List.filter_map(fun raw->
  let p=Mime.parse raw in
  if p.message_id="" then failwith "Public-inbox mail has no Message-ID";
  let id=message_url base p.message_id in
  if Hashtbl.mem seen id then None else begin Hashtbl.add seen id ();
   let references=Mime.ids p.references |> List.map(message_url base) in
   let parents=Mime.ids p.in_reply_to |> List.map(message_url base) in
   let parent_id=match List.rev(if parents=[] then references else parents) with []->None|p::_->Some p in
   let html=match Mime.body "text/html" p with ""->None|s->Some s in
   let message={C.id;thread_id=None;parent_id;references;title=p.subject;author=p.author;
    body=Mime.body "text/plain" p;html;url=id;link_url=None;created=D.date p.date;unavailable=false;
    score=0;my_vote=C.No_vote;activity=Some p.message_id} in
   let enclosures=List.map(fun(a:Mime.attachment)->{D.part=a.part;name=a.name;mime=a.mime;size=a.size;url=id})p.attachments in
   Some{article={D.message;enclosures};raw}
  end)
module Make(H:R.Http.S)=struct
 module Download=R.Make(H)
 type t={cache:(string,article)Hashtbl.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create()={cache=Hashtbl.create 128}
 let load t url=
  let url=canonical url in
  let bytes=Download.download_url ~limit:(64*1024*1024) (url^"t.mbox.gz") in
  let articles=Eio_unix.run_in_systhread(fun()->parse_mbox ~url (gunzip bytes)) in
  List.iter(fun a->Hashtbl.replace t.cache a.article.message.id a)articles;articles
 let list t group ~after=
  if after<>None then invalid_arg "Public-inbox thread mbox has no cursor";
  {C.messages=List.map(fun a->a.article.message)(load t group);notifications=[];after=None}
 let search t group ~query ~after=
  let page=list t group ~after in
  let query=String.lowercase_ascii query in
  let contains text=let text=String.lowercase_ascii text in
   let n=String.length query in let rec scan i=i+n<=String.length text && (String.sub text i n=query || scan(i+1)) in scan 0 in
  {page with messages=List.filter(fun (m:C.message)->contains(m.title^" "^m.author^" "^m.body^Option.value m.html ~default:""))page.messages}
 let read_record t id=let id=canonical id in match Hashtbl.find_opt t.cache id with Some a->a|None->
  ignore(load t id);match Hashtbl.find_opt t.cache id with Some a->a|None->failwith "Message absent from public-inbox thread"
 let read t _ id=(read_record t id).article
 let stream_thread t id ~emit_article ~emit=
  let articles=load t id in
  List.iter(fun a->emit_article a.article;
    emit{C.thread=id;messages=[a.article.message];gaps=[];complete=false})articles;
  emit{C.thread=id;messages=[];gaps=[];complete=true}
 let thread t id ~emit=stream_thread t id ~emit_article:(fun _->())~emit
 let archive_article t (a:D.article)=
  let record=read_record t a.message.id in
  let files=List.map(fun(e:D.enclosure)->e.part,Mime.extract record.raw e.part)a.enclosures in
  {Reader.Content_state.message=a.message;attachments=D.attachments a;raw=Some record.raw;files}
 let archive t group id=archive_article t (read t group id)
 let attachment t _ id part destination=
  let record=read_record t id in let bytes=Mime.extract record.raw part in
  R.Article.attachment ~download:(fun ~limit:_ _->bytes) record.article part destination
end
module Live=Make(R.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
module Threading : Reader.Backend.Threading with type t=Live.t = Live
