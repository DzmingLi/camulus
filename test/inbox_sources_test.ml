module R=Rss_backend
module P=Public_inbox_backend
module N=Netease_backend
let check b why=if not b then failwith why
let fails f=try f();false with _->true
let root="From mboxrd@z Thu Jan  1 00:00:00 1970\nFrom: Alice <a@example.org>\nSubject: Topic\nDate: Fri, 01 Mar 2024 20:34:35 +0000\nMessage-ID: <root@example.org>\nContent-Type: text/plain; charset=utf-8\n\nBody\n>From quoted\n>>From double\n"
let reply="From mboxrd@z Thu Jan  1 00:00:00 1970\nFrom: Bob <b@example.org>\nSubject: Re: Topic\nMessage-ID: <reply@example.org>\nReferences: <root@example.org>\nIn-Reply-To: <root@example.org>\nContent-Type: multipart/mixed; boundary=xx\n\n--xx\nContent-Type: text/plain\n\nReply body\n--xx\nContent-Type: application/octet-stream\nContent-Disposition: attachment; filename=bytes.bin\nContent-Transfer-Encoding: base64\n\nQQBC\n--xx--\n"
let ()=
 let url="https://list.example.org/list/root@example.org/" in
 let articles=P.parse_mbox ~url (root^reply) in
 let a=List.hd articles and b=List.nth articles 1 in
 check(b.article.message.parent_id=Some a.article.message.id && b.article.message.thread_id=None) "Public-inbox reconstructs mail relations rather than assigning thread identity";
 check(Maildir_backend.Mime.extract b.raw 2="A\000B") "native MIME binary attachment";
 check(String.ends_with ~suffix:"Body\nFrom quoted\n>From double\n" a.raw) "mboxrd unquotes one level";
 List.iter(fun url->check(fails(fun()->ignore(P.group url))) "invalid public-inbox URL rejected")
  ["http://host/list/id";"https://host/list/../../etc/passwd";"https://host/list/id?q=secret"];
 check(fails(fun()->ignore(P.parse_mbox ~url "<html>challenge</html>"))) "HTML cannot masquerade as mail";
 let path=Filename.temp_file "public-inbox" ".gz" in let oc=Gzip.open_out path in Gzip.output_substring oc (root^reply) 0 (String.length(root^reply));Gzip.close_out oc;
 let ic=open_in_bin path in let zipped=really_input_string ic(in_channel_length ic) in close_in ic;Sys.remove path;
 check(P.gunzip zipped=root^reply) "native gzip verifies and decompresses mbox";
 check(fails(fun()->ignore(P.gunzip(String.sub zipped 0 (String.length zipped-4))))) "truncated gzip rejected";
 let queue=ref [] and requests=ref [] in
 let module H=struct let request r=requests:=r::!requests;match !queue with x::xs->queue:=xs;x|[]->failwith "Unexpected network" end in
 let module M=P.Make(H) in let module E=N.Make(H) in
 let response body : R.Http.response={status=200;headers=[];body} in
 Eio_posix.run(fun _->
  let p=M.create() in queue:=[response zipped];let page=M.list p url ~after:None in
  check(List.length page.messages=2) "public-inbox list";
  let archived=M.archive p url b.article.message.id in
  check(archived.files=[2,"A\000B"] && archived.raw<>None) "public-inbox stores complete MIME and attachments";
  queue:=[response zipped];let complete=ref false in
  M.thread p a.article.message.id ~emit:(fun b->complete:=b.complete);
  check(!complete) "public-inbox expanded thread";
  let e=E.create() in
  queue:=[response {|{"code":200,"events":[{"id":42,"eventTime":1700000000000,"user":{"userId":7,"nickname":"Alice"},"json":"{\"msg\":\"Listen\",\"song\":{\"id\":123,\"name\":\"Song\",\"artists\":[{\"name\":\"Artist\"}]}}","pics":[{"originUrl":"http://cdn.example/photo.jpg"}]}],"more":true,"lasttime":1699999999000}|}];
  let first=E.list e "events.7" ~after:None in
  check((List.hd first.messages).id="event:42" && first.after<>None) "NetEase stable events and continuation";
  let article=E.read e "events.7" "event:42" in
  check((List.hd article.enclosures).url="https://cdn.example/photo.jpg") "NetEase media use HTTPS";
  queue:=[response "image"];
  check((E.archive e "events.7" "event:42").files=[0,"image"]) "NetEase pictures archived";
  check((List.hd !requests).headers=[]) "no credentials or referer on image download";
  check(fails(fun()->ignore(E.list e "events.8" ~after:first.after))) "NetEase cursor is user scoped";
  queue:=[response {|{"code":200,"events":[],"more":true,"lasttime":1699999999000}|}];
  check(fails(fun()->ignore(E.list e "events.7" ~after:first.after))) "non-progressing NetEase cursor rejected");
 print_endline "PASS: NetEase event pagination/media; public-inbox gzip, mboxrd, MIME, reply relations and archives"
