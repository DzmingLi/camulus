module Mime = Maildir_backend.Mime
let check b s = if not b then failwith s
let rejected f = try f (); false with Failure _ | Invalid_argument _ -> true
let crlf s = String.concat "\r\n" (String.split_on_char '\n' s)
let () =
  let raw=crlf "From: Test <test@example.test>\nTo: Team: a@example.test, b@example.test;\nSubject: =?UTF-8?B?5rWL?=\n =?UTF-8?B?6K+V?=\nMessage-ID: <id@example.test>\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=outer\n\n--outer\nContent-Type: text/plain; charset=UTF-8\n\nOuter body\n--outer\nContent-Type: application/octet-stream\nContent-Disposition: attachment; filename*0*=UTF-8''%E9%99%84;\n filename*1*=%E4%BB%B6.bin\nContent-ID: <binary@test>\nContent-Transfer-Encoding: base64\n\nAAECAw==\n--outer\nContent-Type: message/rfc822\nContent-Disposition: attachment\n\nFrom: nested@example.test\nTo: other@example.test\nMessage-ID: <nested@example.test>\nContent-Type: text/plain; charset=UTF-8\n\nInner body\n--outer--\n" in
  let p=Mime.parse raw in
  check(p.subject="测试") "folded encoded subject";
  check(p.message_id="id@example.test") "outer identity retained";
  check(not(String.contains (Mime.body "text/plain" p) 'I')) "nested body stays an attachment";
  check(List.length p.attachments=2) "two attachments";
  let a=List.hd p.attachments and nested=List.nth p.attachments 1 in
  check(a.name="附件.bin" && a.cid="binary@test") "RFC2231 continuation and CID";
  check(Mime.extract raw a.part="\000\001\002\003") "binary attachment octets";
  check((Mime.parse(Mime.extract raw nested.part)).message_id="nested@example.test") "attached message can reopen";
  let from,to_,cc=Mime.envelope raw in
  check(from=["test@example.test"] && to_=["a@example.test";"b@example.test"] && cc=[]) "envelope groups";
  check(rejected(fun()->ignore(Mime.envelope("Bcc: secret@example.test\r\n"^raw)))) "Bcc rejected";
  let body="From: a@example.test\nTo: b@example.test\nSubject: 草稿\n\n未完成正文\n" in
  check((Mime.parse body).subject="草稿") "LF draft and UTF8 subject";
  let binary="Content-Type: application/octet-stream\r\nContent-Transfer-Encoding: binary\r\nContent-Disposition: attachment; filename=raw.bin\r\n\r\n\000\r\n\n\r\255" in
  let a=List.hd (Mime.parse binary).attachments in
  check(Mime.extract binary a.part="\000\r\n\n\r\255") "raw binary line endings preserved";
  let long_id="40.4E.12345.0ABCDEF@i-12345678901234567.mta1vrest.sd.prd.example" in
  check((Mime.parse("Message-ID: <"^long_id^">\r\n\r\nbody")).message_id=long_id)
    "long Message-ID identity is not changed by header folding";
  let long=String.concat "" (List.init 80 (fun _->"测试")) in
  let rendered=Mime.compose ~sender:"a@example.test" ~to_:["b@example.test"] ~cc:[]
    ~subject:long ~date:"Thu, 15 Oct 2026 10:00:00 +0000" ~message_id:"out@test" ~references:[] ~body:".hello\n.\n" in
  check((Mime.parse rendered).subject=long) "long Unicode subject roundtrip";
  List.iter(fun line->check(String.length line<=998) "RFC line length") (String.split_on_char '\n' rendered);
  print_endline "PASS Mrmime folded headers, RFC2231, embedded mail, address groups, LF drafts and binary integrity"
