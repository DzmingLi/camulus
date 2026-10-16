module B=Maildir_backend
module M=Reader.Content
let check b s=if not b then failwith s
let expect_error f=try ignore(f());failwith "expected Mail error" with B.Error _ -> ()
let write path s=let ch=open_out_bin path in output_string ch s;close_out ch
let raw="From: Alice <alice@example.test>\r\nTo: Bob <bob@example.test>\r\nMessage-ID: <root@test>\r\nSubject: =?UTF-8?B?5rWL6K+V?=\r\nMIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=x\r\n\r\n--x\r\nContent-Type: text/plain; charset=iso-8859-1\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\ncaf=E9\r\n--x\r\nContent-Type: application/octet-stream\r\nContent-Disposition: attachment; filename=\"../file.bin\"\r\nContent-Transfer-Encoding: base64\r\n\r\nYQBiCg==\r\n--x--\r\n"
let json_header path id refs date folder = `Assoc[
  "path",`String path;"message-id",`String id;"references",`List(List.map(fun s -> `String s) refs);
  "date-unix",`Int date;"subject",`String "same subject";"maildir",`String folder;
  "from",`List[`Assoc["email",`String "alice@example.test"]]]
let () =
  let parsed=B.Mime.parse raw in
  check (parsed.subject="测试") "decoded subject";
  check (String.starts_with ~prefix:"café" (B.Mime.body "text/plain" parsed)) "charset/QP";
  check (List.length parsed.attachments=1) "attachment excluded from body";
  let a=List.hd parsed.attachments in
  check (a.name="../file.bin" && a.size=4) "attachment metadata";
  check (B.Mime.extract raw a.part="a\000b\n") "binary attachment";
  let gc=Gc.get () in
  Gc.set {gc with minor_heap_size=4096};
  for i=1 to 300 do
    let p=B.Mime.parse raw in
    check(p.subject="测试" && List.length p.attachments=1) "MIME roots survived GC";
    if i mod 20=0 then Gc.full_major ()
  done;
  Gc.set gc;
  print_endline "PASS MIME charset, encoded headers, binary attachment";
  let root=Filename.temp_file "mail-unit-" "" in Unix.unlink root;Unix.mkdir root 0o700;
  let path=Filename.concat root "mail" in write path raw;
  Fun.protect ~finally:(fun () -> Array.iter(fun f -> Unix.unlink(Filename.concat root f))(Sys.readdir root);Unix.rmdir root) @@ fun () ->
  let rows=[json_header path "root@test" [] 1 "/Inbox";
    json_header path "reply@test" ["root@test"] 2 "/Inbox";
    json_header path "separate@test" [] 3 "/Inbox";
    json_header path "outside@test" [] 4 "/Other"] in
  let calls=ref [] in
  let run ?(input="") argv = calls := (argv,input)::!calls;
    {Maildir_backend.status=0;stdout=Yojson.Safe.to_string (`List rows)} in
  let config={ (B.config ~root ~muhome:root ~sender:"me@example.test") with page_size=1 } in
  let unused ~profile:_ ~sender:_ ~recipients:_ _ = failwith "unexpected SMTP" in
  let b=B.create ~config ~run ~submit:unused in
  let p=B.list b "/Inbox" ~after:None in
  check (List.length p.messages=1 && (List.hd p.messages).id="mail:separate@test") "group-local newest summary";
  let p2=B.list b "/Inbox" ~after:p.after in
  check ((List.hd p2.messages).id="mail:reply@test" && p2.after=None) "thread merge, stable pagination";
  check (List.length !calls=1) "pagination uses snapshot";
  expect_error(fun () -> B.search b "/Other" ~query:"" ~after:p.after);
  ignore(B.search b "/Inbox" ~query:"needle OR subject:other" ~after:None);
  let argv,_=List.hd !calls in
  check (List.hd(List.rev argv)="(maildir:\"/Inbox\") AND (needle OR subject:other)") "query scoped";
  print_endline "PASS grouping, no subject-only merge, scope, cursor isolation";
  let article=B.read b "mail:root@test" in
  check (article.message.thread_id=None) "no invented native thread id";
  let destination=Filename.concat root "download" in
  B.save_attachment b "mail:root@test" a.part destination;
  let ch=open_in_bin destination in let data=really_input_string ch 4 in close_in ch;
  check(data="a\000b\n") "saved binary data";
  check((Unix.stat destination).Unix.st_perm land 0o777=0o600) "private attachment mode";
  (try B.save_attachment b "mail:root@test" a.part destination;failwith "overwrote attachment"
   with Unix.Unix_error(Unix.EEXIST,_,_) -> ());
  print_endline "PASS lazy MIME reading and explicit non-overwriting attachment save";
  let draft={B.to_=["you@example.test"];cc=[];subject="中文主题";body="你好\n";reply_to=None} in
  let _,payload=B.prepare b draft in
  let roundtrip=B.Mime.parse payload in
  check(roundtrip.subject=draft.subject && String.starts_with ~prefix:draft.body (B.Mime.body "text/plain" roundtrip)) "outgoing MIME roundtrip";
  expect_error(fun () -> B.prepare b {draft with subject="ok\r\nBcc: injected@example.test"});
  expect_error(fun () -> B.prepare b {draft with to_=["--bad"]});
  let sent_calls=ref 0 in
  let fail_submit ~profile:_ ~sender:_ ~recipients:_ _ = incr sent_calls;failwith "uncertain" in
  let failing=B.create ~config ~run ~submit:fail_submit in
  expect_error(fun () -> B.send failing draft);
  check(!sent_calls=1) "no automatic SMTP retry";
  let captured=ref None in
  let submit ~profile ~sender ~recipients input = captured:=Some(profile,sender,recipients,input) in
  ignore(B.send (B.create ~config ~run ~submit) draft);
  let profile,sender,recipients,input=Option.get !captured in
  check(profile="" && sender="me@example.test" && recipients=draft.to_ && input<>"") "native SMTP envelope and MIME";
  print_endline "PASS MIME sending, injection rejection, exactly one submit attempt"
