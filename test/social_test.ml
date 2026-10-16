module R=Rss_backend
module Z=Zhihu_backend
module B=Douban_backend
let check b why=if not b then failwith why
let fails f=try f();false with _->true
let ()=
 check(Z.Sign.header ~url:"https://www.zhihu.com/api/v4/members/alice/answers?limit=20&offset=0" ~dc0:"test-dc0" =
  "2.0_4Vc6LsMBPbmW8SBUJcnAsH9OLtAfbSHihvKxRN4SqG3bwu8=gTBbTpsGB5mP/aJv") "ZSE signature matches original Elisp vector";
 check(Z.parts "answers.marisa.moe"=("answers","people","marisa.moe")) "user IDs with dots";
 check(fails(fun()->ignore(Z.parts "answers.../escape"))) "Zhihu group validation";
 let queue=ref [] and requests=ref [] in
 let module H=struct let request r=requests:=r::!requests;match !queue with x::xs->queue:=xs;x|[]->failwith "Unexpected HTTP" end in
 let module Zh=Z.Make(H) in let module Db=B.Make(H) in
 let response body : R.Http.response={status=200;headers=[];body} in
 let answer={|{"id":12,"author":{"name":"Alice"},"question":{"id":7,"title":"Question"},"created_time":1700000000,"content":"<p>Full answer</p><img data-actualsrc='https://cdn.example/image.jpg' src='data:x'>"}|} in
 let status={|{"id":"12","author":{"id":"7","name":"Alice"},"create_time":"2026-10-16 09:00:00","text":"Status body","images":[{"large":{"url":"https://cdn.example/photo.jpg"}}]}|} in
 Eio_posix.run(fun _->
  let z=Zh.create ~cookies:(fun _->["d_c0","test-dc0"])() in
  queue:=[response("{\"data\": ["^answer^"],\"paging\":{\"is_end\":false,\"next\":\"https://evil.test\"}}")];
  let first=Zh.list z "answers.alice" ~after:None in
  check((List.hd first.messages).id="answer:12" && first.after<>None) "Zhihu stable answer and continuation";
  check(List.mem_assoc "x-zse-96" (List.hd !requests).headers) "signature sent";
  check(fails(fun()->ignore(Zh.list z "answers.bob" ~after:first.after))) "Zhihu cursor scoped to author";
  queue:=[response "{\"data\":[],\"paging\":{\"is_end\":true}}"];
  ignore(Zh.list z "answers.alice" ~after:first.after);
  check(Uri.host(Uri.of_string(List.hd !requests).url)=Some "www.zhihu.com") "untrusted paging URL never followed";
  queue:=[response "binary\000"];
  let a=Zh.archive z "answers.alice" "answer:12" in
  check(a.files=[0,"binary\000"]) "Zhihu actual-size images archived";
  check((List.hd !requests).headers=[]) "media downloads receive no cookies";
  let d=Db.create ~cookies:(fun _->["dbcl2","\"7:session\""])() in
  queue:=[response("{\"items\":[{\"status\":"^status^"}],\"total\":1}")];
  let p=Db.list d "timeline.7" ~after:None in
  check((List.hd p.messages).id="status:12" && (List.hd p.messages).thread_id=Some "status:12") "Douban has native discussion identity";
  let comment={|{"id":"20","text":"Reply","author":{"id":"8","name":"Bob"},"create_time":"2026-10-16 10:00:00","total_replies":2,"next_reply_start":1,"replies":[{"id":"21","text":"Child","author":{"name":"Carol"},"ref_comment":{"id":"20","text":"Snapshot"}}]}|} in
  queue:=[response status;
   response("{\"comments\": ["^comment^"],\"start\":0,\"count\":20,\"total\":1}");
   response {|{"replies":[{"id":"22","text":"More","author":{"name":"Dave"},"ref_comment":{"id":"21","text":"Snapshot"}}],"start":1,"count":20,"total":2}|}];
  let batches=ref [] and articles=ref [] in
  Db.stream_thread d "status:12" ~emit_article:(fun a->articles:=a::!articles) ~emit:(fun b->batches:=b::!batches);
  check((List.hd !batches).complete) "Douban complete only after nested pages";
  check(List.length !articles=4) "reference snapshots never overwrite full replies";
  let child=List.find(fun(a:R.Decode.article)->a.message.id="status:12/comment:22")!articles in
  check(child.message.parent_id=Some "status:12/comment:21") "reply parent reconstructed";
  check(B.date "2026-10-16 09:00:00"=R.Decode.date "2026-10-16T01:00:00Z") "Douban Shanghai timestamps";
  queue:=[response status;response {|{"comments":[],"total":50}|}];
  let complete=ref false in
  check(fails(fun()->Db.thread d "status:12" ~emit:(fun b->complete:=b.complete))) "empty non-final comment page fails";
  check(not !complete) "incomplete discussion cannot be archived as complete";
  queue:=[{R.Http.status=302;headers=["location","https://evil.test/steal"];body=""}];
  check(fails(fun()->ignore(Db.list d "timeline.7" ~after:None))) "cross-domain credential redirect rejected";
  check(fails(fun()->ignore(Db.list d "replies.8" ~after:None))) "reply inbox must match logged-in account";
  queue:=[response "<html>Sign in</html>"];
  check(fails(fun()->ignore(Db.list d "replies.7" ~after:None))) "login page is not empty inbox";
  queue:=[response {|<html><div class="notification-list"></div></html>|}];
  check((Db.list d "replies.7" ~after:None).messages=[]) "authenticated empty inbox layout";
  queue:=[response {|<div class="new-reply-item" id="reply_notify_10"><div class="content"><a href="/notification/reply_notify?id=10">Reply</a></div></div>|};
   {R.Http.status=302;headers=["location","https://m.douban.com/status/12/"];body=""};
   response "<html>Status page</html>";response status];
  let p=Db.list d "replies.7" ~after:None in
  check((List.hd p.messages).id="status:12" && (List.hd p.messages).activity=Some "reply_notify_10") "reply notification resolves to canonical thread";
  let profile=Filename.temp_file "browser-cookies" "" in Sys.remove profile;Unix.mkdir profile 0o700;
  let path=Filename.concat profile "cookies.sqlite" in
  let db=Sqlite3.db_open path in
  ignore(Sqlite3.exec db "CREATE TABLE moz_cookies(host TEXT,path TEXT,name TEXT,value TEXT,expiry INTEGER,originAttributes TEXT,creationTime INTEGER DEFAULT 0)");
  List.iter(fun(host,path,name,expiry,origin)->let st=Sqlite3.prepare db "INSERT INTO moz_cookies(host,path,name,value,expiry,originAttributes) VALUES(?,?,?,'secret',?,?)" in
   ignore(Sqlite3.bind_values st [TEXT host;TEXT path;TEXT name;INT expiry;TEXT origin]);ignore(Sqlite3.step st);ignore(Sqlite3.finalize st))
   [".zhihu.com","/","domain",9999999999L,"";"www.zhihu.com","/api","path",9999999999L,"";
    ".evilzhihu.com","/","evil",9999999999L,"";".zhihu.com","/","expired",1L,"";
    ".zhihu.com","/","container",9999999999L,"^userContextId=1"];
  ignore(Sqlite3.db_close db);
  let cookies=Reader.Browser_cookies.get ~profile "https://www.zhihu.com/api/v4/x" in
  check(List.map fst cookies=["path";"domain"]) "cookie expiry, domain, path and Firefox container scope";
  check(List.map fst(Reader.Browser_cookies.get ~profile "https://www.zhihu.com/apix")=["domain"]) "cookie path boundary";
  let locked=Sqlite3.db_open path in
  ignore(Sqlite3.exec locked "PRAGMA journal_mode=WAL; PRAGMA locking_mode=EXCLUSIVE; BEGIN EXCLUSIVE; INSERT INTO moz_cookies(host,path,name,value,expiry,originAttributes) VALUES('.zhihu.com','/','new-login','session',9999999999,''); COMMIT");
  check(List.mem_assoc "new-login" (Reader.Browser_cookies.get ~profile "https://www.zhihu.com/")) "locked Firefox WAL snapshot sees latest login";
  ignore(Sqlite3.db_close locked);
  Array.iter(fun name->Sys.remove(Filename.concat profile name))(Sys.readdir profile);Unix.rmdir profile);
 print_endline "PASS: Zhihu signatures and pages; Douban timeline, nested trees, reply inbox; credential isolation and archives"
