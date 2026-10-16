module S=Reader.Content_state
module M=Reader.Content
let check b msg=if not b then failwith msg
let rows=function `List xs -> xs | _ -> failwith "Expected rows"
let field k=function `Assoc xs -> List.assoc k xs | _ -> failwith "Expected object"
let integer=function `Int n -> n | _ -> failwith "Expected integer"
let msg ?parent ?thread ?activity id =
  {M.id;thread_id=thread;parent_id=parent;references=[];activity;title=id;author="author";body="saved body";
   html=None;url="https://example.test/"^id;link_url=None;created=1.;unavailable=false;score=0;my_vote=No_vote}
let archive message = {S.message;attachments=`List [];raw=Some "MIME\000binary\r\n";files=[]}
let () =
  let path=Filename.temp_file "reader-state-" ".sqlite3" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  let source="reddit:alice" and root=msg ~thread:"t3_root" ~activity:"0" "t3_root" in
  S.with_db path (fun db ->
    S.observe db ~source ~group:None [root];
    check (rows(S.feed db ~source ~group:"ocaml")=[]) "browsing is not subscription";
    S.observe db ~source ~group:(Some "ocaml") [root];
    check (List.length(rows(S.feed db ~source ~group:"ocaml"))=1) "explicit entry page creates cached row";
    S.mark_read db ~source [root.id] true;
    check (List.length(rows(S.feed db ~source ~group:"ocaml"))=1) "read does not remove feed entry";
    let revision=integer(field "revision" (List.hd(rows(S.feed db ~source ~group:"ocaml")))) in
    S.dismiss db ~source ~group:"ocaml" ~id:root.id ~through:revision;
    S.observe db ~source ~group:(Some "ocaml") [root];
    check (rows(S.feed db ~source ~group:"ocaml")=[]) "repeat fetch cannot resurrect dismissed update";
    S.observe db ~source ~group:(Some "ocaml") [{root with activity=Some "1"}];
    check (List.length(rows(S.feed db ~source ~group:"ocaml"))=1) "new activity reappears";
    S.observe db ~source ~group:(Some "other") [root];
    let other=List.hd(rows(S.feed db ~source ~group:"other")) in
    S.dismiss db ~source ~group:"other" ~id:root.id ~through:(integer(field "revision" other));
    check (List.length(rows(S.feed db ~source ~group:"ocaml"))=1) "dismiss is entry scoped";
    check (S.legacy_groups db=[]) "browsing creates no subscriptions";
    S.observe db ~source:"reddit:bob" ~group:None [root];
    check (field "read" (List.hd(rows(field "items" (S.snapshots db ~source:"reddit:bob" [root.id]))))=`Bool false)
      "read states are source scoped";
    let child=msg ~parent:root.id ~thread:root.id "t1_child" in
    S.observe db ~source ~group:None [child];
    S.save_archive db ~source ~target:S.Thread ~id:root.id ~folder:"work/ocaml" [archive root;archive child];
    S.save_archive db ~source ~target:S.Article ~id:child.id ~folder:"work/ocaml" [archive child];
    S.save_archive db ~source ~target:S.Article ~id:child.id ~folder:"keep" [archive child];
    check (List.length(rows(S.favorites db ~folder:"work/ocaml"))=2) "thread and article remain separate entries";
    check (rows(S.favorites db ~folder:"work")=[]) "folders show direct entries only";
    S.remove_folder db "work";
    check (S.folders db=["keep"]) "nested folder deletion";
    check (List.length(rows(S.load_archive db ~source ~target:S.Article ~id:child.id))=1) "other folder preserves archive";
    check (S.raw_archive db ~source ~id:child.id="MIME\000binary\r\n") "binary MIME survives storage";
    S.remove_favorite db ~source ~target:S.Article ~id:child.id ~folder:"keep";
    check (rows(S.load_archive db ~source ~target:S.Article ~id:child.id)=[]) "last membership releases archive";
    S.refresh_archive db ~source ~target:S.Thread ~id:root.id [archive root;archive child];
    check (rows(S.load_archive db ~source ~target:S.Article ~id:child.id)=[]) "late refresh cannot restore removed favorites";
    check (List.length(rows(S.feed db ~source ~group:"ocaml"))=1) "favorite removal does not dismiss feed";
    S.observe ~replace:true db ~source ~group:(Some "ocaml") [];
    check (rows(S.feed db ~source ~group:"ocaml")=[]) "remote removal leaves no stale cached row";
    S.observe ~replace:true db ~source ~group:(Some "ocaml") [root];
    let r=List.hd(rows(S.feed db ~source ~group:"ocaml")) in
    S.dismiss db ~source ~group:"ocaml" ~id:root.id ~through:(integer(field "revision" r));
    S.observe ~replace:true db ~source ~group:(Some "ocaml") [];
    S.observe ~replace:true db ~source ~group:(Some "ocaml") [root];
    check (rows(S.feed db ~source ~group:"ocaml")=[]) "eviction and refetch preserve dismissal");
  S.with_db path (fun db ->
    check (field "read" (List.hd(rows(field "items" (S.snapshots db ~source [root.id]))))=`Bool true)
      "read state survives reopen";
    let c=msg ~parent:"parent" "child" in
    S.observe db ~source:"mail:one" ~group:(Some "Inbox") [c];
    let row=List.hd(rows(S.feed db ~source:"mail:one" ~group:"Inbox")) in
    S.dismiss db ~source:"mail:one" ~group:"Inbox" ~id:"child" ~through:(integer(field "revision" row));
    S.observe db ~source:"mail:one" ~group:None [msg ~parent:"root" "parent";msg "root"];
    check (rows(S.feed db ~source:"mail:one" ~group:"Inbox")=[]) "late mail ancestry does not revive a removed entry";
    S.observe db ~source:"mail:one" ~group:(Some "Inbox") [msg ~parent:"root" "new-reply"];
    check (List.length(rows(S.feed db ~source:"mail:one" ~group:"Inbox"))=1) "new reply merges and revives mail conversation";
    let branch id parent = { (msg ~parent id) with references=["missing-root";parent] } in
    S.observe db ~source:"mail:references" ~group:(Some "Inbox") [branch "a" "unknown-a";branch "b" "unknown-b"];
    let matching=rows(S.feed db ~source:"mail:references" ~group:"Inbox") |> List.filter(fun r -> field "source" r=`String "mail:references") in
    check(List.length matching=1) "References merge branches without loading missing mail";
    S.observe db ~source:"order" ~group:(Some "group") [{(msg "newer") with created=20.}];
    S.observe db ~source:"order" ~group:(Some "group") [{(msg "older") with created=10.}];
    let ordered=rows(S.feed db ~source:"order" ~group:"group") |> List.filter(fun r -> field "source" r=`String "order") in
    check(field "id" (List.hd ordered)=`String "newer") "older pagination must not sort above newer content");
  let pid=Unix.fork() in
  if pid=0 then (S.with_db path(fun db -> S.mark_read db ~source [root.id] false);exit 0);
  let _,status=Unix.waitpid [] pid in check(status=Unix.WEXITED 0) "child database write";
  S.with_db path(fun db -> check(field "read" (List.hd(rows(field "items" (S.snapshots db ~source [root.id]))))=`Bool false)
    "another process observes committed state");
  check ((Unix.stat path).st_perm land 0o777=0o600) "private database permissions";
  S.with_db path (fun db -> S.save_archive db ~source ~target:S.Article ~id:root.id ~folder:"migration" [archive root]);
  let old=Sqlite3.db_open path in
  check (Sqlite3.exec old "DROP TABLE archive_files; DROP TABLE activities; DROP TABLE dismissals; DROP TABLE entry_members; ALTER TABLE legacy_subscriptions RENAME TO subscriptions; ALTER TABLE legacy_activities RENAME TO activities; ALTER TABLE legacy_dismissals RENAME TO dismissals; INSERT INTO subscriptions VALUES('old:source','old:group'); INSERT INTO activities VALUES(1,'old:source','old:item','old:revision',1.0); INSERT INTO dismissals VALUES('old:source','old:item',1); PRAGMA user_version=1"=Sqlite3.Rc.OK) "prepare v1 fixture";
  ignore(Sqlite3.db_close old);
  S.with_db path (fun db ->
    check (S.raw_archive db ~source ~id:root.id="MIME\000binary\r\n") "migration preserves existing MIME";
    check (S.legacy_groups db=["old:source","old:group"]) "legacy configuration remains exportable";
    check (rows(S.feed db ~source:"old:source" ~group:"old:group")=[]) "legacy groups do not populate feeds";
    S.observe db ~source:"old:source" ~group:(Some "old:group") [msg ~activity:"old:revision" "old:item"];
    check (rows(S.feed db ~source:"old:source" ~group:"old:group")=[]) "migration preserves previous dismissals";
    S.observe db ~source:"old:source" ~group:(Some "old:group") [msg ~activity:"new:revision" "old:item"];
    check (List.length(rows(S.feed db ~source:"old:source" ~group:"old:group"))=1) "new activity after migration reappears";
    check (S.attachment_archive db ~source ~id:root.id ~part:0=None) "v1 gains enclosure storage");
  let db=Sqlite3.db_open path in ignore(Sqlite3.exec db "PRAGMA user_version=999");ignore(Sqlite3.db_close db);
  (match S.with_db path(fun _ -> ()) with () -> failwith "future schema accepted" | exception Failure _ -> ());
  print_endline "PASS: persistent reads/feed, source isolation, deduplication, mail merging, nested multi-folder archives and schema guard"

let () =
  let path=Filename.temp_file "reader-home-" ".sqlite3" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  let source="forum" and root=msg ~thread:"thread" ~activity:"v1" "root" in
  let entries=[source,"replies";source,"watching";"rss:blog","url"] in
  S.with_db path (fun db ->
    S.observe db ~source ~group:(Some "replies") [root];
    S.observe db ~source:"rss:blog" ~group:(Some "url") [{(msg "post") with created=30.}];
    S.observe db ~source:"browse" ~group:(Some "latest") [msg "unselected"];
    let home=rows(S.timeline db ~entries) in
    check(List.length home=2) "home includes only selected backend feeds";
    check(field "id" (List.hd home)=`String "post") "home orders sources by activity time";
    let row=List.find(fun r -> field "source" r=`String source) home in
    S.dismiss_timeline db ~source ~id:"root" ~through:(integer(field "revision" row));
    check(List.length(rows(S.feed db ~source ~group:"replies"))=1) "home dismissal preserves group browsing";
    check(field "read" (List.hd(rows(field "items" (S.snapshots db ~source ["root"]))))=`Bool false) "home dismissal does not mark read";
    S.observe db ~source ~group:(Some "watching") [root];
    check(List.length(rows(S.timeline db ~entries))=1) "same update through another feed cannot revive dismissal";
    S.observe db ~source ~group:(Some "watching") [msg ~thread:"thread" ~parent:"root" "reply"];
    let home=rows(S.timeline db ~entries) in
    check(List.length home=2) "new reply revives home conversation";
    let row=List.find(fun r -> field "source" r=`String source) home in
    check(List.length(rows(field "groups" row))=2) "deduplicated thread retains both origins";
    S.observe db ~source:"mail" ~group:(Some "inbox") [msg ~parent:"missing" "mail-reply"];
    S.observe db ~source:"mail" ~group:(Some "mentions") [msg ~parent:"missing" "mail-other"];
    let mailentries=["mail","inbox";"mail","mentions"] in
    let home=rows(S.timeline db ~entries:mailentries) in
    check(List.length home=1) "mail conversations merge across feeds without native thread ID";
    S.dismiss_timeline db ~source:"mail" ~id:"mail-reply" ~through:(integer(field "revision" (List.hd home)));
    S.observe db ~source:"mail" ~group:None [msg ~parent:"earlier" "missing"];
    check(rows(S.timeline db ~entries:mailentries)=[]) "late mail ancestry preserves home dismissal");
  S.with_db path(fun db ->
    check(rows(S.timeline db ~entries:[source,"replies"])=[]) "home dismissal survives restart and entry selection changes");
  print_endline "PASS: unified home identity, ordering, independent dismissal, duplicate discovery, mail ancestry and persistence"

let () =
  let path=Filename.temp_file "reader-source-time-" ".sqlite3" in
  Fun.protect ~finally:(fun () -> Sys.remove path) @@ fun () ->
  let source="timestamps" and group="posts" in
  let old={ (msg ~activity:"v1" "old") with created=10. } in
  let newer={ (msg "newer") with created=20. } in
  let entries=[source,group] in
  S.with_db path (fun db ->
    S.observe db ~source ~group:(Some group) [old;newer];
    let oldrow=List.find(fun r -> field "id" r=`String "old") (rows(S.timeline db ~entries)) in
    S.dismiss_timeline db ~source ~id:"old" ~through:(integer(field "revision" oldrow));
    S.observe db ~source ~group:(Some group) [{old with activity=Some "v2"}];
    let home=rows(S.timeline db ~entries) in
    check(List.length home=2) "new revision still reappears independently of timestamp";
    check(field "id" (List.hd home)=`String "newer") "refetching changed old content must not promote it to now";
    let oldrow=List.find(fun r -> field "id" r=`String "old") home in
    check(field "occurred" oldrow=`Float 10.) "changed article retains source timestamp";
    check(field "occurred" (List.hd(rows(S.feed db ~source ~group)))=`Float 20.) "group also uses source time";
    S.dismiss_timeline db ~source ~id:"old" ~through:(integer(field "revision" oldrow));
    S.observe db ~source ~group:(Some group) [{old with activity=Some "v2";created=11.}];
    check(List.length(rows(S.timeline db ~entries))=1) "corrected source timestamp does not revive dismissed revision";
    S.observe db ~source ~group:(Some group) [{(msg ~thread:"thread" ~parent:"root" "reply") with created=30.}];
    check(field "occurred" (List.hd(rows(S.timeline db ~entries)))=`Float 30.) "reply sorts by its source timestamp";
    S.mark_read db ~source ["newer"] true;
    S.save_archive db ~source ~target:S.Article ~id:"old" ~folder:"keep" [archive old]);
  let db=Sqlite3.db_open path in
  check(Sqlite3.exec db "UPDATE activities SET occurred=1900000000; PRAGMA user_version=3"=Sqlite3.Rc.OK) "prepare old fetch-time cache";
  ignore(Sqlite3.db_close db);
  S.with_db path (fun db ->
    let home=rows(S.timeline db ~entries) in
    check(List.length home=2) "time migration preserves dismissals";
    check(field "occurred" (List.hd home)=`Float 30.) "time migration repairs old cache";
    check(field "read" (List.hd(rows(field "items" (S.snapshots db ~source ["newer"]))))=`Bool true) "time migration preserves read state";
    check(S.raw_archive db ~source ~id:"old"="MIME\000binary\r\n") "time migration preserves archives");
  print_endline "PASS: source timestamps, revision-independent order, timestamp corrections and v3 cache repair"
