module C = Reader.Content
module S = Reader.Content_state
let check condition message = if not condition then failwith message
let message ?thread ?parent id body : C.message =
 {id;thread_id=thread;parent_id=parent;references=[];title=id;author="Fixture";
  body;html=None;url="https://example.test/"^id;link_url=None;created=1.;
  unavailable=false;score=0;my_vote=C.No_vote;activity=None}
let article m = `Assoc["message",S.message_json m;"attachments",`List []]
let field name value = Yojson.Safe.Util.member name value
let body article = field "body" (field "message" article)
let cached db ?(source="fixture") ?(target=S.Article) id = S.cached_content db ~source ~target ~id
let with_database f =
 let path=Filename.temp_file "camulus-cache" ".sqlite3" in
 Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)
let persist_and_keep_state_separate () = with_database(fun path ->
 let m=message "one" "persistent body" in
 S.with_db path (fun db ->
  S.observe db ~source:"fixture" ~group:(Some "feed") [m];
  S.cache_article db ~source:"fixture" (article m));
 S.with_db path (fun db ->
  let articles,complete=cached db "one" in
  check (complete && List.map body articles=[`String "persistent body"]) "Body lost across reopen";
  check (S.memberships db=[]) "Caching unexpectedly favorited an item";
  let states=S.snapshots db ~source:"fixture" ["one"] in
  let state=List.hd(Yojson.Safe.Util.to_list(field "items" states)) in
  check (field "read" state=`Bool false) "Caching unexpectedly marked an item read";
  S.mark_read db ~source:"fixture" ["one"] true;
  S.observe db ~replace:true ~source:"fixture" ~group:(Some "feed") [{m with body="summary only"}];
  check (List.map body (fst(cached db "one"))=[`String "persistent body"]) "Page headers overwrote a body"))
let favorite_resources_and_removal () = with_database(fun path ->
 S.with_db path (fun db ->
  let m=message "one" "body" in
  S.observe db ~source:"fixture" ~group:None [m];
  S.cache_article db ~source:"fixture" (article m);
  check (not(S.copy_favorite db ~source:"fixture" ~target:S.Article ~id:"one" ~folder:"saved"))
   "A body-only cache incorrectly claimed complete archived resources";
  S.save_archive db ~source:"fixture" ~target:S.Article ~id:"one" ~folder:"saved/child"
   [{message=m;attachments=`List [];raw=Some "raw MIME";files=[0,"attachment bytes"]}];
  S.cache_article db ~source:"fixture" (article {m with body="incomplete replacement"});
  check (List.map body (fst(cached db "one"))=[`String "body"]) "Read replaced archived payload";
  check (S.copy_favorite db ~source:"fixture" ~target:S.Article ~id:"one" ~folder:"another") "Archive reuse failed";
  S.remove_folder db "saved";
  S.remove_favorite db ~source:"fixture" ~target:S.Article ~id:"one" ~folder:"another";
  check (S.memberships db=[]) "Favorites not removed";
  check (snd(cached db "one")) "Removing the last favorite deleted persistent content";
  check (S.raw_archive db ~source:"fixture" ~id:"one"="raw MIME") "MIME lost";
  check (S.attachment_archive db ~source:"fixture" ~id:"one" ~part:0=Some "attachment bytes") "Resource lost"))
let threads_and_sources () = with_database(fun path ->
 let root=message ~thread:"topic" "root" "root body" in
 let reply=message ~thread:"topic" ~parent:"root" "reply" "reply body" in
 S.with_db path (fun db ->
  S.observe db ~source:"fixture" ~group:None [root;reply];
  List.iter(fun m -> S.cache_article db ~source:"fixture" (article m)) [root;reply];
  S.cache_thread db ~source:"fixture" ~id:"topic" false);
 S.with_db path (fun db ->
  let articles,complete=cached db ~target:S.Thread "topic" in
  check (List.length articles=2 && not complete) "Partial thread cache lost";
  check (fst(cached db ~source:"another" ~target:S.Thread "topic")=[]) "Cache crossed source identities";
  let child=message ~thread:"topic" ~parent:"reply" "child" "new reply" in
  S.observe db ~source:"fixture" ~group:None [child];
  S.cache_article db ~source:"fixture" (article child);
  S.cache_thread db ~source:"fixture" ~id:"topic" true;
  let articles,complete=cached db ~target:S.Thread "root" in
  check (List.length articles=3 && complete) "New replies did not extend the cached thread";
  S.save_archive db ~source:"fixture" ~target:S.Thread ~id:"topic" ~folder:"thread"
   [{message=root;attachments=`List [];raw=None;files=[]}];
  let archived=S.load_archive db ~source:"fixture" ~target:S.Thread ~id:"topic" |> Yojson.Safe.Util.to_list in
  check (List.length archived=1) "Body-only replies were presented as resource-complete archives"))
let migration_preserves_archives () = with_database(fun path ->
 S.with_db path(fun db ->
  let m=message "one" "old archive" in
  S.save_archive db ~source:"fixture" ~target:S.Article ~id:"one" ~folder:"saved"
   [{message=m;attachments=`List [];raw=Some "old MIME";files=[0,"old resource"]}]);
 let db=Sqlite3.db_open path in
 check (Sqlite3.exec db "DROP TABLE cached_threads; ALTER TABLE archives DROP COLUMN resources_complete; PRAGMA user_version=4"=Sqlite3.Rc.OK) "Could not prepare v4 fixture";
 ignore(Sqlite3.db_close db);
 S.with_db path(fun db ->
  check (snd(cached db "one")) "Migration lost old body";
  check (S.copy_favorite db ~source:"fixture" ~target:S.Article ~id:"one" ~folder:"copy") "Migration lost complete-resource status";
  check (S.raw_archive db ~source:"fixture" ~id:"one"="old MIME") "Migration lost MIME";
  check (S.attachment_archive db ~source:"fixture" ~id:"one" ~part:0=Some "old resource") "Migration lost resource"))
let () =
 persist_and_keep_state_separate ();favorite_resources_and_removal ();
 threads_and_sources ();migration_preserves_archives ();
 print_endline "Persistent content cache tests passed"
