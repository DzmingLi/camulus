module B=Discourse_backend
module D=B.Decode
let check b msg=if not b then failwith msg
let fails f=try f();false with _->true
let base="https://forum.example.test/forum"
let post ?(liked=false) id floor parent = `Assoc[
 "id",`Int id;"topic_id",`Int 7;"post_number",`Int floor;"reply_to_post_number",(if parent=0 then `Null else `Int parent);
 "username",`String "alice";"created_at",`String "2026-10-16T00:00:00Z";"updated_at",`String "2026-10-16T00:00:00Z";
 "cooked",`String "<p>Body</p><a class='attachment' href='/forum/uploads/file.bin'>file.bin</a>";
 "actions_summary",`List[`Assoc["id",`Int 2;"count",`Int 4;"acted",`Bool liked]]]
let summary ?(id=7) ?(category=3) () = `Assoc["id",`Int id;"category_id",`Int category;"title",`String "Topic";
 "highest_post_number",`Int 3;"posts_count",`Int 3;"created_at",`String "2026-10-16T00:00:00Z";"last_posted_at",`String "2026-10-16T01:00:00Z"]
let stream posts = `Assoc["posts",`List posts]
let topic posts = match summary() with `Assoc xs->`Assoc(xs@["post_stream",`Assoc["posts",`List posts;"stream",`List[`Int 100;`Int 102;`Int 105]];
 "details",`Assoc["notification_level",`Int 3]]) | _->assert false
let response ?(status=200) ?(headers=[]) json : B.Http.response={status;headers;body=Yojson.Safe.to_string json}
let () =
 check(D.site "https://Forum.Example.test:443/forum/"=base) "normalized site";
 List.iter(fun s->check(fails(fun()->ignore(D.site s))) "invalid site")
   ["file:///tmp/x";"http://forum.example.test";"https://user:secret@forum.example.test";base^"?q=1";base^"/../x"];
 let first=D.post ~site:base ~topic:7 ~title:"Topic" (post 100 1 0) in
 check(first.message.id=(D.summary ~site:base (summary())).id) "list and post identity match";
 let child=D.post ~site:base ~topic:7 ~title:"Topic" (post 105 3 2) in
 check(child.message.parent_id=Some "topic:7/post:2") "floor relation independent of post ID";
 check(fails(fun()->ignore(D.post ~site:base ~topic:8 ~title:"" (post 100 1 0)))) "cross-topic posts rejected";
 check(fails(fun()->ignore(D.post ~site:base ~topic:7 ~title:"" (post 105 3 3)))) "cycle rejected";
 check((List.hd first.attachments).url=base^"/uploads/file.bin") "HTML attachment resolution";
 let images=D.attachments ~site:base "<a class='lightbox' href='https://cdn.example/original.png'><img src='https://forum.example.test/uploads/small.png'></a>" in
 check(List.length images=2 && (List.hd images).url="https://cdn.example/original.png") "lightbox originals and uploaded images are archived";
 let queue=ref [] and requests=ref [] in
 let module H=struct let request r=requests:=r::!requests;match !queue with
 | x::xs->queue:=xs;x | []->failwith "Unexpected HTTP call" end in
 let module M=B.Make(H) in
 Eio_posix.run(fun _ ->
  let session=M.create(B.config ~api_key:"testkey" ~client_id:"testclient" base) in
  queue:=[response(topic[post 100 1 0]);response(`Assoc["post_stream",stream[post 105 3 2;post 102 2 1]])];
  let batches=ref [] in
  M.thread session "topic:7/post:3" ~emit:(fun b->batches:= !batches@[b]);
  check(List.length !batches=2 && not(List.hd !batches).complete && (List.hd(List.rev !batches)).complete) "progressive complete tree";
  let req=List.hd !requests in
  check((Uri.query (Uri.of_string req.url) |> List.filter(fun(k,_)->k="post_ids[]") |> List.concat_map snd)=["102";"105"]) "fetch missing post IDs";
  check(List.assoc_opt "User-Api-Key" req.headers=Some "testkey") "User API headers";
  queue:=[response(topic[post 100 1 0]);response(`Assoc["post_stream",stream[post 102 2 1]])];
  let complete=ref false in
  check(fails(fun()->M.thread session "topic:7" ~emit:(fun b->complete:=b.complete))) "missing page fails";
  check(not !complete) "missing page never marked complete";
  queue:=[response(`Assoc["category",`Assoc["id",`Int 3;"slug",`String "help"]]);
    response(`Assoc["topic_list",`Assoc["topics",`List[summary();summary ~id:9 ~category:4 ()];"more_topics_url",`String "https://evil.example/steal"]])];
  let page=M.list session (B.Category 3) ~after:None in
  check(List.length page.messages=1 && page.after<>None) "category scope and pagination";
  check(fails(fun()->ignore(M.list session B.Latest ~after:page.after))) "cursor scope";
  queue:=[response(`Assoc["topic_list",`Assoc["topics",`List[]]])];
  ignore(M.list session (B.Category 3) ~after:page.after);
  check(String.starts_with ~prefix:base (List.hd !requests).url) "never trust pagination URL with credentials";
  queue:=[response(`Assoc["topics",`List[summary();summary ~id:9 ~category:4 ()];
    "posts",`List[`Assoc["topic_id",`Int 7];`Assoc["topic_id",`Int 7];`Assoc["topic_id",`Int 9]];
    "grouped_search_result",`Assoc["more_full_page_results",`Bool false]])];
  check(List.length (M.search session (B.Category 3) ~query:"OCaml category:4" ~after:None).messages=1) "search merges threads and enforces current category";
  check(Uri.get_query_param(Uri.of_string (List.hd !requests).url) "q"=Some "OCaml category:4 category:3") "remote scoped query";
  queue:=[response(topic[post 100 1 0]);response(post ~liked:true 100 1 0)];
  M.set_like session ~id:"topic:7/post:1" ~liked:true;
  check((List.hd !requests).meth=B.Http.Post) "like posts to API";
  check(Uri.query_of_encoded (List.hd !requests).body |> List.assoc "id" = ["100"]) "like uses global post ID";
  queue:=[response(topic[post ~liked:true 100 1 0])];
  M.set_like session ~id:"topic:7/post:1" ~liked:true;
  check((List.hd !requests).meth=B.Http.Get) "absolute like avoids duplicate mutation";
  queue:=[response(topic[post ~liked:true 100 1 0]);response(post 100 1 0)];
  M.set_like session ~id:"topic:7/post:1" ~liked:false;
  check((List.hd !requests).meth=B.Http.Delete) "unlike API";
  queue:=[response(post 105 3 2)];
  let receipt=M.send session (B.Reply{parent="topic:7/post:2";text="Reply"}) in
  check(receipt.message.id="topic:7/post:3") "reply receipt";
  queue:=[response ~status:503 `Null];
  let count=List.length !requests in
  check((try ignore(M.send session (B.Reply{parent="topic:7/post:2";text="Reply"}));false with B.Uncertain_write->true)) "uncertain write";
  check(List.length !requests=count+1) "no automatic write retry";
  queue:=[response(`Assoc["success",`String "OK"]);response(topic[post 100 1 0])];
  M.Subscription.set session "topic:7" B.Watching;
  check(M.Subscription.get session "topic:7"=B.Watching) "tracking reads server state";
  let changed=match topic[post 100 1 0] with `Assoc xs -> `Assoc(("details",`Assoc["notification_level",`Int 1])::List.remove_assoc "details" xs) | _->assert false in
  queue:=[response changed];
  check(M.Subscription.get session "topic:7"=B.Regular) "external tracking change is authoritative";
  queue:=[response(`Assoc["topic_list",`Assoc["topics",`List[summary()]]])];
  ignore(M.search session B.Watched_topics ~query:"OCaml" ~after:None);
  let uri=Uri.of_string(List.hd !requests).url in
  check(Uri.get_query_param uri "state"=Some "watching" && Uri.get_query_param uri "search"=Some "OCaml") "personal entry search remains remote scoped";
  queue:=[{B.Http.status=302;headers=["location","https://cdn.example/file.bin"];body=""};
          {B.Http.status=200;headers=[];body="bytes\000"}];
  let archived=M.archive_article session first in
  check(archived.files=[0,"bytes\000"]) "binary archive";
  check((List.hd !requests).headers=[]) "CDN gets no credentials";
  let notification id kind = `Assoc["id",`Int id;"notification_type",`Int kind;"topic_id",`Int 7;"post_number",`Int 2;
    "created_at",`String "2026-10-16T01:00:00Z";"data",`Assoc["topic_title",`String "Reply";"display_username",`String "Alice"]] in
  queue:=[response(`Assoc["notifications",`List[notification 12 2;notification 13 1;notification 14 5];"total_rows_notifications",`Int 70])];
  let replies=M.list session B.Replies ~after:None in
  check(List.length replies.messages=1 && replies.after<>None) "reply notification filter and pagination";
  check((List.hd replies.messages).activity=Some "notification:12") "stable notification activity";
  check((List.hd !requests).meth=B.Http.Get) "notifications fetched without mark-read writes";
  check(fails(fun()->ignore(M.list session B.Notifications ~after:replies.after))) "notification cursor scope";
  check(fails(fun()->ignore(M.search session B.Replies ~query:"x" ~after:None))) "no unscoped search substitute";
  let anonymous=M.create(B.config base) in
  let count=List.length !requests in
  check(fails(fun()->ignore(M.send anonymous (B.Reply{parent="topic:7/post:1";text="test"})))) "anonymous write rejected";
  check(List.length !requests=count) "anonymous failure sends nothing";
  queue:=[response ~status:429 `Null];
  check(fails(fun()->ignore(M.list session B.Latest ~after:None))) "rate limit reported");
 print_endline "PASS: Discourse identities, progressive threads, strict pagination, scoped search, User API, likes, writes, tracking and archives"
