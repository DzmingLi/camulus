module J=Jsonfeed_backend
module R=Rss_backend
let check b why=if not b then failwith why
let fails f=try f();false with _->true
let url="https://example.test/feed.json"
let feed ?(version="1.1") ?(next="") items = Yojson.Safe.to_string (`Assoc[
  "version",`String("https://jsonfeed.org/version/"^version);"title",`String "News";
  "authors",`List[`Assoc["name",`String "Alice"]];"next_url",`String next;"items",`List items])
let item id body=`Assoc["id",`String id;"content_html",`String body;
  "url",`String "/post";"date_published",`String "2026-10-16T02:30:00Z";
  "attachments",`List[`Assoc["url",`String "/file.bin";"mime_type",`String "application/octet-stream";"size_in_bytes",`Int 4]]]
let () =
  let a=List.hd(fst(J.Decode.parse ~url (feed [item "one" "<p>Hello</p>"]))) in
  check(a.message.id="one" && a.message.thread_id=None && a.message.author="Alice") "article identity and author";
  check(a.message.created=R.Decode.date "2026-10-16T02:30:00Z") "source date";
  check(a.message.html=Some "<p>Hello</p>" && a.message.url="https://example.test/post") "HTML and relative links";
  let revised=List.hd(fst(J.Decode.parse ~url (feed [item "one" "Updated"]))) in
  check(a.message.activity<>revised.message.activity) "revision fingerprint";
  let v1=feed ~version:"1" [`Assoc["id",`Int 12;"content_text",`String "Plain <b>text</b>";"author",`Assoc["name",`String "Bob"]]] in
  let b=List.hd(fst(J.Decode.parse ~url v1)) in
  check(b.message.id="12" && b.message.author="Bob" && b.message.created=0. && b.message.html=None) "v1 author, numeric ID and deterministic missing date";
  let bads=[`Assoc[];`Assoc["id",`String "empty"];item "one" "duplicate"] in
  check(List.length(fst(J.Decode.parse ~url (feed (item "one" "ok"::bads))))=1) "invalid entries and duplicate IDs";
  List.iter(fun s->check(fails(fun()->ignore(J.Decode.parse ~url s))) "reject malformed feed") ["{}";"{";feed ~version:"9" []];
  let queue=ref [] and requests=ref [] in
  let module H=struct let request r=requests:=r::!requests;match !queue with x::xs->queue:=xs;x | []->failwith "Unexpected HTTP" end in
  let module B=J.Make(H) in
  let response ?(headers=[]) status body : R.Http.response={status;headers;body} in
  Eio_posix.run(fun _->
    let b=B.create () in
    queue:=[response ~headers:["location","/actual"] 302 "";response ~headers:["etag","v1"] 200 (feed ~next:"/page2" [item "one" "First"])];
    let p=B.list b url ~after:None in
    check(List.length p.messages=1 && p.after<>None) "progressive pagination";
    queue:=[response ~headers:["location","/actual"] 302 "";response 304 ""];
    check((B.list b url ~after:None).after=p.after) "304 retains pagination";
    check(List.assoc_opt "If-None-Match" (List.hd !requests).headers=Some "v1") "conditional HTTP";
    check(fails(fun()->ignore(B.list b "https://other.test/feed" ~after:p.after))) "cursor feed scope";
    check(fails(fun()->ignore(B.search b url ~query:"x" ~after:p.after))) "cursor search scope";
    queue:=[response 200 (feed [item "two" "Second"])];
    let p2=B.list b url ~after:p.after in
    check(p2.after=None && (List.hd p2.messages).id="two") "next page";
    check((B.read b url "two").message.id="two" && !queue=[]) "read cached older page";
    queue:=[response 200 "A\000BC"];
    let archived=B.archive b url "one" in check(archived.files=[0,"A\000BC"]) "archive includes bytes";
    queue:=[response 200 (feed ~next:url [])];
    check(fails(fun()->ignore(B.list b url ~after:None))) "pagination loop";
    queue:=[response 200 (feed [item "one" "Hello"])];
    check(List.length(B.search b url ~query:"HELLO" ~after:None).messages=1) "feed search";
    queue:=[response 200 (feed ~next:"/page2" [item "one" "Head"]);
      response 304 "";response 200 (feed [item "two" "Changed older item"])];
    ignore(B.refresh b url);
    check((B.read b url "two").message.html=Some "Changed older item") "archive refresh revalidates older pages";
    let file=Filename.temp_file "jsonfeed-followed" ".json" in Sys.remove file;
    let t=B.create ~subscriptions_file:file () in B.Subscription.set t url true;
    check(B.Subscription.followed (B.create ~subscriptions_file:file ())=[url]) "persistent subscriptions";
    B.Subscription.set t url false;check(B.Subscription.followed t=[]) "unsubscribe";
    Sys.remove file;Sys.remove(file^".lock"));
  print_endline "PASS: JSON Feed 1/1.1 parsing, pagination, identity, conditional HTTP, archives and subscriptions"
