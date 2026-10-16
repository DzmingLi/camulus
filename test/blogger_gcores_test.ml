module R=Rss_backend
module C=Reader.Content
let check b why=if not b then failwith why
let fails f=try f();false with _->true
let contains text part=let rec find n=n+String.length part<=String.length text && (String.sub text n (String.length part)=part || find(n+1)) in find 0
let atom ?(next="") ?(id="one") ?(updated="2026-10-16T02:30:00Z") ()=
 "<feed xmlns='http://www.w3.org/2005/Atom'><title>Blog</title>"^
 (if next=""then ""else "<link rel='next' href='"^next^"'/>")^
 "<entry><id>"^id^"</id><title>Title "^id^"</title><author><name>Alice</name></author><published>2026-10-15T02:30:00Z</published><updated>"^updated^"</updated><link rel='alternate' href='https://example.blogspot.com/post.html'/><summary type='html'>Short summary</summary></entry></feed>"
let full="<html><aside>Not the article</aside><div class='post-body entry-content'><p>Complete body</p><a href='/link'>Link</a><img src='/image.png'><script>bad()</script></div></html>"
let json=Yojson.Safe.to_string
let person=`Assoc["type",`String "users";"id",`String "7";"attributes",`Assoc["nickname",`String "Writer"]]
let draft=`Assoc["blocks",`List[
 `Assoc["type",`String "header-two";"text",`String "Heading"];
 `Assoc["type",`String "unstyled";"text",`String "Content <safe>";"entityRanges",`List[`Assoc["key",`Int 1]]];
 `Assoc["type",`String "atomic";"text",`String " ";"entityRanges",`List[`Assoc["key",`Int 0]]]];
 "entityMap",`Assoc["0",`Assoc["type",`String "GALLERY";"data",`Assoc["images",`List[`Assoc["path",`String "picture.png"]];"caption",`String "Caption"]];
 "1",`Assoc["type",`String "LINK";"data",`Assoc["url",`String "https://example.test/link"]]]]
let item ?(likes=0) ?(updated="2026-10-16T02:30:00Z") id=`Assoc["type",`String "talks";"id",`String id;
 "attributes",`Assoc["title",`String("Post "^id);"content",`String(json draft);"created-at",`String "2026-10-15T02:30:00Z";
 "updated-at",`String updated;"likes-count",`Int likes];
 "relationships",`Assoc["user",`Assoc["data",`Assoc["type",`String "users";"id",`String "7"]]]]
let listing items count=json(`Assoc["data",`List items;"included",`List[person];"meta",`Assoc["record-count",`Int count]])
let detail i=json(`Assoc["data",i;"included",`List[person]])
let ()=Eio_posix.run(fun _->
 let queue=ref [] and requests=ref [] in
 let module H=struct let request r=requests:=r::!requests;match !queue with x::xs->queue:=xs;x|[]->failwith "Unexpected HTTP" end in
 let module B=Blogger_backend.Make(H) in let module G=Gcores_backend.Make(H) in
 let response ?(headers=[]) status body : R.Http.response={status;headers;body} in
 let b=B.create () and group="posts.example.blogspot.com" in
 List.iter(fun g->check(fails(fun()->ignore(Blogger_backend.host g))) "invalid blog host") ["posts../foo";"posts.user@host.test";"posts.-host.test"];
 queue:=[response ~headers:["etag","v1"] 200(atom ~next:"https://www.blogger.com/feeds/123/posts/default?start-index=26" ())];
 let page=B.list b group ~after:None in
 check(List.length !requests=1 && List.length page.messages=1 && page.after<>None) "Blogger listing does not fetch articles";
 check(contains(Option.value (List.hd page.messages).html ~default:(List.hd page.messages).body) "Short summary") "summary metadata";
 queue:=[response 200 full];let article=B.read b group "one"in
 check(contains(Option.get article.message.html)"Complete body") "full body replaces summary";
 check(not(contains(Option.get article.message.html)"Not the article") && not(contains(Option.get article.message.html)"script")) "body extraction and sanitization";
 check((List.hd article.enclosures).url="https://example.blogspot.com/image.png") "relative media";
 check(article.message.activity=(List.hd page.messages).activity) "full-body load does not create a new timeline revision";
 ignore(B.read b group "one");check(!queue=[]) "cached read";
 check(fails(fun()->ignore(B.search b group ~query:"x" ~after:page.after))) "Blogger query cursor scope";
 queue:=[response 200(atom ~id:"older" ())];ignore(B.list b group ~after:page.after);
 check(contains(List.hd !requests).url "start-index=26" && contains(List.hd !requests).url "example.blogspot.com") "next index remains scoped to blog";
 queue:=[response 200 full;response 200 "IMG\000"];
 let archived=B.archive b group "older"in check(archived.files=[0,"IMG\000"]) "full Blogger archive includes media";
 queue:=[response 304 ""];ignore(B.list b group ~after:None);
 check(List.assoc_opt "If-None-Match"(List.hd !requests).headers=Some "v1") "Blogger conditional refresh";
 queue:=[response 200(atom ~updated:"2026-10-17T02:30:00Z" ());response 200 "<html>Login or missing article</html>"];
 let revised=B.list b group ~after:None in
 check((List.hd revised.messages).activity<>article.message.activity) "Blogger source update invalidates full cache";
 check(fails(fun()->ignore(B.read b group "one"))) "missing body never silently archives summary";
 queue:=[response 200 full];ignore(B.read b group "one");
 let g=G.create() in
 queue:=[response 200(listing[item "1"]2)];let page=G.list g "talks.7" ~after:None in
 check(List.length page.messages=1 && page.after<>None) "Gcores first page";
 queue:=[response 200(detail(item "1"))];let article=G.read g "talks.7" "talks:1"in
 let html=Option.get article.message.html in
 check(article.message.author="Writer" && contains html "<h2>Heading</h2>" && contains html "&lt;safe&gt;" && contains html "https://example.test/link") "Draft text, links, escaping and author";
 check((List.hd article.enclosures).url="https://image.gcores.com/picture.png") "Draft gallery";
 check((Gcores_backend.decode(item ~likes:9 "1")[person]).activity=article.message.activity) "likes do not resurrect dismissed posts";
 check(fails(fun()->ignore(G.list g "talks.8" ~after:page.after))) "Gcores cursor scope";
 queue:=[response 200(listing[item "2"]2)];let next=G.list g "talks.7" ~after:page.after in
 check(next.after=None && contains(List.hd !requests).url "offset") "Gcores pagination offset";
 queue:=[response 200 "FILE"];check((G.archive g "talks.7" "talks:1").files=[0,"FILE"]) "Gcores archive media";
 G.invalidate g "talks:1";
 queue:=[response 200(detail(item ~updated:"2026-10-17T02:30:00Z" "1"))];
 check((G.read g "talks.7" "talks:1").message.activity<>article.message.activity) "archive detail revalidation";
 queue:=[response 200(listing[item "1"]2)];
 check(fails(fun()->ignore(G.list g "talks.7" ~after:page.after))) "Gcores repeated page rejected";
 queue:=[response 503 "unavailable"];
 check(fails(fun()->ignore(G.read g "talks.7" "talks:9"))) "Gcores failed read surfaces error";
 print_endline "PASS: Blogger lazy full text, pagination, media, revision/cache; Gcores Draft.js, author, pagination, detail refresh and archives")
