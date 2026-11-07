module R=Rss_backend
module C=Reader.Content
let check b why=if not b then failwith why
let fails f=try f();false with _->true
let contains text part=let rec find n=n+String.length part<=String.length text && (String.sub text n (String.length part)=part || find(n+1)) in find 0
let atom ?(next="") ?(id="one") ?(updated="2026-10-16T02:30:00Z") ()=
 "<feed xmlns='http://www.w3.org/2005/Atom'><id>urn:test:blog</id><updated>2026-10-16T02:30:00Z</updated><author><name>Alice</name></author><title>Blog</title>"^
 (if next=""then ""else "<link rel='next' href='"^next^"'/>")^
 "<entry><id>"^id^"</id><title>Title "^id^"</title><author><name>Alice</name></author><published>2026-10-15T02:30:00Z</published><updated>"^updated^"</updated><link rel='alternate' href='https://example.blogspot.com/post.html'/><summary type='html'>Short summary</summary></entry></feed>"
let full="<html><aside>Not the article</aside><div class='post-body entry-content'><p>Complete body</p><a href='/link'>Link</a><img src='/image.png'><script>bad()</script></div></html>"
let ()=Eio_posix.run(fun _->
 let queue=ref [] and requests=ref [] in
 let module H=struct let request r=requests:=r::!requests;match !queue with x::xs->queue:=xs;x|[]->failwith "Unexpected HTTP" end in
 let module B=Blogger_backend.Make(H) in
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
 check(fails(fun()->ignore(B.list b "posts.other.blogspot.com" ~after:page.after))) "Blogger group cursor scope";
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
 print_endline "PASS: Blogger lazy full text, pagination, media, revision/cache and archives")
