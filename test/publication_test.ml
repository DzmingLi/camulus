module R=Rss_backend
module D=R.Decode
let check b why=if not b then failwith why
let fails f=try f();false with _->true
let feed={|<feed xmlns="http://www.w3.org/2005/Atom" xmlns:media="http://search.yahoo.com/mrss/"><entry><id>one</id><title type="html">A &lt;i&gt;title&lt;/i&gt;</title><link href="https://blog.janestreet.com/one/"/><updated>2026-10-16T00:00:00Z</updated><content type="html">&lt;p&gt;Body&lt;/p&gt;&lt;p data-id="injected-recirculation-link"&gt;AD&lt;/p&gt;&lt;span class="smallcaps"&gt;First&lt;/span&gt;</content><media:content url="https://example.test/lead.jpg"/></entry></feed>|}
let full={|<html><body><nav>Navigation</nav><article><div class="name"><a>Alice</a></div><div class="featimg-wrapper"><img src="/hero.png"></div><div class="post-content"><p>Full post</p><img src="../body.png" onerror="bad()"><script>bad()</script></div><aside>Recommendations</aside></article></body></html>|}
let contains s part=let n=String.length part in let rec loop i=i+n<=String.length s && (String.sub s i n=part || loop(i+1)) in loop 0
let ()=
 let a=List.hd(Atlantic_backend.decode ~url:"https://www.theatlantic.com/feed/author/test/" feed) in
 let html=Option.get a.message.html in
 check(a.message.title="A title") "Atlantic HTML title";
 check(contains html "<strong" && not(contains html "AD")) "Atlantic body cleaning";
 check(List.length a.enclosures=1) "MediaRSS lead image archived";
 check(fails(fun()->ignore(Atlantic_backend.feed_url "author.../escape"))) "author path validation";
 let queue=ref [] and requests=ref [] in
 let module H=struct let request r=requests:=r::!requests;match !queue with x::xs->queue:=xs;x | []->failwith "Unexpected request" end in
 let module J=Janestreet_backend.Make(H) in
 let response body : R.Http.response={status=200;headers=[];body} in
 Eio_posix.run(fun _->
   let session=J.create() in queue:=[response feed];
   let page=J.list session "posts" ~after:None in
   let id=(List.hd page.messages).id in
   check(id="https://blog.janestreet.com/one/" && List.length !requests=1) "list does not fetch full posts";
   queue:=[response "<html>login</html>"];
   check(fails(fun()->ignore(J.read session "posts" id))) "missing full post fails without caching login page";
   queue:=[response full];let a=J.read session "posts" id in
   let html=Option.get a.message.html in
   check(a.message.author="Alice" && contains html "Full post") "full post and author";
   check(not(contains html "Navigation" || contains html "Recommendations" || contains html "bad()")) "body boundaries and active HTML removed";
   check(List.length a.enclosures=2) "hero and embedded image collected";
   ignore(J.read session "posts" id);check(!queue=[]) "full body cached";
   queue:=[response "A\000";response "B\000"];
   let archived=J.archive session "posts" id in
   check(List.length archived.files=2 && List.assoc 0 archived.files="A\000") "archive includes binary media";
   check(fails(fun()->ignore(J.read session "posts" "https://other.test/post"))) "reject foreign post identity");
 print_endline "PASS: publication body extraction, stable identity, lazy loading, retry, HTML cleanup and binary archive"
