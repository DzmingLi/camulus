module B=Csrin_backend
module C=Reader.Content
module H=B.Http
let check b why=if not b then failwith why
let fails f=try f();false with _->true
let base="https://cs.rin.ru/forum/"
let atom="<feed xmlns='http://www.w3.org/2005/Atom'><entry><id>https://csrin.org/forum/viewtopic.php?t=12&amp;p=101#p101</id><title type='html'>Topic &amp; title</title><author><name>Alice</name></author><published>2026-10-04T02:14:30+01:00</published><updated>2026-10-04T02:14:30+01:00</updated><content type='html'>&lt;p&gt;中文&lt;/p&gt;</content></entry></feed>"
let page post next=Printf.sprintf "<html><a class='titles'>Topic &amp; title</a><table class='tablebg'><tr><td><a name='p%d'></a><b class='postauthor'>Alice</b><a href='./viewtopic.php?f=14&amp;t=12'>Post</a><div><b>Posted:</b> Sunday, 04 Oct 2026, 02:14</div></td></tr><tr><td><div class='postbody'><p>中文 %d</p><img src='./image.png'><a href='./download/file.php?id=7'>file</a><script>bad()</script></div></td></tr></table>%s</html>"post post(match next with None->""|Some offset->Printf.sprintf "<a href='./viewtopic.php?t=12&amp;start=%d'>Next</a>"offset)
let form action tokens="<form action='"^action^"' method='post'>"^(if tokens then "<input type='hidden' name='form_token' value='fresh'><input type='hidden' name='creation_time' value='123'><input type='hidden' name='sid' value='session'>"else "")^"<textarea name='message'></textarea></form>"
let post_form=form "./posting.php?mode=reply&amp;f=14&amp;t=12"true
let response ?(headers=[]) status body : H.response={status;headers;body}
let ()=Eio_posix.run(fun _->
 let queue=ref [] and requests=ref [] in
 let module Mock=struct let request r=requests:=r::!requests;match !queue with f::xs->queue:=xs;f r|[]->failwith "Unexpected HTTP"end in
 let module M=B.Make(Mock)in
 let b=M.create()in
 let expect ?(inspect=fun _->()) path result r=check(Uri.path(Uri.of_string r.H.url)=path)("request route "^r.url^" expected "^path);inspect r;result in
 List.iter(fun url->check(fails(fun()->ignore(B.site url)))"invalid origin") ["https://evil.test/forum/";"https://cs.rin.ru/forum/?x=1";"https://user:pass@csrin.org/forum/"];
 check(fails(fun()->ignore(B.group "forum.-1")))"group validation";
 check(B.html_date "Sunday, 30 Jun 2019, 05:34\194\160\194\160"=1561869240.)"real phpBB nonbreaking whitespace";
 let a=List.hd(B.feed ~base atom)in
 check(a.message.id="topic:12/post:101" && a.message.thread_id=Some"topic:12" && a.message.created=1791076470.)"Atom identity and source timestamp";
 queue:=[expect "/forum/feed.php"(response 200 atom)];
 let listing=M.list b "forum.14"~after:None in
 check(List.length listing.messages=1 && (List.hd listing.messages).thread_id=Some"topic:12" && !queue=[])"list groups by native topic without fetching bodies";
 let batches=ref []and articles=ref []in
 queue:=[expect "/forum/viewtopic.php"(response 200(page 100(Some 15)));expect "/forum/viewtopic.php" ~inspect:(fun r->check(Uri.get_query_param(Uri.of_string r.url)"start"=Some"15")"pagination offset")(response 200(page 101 None))];
 M.stream_thread b "topic:12/post:101" ~emit_article:(fun a->articles:=a::!articles)~emit:(fun batch->batches:=batch::!batches);
 check(List.map(fun(b:C.thread_batch)->b.complete)(List.rev !batches)=[false;true])"progressive completion";
 let reply=List.hd !articles in
 check(reply.message.parent_id=Some"topic:12/post:100" && reply.message.created=1791076440.)"reply parent and HTML timezone";
 check(List.length reply.enclosures=2 && not(B.contains(Option.get reply.message.html)"script"))"sanitize and include linked attachments";
 queue:=[expect "/forum/download/file.php"(response 200 "FILE\255");expect "/forum/image.png"(response 200 "IMG\000")];
 let archive=M.archive_article b reply in check(List.length archive.files=2)"archive binary media";
 queue:=[expect "/forum/viewtopic.php"(response 200(page 100(Some 15)));expect "/forum/viewtopic.php"(response 200(page 100 None))];
 let completed=ref false in
 check(fails(fun()->M.thread b "topic:12"~emit:(fun x->completed:= !completed || x.complete)))"repeated post rejected";check(not !completed)"failed thread never complete";
 let bounded=M.create ~max_pages:1()in
 queue:=[expect "/forum/viewtopic.php"(response 200(page 100(Some 15)))];
 check(fails(fun()->M.thread bounded "topic:12"~emit:(fun _->())))"page limit errors";
 queue:=[expect "/forum/viewtopic.php"(response 200 "<html>Login required</html>")];check(fails(fun()->M.thread b "topic:12"~emit:(fun _->())))"inaccessible topic not empty success";
 (* A single saved post does not require traversing an arbitrarily large thread. *)
 queue:=[expect "/forum/viewtopic.php"(response 200(page 100(Some 15)));
  expect "/forum/viewtopic.php"~inspect:(fun r->check(Uri.get_query_param(Uri.of_string r.url)"p"=Some"900")"direct post lookup")(response 200(page 900(Some 9999)))];
 check((M.read bounded "" "topic:12/post:900").message.parent_id=Some"topic:12/post:100" && !queue=[])"single post bypasses full-thread page limit";
 (* Security check carries only origin cookies and is attempted once. *)
 queue:=[expect "/forum/feed.php"(response 401 "securitytoken=abc;securitytoken_expiration=123;");
  expect "/securitycheck/forum/feed.php" ~inspect:(fun r->check(B.contains(List.assoc"Cookie"r.headers)"securitytoken=abc")"challenge cookie")(response 200 "<feed xmlns='http://www.w3.org/2005/Atom'/>")];
 ignore(M.list b "forum.14"~after:None);
 queue:=[expect "/forum/feed.php"(response ~headers:["location","https://evil.test/"]302 "")];
 check(fails(fun()->ignore(M.list b "forum.14"~after:None)))"offsite redirect rejected";
 let login="<form action='./ucp.php?mode=login'><input name='password'><input type='hidden' name='sid' value='fresh-login'></form>"in
 queue:=[expect "/forum/ucp.php"(response 200 login);
  expect "/forum/ucp.php" ~inspect:(fun r->let q=Uri.query_of_encoded r.body in
   check(r.meth=H.Post && List.assoc "sid"q=["fresh-login"] && List.assoc "password"q=["private"])"login uses hidden fields")
   (response ~headers:["set-cookie","session=ok; Path=/forum/; Secure"]200 "<a href='ucp.php?mode=logout'>Logout</a>")];
 M.login b ~username:"alice"~password:"private";
 queue:=[expect "/forum/index.php"(response 200 "<a href='./viewforum.php?f=14'>Main forum</a><a href='https://evil.test/viewforum.php?f=5'>External</a>")];
 check(M.forums b=["forum.14","Main forum"])"forum directory is source-scoped";
 let draft : M.draft={forum="14";topic=Some"12";subject="Re: Topic";body="[b]中文[/b]"}in
 queue:=[expect "/forum/posting.php"(response 200 post_form);
  expect "/forum/posting.php" ~inspect:(fun r->let q=Uri.query_of_encoded r.body in
   check(r.meth=H.Post && List.assoc"form_token"q=["fresh"] && List.assoc"message"q=["[b]中文[/b]"])"fresh CSRF and BBCode";
   check(B.contains(List.assoc"Cookie"r.headers)"session=ok")"login cookie reused")
   (response 200 "<meta http-equiv='refresh' content='3;url=./viewtopic.php?t=12&amp;p=102#p102'>")];
 check(B.param(M.send b draft)"p"=Some"102")"confirmed receipt";
 let before=List.length !requests in
 queue:=[expect "/forum/posting.php"(response 200 post_form);(fun _->raise(H.Transport_error "timeout"))];
 check((try ignore(M.send b draft);false with B.Submit_uncertain->true))"write timeout uncertain";
 check(List.length !requests=before+2 && !queue=[])"POST attempted once";
 List.iter(fun html->queue:=[expect "/forum/posting.php"(response 200 html)];
  let n=List.length !requests in check(fails(fun()->ignore(M.send b draft)))"invalid form rejected";
  check(List.length !requests=n+1)"no POST for invalid form")
  [form"https://evil.test/"true;form"./posting.php"false;"<form><input name='password'></form>";"<form action='./posting.php'><input name='captcha'><textarea name='message'></textarea></form>"];
 check(!queue=[])"all mocked requests consumed";
 print_endline "PASS: CS.RIN.RU identities, source dates, progressive phpBB trees, bounded pagination, archives, cookies, login, fresh CSRF and single-attempt writes")
