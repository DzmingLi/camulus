module D=Douban_backend
module R=Rss_backend
let check value reason=if not value then failwith reason
let fails f=try f();false with _->true
let gets=ref [] and posts=ref [] and replies=ref []
let reply ?(status=200) ?(headers=[]) body : R.Http.response={status;headers;body}
let next ()=match !replies with r::rest->replies:=rest;r|[]->failwith "Unexpected request"
module H=struct let request r=gets:=r::!gets;next() end
module B=D.Make(H)
let post r ~body=posts:=(r,body)::!posts;next()
let ()=Eio_posix.run(fun _->
 let backend=B.create ~post ~cookies:(fun _->["dbcl2","1:session";"ck","test-ck"])() in
 replies:=[reply {|{"count":3}|}];B.set_like backend ~id:"status:12" ~liked:true;
 let request,body=List.hd !posts in
 check(request.url="https://www.douban.com/j/status/like") "broadcast like endpoint";
 check(List.assoc "X-CSRF-TOKEN" request.headers="test-ck") "CSRF header";
 check(Uri.query_of_encoded body=["ck",["test-ck"];"sid",["12"]]) "encoded broadcast body";
 check((Option.get(B.reaction backend "status:12")).liked) "confirmed state";
 replies:=[reply {|{"count":2}|}];B.set_like backend ~id:"status:12" ~liked:false;
 check((fst(List.hd !posts)).url="https://www.douban.com/j/status/unlike") "broadcast unlike endpoint";
 check(not(Option.get(B.reaction backend "status:12")).liked) "unlike acknowledged";
 List.iter(fun(id,path)->
  replies:=[reply {|{"reaction_type":1}|}];B.set_like backend ~id ~liked:true;
  check((fst(List.hd !posts)).url="https://m.douban.com/rexxar/api/v2/"^path) "reaction target";
  replies:=[reply {|{"reaction_type":0}|}];B.set_like backend ~id ~liked:false;
  check(List.assoc "reaction_type" (Uri.query_of_encoded(snd(List.hd !posts)))=["0"]) "explicit unlike, never toggle")
 ["topic:15","group/topic/15/react";"group-topic:15","group/topic/15/react";
  "status:12/comment:3","status/comment/3/react";"topic:15/comment:3","group/topic/15/comment/3/react"];
 let before=List.length !posts in
 check(fails(fun()->B.set_like backend ~id:"topic:15/comment:3/escape" ~liked:true)) "reject ambiguous target";
 check(List.length !posts=before) "invalid targets never post";
 let boot=B.create ~post ~cookies:(fun _->["dbcl2","1:session"])() in
 replies:=[reply ~headers:["set-cookie","ck=bootstrap; Domain=.douban.com; Path=/"] "";reply {|{"count":1}|}];
 B.set_like boot ~id:"status:12" ~liked:true;
 check((List.hd !gets).url="https://www.douban.com/mine/") "ephemeral CSRF bootstrap";
 check(List.assoc "Cookie" (fst(List.hd !posts)).headers="ck=bootstrap; dbcl2=1:session") "same-account cookies with fresh CSRF";
 let anonymous=B.create ~post ~cookies:(fun _->[])() in
 let before=List.length !posts in
 check(fails(fun()->B.set_like anonymous ~id:"status:12" ~liked:true)) "requires Firefox login";
 check(List.length !posts=before) "anonymous requests never post";
 List.iter(fun response->
  replies:=[response];let before=List.length !posts in
  check(fails(fun()->B.set_like backend ~id:"status:12" ~liked:true)) "server failure is reported";
  check(List.length !posts=before+1 && !replies=[]) "failure not queued or retried";
  check(not(Option.get(B.reaction backend "status:12")).liked) "failed write preserves confirmed state")
 [reply ~status:403 "denied";reply ~status:302 ~headers:["location","https://evil.test/"] "";
  reply "<html>Login</html>";reply {|{"code":403,"localized_message":"no"}|};reply {|{}|}];
 let timeout=B.create ~cookies:(fun _->["dbcl2","1:session";"ck","ck"]) ~post:(fun _ ~body:_->raise(R.Http.Transport_error "timeout"))() in
 check(fails(fun()->B.set_like timeout ~id:"status:12" ~liked:true)) "transport timeout is reported";
 check(B.reaction timeout "status:12"=None) "timeout never confirms success";
 let html="<h1 class='topic-title'>Title</h1><div class='topic-richtext'>\n<p>First\r\n\r\n<b>Second\nline</b></p>\n<pre>code\nline</pre></div><a data-object_id='15' data-reaction_type='1'></a>" in
 let article=D.topic "topic:15" html in
 let rendered=Option.get article.message.html in
 let tree=Reader.Html.parse rendered in
 check(List.length(Reader.Html.select(fun n->Reader.Html.tag n="br")tree)=3) "paragraph and inline newlines preserved, formatting whitespace untouched";
 check(Reader.Html.text(Option.get(Reader.Html.find(fun n->Reader.Html.tag n="pre")tree))="code\nline") "preformatted code untouched";
 check((Option.get(D.topic_reaction "topic:15" html)).liked) "remote topic like state";
 print_endline "PASS: Douban likes, unlike, CSRF, failures and paragraph breaks")
