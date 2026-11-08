module G=Github_backend
module H=Reader.Http
let check condition message=if not condition then failwith message
let obj xs=`Assoc xs
let js s=`String s
let topic ?(typ="Issue") ?(id="I_1") ()=obj[
 "__typename",js typ;"id",js id;"title",js "Topic";"body",js "Body";"bodyHTML",js "<p>Body</p>";
 "url",js("https://github.com/owner/repo/"^(if typ="PullRequest" then "pull" else if typ="Discussion" then "discussions" else "issues")^"/1");
 "createdAt",js "2026-10-01T00:00:00Z";"updatedAt",js "2026-10-02T00:00:00Z";
 "author",obj["login",js "author"];"viewerSubscription",js "SUBSCRIBED"]
let conn ?after nodes=obj["nodes",`List nodes;"pageInfo",obj[
 "hasNextPage",`Bool(Option.is_some after);"endCursor",(match after with Some s->js s|None->`Null)]]
let response ?(status=200) ?(headers=[]) body={H.status;headers;body=Yojson.Safe.to_string body}
let data j=response(obj["data",j])
module Mock=struct
 let requests=ref []
 let script:(H.request->H.response)Queue.t=Queue.create()
 let request r=requests:=r::!requests;
  check (Uri.host(Uri.of_string r.H.url)=Some "api.github.com") "API origin";
  check(List.assoc "Authorization" r.headers="Bearer fixture-token") "Authentication";
  if Queue.is_empty script then failwith "Unexpected request" else Queue.pop script r
end
module B=G.Make(Mock)
let expect f=Queue.push f Mock.script
let query contains reply=expect(fun r->
 check(r.H.meth=Post && Uri.path(Uri.of_string r.url)="/graphql") "GraphQL transport";
 let j=Yojson.Safe.from_string r.body in
 check (let pattern=Str.regexp_string contains in try ignore(Str.search_forward pattern (G.str "query"j) 0);true with Not_found->false) ("Query fragment: "^contains);
 reply j)
let fixed contains value=query contains (fun _->data value)
let raises f=match f() with _->failwith "Expected error" | exception (Failure _|Invalid_argument _)->()
let run name f=
 Queue.clear Mock.script;Mock.requests:=[];
 f(B.create(G.config "fixture-token"));
 check(Queue.is_empty Mock.script) "Unconsumed requests";
 Printf.printf "PASS GitHub %s\n" name
let ()=
 run "URL identity and cursor isolation" (fun b->
  let t=Option.get(G.topic_of_url "https://github.com/owner/repo/pull/42#discussion_r2")in
  check(G.topic_url t="https://github.com/owner/repo/pull/42") "Canonical Forge topic URL";
  check(G.topic_of_url "https://example.test/owner/repo/issues/42"=None) "Wrong host";
  raises(fun()->G.group_of_string "owner/../issues");
  fixed "issues(first:30" (obj["repository",obj["issues",conn ~after:"next" [topic()]]]);
  let p=B.list b (G.Repository("owner","repo",G.Issue)) ~after:None in
  let m=List.hd p.messages in check(m.id="I_1" && m.thread_id=Some "I_1") "Native topic identity";
  query "issues(first:30" (fun j->check(G.str "after"(G.field "variables"j)="next") "Cursor passthrough";data(obj["repository",obj["issues",conn []]]));
  ignore(B.list b (G.Repository("owner","repo",G.Issue)) ~after:p.after);
  raises(fun()->B.list b (G.Repository("other","repo",G.Issue)) ~after:p.after));
 run "progressive discussion replies" (fun b->
  let root=topic ~typ:"Discussion" ~id:"D_1"()in
  fixed "node(id:$id)" (obj["node",root]);
  let comment id=obj["id",js id;"body",js id;"createdAt",js "2026-10-02T00:00:00Z"]in
  fixed "comments(first:50" (obj["node",obj["comments",conn[comment "C_1"]]]);
  fixed "replies(first:50" (obj["node",obj["replies",conn ~after:"more" [comment "C_2"]]]);
  fixed "replies(first:50" (obj["node",obj["replies",conn[comment "C_3"]]]);
  let batches=ref []in B.thread b "D_1" ~emit:(fun x->batches:= !batches@[x]);
  check((List.hd !batches).complete=false && (List.hd(List.rev !batches)).complete) "Progressive completion";
  let messages=List.concat_map(fun (x:Reader.Content.thread_batch)->x.messages)!batches in
  check(List.map(fun (m:Reader.Content.message)->m.id,m.parent_id)messages=["D_1",None;"C_1",Some "D_1";"C_2",Some "C_1";"C_3",Some "C_1"]) "Reply tree");
 run "conditional notifications and Done" (fun b->
  let notification=obj["id",js "123";"updated_at",js "2026-11-01T00:00:00Z";
   "subject",obj["type",js "Issue";"url",js "https://api.github.com/repos/owner/repo/issues/1"];
   "repository",obj["owner",obj["login",js "owner"];"name",js "repo"]]in
  expect(fun r->check(r.H.meth=Get) "Notification GET";
   response ~headers:["x-poll-interval","60";"last-modified","fixture"] (`List[notification]));
  fixed "issue(number:$number)" (obj["repository",obj["issue",topic()]]);
  let p=B.list b G.Notifications ~after:None in
  let m=List.hd p.messages in
  check(m.created=(G.message(topic())).created) "Keep article publication timestamp";
  check(m.activity=Some "123:2026-11-01T00:00:00Z") "Notification revision";
  fixed "issue(number:$number)" (obj["repository",obj["issue",topic()]]);
  ignore(B.list b G.Notifications ~after:None);
  expect(fun r->check(r.H.meth=Delete && Uri.path(Uri.of_string r.url)="/notifications/threads/123") "Remote Done";response ~status:205 `Null);
  B.done_ b m.id;
  raises(fun()->B.done_ b m.id));
 run "reply subscription and errors never retry writes" (fun b->
  fixed "node(id:$id)" (obj["node",topic()]);
  query "addComment" (fun j->check(G.str "body"(G.field "variables"j)="Reply") "Reply body";
   response ~status:503 `Null);
  raises(fun()->B.send b {G.parent="I_1";body="Reply"});
  check(List.length !Mock.requests=2) "No mutation retry";
  fixed "node(id:$id)" (obj["node",topic()]);
  query "updateSubscription" (fun j->check(G.str "state"(G.field "variables"j)="IGNORED") "Native subscription setting";data(obj["updateSubscription",obj[]]));
  B.Subscription.set b "I_1" G.Ignored);
 run "review head guard and comment reconstruction" (fun b->
  fixed "changedFiles" (obj["node",obj["__typename",js "PullRequest";"id",js "PR_1";"headRefOid",js "new"]]);
  raises(fun()->B.submit_review b {G.pull_request="PR_1";commit="old";body="Review";event=G.Comment;comments=[]});
  check(List.length !Mock.requests=1) "Never review stale code";
  fixed "PullRequestReviewComment" (obj["node",obj["__typename",js "PullRequestReviewComment";"id",js "RC_1";"body",js "Comment"]]);
  fixed "PullRequestReviewComment" (obj["node",obj["__typename",js "PullRequestReviewComment";"pullRequest",topic ~typ:"PullRequest" ~id:"PR_1"();"replyTo",obj["id",js "RC_0"]]]);
  let m=B.read b "RC_1" in check(m.thread_id=Some "PR_1" && m.parent_id=Some "RC_0") "Archived review reply identity")
;
 run "first discussion comment is never its own parent" (fun b->
  fixed "DiscussionComment" (obj["node",obj["__typename",js "DiscussionComment";"id",js "C_1";"body",js "Comment"]]);
  fixed "DiscussionComment" (obj["node",obj["__typename",js "DiscussionComment";"discussion",topic ~typ:"Discussion" ~id:"D_1"();"replyTo",`Null]]);
  let m=B.read b "C_1" in check(m.parent_id=Some "D_1") "Root comment parent");
 run "inline reply stays in its review discussion" (fun b->
  fixed "PullRequestReviewComment" (obj["node",obj["__typename",js "PullRequestReviewComment";"pullRequest",topic ~typ:"PullRequest" ~id:"PR_1"();"replyTo",`Null]]);
  fixed "reviewThreads(first:50" (obj["node",obj["reviewThreads",conn[obj["id",js "RT_1"]]]]);
  fixed "comments(first:50" (obj["node",obj["comments",conn[obj["id",js "RC_1"]]]]);
  query "addPullRequestReviewThreadReply" (fun j->
   check(G.str "id"(G.field "variables" j)="RT_1") "Native review thread target";
   data(obj["addPullRequestReviewThreadReply",obj["comment",obj["id",js "RC_2";"body",js "Reply"]]]));
  let m=B.send b {G.parent="RC_1";body="Reply"}in
  check(m.thread_id=Some "PR_1" && m.parent_id=Some "RC_1") "Inline reply relationship")
