(** GitHub owns identity, notification and subscription state. No browser cookies. *)
module Http = Reader.Http
module Transport = struct let request r = Http.request ~policy:Http.Https_or_loopback ~user_agent:"camulus/0.1 (GitHub)" r end
module C = Reader.Content
module CS = Reader.Content_state
let source = "github"
let field k = function `Assoc xs -> Option.value (List.assoc_opt k xs) ~default:`Null | _ -> `Null
let str k j = match field k j with `String s -> s | _ -> ""
let array = function `List xs -> xs | _ -> []
let nodes j = array (field "nodes" j)
let required k j = let s=str k j in if s="" then failwith ("GitHub response missing "^k);s
let js s = `String s
let optional = function None -> `Null | Some s -> js s
let time s = match Ptime.of_rfc3339 s with Ok(t,_,_) -> Ptime.to_float_s t | Error _ -> 0.
type kind = Issue | Pull_request | Discussion
let field_name = function Issue -> "issue" | Pull_request -> "pullRequest" | Discussion -> "discussion"
let connection = function Issue -> "issues" | Pull_request -> "pullRequests" | Discussion -> "discussions"
let kind_name = function Issue -> "Issue" | Pull_request -> "PullRequest" | Discussion -> "Discussion"
let kind = function "Issue" -> Issue | "PullRequest" -> Pull_request | "Discussion" -> Discussion | _ -> invalid_arg "Not a GitHub topic"
type group = Repository of string * string * kind | Notifications
type cursor = string
let segment s = s<>"" && s<>"." && s<>".." && String.for_all (function 'a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_'|'.' -> true | _ -> false) s
let group_of_string = function
 | "notifications" -> Notifications
 | s -> match String.split_on_char '/' s with
   | [owner;repo;section] when segment owner && segment repo ->
     let k=match section with "issues"->Issue|"pulls"->Pull_request|"discussions"->Discussion|_->invalid_arg "Use owner/repo/issues, pulls or discussions" in
     Repository(owner,repo,k)
   | _ -> invalid_arg "GitHub group must be owner/repo/issues, pulls or discussions"
let group_string = function Notifications -> "notifications" | Repository(o,r,k) -> o^"/"^r^"/"^(match k with Issue->"issues"|Pull_request->"pulls"|Discussion->"discussions")
type topic_location = { owner:string; repo:string; kind:kind; number:int }
let topic_of_url url =
 let uri=Uri.of_string url in
 if Uri.scheme uri<>Some "https" || Uri.host uri<>Some "github.com" then None else
 match String.split_on_char '/' (Uri.path uri) with
 | ["";owner;repo;section;number] when segment owner && segment repo ->
   let kind=match section with "issues"->Some Issue|"pull"->Some Pull_request|"discussions"->Some Discussion|_->None in
   (match kind,int_of_string_opt number with Some kind,Some number when number>0 ->Some {owner;repo;kind;number}|_->None)
 | _->None
let topic_url t = Printf.sprintf "https://github.com/%s/%s/%s/%d" t.owner t.repo
 (match t.kind with Issue->"issues"|Pull_request->"pull"|Discussion->"discussions") t.number
let scope_cursor scope after = match after with
 | None -> None
 | Some s -> (try let j=Yojson.Safe.from_string s in
   if str "scope" j<>scope then invalid_arg "Cursor belongs to another GitHub view";
   Some(required "after" j) with Yojson.Json_error _ -> invalid_arg "Invalid GitHub cursor")
let cursor scope after = Yojson.Safe.to_string (`Assoc["scope",js scope;"after",js after])
let next j = let info=field "pageInfo" j in
 if field "hasNextPage" info=`Bool true then Some(required "endCursor" info) else None
let page_info = "pageInfo { hasNextPage endCursor }"
let post_fields = "id body bodyHTML url createdAt author { login }"
let topic_fields = "__typename id number title body bodyHTML url createdAt updatedAt author { login } viewerSubscription"
let topic_fragment = String.concat " " (List.map (fun k -> "... on "^kind_name k^" { "^topic_fields^" }") [Issue;Pull_request;Discussion])
let message ?parent ?thread ?title j : C.message =
 let id=required "id" j in
 {id;thread_id=Some(Option.value thread ~default:id);parent_id=parent;references=[];
  title=Option.value title ~default:(str "title" j);author=str "login"(field "author" j);
  body=str "body" j;html=(match str "bodyHTML" j with ""->None|s->Some s);
  url=str "url" j;link_url=None;created=time(str "createdAt" j);unavailable=false;
  score=0;my_vote=No_vote;activity=(match str "updatedAt" j with ""->None|s->Some s)}
type config = {token:string}
let config token =
 if String.exists (fun c -> Char.code c<32 || Char.code c=127) token then invalid_arg "Invalid GitHub token";
 {token}
type tracking = Subscribed | Unsubscribed | Ignored
let tracking_string = function Subscribed->"SUBSCRIBED"|Unsubscribed->"UNSUBSCRIBED"|Ignored->"IGNORED"
let tracking = function "SUBSCRIBED"->Subscribed|"UNSUBSCRIBED"->Unsubscribed|"IGNORED"->Ignored|_->failwith "GitHub subscription state unavailable"
type draft = {parent:string;body:string}
type review_event = Comment | Approve | Request_changes
let review_event = function Comment->"COMMENT"|Approve->"APPROVE"|Request_changes->"REQUEST_CHANGES"
type side = Left | Right
let side = function Left->"LEFT"|Right->"RIGHT"
type location = {path:string;line:int;side:side;start_line:int option;start_side:side option}
type review_comment = {body:string;location:location}
type review = {pull_request:string;commit:string;body:string;event:review_event;comments:review_comment list}
module Make(H:Http.S) = struct
 type t = {config:config; now:unit->float; notifications:(string,(float * string option * Yojson.Safe.t * bool)) Hashtbl.t;
           notification_ids:(string,string list) Hashtbl.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create ?(now=Unix.gettimeofday) config = {config;now;notifications=Hashtbl.create 8;notification_ids=Hashtbl.create 32}
 let request t ?(meth=Http.Get) ?(headers=[]) ?(body="") path =
  if t.config.token="" then failwith "Configure GitHub authentication for Forge/ghub before using GitHub";
  if not(String.starts_with ~prefix:"/" path) || String.starts_with ~prefix:"//" path then invalid_arg "Invalid GitHub API path";
  let r=H.request {Http.meth;url="https://api.github.com"^path;body;
   headers=["Authorization","Bearer "^t.config.token;"Accept","application/vnd.github+json";"X-GitHub-Api-Version","2022-11-28";"Content-Type","application/json"]@headers} in
  if r.status<>304 && (r.status<200 || r.status>=300) then
    failwith(Printf.sprintf "GitHub HTTP %d%s" r.status (if r.status=401 then ": check token" else if r.status=403 || r.status=429 then ": permissions or rate limit; no automatic write retry" else ""));
  r
 let json r = if r.Http.body="" then `Null else Yojson.Safe.from_string r.body
 let graphql t query variables =
  let j=request t ~meth:Post ~body:(Yojson.Safe.to_string(`Assoc["query",js query;"variables",`Assoc variables])) "/graphql" |> json in
  if array(field "errors" j)<>[] then failwith "GitHub GraphQL request failed (permissions, query or unavailable object); no write retry";
  let data=field "data" j in if data=`Null then failwith "GitHub returned no GraphQL data";data
 let node t id selection =
  let n=graphql t ("query($id:ID!){node(id:$id){__typename "^selection^"}}") ["id",js id] |> field "node" in
  if n=`Null then failwith "GitHub content is unavailable";n
 let topic t id = node t id topic_fragment
 let context t id =
  let j=node t id (topic_fragment^" ... on IssueComment { issue { "^topic_fields^" } pullRequest { "^topic_fields^" } } ... on DiscussionComment { discussion { "^topic_fields^" } replyTo { id } } ... on PullRequestReviewComment { pullRequest { "^topic_fields^" } replyTo { id } }") in
  let root,parent=match str "__typename" j with
  | "Issue"|"PullRequest"|"Discussion" -> j,None
  | "IssueComment" -> let pr=field "pullRequest" j in (if pr<>`Null then pr else field "issue" j),None
  | "DiscussionComment" -> field "discussion" j,Some (match str "id" (field "replyTo" j) with ""->id|p->p)
  | "PullRequestReviewComment" -> field "pullRequest" j,Some (match str "id" (field "replyTo" j) with ""->id|p->p)
  | _ -> failwith "Unsupported GitHub content" in
  root,parent,str "__typename" j
 let root t id = let root,parent,_=context t id in root,parent
 let list_repository t group ~after =
  match group with Notifications -> assert false | Repository(o,r,k) ->
  let scope=group_string group in
  let data=graphql t ("query($owner:String!,$repo:String!,$after:String){repository(owner:$owner,name:$repo){"^connection k^
   "(first:30,after:$after,orderBy:{field:UPDATED_AT,direction:DESC}){nodes{"^topic_fields^"} "^page_info^"}}}")
   ["owner",js o;"repo",js r;"after",optional(scope_cursor scope after)] |> field "repository" in
  if data=`Null then failwith "GitHub repository unavailable";
  let conn=field (connection k) data in
  {C.messages=List.map message(nodes conn);after=Option.map(cursor scope)(next conn)}
 let notification_page t page =
  let cached=Hashtbl.find_opt t.notifications page in
  match cached with
  | Some(until,_,data,more) when t.now()<until -> data,more
  | _ ->
    let headers=match cached with Some(_,Some modified,_,_)->["If-Modified-Since",modified]|_->[] in
    let r=request t ~headers ("/notifications?all=true&is_done=false&per_page=50&page="^page) in
    let interval=Option.bind(List.assoc_opt "x-poll-interval" r.headers) int_of_string_opt |> Option.value ~default:60 |> max 1 in
    let data,more=if r.status=304 then match cached with Some(_,_,j,m)->j,m|None->failwith "Unexpected GitHub 304" else
      let more=match List.assoc_opt "link" r.headers with None->false|Some s->
       List.exists(fun part->String.ends_with ~suffix:"rel=\"next\"" (String.trim part))(String.split_on_char ',' s) in json r,more in
    let modified=match List.assoc_opt "last-modified" r.headers,cached with None,Some(_,m,_,_)->m|m,_->m in
    Hashtbl.replace t.notifications page (t.now()+.float interval,modified,data,more);data,more
 let list_notifications t ~after =
  let p=match scope_cursor "notifications" after with None->1|Some s->Option.value(int_of_string_opt s)~default:0 in
  if p<1 then invalid_arg "Invalid notification page";
  let data,more=notification_page t (string_of_int p) in
  let rows=List.filter_map(fun n ->
   let subject=field "subject" n in
   match str "type" subject with
   | "Issue"|"PullRequest"|"Discussion" as typ ->
     let k=kind typ in
     let repo=field "repository" n in let owner=str "login"(field "owner" repo) and name=str "name" repo in
     let path=Uri.path(Uri.of_string(str "url" subject)) in
     let number=Option.bind(List.rev(String.split_on_char '/' path) |> List.find_opt (fun _ -> true))int_of_string_opt in
     (match number with None->None|Some number ->
      let j=graphql t ("query($owner:String!,$repo:String!,$number:Int!){repository(owner:$owner,name:$repo){"^field_name k^"(number:$number){"^topic_fields^"}}}")
        ["owner",js owner;"repo",js name;"number",`Int number] |> field "repository" |> field(field_name k) in
      if j=`Null then None else
      let m=message j in let event_id=required "id" n in
      let ids=Option.value(Hashtbl.find_opt t.notification_ids m.id)~default:[] in
      Hashtbl.replace t.notification_ids m.id (List.sort_uniq String.compare(event_id::ids));
      let updated=required "updated_at" n in
      Some {m with C.activity=Some(event_id^":"^updated)})
   | _ -> None) (array data) in
  {C.messages=rows;
   after=(if more then Some(cursor "notifications" (string_of_int(p+1)))else None)}
 let list t group ~after = match group with Notifications->list_notifications t ~after|_->list_repository t group ~after

 let iter_connection t ~id ~typename ~name ~fields f =
  let rec loop seen after =
   let conn=graphql t ("query($id:ID!,$after:String){node(id:$id){... on "^typename^"{"^name^"(first:50,after:$after){nodes{"^fields^"} "^page_info^"}}}}")
    ["id",js id;"after",optional after] |> field "node" |> field name in
   if conn=`Null then failwith "GitHub connection unavailable";
   f(nodes conn);
   match next conn with None->()|Some c->if List.mem c seen then failwith "Repeated GitHub cursor" else loop(c::seen)(Some c) in
  loop [] None
 let review_threads t id =
  let threads=ref [] in
  iter_connection t ~id ~typename:"PullRequest" ~name:"reviewThreads"
   ~fields:"id path line startLine diffSide startDiffSide isResolved isOutdated viewerCanResolve viewerCanUnresolve"
   (fun rows -> List.iter(fun row ->
     let comments=ref [] in
     iter_connection t ~id:(required "id" row) ~typename:"PullRequestReviewThread" ~name:"comments"
       ~fields:(post_fields^" diffHunk replyTo { id }") (fun xs -> comments:= !comments@xs);
     match row with `Assoc xs -> threads:= !threads@[`Assoc(("comments",`List !comments)::xs)] | _ -> ()) rows);
  !threads
 let pull_request t id =
  let j=node t id "... on PullRequest { id number url title headRefOid baseRefOid changedFiles repository { nameWithOwner } }" in
  if str "__typename" j<>"PullRequest" then invalid_arg "Select a pull request";j
 let pull_path j =
  let repo=required "nameWithOwner" (field "repository" j) in
  (match String.split_on_char '/' repo with [o;r] when segment o && segment r -> () | _ -> failwith "Invalid GitHub repository");
  let n=match field "number" j with `Int n when n>0->n|_->failwith "Invalid pull request number" in
  Printf.sprintf "/repos/%s/pulls/%d" repo n
 let review_snapshot t id =
  let pr=pull_request t id in
  let rec files page acc =
   if page>30 then failwith "GitHub PR exceeds the 3000 file API limit";
   let xs=request t (pull_path pr^"/files?per_page=100&page="^string_of_int page) |> json |> array in
   let acc=acc@xs in if List.length xs<100 then acc else files(page+1)acc in
  let files=files 1 [] in
  let expected=match field "changedFiles" pr with `Int n->n|_->List.length files in
  if List.length files<>expected then failwith "GitHub returned an incomplete changed-file list";
  let threads=review_threads t id in
  let current=pull_request t id in
  if str "headRefOid" current<>str "headRefOid" pr || str "baseRefOid" current<>str "baseRefOid" pr then
   failwith "PR changed while loading; refresh the diff";
  `Assoc["pr",pr;"files",`List files;"threads",`List threads]
 let resolve_thread t id resolved =
  let action=if resolved then "resolveReviewThread" else "unresolveReviewThread" in
  ignore(graphql t ("mutation($id:ID!){"^action^"(input:{threadId:$id}){thread{id isResolved}}}") ["id",js id])
 let reply_review t id body =
  if String.trim body="" then invalid_arg "Empty review reply";
  ignore(graphql t "mutation($id:ID!,$body:String!){addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:$id,body:$body}){comment{id}}}"
   ["id",js id;"body",js body])
 let submit_review t (review:review) =
  let pr=pull_request t review.pull_request in
  if required "headRefOid" pr<>review.commit then failwith "PR head changed: refresh and review the new diff before submitting";
  if review.event=Request_changes && String.trim review.body="" then invalid_arg "Explain the requested changes";
  let threads=List.map(fun (c:review_comment) ->
   let l=c.location in
   if l.path="" || l.line<1 || String.trim c.body="" then invalid_arg "Invalid review comment";
   (match l.start_line,l.start_side with
    | None,None -> ()
    | Some start,Some side when start>0 && side=l.side && start<=l.line -> ()
    | _ -> invalid_arg "Invalid review line range");
   `Assoc(["body",js c.body;"path",js l.path;"line",`Int l.line;"side",js(side l.side)] @
     (match l.start_line,l.start_side with Some n,Some s->["startLine",`Int n;"startSide",js(side s)]|_->[]))) review.comments in
  let result=graphql t "mutation($input:AddPullRequestReviewInput!){addPullRequestReview(input:$input){pullRequestReview{id state url}}}"
    ["input",`Assoc["pullRequestId",js review.pull_request;"commitOID",js review.commit;"body",js review.body;
     "event",js(review_event review.event);"threads",`List threads]] in
  field "pullRequestReview"(field "addPullRequestReview" result)
 let thread t id ~emit =
  let j,_=root t id in let root=message j in let typ=str "__typename" j in
  let batch messages complete=emit {C.thread=root.id;messages;gaps=[];complete} in
  batch [root] false;
  iter_connection t ~id:root.id ~typename:typ ~name:"comments" ~fields:post_fields (fun comments->
   batch (List.map(message ~parent:root.id ~thread:root.id ~title:root.title) comments) false;
   if typ="Discussion" then List.iter(fun c ->
    let parent=required "id" c in
    iter_connection t ~id:parent ~typename:"DiscussionComment" ~name:"replies" ~fields:post_fields (fun replies ->
     batch(List.map(message ~parent ~thread:root.id ~title:root.title)replies)false)) comments);
  if typ="PullRequest" then List.iter(fun review ->
   let comments=array(field "comments" review) in
   match comments with []->()|first::_ ->
    let parent=required "id" first in
    let state=(if field "isResolved" review=`Bool true then "resolved" else "unresolved")^
      (if field "isOutdated" review=`Bool true then ", outdated" else "") in
    batch(List.map(fun c ->
      let m=message ~parent:(if required "id" c=parent then root.id else parent) ~thread:root.id ~title:root.title c in
      if m.id<>parent then m else
      let context=required "path" review^" ("^state^")\n"^str "diffHunk" c in
      {m with C.body=context^"\n\n"^m.body;
       html=Some("<pre>"^Reader.Html.escape context^"</pre>"^Option.value m.html ~default:(Reader.Html.escape m.body))})comments)false)
    (review_threads t root.id);
  batch [] true
 let read t id =
  let j=node t id (topic_fragment^" ... on IssueComment { "^post_fields^" } ... on DiscussionComment { "^post_fields^" } ... on PullRequestReviewComment { "^post_fields^" }") in
  let root,parent=root t id in let rid=required "id" root in
  let parent=Option.value parent ~default:rid in
  message ?parent:(if rid=id then None else Some(if parent=id then rid else parent)) ~thread:rid ~title:(str "title" root) j
 let archive_article _ message = {CS.message;attachments=`List [];raw=None;files=[]}
 let done_ t id =
  let ids=Option.value(Hashtbl.find_opt t.notification_ids id)~default:[] in
  if ids=[] then failwith "Refresh GitHub notifications before marking this topic Done";
  List.iter(fun id ->
   if not(String.for_all(function '0'..'9'->true|_->false)id) || id="" then failwith "Invalid notification identity";
   ignore(request t ~meth:Delete ("/notifications/threads/"^id))) ids;
  Hashtbl.clear t.notifications;Hashtbl.remove t.notification_ids id
 module Subscription = struct
  type nonrec t=t
  type target=string
  type settings=tracking
  let get t id = let j,_=root t id in tracking(required "viewerSubscription" j)
  let set t id settings =
   let j,_=root t id in
   ignore(graphql t "mutation($id:ID!,$state:SubscriptionState!){updateSubscription(input:{subscribableId:$id,state:$state}){subscribable{id}}}"
    ["id",field "id" j;"state",js(tracking_string settings)])
 end
 let send t (draft:draft) =
  if String.trim draft.body="" then invalid_arg "Empty GitHub reply";
  let j,parent,typ=context t draft.parent in
  (* Review comments belong to a diff discussion, not the PR's general comment
     stream. Resolve its native thread ID before issuing the single mutation. *)
  let review_thread=if typ="PullRequestReviewComment" then
    Some(List.find(fun thread->List.exists(fun c->str "id" c=draft.parent)(array(field "comments" thread)))
      (review_threads t (required "id" j))) else None in
  let parent=match review_thread with
   | None->parent
   | Some thread->(match array(field "comments" thread)with c::_->Some(required "id" c)|[]->assert false)in
  let data=if str "__typename" j="Discussion" then
    graphql t ("mutation($id:ID!,$body:String!,$reply:ID){addDiscussionComment(input:{discussionId:$id,body:$body,replyToId:$reply}){comment{"^post_fields^"}}}")
     ["id",field "id" j;"body",js draft.body;"reply",optional parent] |> field "addDiscussionComment" |> field "comment"
   else match review_thread with
   | Some thread->graphql t ("mutation($id:ID!,$body:String!){addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:$id,body:$body}){comment{"^post_fields^"}}}")
     ["id",field "id" thread;"body",js draft.body] |> field "addPullRequestReviewThreadReply" |> field "comment"
   | None->graphql t ("mutation($id:ID!,$body:String!){addComment(input:{subjectId:$id,body:$body}){commentEdge{node{"^post_fields^"}}}}")
     ["id",field "id" j;"body",js draft.body] |> field "addComment" |> field "commentEdge" |> field "node" in
  message ~thread:(required "id" j) ~parent:(Option.value parent ~default:(required "id" j)) ~title:(str "title" j) data
end
module Live=Make(Transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
module Threading : Reader.Backend.Threading with type t=Live.t = Live
module Subscription=Live.Subscription
