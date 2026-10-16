module R = Reddit_backend
module D = R.Decode
module M = Reader.Content
module H = R.Http

let check b message = if not b then failwith message
let obj xs = `Assoc xs
let str s = `String s
let listing ?(after=`Null) xs = obj ["kind",str "Listing";"data",obj ["children",`List xs;"after",after]]
let post = obj ["kind",str "t3";"data",obj ["name",str "t3_root";"title",str "Thread";
  "selftext",str "Body";"created_utc",`Int 1;"author",str "author"]]
let comment ?(parent="t3_root") ?(root="t3_root") ?(time=2) ?(replies=`String "") id =
  obj ["kind",str "t1";"data",obj ["name",str ("t1_"^id);"link_id",str root;
    "parent_id",str parent;"body",str ("comment "^id);"created_utc",`Int time;
    "author",str "reader";"score",`Int 42;"replies",replies]]
let more ?(parent="t3_root") ids = obj ["kind",str "more";
  "data",obj ["parent_id",str parent;"children",`List (List.map str ids)]]
let tree xs = `List [listing [post];listing xs]
let action xs = obj ["json",obj ["errors",`List [];"data",obj ["things",`List xs]]]
let response ?(status=200) ?(headers=[]) j = {H.status;headers;body=Yojson.Safe.to_string j}
let token ?(scope="identity read privatemessages submit vote") () = response (obj [
  "access_token",str "test-access-token";"expires_in",`Int 3600;
  "token_type",str "bearer";"scope",str scope])
let path r = Uri.path (Uri.of_string r.H.url)
let fields r = if r.H.meth=Get then Uri.query (Uri.of_string r.url)
  else Uri.query_of_encoded r.body
let field r key = match List.assoc_opt key (fields r) with Some [x] -> x | _ -> ""

module Mock = struct
  let script : (H.request -> H.response) Queue.t = Queue.create ()
  let requests = ref []
  let request r =
    requests := r :: !requests;
    if Queue.is_empty script then failwith ("Unexpected request: " ^ path r);
    (Queue.pop script) r
end
module B = R.Make(Mock)
let expect p answer = Queue.push (fun r -> check (path r=p) ("path "^p); answer r) Mock.script
let fixed p answer = expect p (fun _ -> answer)
let auth ?scope () = expect "/api/v1/access_token" (fun r ->
  check (field r "grant_type" = "refresh_token") "refresh grant";
  check (field r "refresh_token" = "test-refresh-token") "reuse token file";
  check (List.assoc "Authorization" r.headers = "Basic " ^ Base64.encode_exn (R.default_client_id ^ ":")) "same installed app";
  token ?scope ())
let raises predicate f =
  match f () with
  | _ -> failwith "Expected backend error"
  | exception R.Error e -> check (predicate e) (R.to_string e)
let with_backend f =
  Queue.clear Mock.script; Mock.requests := [];
  let file = Filename.temp_file "reddit-test-token-" "" in
  let ch = open_out file in output_string ch "test-refresh-token\n"; close_out ch;
  Unix.chmod file 0o600;
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  let time = ref 1000. and sleeps = ref [] in
  let sleep d = sleeps := d :: !sleeps; time := !time +. d; Eio.Fiber.yield () in
  let config = { (R.config ~username:"tester" ~token_file:file) with request_interval=0. } in
  let b = B.create ~config ~now:(fun () -> !time) ~sleep in
  f b file sleeps;
  check (Queue.is_empty Mock.script) "unconsumed expected requests"

let listing_search () = with_backend @@ fun b _ _ ->
  auth ();
  expect "/r/ocaml/new.json" (fun r -> check (field r "raw_json"="1") "raw JSON";
    response (listing ~after:(str "t3_next") [post]));
  expect "/r/ocaml/new.json" (fun r -> check (field r "after"="t3_next") "cursor"; response (listing [post]));
  expect "/r/ocaml/search.json" (fun r ->
    check (field r "restrict_sr"="on" && field r "q"="effects & 模块") "scoped encoding";
    check (field r "sort"="new") "no popularity sorting"; response (listing [post]));
  let p = B.list b (R.Subreddit "ocaml") ~after:None in
  check (p.after=Some "t3_next") "listing next cursor";
  let p2 = B.list b (R.Subreddit "ocaml") ~after:p.after in
  let found = B.search b (R.Subreddit "ocaml") ~query:"effects & 模块" ~after:None in
  check ((List.hd p.messages).id=(List.hd p2.messages).id && p.messages=found.messages) "identity across entrances";
  raises (function R.Invalid_input _ -> true | _ -> false)
    (fun () -> B.list b (R.Subreddit "ocaml/../../api") ~after:None);
  raises (function R.Unsupported _ -> true | _ -> false)
    (fun () -> B.search b R.Notifications ~query:"x" ~after:None)

let progressive_thread () = with_backend @@ fun b _ _ ->
  let batches = ref [] in
  auth ();
  fixed "/comments/root.json" (response (tree [comment ~time:4 "b";more ["a";"c"]]));
  expect "/api/morechildren.json" (fun r ->
    check (!batches<>[]) "first page must publish before requesting more";
    check (field r "sort"="old" && field r "link_id"="t3_root") "chronological comments";
    response (action [comment ~parent:"t1_a" ~time:3 "c";comment ~time:2 "a";comment ~time:4 "b"]));
  B.thread b "t3_root" ~emit:(fun batch -> batches := batch :: !batches);
  check ((List.hd !batches).complete) "thread complete";
  let messages = List.concat_map (fun (x:M.thread_batch) -> x.messages) (List.rev !batches) in
  let ordered = M.tree_order messages |> List.map (fun (depth,m) -> depth,m.M.id) in
  check (ordered=[0,"t3_root";1,"t1_a";2,"t1_c";1,"t1_b"]) "tree order by parent then time"

let batch_limits () = with_backend @@ fun b _ _ ->
  let ids = List.init 101 (fun i -> Printf.sprintf "c%x" i) in
  auth (); fixed "/comments/root.json" (response (tree [more ids]));
  List.iter (fun expected -> expect "/api/morechildren.json" (fun r ->
    let ids = String.split_on_char ',' (field r "children") in
    check (List.length ids=expected) "morechildren maximum 100";
    response (action (List.map comment ids)))) [100;1];
  let complete = ref false in B.thread b "t3_root" ~emit:(fun p -> complete := p.complete);
  check !complete "all 101 comments loaded"

let partial_and_validation () = with_backend @@ fun b _ _ ->
  auth (); fixed "/comments/root.json" (response (tree [comment "a";more ~parent:"t1_a" []]));
  expect "/comments/root.json" (fun r ->
    check (field r "comment"="a" && field r "context"="0") "continued branch";
    response (tree [comment "a";more ~parent:"t1_a" []]));
  let last = ref None in B.thread b "t3_root" ~emit:(fun p -> last:=Some p);
  let last = Option.get !last in check (not last.complete && last.gaps<>[]) "unresolved continuation must not look complete";
  fixed "/comments/root.json" (response (tree [comment ~root:"t3_other" "z"]));
  raises (function R.Invalid_response _ -> true | _ -> false) (fun () -> B.thread b "t3_root" ~emit:ignore)

let notifications () = with_backend @@ fun b _ _ ->
  auth ();
  expect "/message/inbox.json" (fun r ->
    check (field r "mark"="false") "reading inbox must not mark server notifications read";
    response (listing [comment "a";comment "b";obj ["kind",str "t4";"data",obj ["name",str "t4_private"]]]));
  let p = B.list b R.Notifications ~after:None in
  check (List.length p.notifications=2 && List.length p.messages=2 && List.for_all (fun (m:Reader.Content.message) -> m.body="" && m.thread_id=Some "t3_root") p.messages) "lazy notifications";
  check ((List.hd p.notifications).thread="t3_root") "notification thread key";
  check (List.length !Mock.requests=2) "notification did not fetch thread bodies"

let refresh_and_rate () = with_backend @@ fun b _ sleeps ->
  auth (); fixed "/r/ocaml/new.json" (response ~status:401 `Null);
  auth (); fixed "/r/ocaml/new.json" (response (listing []));
  ignore (B.list b (R.Subreddit "ocaml") ~after:None);
  fixed "/r/ocaml/new.json" (response ~status:429 ~headers:["retry-after","3"] `Null);
  raises (function R.Rate_limited 3. -> true | _ -> false)
    (fun () -> B.list b (R.Subreddit "ocaml") ~after:None);
  fixed "/r/ocaml/new.json" (response (listing []));
  ignore (B.list b (R.Subreddit "ocaml") ~after:None);
  check (List.mem 3. !sleeps) "rate cooldown shared by later requests"

let writes () = with_backend @@ fun b _ _ ->
  auth ();
  expect "/api/comment.json" (fun r -> check (r.meth=Post && field r "text"="你好 & effects") "reply form";
    response (action [comment "new"]));
  let reply = B.reply b ~parent:"t3_root" ~text:"你好 & effects" in check (reply.id="t1_new") "reply identity";
  expect "/api/submit.json" (fun r -> check (field r "kind"="self") "text post";
    response (obj ["json",obj ["errors",`List [];"data",obj ["name",str "t3_new"]]]));
  check (B.submit b ~subreddit:"ocaml" ~title:"Title" ~text:"Body"="t3_new") "submit id";
  List.iter (fun (dir,code) -> expect "/api/vote.json" (fun r ->
    check (field r "dir"=string_of_int code) "vote direction"; response (obj []));
    B.vote b ~id:"t1_new" ~direction:dir) [M.Upvote,1;M.No_vote,0;M.Downvote,-1];
  fixed "/api/comment.json" (response ~status:503 `Null);
  raises ((=) R.Uncertain_write) (fun () -> B.reply b ~parent:"t3_root" ~text:"do not replay");
  fixed "/api/comment.json" (response (obj ["json",obj ["errors",`List [`List [str "RATELIMIT";str "wait";str ""]]] ]));
  raises (function R.Rejected ["RATELIMIT"] -> true | _ -> false)
    (fun () -> B.reply b ~parent:"t3_root" ~text:"explicit rejection")

let vote_boundary () =
  List.iter (fun (code,likes,direction) ->
    let request = obj ["op",str "vote";"id",str "t1_a";"direction",`Int code] in
    check (R.operation_of_json request = R.Vote ("t1_a",direction)) "typed vote request";
    let result = R.result_json (R.Voted ("t1_a",direction)) in
    check (D.get "direction" result = `Int code) "vote result JSON compatibility";
    let thing = match post with
      | `Assoc fields ->
        let data = match List.assoc "data" fields with `Assoc xs -> xs | _ -> assert false in
        obj ["kind",str "t3";"data",obj (("likes",likes)::data)]
      | _ -> assert false in
    let message = D.message ~root:None thing in
    check (message.my_vote = direction) "Reddit likes decoded to typed vote";
    let encoded = D.get "message" (R.result_json (R.Replied message)) in
    check (D.get "my_vote" encoded = `Int code) "message vote JSON compatibility")
    [1,`Bool true,M.Upvote; -1,`Bool false,M.Downvote; 0,`Null,M.No_vote];
  List.iter (fun direction ->
    raises (function R.Invalid_input _ -> true | _ -> false) (fun () ->
      R.operation_of_json (obj ["op",str "vote";"id",str "t1_a";"direction",direction])))
    [`Int 2; `Int (-2); `String "1"; `Float 1.; `Bool true; `Null]

let missing_scope () = with_backend @@ fun b _ _ ->
  auth ~scope:"identity read privatemessages submit" ();
  raises (function R.Missing_scope "vote" -> true | _ -> false) (fun () -> B.vote b ~id:"t3_root" ~direction:M.Upvote);
  check (List.length !Mock.requests=1) "old grant not silently upgraded"

let oauth_state () = with_backend @@ fun b file _ ->
  let url = B.authorize b |> Uri.of_string in
  check (Uri.get_query_param url "client_id"=Some R.default_client_id) "authorization app ID";
  check (Uri.get_query_param url "redirect_uri"=Some R.default_redirect_uri) "same redirect";
  let state = Option.get (Uri.get_query_param url "state") in
  check (String.length state=64) "random state entropy";
  raises (function R.Authorization _ -> true | _ -> false)
    (fun () -> B.complete_authorization b (R.default_redirect_uri ^ "?code=x&state=wrong"));
  expect "/api/v1/access_token" (fun r ->
    check (field r "grant_type"="authorization_code" && field r "code"="test-code") "authorization exchange";
    response (obj ["access_token",str "access";"expires_in",`Int 3600;"token_type",str "bearer";
                   "scope",str "read vote";"refresh_token",str "new-refresh"]));
  let callback = R.default_redirect_uri ^ "?code=test-code&state=" ^ state in
  B.complete_authorization b callback;
  check (R.Credentials.read file="new-refresh" && (Unix.stat file).st_perm land 0o777=0o600) "private atomic credential save";
  raises (function R.Authorization _ -> true | _ -> false) (fun () -> B.complete_authorization b callback)

let cancellation_and_single_refresh () = with_backend @@ fun b _ _ ->
  auth ();
  let waiting,signal = Eio.Promise.create () in
  expect "/r/ocaml/new.json" (fun _ -> Eio.Promise.resolve signal (); let p,_=Eio.Promise.create () in Eio.Promise.await p);
  Eio.Fiber.first
    (fun () -> ignore (B.list b (R.Subreddit "ocaml") ~after:None))
    (fun () -> Eio.Promise.await waiting);
  fixed "/r/ocaml/new.json" (response (listing []));
  fixed "/r/ocaml/new.json" (response (listing []));
  Eio.Fiber.both
    (fun () -> ignore (B.list b (R.Subreddit "ocaml") ~after:None))
    (fun () -> ignore (B.list b (R.Subreddit "ocaml") ~after:None));
  let refreshes = List.filter (fun r -> path r="/api/v1/access_token") !Mock.requests in
  check (List.length refreshes=1) "cancelled read does not poison session or duplicate refresh"

let tests = ["listing/search",listing_search;"progressive tree",progressive_thread;
  "morechildren batching",batch_limits;"partial trees",partial_and_validation;
  "lazy notifications",notifications;"401/429",refresh_and_rate;"writes",writes;
  "vote boundary",vote_boundary;"OAuth scopes",missing_scope;"OAuth state/storage",oauth_state;
  "cancellation/session",cancellation_and_single_refresh]
let () = Eio_posix.run ~manage_signals:false @@ fun _ ->
  List.iter (fun (name,test) -> test (); Printf.printf "PASS: Reddit %s\n%!" name) tests
