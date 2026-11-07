(* Explicit, read-only smoke check. Never run by dune runtest. No bodies or credentials logged. *)
let ()=Eio_posix.run(fun _->
 let errors=ref 0 in
 let run name f=if Option.fold ~none:true ~some:(fun selected->selected=name)(Sys.getenv_opt "EF_LIVE_ONLY") then try let count=f() in Printf.printf "PASS %s: %d items\n%!" name count
  with ex->incr errors;Printf.printf "FAIL %s: %s\n%!" name(Printexc.to_string ex) in
 let a=Atlantic_backend.Live.create() and j=Janestreet_backend.Live.create() in
 run "Atlantic author" (fun()->let page=Atlantic_backend.Live.list a "author.ian-bogost" ~after:None in
  ignore(Atlantic_backend.Live.read a "author.ian-bogost" (List.hd page.messages).id);List.length page.messages);
 run "Jane Street full post" (fun()->let page=Janestreet_backend.Live.list j "posts" ~after:None in
  let body=Janestreet_backend.Live.read j "posts" (List.hd page.messages).id in
  if String.length(Option.get body.message.html)<200 then failwith "Body unexpectedly short";List.length page.messages);
 let b=Blogger_backend.Live.create() in
 run "Blogger full post" (fun()->let group="posts.dostoe.blogspot.com" in
  let p=Blogger_backend.Live.list b group ~after:None in
  let a=Blogger_backend.Live.read b group (List.hd p.messages).id in
  if String.length(Option.get a.message.html)<200 then failwith "Missing full Blogger body";
  Option.iter(fun c->ignore(Blogger_backend.Live.list b group ~after:(Some c)))(p.after);
  List.length p.messages);
 let n=Netease_backend.Live.create() and p=Public_inbox_backend.Live.create() in
 run "NetEase events" (fun()->List.length(Netease_backend.Live.list n "events.398686067" ~after:None).messages);
 run "Public-inbox MIME thread" (fun()->List.length(Public_inbox_backend.Live.list p "https://list.orgmode.org/orgmode/875xwqj4tl.fsf@localhost/" ~after:None).messages);
 Option.iter(fun profile->
  let browser=Reader.Firefox_cookies.create ~profile () in
  let cookies=Reader.Firefox_cookies.get browser in
  let module H=struct let request r=
   let result=Rss_backend.Transport.request r in
   Option.iter(fun loc->let u=Uri.of_string loc in Printf.printf "Redirect: %s%s\n%!" (Option.value(Uri.host u)~default:"")(Uri.path u))(List.assoc_opt "location" result.headers);
   if result.status<>200 then
     Printf.printf "HTTP %d %s\n%!" result.status (Uri.path(Uri.of_string r.url));
   if result.status=403 then (try
    let j=Yojson.Safe.from_string result.body in
    let code=Zhihu_backend.field "code" (Zhihu_backend.field "error" j) in
    match code with `Int n->Printf.printf "Remote API error code: %d\n%!" n|_->()
    with _->());
   result end in
  let module B=Douban_backend.Make(H) in
  let d=B.create ~cookies () in
  run "Douban timeline" (fun()->let p=B.list d "timeline.152457381" ~after:None in List.length p.messages);
  run "Douban reply inbox" (fun()->let p=B.list d "replies.266418270" ~after:None in List.length p.messages);
  let z=Zhihu_backend.create_live ~cookies () in
  List.iter(fun group->run ("Zhihu "^group)(fun()->
   let p=Zhihu_backend.Live.list z group ~after:None in List.length p.messages))
   ["articles.people.xiscei";"answers.xiscei"]
 )(Sys.getenv_opt "EF_FIREFOX_PROFILE");
 if !errors>0 then exit 1)
