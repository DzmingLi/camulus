open! Core
open! Ecaml
open Reader_backend
module G=Github_backend
module P=Reader.Protocol
module CS=Reader.Content_state
let credentials=ref(G.config "")
type operation = Thread of string | Done of string | Reply of G.draft
 | Review of string | Submit_review of G.review | Resolve of string * bool | Review_reply of string * string
 | Tracking of string * string option
 type P.work += Github of operation
let prepare ~notify:_ =
 let config= !credentials in
 Some {drain=(fun()->[]);close=(fun()->());create=(fun ~net:_ ~now ~sleep:_ ~groups:_ ->
  let t=G.Live.create ~now config in
  {matches_source=String.equal G.source;all_pages=false;
   page=(fun ~source:_ group ~after->let group=G.group_of_string group in G.Live.list t group ~after);
   archive=(fun ~source:_ ~refresh:_ target id ~native_thread ~add ->
    match target with CS.Article->add(G.Live.archive_article t (G.Live.read t id));true
    | CS.Thread ->let complete=ref false in G.Live.thread t (native_thread id) ~emit:(fun b->
       List.iter b.messages ~f:(fun m->add(G.Live.archive_article t m));complete:=b.complete);!complete);
   execute=(fun work ~observe ~emit ->
    let result kind fields=emit(Presentation("github",`Assoc(("kind",`String kind)::fields)))in
    let subscription id setting=
      Option.iter setting ~f:(fun s->G.Subscription.set t id (G.tracking s));
      result "subscription" ["source",`String G.source;"target",`String id;
        "setting",`String(G.tracking_string(G.Subscription.get t id));
        "choices",`List(List.map [G.Subscribed;Unsubscribed;Ignored]~f:(fun s->`String(G.tracking_string s)))] in
    match work with
    | Reader_backend.Subscription(source,id,setting) when String.equal source G.source ->subscription id setting;true
    | Github op ->
      (match op with
       | Thread id->G.Live.thread t id ~emit:(fun b->observe ~source:G.source b.messages;
          result "thread" ["thread",`String b.thread;"messages",`List(List.map b.messages ~f:CS.message_json);"complete",`Bool b.complete])
       | Done id->G.Live.done_ t id;result "done" []
       | Reply draft->let m=G.Live.send t draft in observe ~source:G.source [m];result "sent" []
       | Review id->result "review" ["data",G.Live.review_snapshot t id]
       | Submit_review review->result "sent" ["review",G.Live.submit_review t review]
       | Resolve(id,value)->G.Live.resolve_thread t id value;result "resolved" []
       | Review_reply(id,body)->G.Live.reply_review t id body;result "sent" []
       | Tracking(id,setting)->subscription id setting);true
    | _->false)})}
let install_protocol ~ensure_ui:_ ~idle:_ ~submit:_ = worker_factories:=prepare :: !worker_factories
let assign key value row = call "cons"[call "cons"[sym key;value];call "assq-delete-all"[sym key;call "copy-alist"[row]]]
let callbacks : (int, Value.t -> unit) Hashtbl.t = Hashtbl.create(module Int)
let completions : (int, bool -> unit) Hashtbl.t = Hashtbl.create(module Int)
let request ~submit ?(done_=(fun _->())) op receive =
 let id=submit (Github op) in
 Hashtbl.set callbacks ~key:id ~data:receive;Hashtbl.set completions ~key:id ~data:done_;id
let selected () =
 let row=Forge.selected () in
 if truth(get "camulus-reader--archive") then user_error "Open a live GitHub conversation";
 row
let draft_path name =
 let directory=call "expand-file-name"[get "camulus-github-drafts-directory"]in
 run "make-directory"[directory;sym "t"];run "set-file-modes"[directory;i 0o700];
 text(call "expand-file-name"[s name;directory])
let digest id = Stdlib.Digest.to_hex(Stdlib.Digest.string id)
let write_json path value =
 let temp=path^".tmp" in
 let channel=Stdlib.open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] 0o600 temp in
 Exn.protect ~finally:(fun()->Stdlib.close_out_noerr channel) ~f:(fun()->
  Stdlib.output_string channel (Yojson.Safe.to_string value);Stdlib.flush channel);
 Stdlib.Sys.rename temp path
let read_json path = Yojson.Safe.from_file path
let review_path id = draft_path ("review-"^digest id^".json")
let open_draft ~send meta =
 let id=G.required "id" meta and kind=G.required "kind" meta in
 let path=draft_path(kind^"-"^digest id^".md")in
 let metadata=path^".json" in
 if not(Stdlib.Sys.file_exists metadata)then write_json metadata meta;
 (* Prefer the summary window, keeping the article visible on the right. *)
 let target=List.find_map (values(call "window-list"[]))~f:(fun w->
  let buffer=call "window-buffer"[w]in
  if in_buffer buffer(fun()->truth(call "derived-mode-p"[sym "camulus-reader-feed-mode"]))then Some w else None)in
 Option.iter target ~f:(fun w->run "select-window"[w]);
 run "find-file"[s path];
 if not(truth(call "derived-mode-p"[sym "camulus-github-draft-mode"])) then (
  run "camulus-github-draft-mode"[];
  local "camulus-github--draft-meta"(of_json(read_json metadata));
  Writing.attach {
   snapshot=(fun()->text(call "buffer-substring-no-properties"[call "point-min"[];call "point-max"[]]));
   save=(fun ~source ~complete->
    try
     run "write-region"[s source;nil;s path;nil;sym "silent"];
     run "set-file-modes"[s path;i 0o600];complete(Ok ())
    with ex->complete(Error(Exn.to_string ex)));
   send})
let location_json (l:G.location)=`Assoc["path",`String l.path;"line",`Int l.line;"side",`String(G.side l.side);
 "startLine",(match l.start_line with None->`Null|Some n->`Int n);"startSide",(match l.start_side with None->`Null|Some s->`String(G.side s))]
let location_of_json j : G.location =
 let integer k=match G.field k j with `Int n->n|_->invalid_arg("Missing "^k)in
 let side=function "LEFT"->G.Left|"RIGHT"->G.Right|_->invalid_arg "Invalid diff side"in
 {path=G.required "path" j;line=integer "line";side=side(G.required "side" j);
 start_line=(match G.field "startLine" j with `Null->None|_->Some(integer "startLine"));
 start_side=(match G.field "startSide" j with `Null->None|_->Some(side(G.required "startSide" j)))}
let review_comments j=G.array(G.field "comments"j)|>List.map~f:(fun c->
 {G.body=G.required "body"c;location=location_of_json(G.field "location"c)})
let review_state data =
 let pr=G.field "pr" data in let id=G.required "id" pr in let path=review_path id in
 if Stdlib.Sys.file_exists path then read_json path else
 let state=`Assoc["id",`String id;"commit",G.field "headRefOid"pr;"comments",`List[]]in
 write_json path state;state
let render_review data =
 let pr=field "pr" data in
 let state=review_state(to_json data)in
 let buffer=call "get-buffer-create"[s("*Camulus PR: "^text(field "title"pr)^"*")]in
 run "pop-to-buffer"[buffer];run "camulus-github-review-mode"[];
 local "camulus-github--review-data" data;
 local "header-line-format"(s "GitHub review · c: comment  x: remove draft comment  r: reply  t: resolve  o: local code  C-c C-c: submit review") ;
 Current_buffer.set_value_temporarily Sync (Var.create(Symbol.intern "inhibit-read-only")Value.Type.value)(sym "t")~f:(fun()->
  Current_buffer.erase();Point.insert(text(field "title"pr)^"\n"^text(field "url"pr)^"\nHead: "^text(field "headRefOid"pr)^"\n\n");
  if not(String.equal(G.str "commit"state)(text(field "headRefOid"pr)))then
   Point.insert "Saved comments refer to an older commit. Remove the old draft comments before starting a new review.\n\n";
  let insert ?location ?thread text_ =
   let start=call "point"[] in Point.insert(text_^"\n");let finish=call "point"[]in
   Option.iter location~f:(fun l->run "put-text-property"[start;finish;sym "camulus-github-location";of_json(location_json l)]);
   Option.iter thread~f:(fun row->run "put-text-property"[start;finish;sym "camulus-github-review-thread";row]) in
  List.iter(values(field "files"data))~f:(fun file->
   let path=text(field "filename"file)in
   insert("diff --git a/"^path^" b/"^path);
   let patch=text(field "patch"file)in
   if String.is_empty patch then insert "[GitHub did not supply a text patch; open the PR to inspect this file]"
   else (
    let old=ref 0 and new_=ref 0 in
    List.iter(String.split_lines patch)~f:(fun line->
     if String.is_prefix line ~prefix:"@@ " then(
      (try
        let parts=String.split line ~on:' 'in
        let number s=String.drop_prefix s 1 |> String.split ~on:',' |> List.hd_exn |> Int.of_string in
        old:=number(List.nth_exn parts 1);new_:=number(List.nth_exn parts 2)
       with _->old:=0;new_:=0);
      insert line)
     else if String.is_prefix line ~prefix:"+" && !new_>0 then(
       insert ~location:{G.path;line= !new_;side=Right;start_line=None;start_side=None} line;Int.incr new_)
     else if String.is_prefix line ~prefix:"-" && !old>0 then(
       insert ~location:{G.path;line= !old;side=Left;start_line=None;start_side=None} line;Int.incr old)
     else if String.is_prefix line ~prefix:" " && !new_>0 then(
       insert ~location:{G.path;line= !new_;side=Right;start_line=None;start_side=None} line;Int.incr new_;Int.incr old)
     else insert line));insert "");
  insert "Review discussions";
  List.iter(values(field "threads"data))~f:(fun thread->
   insert ~thread (text(field "path"thread)^" · "^(if truth(field "isResolved"thread)then "resolved"else "unresolved")^
     (if truth(field "isOutdated"thread)then " · outdated"else ""));
   List.iter(values(field "comments"thread))~f:(fun c->insert ~thread (text(field "login"(field "author"c))^": "^text(field "body"c)));
   insert "");
  insert (Printf.sprintf "Saved draft comments: %d" (List.length(review_comments state))));
 local "buffer-read-only"(sym "t");run "goto-char"[i 1]
let install ~submit () =
 let request=request ~submit in
 run "require"[sym "auth-source"];run "require"[sym "diff-mode"];
 custom "camulus-github-auth-user" nil "Optional auth-source login at api.github.com. Nil reuses Forge/ghub's GitHub user and token.";
 custom ~kind:Customization.Type.Boolean "camulus-github-notifications" nil "Include GitHub's conversation notifications in Home; repository registration is separate.";
 custom ~kind:Customization.Type.Directory "camulus-github-drafts-directory" (data_path "XDG_STATE_HOME" "~/.local/state" "github-drafts/") "Local GitHub reply and review drafts.";
 List.iter["camulus-github--draft-meta";"camulus-github--review-data"]~f:(fun name->variable name nil);
 let setup ()=
  (* ghub resolves Forge's lazy auth-source secret on the owner thread. No
     credentials are requested or created while using unrelated backends. *)
  let explicit=get "camulus-github-auth-user" in
  let token=if truth explicit then
    Option.value_map (Credentials_emacs.auth_source ~user:(text explicit) "api.github.com")
      ~default:"" ~f:(fun credential->credential.secret)
   else if truth(call "require"[sym "ghub";nil;sym "t"]) then
    (try
      let username=call "ghub--username"[s "api.github.com";sym "github"]in
      text(call "ghub--token"[s "api.github.com";username;sym "forge";sym "t"])
     with _->"")
   else "" in
  credentials:=G.config token in
 let browse group=ignore(G.group_of_string group:G.group);
  run "camulus-reader-browse-entry"[alist["name",s("GitHub · "^group);"source",s G.source;"group",s group]]in
 let send_draft ~complete =
  let meta=to_json(get "camulus-github--draft-meta")in
  let id=G.required "id"meta and kind=G.required "kind"meta in
  let body=text(call "buffer-substring-no-properties"[call "point-min"[];call "point-max"[]])
  and file=text(get "buffer-file-name")in
  let op=match kind with
   | "reply"->Reply{G.parent=id;body}
   | "review-reply"->Review_reply(id,body)
   | "review"->let state=read_json(review_path id)in
     let choice=text(call "completing-read"[s "Submit review: ";Value.list(List.map["COMMENT";"APPROVE";"REQUEST_CHANGES"]~f:s);nil;sym "t"])in
     let event=match choice with "APPROVE"->G.Approve|"REQUEST_CHANGES"->Request_changes|_->Comment in
     Submit_review{G.pull_request=id;commit=G.required "commit"state;body;event;comments=review_comments state}
   | _->invalid_arg "Invalid GitHub draft"in
  let received=ref false in
  ignore(request ~done_:(fun ok->
   if not(ok && !received)then complete(Error "GitHub submission failed; draft retained. Check GitHub before resending after a timeout.")
   else (
    let cleaned=try
      List.iter[file;file^".json"]~f:(fun path->if Stdlib.Sys.file_exists path then Stdlib.Sys.remove path);
      if String.equal kind "review" then Stdlib.Sys.remove(review_path id);true
     with _->false in
    complete(Ok {Writing.close=cleaned;message=if cleaned then "GitHub: submitted" else "GitHub: submitted; draft cleanup failed, do not resend"})))
   op (fun data->if String.equal(text(field "kind"data))"sent"then received:=true):int)in
 let open_draft=open_draft ~send:send_draft in
 Forge.install ~browse;
 let reply ()=let m=selected()in open_draft(`Assoc["kind",`String "reply";"id",to_json(field "id"m)])in
 let review ()=let m=selected()in let id=text(field "thread_id"m)in
  ignore(request(Review(if String.is_empty id then text(field "id"m)else id))(fun data->if String.equal(text(field "kind"data))"review"then render_review(field "data"data)):int)in
 define1 "camulus-github--event"(fun event->let id=integer(call "nth"[i 1;event])in
  let kind=text(call "symbol-name"[call "car"[event]])in
  if String.equal kind "github" then Option.iter(Hashtbl.find callbacks id)~f:(fun f->f(call "nth"[i 2;event]))
  else if String.equal kind "finished" then(
   Hashtbl.remove callbacks id;
   Option.iter(Hashtbl.find completions id)~f:(fun f->
    Hashtbl.remove completions id;let ok=Value.eq(call "nth"[i 2;event])(sym "completed")in
    if not ok then message "GitHub: %s"[call "nth"[i 3;event]];f ok));nil);
 hook "camulus-probe-event-hook" "camulus-github--event";
 register { (base ~name:"GitHub" ~matches:(String.equal G.source) ~threaded:true
   ~read:(fun ~source:_ ~id ~thread_id ~group:_->i(request(Thread(Option.value thread_id~default:id))ignore))) with
  start=setup;thread_contains_bodies=true;events=["github"];
  normalize=(fun row->ignore(G.group_of_string(text(field "group"row)):G.group);row);
  followed=Some(fun()->if truth(get "camulus-github-notifications")then
   [alist["name",s "GitHub · 通知";"source",s G.source;"group",s "notifications"]]else []);
  subscription_target=Some(fun ~source:_ ~selected->let row=selected()in
   Some(text(let v=field "thread_id"row in if truth v then v else field "id"row)));
  browser=Some("GitHub","camulus-github");
  controls=[{key="F";hint="F: Forge";command="camulus-github-forge-topic"};{key="M";hint="M: Magit";command="camulus-github-magit"};{key="r";hint="r: reply";command="camulus-github-reply"};{key="v";hint="v: PR review";command="camulus-github-review"}];
  list_controls=[{key="F";hint="F: Forge";command="camulus-github-forge-topic"};{key="M";hint="M: Magit";command="camulus-github-magit"};{key="v";hint="v: PR review";command="camulus-github-review"};{key="D";hint="D: drafts";command="camulus-github-drafts"}];
  delete=Some(fun ~entry ~row ~refresh->
   if not(String.equal(text(field "group"entry))"notifications")then user_error "Done applies to GitHub notifications; browse-only topics are not deleted";
   ignore(request ~done_:(fun ok->if ok then refresh())(Done(text(field "id"row)))ignore:int)) };
 define0 ~interactive:true "camulus-github"(fun()->
  let entries=List.filter(values(get "camulus-groups"))~f:(fun e->String.equal(text(field "source"e))G.source)in
  if List.is_empty entries then user_error "Register a GitHub group with C / camulus-groups first";
  let chosen=call "completing-read"[s "GitHub group: ";Value.list(List.map entries~f:(field "name"));nil;sym "t"]in
  Option.iter(List.find entries~f:(fun e->Value.equal(field "name"e)chosen))~f:(fun e->run "camulus-reader-browse-entry"[e]));
 Defun.defun(Symbol.intern "camulus-github-browse")[%here]~docstring:"Browse owner/repo/issues, owner/repo/pulls or owner/repo/discussions."
  ~interactive:(Prompt "GitHub group: ")(Returns Value.Type.unit)(let open Defun.Let_syntax in let%map_open group=required "group"string in browse group);
 define0 ~interactive:true "camulus-github-reply" reply;
 define0 ~interactive:true "camulus-github-review" review;
 mode "camulus-github-review-mode" "diff-mode" "GitHub review" "";
 run "put"[sym "camulus-github-review-mode";sym "mode-class";sym "special"];
 mode "camulus-github-draft-mode" "text-mode" "GitHub draft" "";
 define0 ~interactive:true "camulus-github-drafts"(fun()->
  let directory=text(call "file-name-directory"[s(draft_path "placeholder")])in
  let files=values(call "directory-files"[s directory;sym "t";s "\\.md$"])in
  if List.is_empty files then user_error "No saved GitHub drafts";
  let file=text(call "completing-read"[s "Resume GitHub draft: ";Value.list files;nil;sym "t"])in
  open_draft(read_json(file^".json")));
 let current_review ()=let data=to_json(get "camulus-github--review-data")in
  let id=G.required "id"(G.field "pr"data)in data,id,review_state data in
 define0 ~interactive:true "camulus-github-review-comment"(fun()->
  let data,id,state=current_review()in
  if not(String.equal(G.str "commit" state)(G.str "headRefOid"(G.field "pr"data)))then user_error "Draft belongs to an older commit; remove it before commenting";
  let at point=call "get-text-property"[point;sym "camulus-github-location"]in
  let start,finish=if truth(call "use-region-p"[])then
   call "region-beginning"[],call "1-"[call "region-end"[]]else call "point"[],call "point"[]in
  let first=at start and last=at finish in
  if not(truth first && truth last)then user_error "Select a line or range inside one diff hunk";
  let a=location_of_json(to_json first)and z=location_of_json(to_json last)in
  if not(String.equal a.path z.path) || not(Poly.equal a.side z.side) || a.line>z.line then user_error "Select a range on one side of one file";
  let location={z with G.start_line=(if a.line=z.line then None else Some a.line);start_side=(if a.line=z.line then None else Some a.side)}in
  let body=text(call "read-string"[s "Review comment (saved locally until submit): "])in
  if String.is_empty(String.strip body)then user_error "Empty comment";
  let comments=G.array(G.field "comments"state)@[`Assoc["body",`String body;"location",location_json location]]in
  let state=match state with `Assoc xs->`Assoc(("comments",`List comments)::List.Assoc.remove xs "comments"~equal:String.equal)|_->assert false in
  write_json(review_path id)state;message "Saved review comment (%d total)"[i(List.length comments)]);
 define0 ~interactive:true "camulus-github-review-remove-comment"(fun()->
  let data,id,state=current_review()in let comments=G.array(G.field "comments"state)in
  let choices=List.mapi comments~f:(fun index c->Printf.sprintf "%d: %s:%s %s" index (G.str "path"(G.field "location"c))
    (Yojson.Safe.to_string(G.field "line"(G.field "location"c)))(G.str "body"c),index)in
  if List.is_empty choices then(
    write_json(review_path id)(`Assoc["id",`String id;"commit",G.field "headRefOid"(G.field "pr"data);"comments",`List[]]);
    message "Review draft now targets the displayed commit"[])
  else let chosen=text(call "completing-read"[s "Remove draft comment: ";Value.list(List.map choices~f:(fun(k,_)->s k));nil;sym "t"])in
   let index=List.Assoc.find_exn choices chosen ~equal:String.equal in
   let remaining=List.filteri comments~f:(fun i _->i<>index)in
   write_json(review_path id)(`Assoc["id",`String id;"commit",(if List.is_empty remaining then G.field "headRefOid"(G.field "pr"data)else G.field "commit"state);"comments",`List remaining]));
 define0 ~interactive:true "camulus-github-review-submit"(fun()->let _,id,_=current_review()in open_draft(`Assoc["kind",`String "review";"id",`String id]));
 let thread_at_point ()=let row=call "get-text-property"[call "point"[];sym "camulus-github-review-thread"]in
  if not(truth row)then user_error "Move to an existing review discussion";row in
 define0 ~interactive:true "camulus-github-review-reply"(fun()->let thread=thread_at_point()in
  open_draft(`Assoc["kind",`String "review-reply";"id",to_json(field "id"thread)]));
 define0 ~interactive:true "camulus-github-review-resolve"(fun()->let row=thread_at_point()in
  let resolved=not(truth(field "isResolved"row))in
  if not(truth(field(if resolved then "viewerCanResolve"else "viewerCanUnresolve")row))then user_error "GitHub does not allow this action";
  ignore(request(Resolve(text(field "id"row),resolved))(fun data->if String.equal(text(field "kind"data))"resolved"then
   message "GitHub discussion updated; reopen review to refresh"[]):int));
 let map=get "camulus-github-review-mode-map"in
 List.iter["c","camulus-github-review-comment";"x","camulus-github-review-remove-comment";"r","camulus-github-review-reply";
  "t","camulus-github-review-resolve";"o","camulus-github-review-find-file";"C-c C-c","camulus-github-review-submit"]~f:(fun(k,c)->bind map k c)
