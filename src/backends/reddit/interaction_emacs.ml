open! Core
open! Ecaml
open Reader_backend
let vote_label message=
 let state=call "camulus-reddit-vote-state"[field "id"message]in
 let direction=if truth state then integer(call "car"[state])else let vote=field "my_vote"message in if truth vote then integer vote else 0 in
 let mark=match direction with 1->"+"| -1->"-"|_->"·"in
 let status=call "nth"[i 1;state]in
 mark^(if Value.eq status(sym "pending")then " pending"else if Value.eq status(sym "rejected")then " failed"else if Value.eq status(sym "uncertain")then " unknown"else "")
let redraw _id=
 List.iter(Buffer.all_live())~f:(fun buffer->Current_buffer.set_temporarily Sync buffer ~f:(fun()->
  if String.is_prefix(text(get "camulus-reader--source"))~prefix:"reddit:"then
   if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"])then run "camulus-reader--render"[]
   else if truth(call "derived-mode-p"[sym "camulus-reader-feed-mode"]) && truth(call "hash-table-p"[get "camulus-reader--rows"])then
    run "camulus-reader--set-rows"[call "hash-table-values"[get "camulus-reader--rows"]]))
let vote direction=
 let source=get "camulus-reader--source"in
 if not(String.is_prefix(text source)~prefix:"reddit:") || truth(get "camulus-reader--archive")then user_error "Open live Reddit content";
 let row=call "camulus-reader--selected"[]in let id=field "id"row in
 let m=if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"])then call "gethash"[id;get "camulus-reader--messages"]else row in
 let state=call "camulus-reddit-vote-state"[id]in
 let previous=if truth state then call "car"[state]else let v=field "my_vote"m in if truth v then v else i 0 in
 ignore(call "camulus-reddit-vote"[id;previous;i direction]:Value.t);redraw id
let auth_requests : (int,Value.t->unit) Hashtbl.t=Hashtbl.create(module Int)
let auth_action data f=let id=integer(call "camulus-reddit-submit"[data])in Hashtbl.set auth_requests~key:id~data:f
let install ()=
 List.iter["camulus-reddit-upvote",1;"camulus-reddit-downvote",-1;"camulus-reddit-clear-vote",0]~f:(fun(name,n)->define0 ~interactive:true name(fun()->vote n));
 define1 "camulus-reddit--vote-event"(fun event->if Value.eq(call "car"[event])(sym "reddit-vote")then(
  redraw(call "nth"[i 2;event]);let reason=call "nth"[i 5;event]in if not(String.is_empty(text reason))then message "%s"[reason]);nil);
 hook "camulus-probe-event-hook""camulus-reddit--vote-event";
 define1 "camulus-reddit--auth-event"(fun event->let id=call "nth"[i 1;event]in let id=if truth(call "integerp"[id])then integer id else -1 in
  Option.iter(Hashtbl.find auth_requests id)~f:(fun f->let kind=call "car"[event]in
   if Value.eq kind(sym "reddit")then(let data=call "nth"[i 2;event]in
    if String.equal(text(field "kind"data))"error"then message "Reddit: %s"[field "message"data]else f data)
   else if Value.eq kind(sym "finished")then(Hashtbl.remove auth_requests id;
    if not(Value.eq(call "nth"[i 2;event])(sym "completed"))then message "Reddit authorization failed: %s"[call "nth"[i 3;event]]));nil);
 hook "camulus-probe-event-hook""camulus-reddit--auth-event";
 define0 ~interactive:true "camulus-reddit-authorize"(fun()->
  if not(truth(get "camulus-reddit-username"))then set "camulus-reddit-username"(call "read-string"[s "Reddit username: "]);
  run "camulus-reader-start"[];
  auth_action(alist["op",s "authorize"])(fun data->let url=field "url"data in
   if not(truth url)then user_error "No authorization URL returned";
   run "browse-url"[url];
   let callback=Current_buffer.set_value_temporarily Sync (Var.create(Symbol.intern "history-length")Value.Type.value)(i 0)
    ~f:(fun()->call "read-string"[s "Paste the complete Reddit redirect URL: "])in
   auth_action(alist["op",s "authorize-complete";"callback",callback])(fun _->message "Reddit authorization saved"[])))
let controls=[{key="+";hint="+: upvote";command="camulus-reddit-upvote"};{key="-";hint="-: downvote";command="camulus-reddit-downvote"};{key="0";hint="0: clear vote";command="camulus-reddit-clear-vote"}]
