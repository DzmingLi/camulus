open! Core
open! Ecaml
open Reader_backend
let actions : (int, (Value.t->unit) * (Value.t->Value.t->unit) option) Hashtbl.t=Hashtbl.create(module Int)
let action request receive finish=
 let id=integer(call "camulus-mail-submit"[request])in Hashtbl.set actions ~key:id ~data:(receive,finish)
let trash ~entry ~row ~refresh =
 let group=field "group"entry and id=field "id"row in
 if not(truth group && truth id)then user_error "Select mail from a mail group to move to Trash";
 action(alist["op",s "trash";"group",group;"id",id;"thread",sym "t"])
  (fun data->
   refresh();
   message "Moved %d messages to %s; %d failed"[call "length"[field "moved"data];field "destination"data;field "failed"data])None
let current ()=Buffer.to_value(Current_buffer.get())
let group ()=let v=get "camulus-mail--group"in if truth v then v else field "group"(get "camulus-reader--entry")
let selected ()=field "id"(call "camulus-reader--selected"[])
let summary_window ()=
 if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"])then
  Option.value(List.find(values(call "window-list"[]))~f:(fun w->
   Value.eq(call "buffer-local-value"[sym "major-mode";call "window-buffer"[w]])(sym "camulus-reader-feed-mode")))~default:(call "selected-window"[])
 else call "selected-window"[]
let draft_source ()=
 let source=text(call "buffer-substring-no-properties"[call "point-min"[];call "point-max"[]])in
 let separator="\n"^text(get "mail-header-separator")^"\n"in
 match String.substr_index source ~pattern:separator with None->s source|Some p->s(String.prefix source p^"\n\n"^String.drop_prefix source(p+String.length separator))
(* The shared writing session sees plain source snapshots and completion
   callbacks. Maildir keys/revisions and mail result events remain Mu-specific. *)
let writing_backend buffer = {
 Writing.snapshot=(fun () -> text(draft_source()));
 save=(fun ~source ~complete ->
  let received=ref false in
  action(alist["op",s "draft-save";"key",get "camulus-mail-draft-key";
    "revision",get "camulus-mail-draft-revision";"source",s source])
   (fun data->if live buffer then in_buffer buffer(fun()->
    let d=field "draft"data in
    set "camulus-mail-draft-revision"(field "revision"d);received:=true;
    if not(truth(field "indexed"d))then message "Draft saved; mu index update pending"[]))
   (Some(fun state reason->
    complete(if Value.eq state(sym "completed") && !received then Ok ()
     else Error(if Value.eq state(sym "completed")then "Mu returned no saved draft"else text reason)))));
 send=(fun ~complete ->
  let received=ref false in
  action(alist["op",s "draft-send";"key",get "camulus-mail-draft-key";
    "revision",get "camulus-mail-draft-revision"])
   (fun data->
    received:=true;
    if live buffer then in_buffer buffer(fun()->set "camulus-mail-draft-key"nil);
    let close=truth(field "cleaned"data)in
    complete(Ok {Writing.close;message=if close then "Mail sent; copy saved in Sent"
     else "Mail sent · local cleanup incomplete; do not resend"}))
   (Some(fun state reason->if not !received then
    complete(Error(if Value.eq state(sym "completed")then "Mu returned no send receipt"else text reason)))))
}
let open_composer data window=
 let key=field "key"data in
 let existing=List.find(Buffer.all_live())~f:(fun buffer->Current_buffer.set_temporarily Sync buffer ~f:(fun()->Value.equal(get "camulus-mail-draft-key")key))in
 let buffer=Option.value_or_thunk existing ~default:(fun()->Buffer.create ~name:"*Mail draft*")in
 if Option.is_none existing then Current_buffer.set_temporarily Sync buffer ~f:(fun()->
  run "message-mode"[];
  Point.insert(String.substr_replace_all(text(field "source"data))~pattern:"\r\n"~with_:"\n");
  run "goto-char"[call "point-min"[]];
  if truth(call "search-forward"[s "\n\n";nil;sym "t"])then run "replace-match"[s("\n"^text(get "mail-header-separator")^"\n");sym "t";sym "t"];
  List.iter["camulus-mail-draft-key",key;"camulus-mail-draft-revision",field "revision"data;"camulus-mail--group",field "group"data]~f:(fun(k,v)->local k v);
  local "message-send-mail-function"(sym "camulus-mail--no-direct-send");
  Writing.attach(writing_backend(Buffer.to_value buffer));
  run "set-buffer-modified-p"[nil];run "message-goto-body"[]);
 if truth(call "window-live-p"[window])then(run "set-window-buffer"[window;Buffer.to_value buffer];run "select-window"[window])else run "pop-to-buffer"[Buffer.to_value buffer]
let compose reply=
 let window=summary_window()and group=group()in
 let id=if reply then selected()else nil in
 if not(truth group)then user_error "Open a mail group first";
 if reply && not(truth id)then user_error "Select a message to reply to";
 action(alist["op",s "draft-new";"group",group;"reply_to",id])(fun data->open_composer(field "draft"data)window)None
let drafts_refresh ()=
 let buffer=current()in
 action(alist["op",s "draft-list";"group",get "camulus-mail--group"])(fun data->if live buffer then in_buffer buffer(fun()->
  let rows=List.map(values(field "drafts"data))~f:(fun d->Value.list[field "key"d;call "vector"[s(text(field "subject"d))]])in
  set "tabulated-list-entries"(Value.list rows);run "tabulated-list-print"[sym "t"]))None
let install ()=
 run "require"[sym "message"];
 Writing.install();
 List.iter["camulus-mail--group";"camulus-mail-draft-key";"camulus-mail-draft-revision"]~f:(fun name->variable name nil;run "make-variable-buffer-local"[sym name]);
 define1 "camulus-mail--action-event"(fun event->let id=call "nth"[i 1;event]in let id=if truth(call "integerp"[id])then integer id else -1 in
  Option.iter(Hashtbl.find actions id)~f:(fun(receive,finish)->let kind=call "car"[event]in
   if Value.eq kind(sym "mail")then receive(call "nth"[i 2;event])
   else if Value.eq kind(sym "finished")then(
    Hashtbl.remove actions id;let state=call "nth"[i 2;event]and reason=call "nth"[i 3;event]in
    match finish with Some f->f state reason|None->if not(Value.eq state(sym "completed"))then message "Mail: %s"[reason]));nil);
 hook "camulus-probe-event-hook""camulus-mail--action-event";
 define0 "camulus-mail--no-direct-send"(fun()->user_error "Use C-c C-c to send through the OCaml mail backend");
 optional ~interactive:true "camulus-mail-compose"(fun reply->compose(truth reply);nil);
 define0 ~interactive:true "camulus-mail-reply"(fun()->compose true);
 Defun.defun(Symbol.intern "camulus-mail--open-composer")[%here]~docstring:"Open a native mail draft."(Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open data=required "data"value and window=required "window"value in open_composer data window);
 mode "camulus-mail-drafts-mode""tabulated-list-mode""Mail drafts""RET: resume  g: refresh  c: compose";
 define0 ~interactive:true "camulus-mail-drafts"(fun()->let group=group()in
  if not(truth group)then user_error "Open a mail group first";
  run "switch-to-buffer"[call "get-buffer-create"[s("*Mail drafts "^text group^"*")]];run "camulus-mail-drafts-mode"[];
  set "tabulated-list-format"(call "vector"[Value.list[s "Subject";i 60;sym "t"]]);run "tabulated-list-init-header"[];
  local "camulus-mail--group"group;drafts_refresh());
 define0 ~interactive:true "camulus-mail-drafts-refresh"drafts_refresh;
 define0 ~interactive:true "camulus-mail-resume-draft"(fun()->let key=call "tabulated-list-get-id"[]and window=call "selected-window"[]in
  if not(truth key)then user_error "Select a draft";
  action(alist["op",s "draft-open";"key",key])(fun data->open_composer(field "draft"data)window)None);
 define0 ~interactive:true "camulus-mail-archive"(fun()->let id=selected()and group=group()and buffer=current()in
  if not(truth id && truth group)then user_error "Select mail to archive";
  let whole=truth(call "derived-mode-p"[sym "camulus-reader-feed-mode"])in
  action(alist["op",s "archive";"group",group;"id",id;"thread",if whole then sym "t"else sym ":json-false"])(fun data->
   message "Archived %d messages to %s; %d failed"[call "length"[field "moved"data];field "destination"data;field "failed"data];
   if live buffer then in_buffer buffer(fun()->if whole then run "camulus-reader-refresh"[]))None);
 let map=get "camulus-mail-drafts-mode-map"in
 List.iter["RET","camulus-mail-resume-draft";"g","camulus-mail-drafts-refresh";"c","camulus-mail-compose"]~f:(fun(k,c)->bind map k c)
