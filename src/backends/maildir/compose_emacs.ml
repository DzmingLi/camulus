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
let group ()=let v=get "ef-mail--group"in if truth v then v else field "group"(get "ef-reader--entry")
let selected ()=field "id"(call "ef-reader--selected"[])
let summary_window ()=
 if truth(call "derived-mode-p"[sym "ef-reader-article-mode"])then
  Option.value(List.find(values(call "window-list"[]))~f:(fun w->
   Value.eq(call "buffer-local-value"[sym "major-mode";call "window-buffer"[w]])(sym "ef-reader-feed-mode")))~default:(call "selected-window"[])
 else call "selected-window"[]
let draft_source ()=
 let source=text(call "buffer-substring-no-properties"[call "point-min"[];call "point-max"[]])in
 let separator="\n"^text(get "mail-header-separator")^"\n"in
 match String.substr_index source ~pattern:separator with None->s source|Some p->s(String.prefix source p^"\n\n"^String.drop_prefix source(p+String.length separator))
let cancel_timer ()=let timer=get "ef-mail-draft-timer"in
 if truth(call "timerp"[timer])then run "cancel-timer"[timer];set "ef-mail-draft-timer"nil
let open_composer data window=
 let key=field "key"data in
 let existing=List.find(Buffer.all_live())~f:(fun buffer->Current_buffer.set_temporarily Sync buffer ~f:(fun()->Value.equal(get "ef-mail-draft-key")key))in
 let buffer=Option.value_or_thunk existing ~default:(fun()->Buffer.create ~name:"*Mail draft*")in
 if Option.is_none existing then Current_buffer.set_temporarily Sync buffer ~f:(fun()->
  run "message-mode"[];
  Point.insert(String.substr_replace_all(text(field "source"data))~pattern:"\r\n"~with_:"\n");
  run "goto-char"[call "point-min"[]];
  if truth(call "search-forward"[s "\n\n";nil;sym "t"])then run "replace-match"[s("\n"^text(get "mail-header-separator")^"\n");sym "t";sym "t"];
  List.iter["ef-mail-draft-key",key;"ef-mail-draft-revision",field "revision"data;"ef-mail--group",field "group"data;
   "header-line-format",s "Draft · autosave  C-c C-s: save  C-c C-c: send  C-c C-k: save and close"]~f:(fun(k,v)->local k v);
  let map=call "copy-keymap"[get "message-mode-map"]in run "use-local-map"[map];
  List.iter["C-x C-s","ef-mail-save-draft";"C-c C-s","ef-mail-save-draft";"C-c C-c","ef-mail-send-draft";"C-c C-k","ef-mail-close-draft"]~f:(fun(k,c)->bind map k c);
  local "message-send-mail-function"(sym "ef-mail--no-direct-send");
  hook ~local:true "after-change-functions" "ef-mail--draft-changed";
  hook ~local:true "kill-buffer-query-functions" "ef-mail--draft-before-kill";
  hook ~local:true "kill-buffer-hook" "ef-mail--draft-cancel-timer";
  run "set-buffer-modified-p"[nil];run "message-goto-body"[]);
 if truth(call "window-live-p"[window])then(run "set-window-buffer"[window;Buffer.to_value buffer];run "select-window"[window])else run "pop-to-buffer"[Buffer.to_value buffer]
let save_draft ()=
 cancel_timer();
 if truth(get "ef-mail-draft-key") && truth(call "buffer-modified-p"[]) && not(truth(get "ef-mail-draft-saving")) && not(truth(get "ef-mail-draft-sending"))then(
  let buffer=current()and tick=call "buffer-chars-modified-tick"[]in set "ef-mail-draft-saving"(sym "t");
  try action(alist["op",s "draft-save";"key",get "ef-mail-draft-key";"revision",get "ef-mail-draft-revision";"source",draft_source()])
   (fun data->if live buffer then in_buffer buffer(fun()->
    let d=field "draft"data in set "ef-mail-draft-revision"(field "revision"d);set "ef-mail-draft-error"nil;
    if Value.equal tick(call "buffer-chars-modified-tick"[])then run "set-buffer-modified-p"[nil];
    if not(truth(field "indexed"d))then message "Draft saved; mu index update pending"[]))
   (Some(fun state reason->if live buffer then in_buffer buffer(fun()->
    set "ef-mail-draft-saving"nil;
    if Value.eq state(sym "completed")then(if truth(call "buffer-modified-p"[])then run "ef-mail--draft-changed"[i 0;i 0;i 0])
    else(set "ef-mail-draft-error"reason;message "Draft not saved: %s"[reason]))))
  with ex->set "ef-mail-draft-saving"nil;set "ef-mail-draft-error"(s(Exn.to_string ex));message "Draft not saved: %s"[get "ef-mail-draft-error"])
let flush ()=
 if truth(get "ef-mail-draft-error")then user_error(text(get "ef-mail-draft-error"));
 save_draft();
 run "ef-probe-await"[lambda0(fun()->b(truth(get "ef-mail-draft-error") || (not(truth(get "ef-mail-draft-saving")) && not(truth(call "buffer-modified-p"[])))));i 15];
 if truth(get "ef-mail-draft-error")then user_error(text(get "ef-mail-draft-error"))
let before_kill ()=
 try if truth(get "ef-mail-draft-sending")then user_error "Wait for the send result before closing";flush();true
 with ex->message "Keeping unsaved draft open: %s"[s(Exn.to_string ex)];false
let compose reply=
 let window=summary_window()and group=group()in
 let id=if reply then selected()else nil in
 if not(truth group)then user_error "Open a mail group first";
 if reply && not(truth id)then user_error "Select a message to reply to";
 action(alist["op",s "draft-new";"group",group;"reply_to",id])(fun data->open_composer(field "draft"data)window)None
let send ()=
 if truth(get "ef-mail-draft-sending")then user_error "Sending is already in progress";
 flush();let buffer=current()in
 set "ef-mail-draft-sending"(sym "t");set "buffer-read-only"(sym "t");
 try action(alist["op",s "draft-send";"key",get "ef-mail-draft-key";"revision",get "ef-mail-draft-revision"])
  (fun data->if live buffer then in_buffer buffer(fun()->
   set "ef-mail-draft-sending"nil;set "ef-mail-draft-key"nil;run "set-buffer-modified-p"[nil];
   if truth(field "cleaned"data)then(message "Mail sent; copy saved in Sent"[];run "kill-buffer"[buffer])
   else(set "header-line-format"(s "Mail sent · local cleanup incomplete; do not resend");message "Mail sent; local cleanup incomplete. Do not resend."[])))
  (Some(fun state reason->if live buffer && not(Value.eq state(sym "completed"))then in_buffer buffer(fun()->
   set "ef-mail-draft-sending"nil;set "buffer-read-only"nil;message "Mail send: %s"[reason])))
 with ex->set "ef-mail-draft-sending"nil;set "buffer-read-only"nil;raise ex
let drafts_refresh ()=
 let buffer=current()in
 action(alist["op",s "draft-list";"group",get "ef-mail--group"])(fun data->if live buffer then in_buffer buffer(fun()->
  let rows=List.map(values(field "drafts"data))~f:(fun d->Value.list[field "key"d;call "vector"[s(text(field "subject"d))]])in
  set "tabulated-list-entries"(Value.list rows);run "tabulated-list-print"[sym "t"]))None
let install ()=
 run "require"[sym "message"];
 List.iter["ef-mail--group";"ef-mail-draft-key";"ef-mail-draft-revision";"ef-mail-draft-timer";"ef-mail-draft-saving";"ef-mail-draft-error";"ef-mail-draft-sending"]~f:(fun name->variable name nil;run "make-variable-buffer-local"[sym name]);
 variable "ef-mail-autosave-delay"(Value.of_float 0.75);
 define1 "ef-mail--action-event"(fun event->let id=call "nth"[i 1;event]in let id=if truth(call "integerp"[id])then integer id else -1 in
  Option.iter(Hashtbl.find actions id)~f:(fun(receive,finish)->let kind=call "car"[event]in
   if Value.eq kind(sym "mail")then receive(call "nth"[i 3;event])
   else if Value.eq kind(sym "finished")then(
    Hashtbl.remove actions id;let state=call "nth"[i 3;event]and reason=call "nth"[i 4;event]in
    match finish with Some f->f state reason|None->if not(Value.eq state(sym "completed"))then message "Mail: %s"[reason]));nil);
 hook "ef-probe-event-hook""ef-mail--action-event";
 define0 "ef-mail--no-direct-send"(fun()->user_error "Use C-c C-c to send through the OCaml mail backend");
 optional ~interactive:true "ef-mail-compose"(fun reply->compose(truth reply);nil);
 define0 ~interactive:true "ef-mail-reply"(fun()->compose true);
 Defun.defun(Symbol.intern "ef-mail--open-composer")[%here]~docstring:"Open a native mail draft."(Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open data=required "data"value and window=required "window"value in open_composer data window);
 define0 "ef-mail--draft-cancel-timer"cancel_timer;
 Defun.defun(Symbol.intern "ef-mail--draft-changed")[%here]~docstring:"Schedule native draft persistence."(Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open _a=required "start"value and _b=required "end"value and _c=required "old-length"value in
   cancel_timer();let buffer=current()in
   set "ef-mail-draft-timer"(call "run-at-time"[get "ef-mail-autosave-delay";nil;lambda0(fun()->if live buffer then in_buffer buffer(fun()->
    if not(truth(get "ef-mail-draft-error")) && not(truth(get "ef-mail-draft-sending"))then save_draft());nil)]));
 define0 ~interactive:true "ef-mail-save-draft"(fun()->set "ef-mail-draft-error"nil;save_draft());
 define0 "ef-mail--draft-flush"flush;
 Defun.defun_nullary(Symbol.intern "ef-mail--draft-before-kill")[%here]~docstring:"Persist edits before closing."(Returns Value.Type.bool)before_kill;
 define0 ~interactive:true "ef-mail-close-draft"(fun()->if before_kill()then run "kill-buffer"[]);
 define0 ~interactive:true "ef-mail-send-draft"send;
 define0 "ef-mail--flush-all-drafts"(fun()->List.iter(Buffer.all_live())~f:(fun buffer->Current_buffer.set_temporarily Sync buffer~f:(fun()->if truth(get "ef-mail-draft-key")then flush())));
 hook "kill-emacs-hook""ef-mail--flush-all-drafts";
 mode "ef-mail-drafts-mode""tabulated-list-mode""Mail drafts""RET: resume  g: refresh  c: compose";
 define0 ~interactive:true "ef-mail-drafts"(fun()->let group=group()in
  if not(truth group)then user_error "Open a mail group first";
  run "switch-to-buffer"[call "get-buffer-create"[s("*Mail drafts "^text group^"*")]];run "ef-mail-drafts-mode"[];
  set "tabulated-list-format"(call "vector"[Value.list[s "Subject";i 60;sym "t"]]);run "tabulated-list-init-header"[];
  local "ef-mail--group"group;drafts_refresh());
 define0 ~interactive:true "ef-mail-drafts-refresh"drafts_refresh;
 define0 ~interactive:true "ef-mail-resume-draft"(fun()->let key=call "tabulated-list-get-id"[]and window=call "selected-window"[]in
  if not(truth key)then user_error "Select a draft";
  action(alist["op",s "draft-open";"key",key])(fun data->open_composer(field "draft"data)window)None);
 define0 ~interactive:true "ef-mail-archive"(fun()->let id=selected()and group=group()and buffer=current()in
  if not(truth id && truth group)then user_error "Select mail to archive";
  let whole=truth(call "derived-mode-p"[sym "ef-reader-feed-mode"])in
  action(alist["op",s "archive";"group",group;"id",id;"thread",if whole then sym "t"else sym ":json-false"])(fun data->
   message "Archived %d messages to %s; %d failed"[call "length"[field "moved"data];field "destination"data;field "failed"data];
   if live buffer then in_buffer buffer(fun()->if whole then run "ef-reader-refresh"[]))None);
 let map=get "ef-mail-drafts-mode-map"in
 List.iter["RET","ef-mail-resume-draft";"g","ef-mail-drafts-refresh";"c","ef-mail-compose"]~f:(fun(k,c)->bind map k c)
