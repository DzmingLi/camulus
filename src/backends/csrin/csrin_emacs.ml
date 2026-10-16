open! Core
open! Ecaml
open Reader_backend
let truth v=not(Value.eq v Value.nil)
let get name=Current_buffer.symbol_value(Symbol.intern name)
let field name value=call "alist-get"[Value.intern name;value]
let text value=if truth value then Value.to_utf8_bytes_exn value else ""
let local name value=run "make-local-variable"[Value.intern name];run "set"[Value.intern name;value]
let callbacks : (int * (Value.t -> string option -> unit)) list ref=ref []
let data : (int * Value.t) list ref=ref []
let action submit operation callback=let id=submit operation in callbacks:=(id,callback)::!callbacks;id
let error reason=run "user-error"[s "%s";s reason]
let state_directory ()=
 let base=let value=call "getenv"[s "XDG_STATE_HOME"]in if truth value then text value else text(call "expand-file-name"[s "~/.local/state"])in
 Stdlib.Filename.concat base "camulus/csrin-drafts"
let bind key command=run "local-set-key"[call "kbd"[s key];Value.intern command]
let header name=call "message-fetch-field"[s name] |> text
let wire_draft ()=
 let forum=header "Forum"and topic=header "Topic"and subject=header "Subject"in
 let point=call "point"[] in
 run "message-goto-body"[];
 let body=text(call "buffer-substring-no-properties"[call "point"[];call "point-max"[]])in
 run "goto-char"[point];
 {Csrin_backend.Live.forum;topic=(if String.is_empty topic then None else Some topic);subject;body}
let draft_setup ()=
 run "require"[Value.intern "message"];
 run "message-mode"[];
 local "make-backup-files"Value.nil;
 local "header-line-format"(s (if String.is_empty(header "X-Camulus-Delivery")then
   "C-c C-c: submit  C-x C-s: save draft  C-c C-k: save and close"
   else "Previous send may have reached the server: check the website before resending"));
 local "camulus-csrin--save-timer"Value.nil;
 run "add-hook"[Value.intern "after-change-functions";Value.intern "camulus-csrin--changed";Value.nil;Value.intern "t"];
 run "add-hook"[Value.intern "kill-buffer-hook";Value.intern "camulus-csrin--cancel-save";Value.nil;Value.intern "t"];
 bind "C-c C-c" "camulus-csrin-send";bind "C-c C-k" "camulus-csrin-close-draft"
let editing_window ()=
 if truth(call "derived-mode-p"[Value.intern "ef-reader-article-mode"])then
  match List.find(Value.to_list_exn(call "window-list"[])~f:Fn.id)~f:(fun w->
   let b=call "window-buffer"[w] in
   let mode=call "buffer-local-value"[Value.intern "major-mode";b]in Value.eq mode(Value.intern "ef-reader-feed-mode"))with
   |Some w->w|None->call "selected-window"[]
 else call "selected-window"[]
let open_draft path window=
 let buffer=call "find-file-noselect"[s path]in
 run "set-window-buffer"[window;buffer];run "select-window"[window];run "set-buffer"[buffer];draft_setup ()
let install_reader ~read ~attachment ()=
 register_local_groups{(base ~name:"CS.RIN.RU" ~matches:(String.equal "csrin")~threaded:true
   ~read:(fun ~source:_ ~id ~thread_id:_ ~group->Value.of_int_exn(read id group)))with
   start=(fun()->run "camulus-csrin-configure"[get "camulus-csrin-site"]);
   thread_contains_bodies=true;
   attachment=Some(fun ~source:_ ~id ~group ~part ~destination->Value.of_int_exn(attachment id group part destination));
   events=["rss"];browser=Some("CS.RIN.RU","camulus-csrin");
   controls=[{key="r";hint="r: reply";command="camulus-csrin-reply"};{key="c";hint="c: new topic";command="camulus-csrin-compose"}];
   list_controls=[{key="r";hint="r: reply";command="camulus-csrin-reply"};{key="c";hint="c: new topic";command="camulus-csrin-compose"};{key="D";hint="D: drafts";command="camulus-csrin-drafts"}];
   entrances=(fun()->[{label="CS.RIN.RU forum/topic";open_=(fun()->run "call-interactively"[Value.intern "camulus-csrin"])}])}
let install ~submit ~configure ()=
 variable "camulus-csrin-site"(s "https://cs.rin.ru/forum/");
 register_settings "csrin"(string_settings["site","camulus-csrin-site"]);
 variable "camulus-csrin--sending"Value.nil;
 variable "camulus-csrin--save-timer"Value.nil;
 let define name doc f=Defun.defun_nullary_nil(Symbol.intern name)[%here]~docstring:doc ~interactive:No_arg f in
 Defun.defun(Symbol.intern "camulus-csrin-configure")[%here]~docstring:"Configure the forum origin before starting the runtime."
  (Returns Value.Type.unit)(let open Defun.Let_syntax in let%map_open site=required "site"string in configure site);
 Defun.defun(Symbol.intern "camulus-csrin")[%here]~docstring:"Browse forum.ID or topic.ID without subscribing."
  ~interactive:(Prompt "CS.RIN.RU group (forum.ID / topic.ID; empty lists forums): ")(Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open target=required "group"string in
   if String.is_empty(String.strip target)then run "camulus-csrin-forums"[]else(
    ignore(Csrin_backend.group target : string*string);
    run "ef-reader-browse-entry"[alist["name",s("CS.RIN.RU: "^target);"source",s "csrin";"group",s target]]));
 define "camulus-csrin-forums" "Discover forums visible to the current session."(fun()->
  run "ef-reader-start"[];
  ignore(action submit Csrin_backend.Forums(fun d reason->match reason with Some r->run "message"[s "%s";s r]|None->
   let choices=Value.to_list_exn(field "rows"d)~f:(fun row->text(field "name"row)^" ["^text(field "group"row)^"]",text(field "group"row))in
   if List.is_empty choices then run "message"[s "No visible forums; try logging in"]else
   let choice=text(call "completing-read"[s "Forum: ";Value.list(List.map choices ~f:(fun(k,_)->s k));Value.nil;Value.intern "t"])in
   Option.iter(List.Assoc.find choices choice ~equal:String.equal)~f:(fun group->run "camulus-csrin"[s group])) : int));
 Defun.defun(Symbol.intern "camulus-csrin--event")[%here]~docstring:"Handle this backend's pending UI operations."
  (Returns Value.Type.unit)(let open Defun.Let_syntax in let%map_open event=required "event"value in
   let kind=text(call "symbol-name"[call "car"[event]])and idv=call "nth"[Value.of_int_exn 1;event]in
   if truth idv then let id=Value.to_int_exn idv in
    match List.Assoc.find !callbacks id ~equal:Int.equal with
    |None->()
    |Some callback->
      if String.equal kind "content-state"then data:=(id,call "nth"[Value.of_int_exn 3;event])::List.Assoc.remove !data id ~equal:Int.equal
      else if String.equal kind "finished"then(
       let result=Option.value(List.Assoc.find !data id ~equal:Int.equal)~default:Value.nil in
       data:=List.Assoc.remove !data id ~equal:Int.equal;callbacks:=List.Assoc.remove !callbacks id ~equal:Int.equal;
       let status=call "nth"[Value.of_int_exn 3;event]in
       callback result(if Value.eq status(Value.intern "completed")then None else Some(text(call "nth"[Value.of_int_exn 4;event])))));
 run "add-hook"[Value.intern "ef-probe-event-hook";Value.intern "camulus-csrin--event"];
 define "camulus-csrin-login" "Log in using auth-source or a private password prompt."(fun()->
  run "ef-reader-start"[];run "require"[Value.intern "auth-source"];
  let host=Uri.host(Uri.of_string(text(get "camulus-csrin-site")))|>Option.value ~default:"cs.rin.ru"in
  let auth=call "car"[call "auth-source-search"[Value.intern ":host";s host;Value.intern ":port";s "https";Value.intern ":require";Value.list[Value.intern ":user";Value.intern ":secret"];Value.intern ":max";Value.of_int_exn 1]]in
  let user=call "plist-get"[auth;Value.intern ":user"]and secret=call "plist-get"[auth;Value.intern ":secret"]in
  let user=if truth user then text user else text(call "read-string"[s "CS.RIN.RU username: "])in
  let password=if truth secret then text(if truth(call "functionp"[secret])then call "funcall"[secret]else secret)else text(call "read-passwd"[s "CS.RIN.RU password: "])in
  ignore(action submit(Csrin_backend.Login(user,password))(fun _ reason->match reason with Some r->run "message"[s "CS.RIN.RU login failed: %s";s r]|None->run "message"[s "CS.RIN.RU logged in"]) : int));
 let cancel_save ()=let timer=get "camulus-csrin--save-timer"in
  if truth(call "timerp"[timer])then run "cancel-timer"[timer];local "camulus-csrin--save-timer"Value.nil in
 define "camulus-csrin--cancel-save" "Cancel this draft's save timer." cancel_save;
 Defun.defun(Symbol.intern "camulus-csrin--save-buffer")[%here]~docstring:"Save the given draft after editing pauses."
  (Returns Value.Type.unit)(let open Defun.Let_syntax in let%map_open buffer=required "buffer"value in
   let buffer=Buffer.of_value_exn buffer in
   if Buffer.is_live buffer then Current_buffer.set_temporarily Sync buffer ~f:(fun()->
    local "camulus-csrin--save-timer"Value.nil;
    if truth(call "buffer-modified-p"[]) && not(truth(get "camulus-csrin--sending"))then run "save-buffer"[]));
 Defun.defun(Symbol.intern "camulus-csrin--changed")[%here]~docstring:"Schedule a save for this draft only."
  (Returns Value.Type.unit)(let open Defun.Let_syntax in let%map_open _start=required "start"int and _end=required "end"int and _old=required "old"int in
   cancel_save();
   local "camulus-csrin--save-timer"(call "run-at-time"[Value.of_float 0.75;Value.nil;Value.intern "camulus-csrin--save-buffer";Buffer.to_value(Current_buffer.get())]));
 let compose reply=
  if not(String.equal(text(get "ef-reader--source"))"csrin")then error "Open a CS.RIN.RU group or post first"else(
   let window=editing_window ()in
   let entry=get "ef-reader--entry"in let group=field "group"entry |> text in
   let row=call "ef-reader--selected"[]in
   let id=if reply then Some(text(field "id"row))else None in
   if reply && String.is_empty(Option.value id ~default:"")then error "Select a forum post"else
   ignore(action submit(Csrin_backend.Prepare(group,id))(fun d reason->match reason with Some r->run "message"[s "%s";s r]|None->
    let dir=state_directory()in run "make-directory"[s dir;Value.intern "t"];run "set-file-modes"[s dir;Value.of_int_exn 0o700];
    let file=text(call "make-temp-file"[s(Stdlib.Filename.concat dir "draft-")])in
    open_draft file window;
    run "erase-buffer"[];
    Point.insert("Forum: "^text(field "forum"d)^"\nTopic: "^text(field "topic"d)^"\nSubject: "^text(field "subject"d)^"\n--text follows this line--\n");
    run "save-buffer"[];run "message-goto-body"[]) : int))in
 define "camulus-csrin-compose" "Compose a new topic in the current forum."(fun()->compose false);
 define "camulus-csrin-reply" "Reply to the selected discussion using BBCode or plain text."(fun()->compose true);
 define "camulus-csrin-close-draft" "Save and close this draft."(fun()->
  if truth(get "camulus-csrin--sending")then error "Wait for the send result"else(run "save-buffer"[];run "kill-buffer"[]));
 define "camulus-csrin-send" "Submit this draft once; uncertain delivery leaves it open."(fun()->
  if truth(get "camulus-csrin--sending")then error "Sending is already in progress"else(
   let draft=wire_draft()in
   run "message-replace-header"[s "X-Camulus-Delivery";s "submitting"];
   run "save-buffer"[];run "ef-reader-start"[];
   let buffer=Current_buffer.get()and path=text(get "buffer-file-name")in
   local "camulus-csrin--sending"(Value.intern "t");local "buffer-read-only"(Value.intern "t");
   try ignore(action submit(Csrin_backend.Send draft)(fun d reason->
    if Buffer.is_live buffer then Current_buffer.set_temporarily Sync buffer ~f:(fun()->
     local "camulus-csrin--sending"Value.nil;local "buffer-read-only"Value.nil;
     match reason with
     |Some r->local "header-line-format"(s "Send unconfirmed: check the website before resending; draft kept");run "message"[s "Draft kept: %s";s r]
     |None->run "delete-file"[s path];run "set-buffer-modified-p"[Value.nil];run "kill-buffer"[Buffer.to_value buffer];run "message"[s "Posted: %s";field "url"d])) : int)
   with ex->local "camulus-csrin--sending"Value.nil;local "buffer-read-only"Value.nil;raise ex));
 define "camulus-csrin-drafts" "Choose and resume a saved CS.RIN.RU draft."(fun()->
  let dir=state_directory()in
  run "make-directory"[s dir;Value.intern "t"];run "set-file-modes"[s dir;Value.of_int_exn 0o700];
  let files=call "directory-files"[s dir;Value.intern "t";s "^draft-"]in
  if Value.eq files Value.nil then run "message"[s "No CS.RIN.RU drafts"]else
  let file=text(call "completing-read"[s "CS.RIN.RU draft: ";files;Value.nil;Value.intern "t"])in
  open_draft file(call "selected-window"[]))
