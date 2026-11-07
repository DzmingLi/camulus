open! Core
open! Ecaml
open Reader_backend
let hash name=get name
let hget name key=call "gethash"[key;hash name]
let hput name key value=run "puthash"[key;value;hash name]
let remove name key=run "remhash"[key;hash name]
let member name value=String.equal(text(field "kind"value))name
let assign key value message=
 (* setf on alist-get can prepend a missing field; keep that behavior explicitly. *)
 let cell=call "assq"[sym key;message]in
 if truth cell then(run "setcdr"[cell;value];message)else call "cons"[call "cons"[sym key;value];message]
let apply_like message source request=
 let key=Value.list[source;field "id"message]in
 let pending=hget "camulus-discourse--likes"key and overlay=hget "camulus-discourse--like-values"key in
 if truth pending then assign "liked"(call "cdr"[pending])message
 else if truth overlay && (if truth request then integer request else 0)<=integer(call "car"[overlay])then
  assign "liked"(call "nth"[i 1;overlay])message |> assign "like-uncertain"(call "nth"[i 2;overlay])
 else(if truth overlay then remove "camulus-discourse--like-values"key;message)
let request data callback=
 let id=call "camulus-discourse-submit"[data]in hput "camulus-discourse--callbacks"id callback;id
let callback f=lambda2(fun data reason->f data(if truth reason then Some(text reason)else None);nil)
let for_readers source f=
 List.iter(Buffer.all_live())~f:(fun buffer->Current_buffer.set_temporarily Sync buffer~f:(fun()->
  if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"]) && Value.equal(get "camulus-reader--source")source then f()))
let setup ()=
 set "camulus-discourse--authenticated-sites" nil;
 List.iter["camulus-discourse--callbacks";"camulus-discourse--likes";"camulus-discourse--like-values"]~f:(fun name->run "clrhash"[get name]);
 run "camulus-discourse-clear-config"[];
 Credentials_emacs.with_sources (List.map(values(get "camulus-discourse-auth-sources"))~f:text)(fun()->
  List.iter(values(get "camulus-discourse-sites"))~f:(fun value->
   let site=call "camulus-discourse-site"[value]in
   let headers=if truth(call "fboundp"[sym "discourse-auth-headers"])then call "discourse-auth-headers"[site]else(
    match Credentials_emacs.auth_source ~port:"discourse-user-api" ~require_user:true (text site) with
    |None->nil
    |Some credential->Value.list[call "cons"[s "User-Api-Key";s credential.secret];call "cons"[s "User-Api-Client-Id";s credential.user]])in
   let header name=text(call "cdr"[call "assoc"[s name;headers]])in
   run "camulus-discourse-configure"[site;s(header "User-Api-Key");s(header "User-Api-Client-Id")];
   if not(String.is_empty(header "User-Api-Key"))then
     set "camulus-discourse--authenticated-sites"(call "cons"[site;get "camulus-discourse--authenticated-sites"])))
let browse site group=
 let site=Discourse_backend.Decode.site site in
 if not(List.exists(values(get "camulus-discourse-sites"))~f:(Value.equal(s site)))then set "camulus-discourse-sites"(call "cons"[s site;get "camulus-discourse-sites"]);
 run "camulus-reader-browse-entry"[alist["name",s("Discourse: "^site);"source",s("discourse:"^site);"group",s group]]
let like unlike=
 let source=get "camulus-reader--source"in
 if not(truth(call "derived-mode-p"[sym "camulus-reader-article-mode"])) || not(String.is_prefix(text source)~prefix:"discourse:") || truth(get "camulus-reader--archive")then user_error "Open a live Discourse article";
 let id=field "id"(call "camulus-reader--selected"[])in let key=Value.list[source;id]in
 let item=call "gethash"[id;get "camulus-reader--messages"]in let previous=field "liked"item and liked=not unlike in
 if truth(hget "camulus-discourse--likes"key)then user_error "A like request for this article is pending";
 if not(truth(call "gethash"[id;get "camulus-reader--loaded"]))then user_error "Wait for the article to load";
 let req=request(alist["op",s "like";"site",s(String.drop_prefix(text source)10);"id",id;"liked",if liked then sym "t"else sym ":json-false"])
  (callback(fun data reason->if Option.is_some reason || member "liked"data then(
   let pending=hget "camulus-discourse--likes"key in
   let barrier=ref(if truth pending then integer(call "car"[pending])else 0)in
   let uncertain=Option.exists reason~f:(fun r->String.is_empty r || String.is_substring r ~substring:"uncertain")in
   let confirmed=if Option.is_some reason && not uncertain then previous else b liked in
   for_readers source(fun()->let active=get "camulus-reader--active"in if truth active then barrier:=Int.max !barrier(integer active));
   hput "camulus-discourse--like-values"key(Value.list[i !barrier;confirmed;b uncertain]);
   remove "camulus-discourse--likes"key;
   let article=field "article"data in
   for_readers source(fun()->if not(truth(get "camulus-reader--archive")) && truth(call "gethash"[id;get "camulus-reader--messages"])then
    if truth article then run "camulus-reader--receive"[article]else(
     let message=apply_like(call "gethash"[id;get "camulus-reader--messages"])source(get "camulus-reader--active")in
     run "puthash"[id;message;get "camulus-reader--messages"];run "camulus-reader--render"[])))))in
 hput "camulus-discourse--likes"key(call "cons"[req;b liked]);
 for_readers source(fun()->let table=get "camulus-reader--messages"in let item=call "gethash"[id;table]in
  if not(truth(get "camulus-reader--archive")) && truth item then(run "puthash"[id;assign "liked"(b liked)item;table];run "camulus-reader--render"[]))
let install ()=
 run "require"[sym "auth-source"];
 custom ~kind:(Customization.Type.Repeat String) "camulus-discourse-auth-sources" nil "Auth-source files used for Discourse authentication.";
 variable "camulus-discourse-sites" nil;
 variable "camulus-discourse--authenticated-sites" nil;
 List.iter["camulus-discourse--callbacks","eql";"camulus-discourse--likes","equal";"camulus-discourse--like-values","equal"]~f:(fun(n,test)->variable n(call "make-hash-table"[sym ":test";sym test]));
 define0 "camulus-discourse-setup"setup;
 define1 "camulus-discourse--event"(fun event->let id=call "nth"[i 1;event]in
  let f=hget "camulus-discourse--callbacks"id in if truth f then(
   let kind=call "car"[event]in
   if Value.eq kind(sym "discourse")then ignore(Value.funcallN f[call "nth"[i 3;event];nil]:Value.t)
   else if Value.eq kind(sym "finished")then(remove "camulus-discourse--callbacks"id;
    if not(Value.eq(call "nth"[i 3;event])(sym "completed"))then(
     let reason=call "nth"[i 4;event]in ignore(Value.funcallN f[nil;reason]:Value.t);message "Discourse: %s"[reason])));nil);
 hook "camulus-probe-event-hook""camulus-discourse--event";
 Defun.defun(Symbol.intern "camulus-discourse--apply-like")[%here]~docstring:"Reconcile like responses with pending edits."(Returns Value.Type.value)
  (let open Defun.Let_syntax in let%map_open item=required "message"value and source=required "source"value and request=required "request"value in apply_like item source request);
 Defun.defun(Symbol.intern "camulus-reader-discourse")[%here]~docstring:"Browse a Discourse group."
  ~interactive:(Prompt "Discourse forum URL: ")(Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open site=required "site"string and group=optional_with_nil "group"value in browse site(if truth group then text group else "latest"));
 optional ~interactive:true "camulus-reader-discourse-refresh"(fun more->if truth more then run "camulus-reader-more"[]else run "camulus-reader-refresh"[];nil);
 define0 ~interactive:true "camulus-reader-discourse-more"(fun()->run "camulus-reader-more"[]);
 Defun.defun(Symbol.intern "camulus-reader-discourse-search")[%here]~docstring:"Search the current forum group."
  ~interactive:(Prompt "Search this Discourse group: ")(Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open query=required "query"string in run "camulus-reader-search"[s query]);
 define0 ~interactive:true "camulus-reader-discourse-categories"(fun()->
  let buffer=Buffer.to_value(Current_buffer.get())and source=get "camulus-reader--source"in
  ignore(request(alist["op",s "categories";"site",s(String.drop_prefix(text source)10)])(callback(fun data reason->
   if Option.is_none reason && live buffer && member "categories"data then(
    let choices=List.map(values(field "rows"data))~f:(fun row->text(field "name"row)^" ["^text(call "format"[s "%s";field "id"row])^"]",field "group"row)in
    let choice=text(call "completing-read"[s "Category: ";Value.list(List.map choices~f:(fun(k,_)->s k));nil;sym "t"])in
    Option.iter(List.Assoc.find choices choice~equal:String.equal)~f:(fun group->in_buffer buffer(fun()->browse(String.drop_prefix(text source)10)(text group)))))):Value.t));
 Defun.defun(Symbol.intern "camulus-reader-discourse-like")[%here]~docstring:"Like the current article; prefix removes the like."
  ~interactive:Raw_prefix(Returns Value.Type.unit)(let open Defun.Let_syntax in let%map_open unlike=optional_with_nil "unlike"value in like(truth unlike))
