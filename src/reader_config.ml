open! Core
open! Ecaml
open Reader_backend
let fail reason=user_error reason;failwith reason
let parse_file path=
 let buffer=call "generate-new-buffer"[s " *Camulus config*"]in
 Exn.protect ~finally:(fun()->run "kill-buffer"[buffer])~f:(fun()->in_buffer buffer(fun()->
  run "insert-file-contents"[path];Yojson.Safe.from_string(text(call "buffer-string"[]))))
let load ()=
 let path=get "ef-reader-config-file"in
 if truth(call "file-exists-p"[path])then(
  let data=parse_file path in
  let entries=match json_field "entries"data with `List xs->xs|_->fail "Invalid reader configuration: expected entries"in
  if not(Poly.equal(json_field "version"data)(`Int 1))then fail "Invalid reader configuration: expected version 1";
  let home=match json_field "home"data with `Null->None|`String s->Some s|_->fail "Home must name a configured entry"in
  let settings_json=json_field "backend_settings"data in
  (match settings_json with `Null|`Assoc _->()|_->fail "Backend settings must be an object");
  let assignments=try List.concat_map !settings~f:(fun(name,f)->
   let value=json_field name settings_json in
   (match value with `Null|`Assoc _->()|_->failwith("Invalid settings object: "^name));f value)
   with ex->fail(Exn.to_string ex)in
  let names=Hash_set.create(module String)in
  let entries=List.map entries~f:(fun entry->
   List.iter["name";"source";"group"]~f:(fun key->match json_field key entry with
    |`String value when not(String.is_empty value) && not(String.contains value '\000')->()
    |_->fail "Each entry needs nonempty name, source and group strings");
   (match json_field "timeline"entry with `Null|`Bool _->()|_->fail "Entry timeline must be true or false");
   let value=of_json entry in let name=text(field "name"value)in
   if Hash_set.mem names name then fail "Duplicate entry name";Hash_set.add names name;
   match find(text(field "source"value))with None->value|Some backend->backend.normalize value)in
  Option.iter home ~f:(fun home->if not(Hash_set.mem names home)then fail "Home must name a configured entry");
  set "ef-reader-entries"(Value.list entries);set "ef-reader-home"(Option.value_map home~default:nil~f:s);
  List.iter assignments~f:(fun(name,value)->set name value));
 List.iter(values(get "ef-reader-entries"))~f:(fun entry->Option.iter(find(text(field "source"entry)))~f:(fun backend->backend.on_entry entry))
let setup_variables ()=
 List.iter["cl-lib";"tabulated-list";"json";"shr";"subr-x";"button"]~f:(fun name->run "require"[sym name]);
 variable "ef-reader-config-file"(data_path "XDG_CONFIG_HOME" "~/.config" "entries.json");
 variable "ef-reader-state-file"(data_path "XDG_STATE_HOME" "~/.local/state" "state.sqlite3");
 List.iter["ef-reader-entries";"ef-reader-home";"ef-reader-firefox-profile"]~f:(fun n->variable n nil);
 register_settings "browser"(string_settings["firefox_profile","ef-reader-firefox-profile"]);
 List.iter["ef-reader--requests";"ef-reader--completions";"ef-reader--loads";"ef-reader--states";"ef-reader--reading"]~f:(fun n->variable n(call "make-hash-table"[sym ":test";sym "equal"]));
 List.iter["entry";"home";"cursor";"query";"search-rows";"list-token";"list-request";"rows";"folder";"source";"title";"messages";"attachments";"loaded";"complete";"archive";"active";"positioned";"view-token";"visibility-timer";"rss-query"]~f:(fun n->let name="ef-reader--"^n in variable name nil;run "make-variable-buffer-local"[sym name]);
 variable "ef-reader-reply-indent"(i 4);
 run "custom-declare-group"[sym "camulus";nil;s "Read feeds and discussions.";sym ":group";sym "applications"];
 List.iter["ef-reader-heading",Value.list[Value.list[sym "t";Value.list[sym ":weight";sym "bold"]]];
  "ef-reader-separator",Value.list[Value.list[sym "t";Value.list[sym ":inherit";sym "shadow";sym ":underline";sym "t"]]]]
 ~f:(fun(name,spec)->run "custom-declare-face"[sym name;spec;s "Reader presentation.";sym ":group";sym "camulus"])
let register ~idle =
 define0 "ef-reader-load-config"load;
 define0 ~interactive:true "ef-reader-edit-config"(fun()->let path=get "ef-reader-config-file"in
  run "make-directory"[call "file-name-directory"[path];sym "t"];run "find-file"[path];
  if integer(call "buffer-size"[])=0 then Point.insert "{\n  \"version\": 1,\n  \"entries\": []\n}\n";
  message "Save entries, then G selects an entry. New identities require a runtime restart."[]);
 define0 "ef-reader-start"(fun()->if idle()then(
  load();run "ef-content-state-configure"[get "ef-reader-state-file"];
  run "ef-browser-profile-configure"[s(text(get "ef-reader-firefox-profile"))];
  List.iter(all())~f:(fun backend->backend.start());
  List.iter["ef-reader--requests";"ef-reader--completions";"ef-reader--loads";"ef-reader--states";"ef-reader--reading"]~f:(fun n->run "clrhash"[get n]);
  run "ef-probe-session-start"[]))
