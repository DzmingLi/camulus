open! Core
open! Ecaml
open Reader_backend
let fail reason=user_error reason;failwith reason
let parse_file path=
 let buffer=call "generate-new-buffer"[s " *Camulus config*"]in
 Exn.protect ~finally:(fun()->run "kill-buffer"[buffer])~f:(fun()->in_buffer buffer(fun()->
  run "insert-file-contents"[path];Yojson.Safe.from_string(text(call "buffer-string"[]))))
let normalize entry =
 match find(text(field "source"entry))with None->entry|Some backend->backend.normalize entry
let require_string key entry = match json_field key entry with
 |`String value when not(String.is_empty value) && not(String.contains value '\000')->()
 |_->fail ("Expected nonempty "^key^" string")
let read_config path =
 let data=parse_file path in
 if not(Poly.equal(json_field "version"data)(`Int 1))then fail "Invalid configuration: expected version 1";
 data
let load ()=
 let path=get "ef-reader-config-file" and groups_path=get "ef-reader-groups-file"in
 let exists path=truth(call "file-exists-p"[path])in
 if exists path || exists groups_path then(
  let data=if exists path then read_config path else `Assoc []in
  if not(Poly.equal(json_field "feed_sources"data)`Null)then fail "Subscriptions are runtime state, not configuration";
  let registry=if exists groups_path then read_config groups_path else `Assoc ["groups",`List []]in
  if not(Poly.equal(json_field "entries"data)`Null) then fail "Put registered groups in groups.json";
  let entries=match json_field "groups"registry with `List xs->xs|_->fail "groups.json must contain a groups array"in
  let settings_json=json_field "backend_settings"data in
  (match settings_json with `Null|`Assoc _->()|_->fail "Backend settings must be an object");
  let assignments=try List.concat_map !settings~f:(fun(name,f)->
   let value=json_field name settings_json in
   (match value with `Null|`Assoc _->()|_->failwith("Invalid settings object: "^name));f value)
   with ex->fail(Exn.to_string ex)in
  let names=Hash_set.create(module String)in
  let entries=List.map entries~f:(fun entry->
   List.iter["name";"source";"group"]~f:(fun key->require_string key entry);
   if not(Poly.equal(json_field "timeline"entry)`Null)then fail "Group registration does not subscribe; use the backend subscription command";
   let value=normalize(of_json entry)in let name=text(field "name"value)in
   if Hash_set.mem names name then fail "Duplicate group name";Hash_set.add names name;value)in
  set "ef-reader-entries"(Value.list entries);
  List.iter assignments~f:(fun(name,value)->set name value));
 List.iter(values(get "ef-reader-entries"))~f:(fun entry->Option.iter(find(text(field "source"entry)))~f:(fun backend->backend.on_entry entry))
let setup_variables ()=
 List.iter["cl-lib";"tabulated-list";"json";"shr";"subr-x";"button"]~f:(fun name->run "require"[sym name]);
 variable "ef-reader-config-file"(data_path "XDG_CONFIG_HOME" "~/.config" "config.json");
 variable "ef-reader-groups-file"(data_path "XDG_CONFIG_HOME" "~/.config" "groups.json");
 variable "camulus-group-subscriptions-file"(data_path "XDG_STATE_HOME" "~/.local/state" "group-subscriptions.json");
 variable "ef-reader-state-file"(data_path "XDG_STATE_HOME" "~/.local/state" "state.sqlite3");
 List.iter["ef-reader-entries";"ef-reader-firefox-profile"]~f:(fun n->variable n nil);
 register_settings "browser"(string_settings["firefox_profile","ef-reader-firefox-profile"]);
 List.iter["ef-reader--requests";"ef-reader--completions";"ef-reader--loads";"ef-reader--states";"ef-reader--reading"]~f:(fun n->variable n(call "make-hash-table"[sym ":test";sym "equal"]));
 List.iter["entry";"home";"cursor";"query";"search-rows";"list-token";"list-request";"rows";"folder";"source";"title";"messages";"attachments";"loaded";"complete";"archive";"active";"positioned";"view-token";"visibility-timer";"rss-query"]~f:(fun n->let name="ef-reader--"^n in variable name nil;run "make-variable-buffer-local"[sym name]);
 variable "ef-reader-reply-indent"(i 4);
 run "custom-declare-group"[sym "camulus";nil;s "Read feeds and discussions.";sym ":group";sym "applications"];
 let read_attributes color=Value.list[sym ":inherit";sym "shadow";sym ":foreground";s color;sym ":weight";sym "normal"]in
 let background name color=Value.list[Value.list[Value.list[sym "background";sym name]];read_attributes color]in
 List.iter["ef-reader-heading",Value.list[Value.list[sym "t";Value.list[sym ":weight";sym "bold"]]];
  "ef-reader-separator",Value.list[Value.list[sym "t";Value.list[sym ":inherit";sym "shadow";sym ":underline";sym "t"]]];
  "ef-reader-read",Value.list[background "light" "gray55";background "dark" "gray65";
    Value.list[sym "t";Value.list[sym ":inherit";sym "shadow"]]]]
 ~f:(fun(name,spec)->run "custom-declare-face"[sym name;spec;s "Reader presentation.";sym ":group";sym "camulus"])
let register ~idle =
 define0 "ef-reader-load-config"load;
 let edit variable initial message_text =
  let path=get variable in
  run "make-directory"[call "file-name-directory"[path];sym "t"];run "find-file"[path];
  if integer(call "buffer-size"[])=0 then Point.insert initial;
  message message_text []in
 define0 ~interactive:true "ef-reader-edit-groups"(fun()->edit "ef-reader-groups-file"
  "{\n  \"version\": 1,\n  \"groups\": []\n}\n"
  "Save groups, then B reloads the browsing directory. Registering a group does not subscribe.");
 define0 ~interactive:true "ef-reader-edit-config"(fun()->edit "ef-reader-config-file"
  "{\n  \"version\": 1,\n  \"backend_settings\": {}\n}\n"
  "Save settings, then g refreshes the home feed. New identities require a runtime restart.");
 define0 "ef-reader-start"(fun()->if idle()then(
  load();run "camulus-group-subscriptions-configure"[get "camulus-group-subscriptions-file"];
  run "ef-content-state-configure"[get "ef-reader-state-file"];
  run "ef-browser-profile-configure"[s(text(get "ef-reader-firefox-profile"))];
  List.iter(all())~f:(fun backend->backend.start());
  List.iter["ef-reader--requests";"ef-reader--completions";"ef-reader--loads";"ef-reader--states";"ef-reader--reading"]~f:(fun n->run "clrhash"[get n]);
  run "ef-probe-session-start"[]))
