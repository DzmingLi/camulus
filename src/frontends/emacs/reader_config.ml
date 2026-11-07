open! Core
open! Ecaml
open Reader_backend
module Settings=Reader.Settings
let fail reason=user_error reason;failwith reason
let path ()=text(get "camulus-settings-file")
let normalize entry=match find(text(field "source"entry))with None->entry|Some backend->backend.normalize entry
let priority value=
 if not(truth(call "integerp"[value])) || integer value<0 then fail "Priorities must be nonnegative integers";
 integer value
let priority_rules ()=values(get "camulus-subscription-priorities")|>List.map ~f:(fun row->match values row with
 |[source;group;level]->(text source,text group),priority level|_->fail "Invalid priority rule")
let subscription_priority entry=Option.value(List.Assoc.find(priority_rules())
 (text(field "source"entry),text(field "group"entry))~equal:Poly.equal)~default:(priority(get "camulus-default-subscription-priority"))
let visible_subscriptions entries=let levels=values(get "camulus-home-priorities")|>List.map ~f:priority in
 List.filter entries ~f:(fun e->List.is_empty levels||List.mem levels(subscription_priority e)~equal:Int.equal)
let apply (config:Settings.t)=
 (* Validate every backend before changing the running frontend's mirrors. *)
 let assignments=List.concat_map !settings ~f:(fun(name,parse)->parse(Settings.field name config.backend_settings))in
 let groups=List.map config.groups ~f:(fun g->normalize(of_json(Settings.group_json g)))in
 List.iter !setting_defaults ~f:(fun(name,default)->set name default);
 List.iter assignments ~f:(fun(name,value)->set name value);
 set "camulus-groups"(Value.list groups);
 set "camulus-subscription-priorities"(Value.list(List.map config.priorities ~f:(fun p->Value.list[s p.source;s p.group;i p.level])));
 set "camulus-default-subscription-priority"(i config.default_priority);
 set "camulus-home-priorities"(Value.list(List.map config.home_priorities ~f:i))
let current ()={Settings.groups=List.map(values(get "camulus-groups"))~f:(fun e->{Settings.name=text(field "name"e);source=text(field "source"e);group=text(field "group"e)});
 priorities=List.map(priority_rules())~f:(fun((source,group),level)->{Settings.source;group;level});
 default_priority=priority(get "camulus-default-subscription-priority");home_priorities=List.map(values(get "camulus-home-priorities"))~f:priority;
 backend_settings=(Settings.load(path())).backend_settings}
let load ()=
 (try if truth(call "file-exists-p"[s(path())])then apply(Settings.load(path())) with ex->fail(Exn.to_string ex));
 List.iter(values(get "camulus-groups"))~f:(fun entry->Option.iter(find(text(field "source"entry)))~f:(fun backend->backend.on_entry entry))
let update f=
 let fallback=current()in
 let config=Settings.update(path())(fun existing->
  let base=if truth(call "file-exists-p"[s(path())])then existing else fallback in f base)in
 apply config
let save_priority entry level=update(fun config->let source=text(field "source"entry)and group=text(field "group"entry)in
 {config with priorities={Settings.source;group;level}::List.filter config.priorities ~f:(fun p->not(String.equal p.source source && String.equal p.group group))})
let save_home_priorities levels=update(fun config->{config with home_priorities=levels})
let setup_variables ()=
 List.iter["cl-lib";"tabulated-list";"json";"shr";"subr-x";"button"]~f:(fun name->run "require"[sym name]);
 variable "camulus-settings-file"(s(Settings.path()));
 variable "camulus-groups"nil;
 variable "camulus-subscription-priorities"nil;
 variable "camulus-default-subscription-priority"(i 2);
 variable "camulus-home-priorities"nil;
 variable "camulus-group-subscriptions-file"(data_path "XDG_STATE_HOME""~/.local/state""group-subscriptions.json");
 variable "camulus-state-file"(data_path "XDG_STATE_HOME""~/.local/state""state.sqlite3");
 setting "camulus-firefox-profile"nil;
 register_settings "browser"(string_settings["firefox_profile","camulus-firefox-profile"])
let register ~idle=
 define0 "camulus-reader-load-config"load;
 define0 "camulus-settings-validate-buffer"(fun()->
  ignore(Settings.of_json(Yojson.Safe.from_string(text(call "buffer-string"[]))) : Settings.t));
 let edit ()=
  if not(truth(call "file-exists-p"[s(path())]))then Settings.save(path())(current());
  run "find-file"[s(path())];
  hook ~local:true "before-save-hook""camulus-settings-validate-buffer";
  hook ~local:true "after-save-hook""camulus-reader-load-config";
  message "Shared Camulus settings. Connection changes take effect after restarting the reader."[]in
 define0 ~interactive:true "camulus-reader-edit-groups"edit;
 define0 ~interactive:true "camulus-reader-edit-config"edit;
 define0 "camulus-reader-start"(fun()->if idle()then(
  load();run "camulus-group-subscriptions-configure"[get "camulus-group-subscriptions-file"];
  run "camulus-content-state-configure"[get "camulus-state-file"];
  List.iter(all())~f:(fun backend->backend.start());
  List.iter["camulus-reader--requests";"camulus-reader--completions";"camulus-reader--loads";"camulus-reader--states";"camulus-reader--reading"]~f:(fun n->run "clrhash"[get n]);
  run "camulus-probe-session-start"[]))
