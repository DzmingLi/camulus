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
let import_files path groups_path=
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
  set "camulus-groups"(Value.list entries);
  List.iter assignments~f:(fun(name,value)->set name value));
 List.iter(values(get "camulus-groups"))~f:(fun entry->Option.iter(find(text(field "source"entry)))~f:(fun backend->backend.on_entry entry))
let priority value =
  if not(truth(call "integerp" [value])) || integer value < 0 then fail "Subscription priorities must be nonnegative integers";
  integer value
let priority_rules () =
  values(get "camulus-subscription-priorities") |> List.map ~f:(fun row ->
    match values row with
    | [source;group;level] ->
      let key=text source,text group in
      if String.is_empty(fst key) || String.is_empty(snd key) then fail "Priority rules require source and group";
      key,priority level
    | _ -> fail "Each priority rule must be (SOURCE GROUP LEVEL)")
let subscription_priority entry =
  Option.value (List.Assoc.find (priority_rules ()) (text(field "source" entry),text(field "group" entry)) ~equal:Poly.equal)
    ~default:(priority(get "camulus-default-subscription-priority"))
let visible_subscriptions entries =
  let levels=values(get "camulus-home-priorities") |> List.map ~f:priority in
  List.filter entries ~f:(fun entry -> List.is_empty levels || List.mem levels (subscription_priority entry) ~equal:Int.equal)
let validate_priorities () =
  ignore(priority_rules () : ((string * string) * int) list);
  ignore(priority(get "camulus-default-subscription-priority") : int);
  List.iter (values(get "camulus-home-priorities")) ~f:(fun value -> ignore(priority value : int))
let setup_variables ()=
 custom ~kind:(Customization.Type.Repeat (List [String;String;Integer])) "camulus-subscription-priorities" nil
   "Local display priorities as (SOURCE GROUP LEVEL). Smaller levels mean higher priority. This never changes remote subscriptions.";
 custom ~kind:Customization.Type.Integer "camulus-default-subscription-priority" (i 2)
   "Priority of a subscription source without an explicit rule.";
 custom ~kind:(Customization.Type.Option {doc_for_none="Show all priorities";t=Repeat Integer}) "camulus-home-priorities" nil
   "Subscription priority levels included in Home. Nil shows all. Refresh Home after changing this option.";
 List.iter["cl-lib";"tabulated-list";"json";"shr";"subr-x";"button"]~f:(fun name->run "require"[sym name]);
 variable "camulus-state-file"(data_path "XDG_STATE_HOME" "~/.local/state" "state.sqlite3");
 custom ~kind:(Customization.Type.Repeat (Alist (Symbol,String))) "camulus-groups" nil
   "Registered browsing groups. Each entry has name, source and group string fields. Registration is independent of subscription.";
 custom ~kind:(Customization.Type.Option {doc_for_none="No browser profile";t=Directory})
   "camulus-firefox-profile" nil "Firefox profile used by backends that read browser cookies.";
 register_settings "browser"(string_settings["firefox_profile","camulus-firefox-profile"])
let load () =
 validate_priorities ();
 let names=Hash_set.create(module String) in
 let entries=List.map (values(get "camulus-groups")) ~f:(fun entry ->
   let data=to_json entry in
   List.iter ["name";"source";"group"] ~f:(fun key -> require_string key data);
   if truth(field "timeline" entry) then fail "Set subscription through the backend";
   let entry=normalize entry in
   let name=text(field "name" entry) in
   if Hash_set.mem names name then fail "Duplicate group name";
   Hash_set.add names name;entry) in
 set "camulus-groups" (Value.list entries);
 List.iter entries ~f:(fun entry -> Option.iter(find(text(field "source" entry))) ~f:(fun backend -> backend.on_entry entry))
let register ~idle =
 define0 "camulus-reader-load-config" load;
 Defun.defun (Symbol.intern "camulus-import-json-config") [%here]
   ~docstring:"Import existing JSON settings once into current Emacs option values. Save them with Customize to persist."
   (Returns Value.Type.unit)
   (let open Defun.Let_syntax in
    let%map_open config=required "config-file" value and groups=required "groups-file" value in
    import_files config groups);
 define0 ~interactive:true "camulus-reader-edit-groups" (fun () ->
   Customization.customize_variable (Symbol.intern "camulus-groups"));
 define0 ~interactive:true "camulus-reader-edit-config" (fun () -> run "customize-group" [sym "camulus"]);
 define0 "camulus-reader-start"(fun()->if idle()then(
  load();
  run "camulus-content-state-configure"[get "camulus-state-file"];
  List.iter(all())~f:(fun backend->backend.start());
  List.iter["camulus-reader--requests";"camulus-reader--completions";"camulus-reader--loads";"camulus-reader--states";"camulus-reader--reading"]~f:(fun n->run "clrhash"[get n]);
  run "camulus-probe-session-start"[]))
