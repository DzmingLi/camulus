open! Core
open! Ecaml
open Reader_backend
let fail reason=user_error reason;failwith reason
let entry value =
 List.iter ["name";"source";"group"] ~f:(fun key ->
   let field=field key value in
   if not(truth(call "stringp" [field])) || String.is_empty(text field) || String.contains (text field) '\000' then
     fail ("Expected nonempty "^key^" string"));
 match find(text(field "source" value)) with
 | None -> value
 | Some backend ->
   try backend.normalize value with Invalid_argument reason -> fail reason
let priority value =
  if not(truth(call "integerp" [value])) then fail "Subscription priorities must be integers";
  integer value
let priority_rules () =
  values(get "camulus-subscription-priorities") |> List.map ~f:(fun row ->
    match values row with
    | [source;group;level] ->
      let key=text source,text group in
      if String.is_empty(fst key) || String.is_empty(snd key) then fail "Priority rules require source and group";
      key,priority level
    | _ -> fail "Each priority rule must be (SOURCE GROUP LEVEL)")
let subscription_priorities () =
  let rules=priority_rules () and default=priority(get "camulus-default-subscription-priority") in
  fun entry ->
    Option.value (List.Assoc.find rules (text(field "source" entry),text(field "group" entry)) ~equal:Poly.equal) ~default
let visible_subscriptions entries =
  let levels=values(get "camulus-home-priorities") |> List.map ~f:priority in
  if List.is_empty levels then entries else
    let priority=subscription_priorities () in
    List.filter entries ~f:(fun entry -> List.mem levels (priority entry) ~equal:Int.equal)
let setup_variables ()=
 custom ~kind:(Customization.Type.Repeat (List [String;String;Integer])) "camulus-subscription-priorities" nil
   "Local display priorities as (SOURCE GROUP LEVEL). Levels are integers, including negative values; smaller means higher priority. This never changes remote subscriptions.";
 custom ~kind:Customization.Type.Integer "camulus-default-subscription-priority" (i 2)
   "Priority of a subscription source without an explicit rule.";
 custom ~kind:(Customization.Type.Option {doc_for_none="Show all priorities";t=Repeat Integer}) "camulus-home-priorities" nil
   "Subscription priority levels included in Home. Nil shows all. Refresh Home after changing this option.";
 List.iter["cl-lib";"tabulated-list";"json";"shr";"subr-x";"button"]~f:(fun name->run "require"[sym name]);
 variable "camulus-state-file"(data_path "XDG_STATE_HOME" "~/.local/state" "state.sqlite3");
 custom ~kind:(Customization.Type.Repeat (Alist (Symbol,String))) "camulus-groups" nil
   "Registered browsing groups. Each entry has name, source and group string fields. Registration is independent of subscription.";
 custom ~kind:(Customization.Type.Option {doc_for_none="No browser profile";t=Directory})
   "camulus-firefox-profile" nil "Firefox profile used by backends that read browser cookies."
let register ~idle =
 define0 ~interactive:true "camulus-reader-edit-groups" (fun () ->
   Customization.customize_variable (Symbol.intern "camulus-groups"));
 define0 ~interactive:true "camulus-reader-edit-config" (fun () -> run "customize-group" [sym "camulus"]);
 define0 "camulus-reader-start"(fun()->if idle()then(
  run "camulus-content-state-configure"[get "camulus-state-file"];
  List.iter(all())~f:(fun backend->backend.start());
  List.iter["camulus-reader--requests";"camulus-reader--completions";"camulus-reader--loads";"camulus-reader--states";"camulus-reader--reading"]~f:(fun n->run "clrhash"[get n]);
  run "camulus-probe-session-start"[]))
