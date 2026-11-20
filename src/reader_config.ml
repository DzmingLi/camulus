open! Core
open! Ecaml
open Reader_backend
let fail reason=user_error reason;failwith reason
let entry value =
 List.iter ["name";"group"] ~f:(fun key ->
   let field=field key value in
   if not(truth(call "stringp" [field])) || String.is_empty(text field) || String.contains (text field) '\000' then
     fail ("Expected nonempty "^key^" string"));
 try resolve_group value with Invalid_argument reason -> fail reason
type inbox = {
  backends : string list;
  subscriptions : (string * string) list;
  filter : Reader.Content_state.view_filter;
}
type destination = Inbox of inbox | Group of Value.t | Folder of string
type view = { name : string; key : string option; destination : destination }

let view_string label value =
  if not (truth (call "stringp" [value])) || String.is_empty (String.strip (text value))
     || String.exists (text value) ~f:(fun c -> Char.to_int c < 32 || Char.to_int c = 127) then fail ("Invalid "^label);
  text value

let view_binding key =
  let key=call "kbd" [s key] in
  if integer (call "length" [key]) <> 1 then fail "A home view shortcut must be a single key";
  key

let parse_view value =
  let get key=call "plist-get" [value;sym key] in
  let name=view_string "view :name" (get ":name") in
  let key=get ":key" in
  let key=if not (truth key) then None
    else if truth (call "characterp" [key]) then Some (text (call "single-key-description" [key]))
    else Some (view_string "view :key" key) in
  let group=get ":group" and folder=get ":folder" and inbox=get ":inbox" in
  let backends=values (get ":backends") |> List.map ~f:(fun value ->
    let id=view_string "view backend" value in
    if Option.is_none (find_id id) then fail ("Unknown view backend "^id);
    id) in
  let subscriptions=values (get ":subscriptions") |> List.map ~f:(fun value ->
    let group=try resolve_group value with Invalid_argument reason -> fail reason in
    let id=view_string "subscription group" (field "group" group) in
    text (field "backend_key" group),id) in
  let days=get ":days" in
  let filter : Reader.Content_state.view_filter = {
    unread_only=truth (get ":unread-only");
    favorite_only=truth (get ":favorite-only");
    days=(if truth days then (
      if not (truth (call "integerp" [days])) || integer days < 1 then fail "View :days must be a positive integer";
      Some (integer days)) else None);
  } in
  let destination=match truth inbox,truth group,truth folder with
    | true,false,false -> Inbox {backends;subscriptions;filter}
    | false,true,false -> Group (entry (if truth (field "name" group) then group
        else call "cons" [call "cons" [sym "name";s name];group]))
    | false,false,true -> Folder (view_string "view :folder" folder)
    | _ -> fail "Specify exactly one of :inbox, :group, and :folder" in
  (match destination with
   | Inbox _ -> ()
   | Group _ | Folder _ ->
     if not (List.is_empty backends && List.is_empty subscriptions) || filter.unread_only || filter.favorite_only || Option.is_some filter.days then
       fail "Subscription and state filters require an inbox view");
  {name;key;destination}

let home_views () =
  let views=List.map (values (get "camulus-home-views")) ~f:parse_view in
  let reserved=List.map ["B";"G";"C";"U";"V";"f";"g";"h";"e";"q";"RET";"TAB";"<backtab>";"C-c";"C-g"]
      ~f:(fun key -> call "kbd" [s key]) in
  ignore (List.fold views ~init:([],reserved) ~f:(fun (names,keys) {name;key;_} ->
    if List.mem names name ~equal:String.equal then fail ("Duplicate home view name "^name);
    let keys=match key with
      | None -> keys
      | Some shortcut ->
        let key=view_binding shortcut in
        if List.mem keys key ~equal:Value.equal then fail ("Unavailable home view shortcut "^shortcut);
        key::keys in
    name::names,keys) : string list * Value.t list);
  views

let setup_variables ()=
 custom ~kind:(Customization.Type.Repeat Sexp) "camulus-home-views"
   (Value.list [
      Value.list [sym ":name";s "Unread";sym ":key";s "i";sym ":inbox";sym "t";sym ":unread-only";sym "t"];
      Value.list [sym ":name";s "All subscriptions";sym ":key";s "a";sym ":inbox";sym "t"]])
   "Ordered views on the Camulus home page. Each plist has :name, an optional
:key, and exactly one of :inbox t, :group GROUP, or :folder PATH. Inbox views
can filter by :backends (backend IDs), :subscriptions (group references),
:unread-only, :favorite-only, and :days. Group references are alists in the
same format as camulus-groups; the :group destination can omit its name.";
 List.iter["cl-lib";"tabulated-list";"json";"shr";"subr-x";"button"]~f:(fun name->run "require"[sym name]);
 variable "camulus-state-file"(data_path "XDG_STATE_HOME" "~/.local/state" "state.sqlite3");
 custom ~kind:(Customization.Type.Repeat (Alist (Symbol,String))) "camulus-groups" nil
   "Registered browsing groups with name, backend and group string fields.
Backends can use extra fields, such as site. Registration is independent of subscription.";
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
