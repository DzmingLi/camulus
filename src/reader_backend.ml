(* Host-side capabilities registered by each backend. The shared UI knows neither
   backend names nor their request formats. This module is separate from the
   worker-side libraries so those stay usable without an Emacs runtime. *)
open! Core
open! Ecaml

type control = { key : string; hint : string; command : string }
type entrance = { label : string; open_ : unit -> unit }
type removal = Refresh_membership | Dismiss_notification | Delete_inbox_article
type composer = {
  in_group : Value.t -> (unit -> unit) option;
  default : unit -> (unit -> unit) option;
}
type folder_move = {
  list : item:Value.t -> receive:(string list -> unit) -> unit;
  move : item:Value.t -> folder:string -> unit;
}
type topic_tracking = {
  filters : (string * string) list;
  (* Request results use subscription-page {rows: [{id; title; setting}]; after}
     and subscription {source; target; setting; choices}. Policy values are opaque
     strings here, converted from the backend's types only at the Emacs boundary. *)
  list : source:string -> filter:string -> after:string option -> Value.t;
  change : source:string -> target:string -> setting:string option -> Value.t;
}
type t = {
  id : string;
  storage_prefix : string;
  name : string;
  start : unit -> unit;
  on_refresh : unit -> unit;
  inline_attachments : bool;
  normalize : Value.t -> Value.t;
  group_key : Value.t -> string;
  threaded : bool;
  thread_contains_bodies : bool;
  available : string -> bool;
  (* Describe a read; the common runtime serves/persists bodies before dispatch. *)
  read : source:string -> id:string -> thread_id:string option -> group:string option -> Reader.Protocol.work;
  attachment : (source:string -> id:string -> group:string option -> part:int -> destination:string -> Value.t) option;
  controls : control list;
  list_controls : control list;
  composer : composer option;
  refile : item:Value.t -> (string * (unit -> unit)) option;
  folder_move : folder_move option;
  (* Completion acknowledges the backend operation, not merely queueing it.
     The reader owns optimistic hiding and rollback of list rows. *)
  delete : (entry:Value.t -> row:Value.t -> complete:((removal, string) Result.t -> unit) -> unit) option;
  article_marks : Value.t -> string;
  row_title : Value.t -> Value.t;
  browser : (string * string) option;
  entrances : unit -> entrance list;
  (* Read configured sources on the UI thread; fetching content is worker work. *)
  followed : (unit -> Value.t list) option;
  can_subscribe_group : source:string -> group:string -> bool;
  subscription_option : string option;
  subscription_target : (source:string -> selected:(unit -> Value.t) -> string option) option;
  topic_tracking : topic_tracking option;
  events : string list;
  reconcile : source:string -> existing:Value.t -> request:Value.t -> archived:bool -> Value.t -> Value.t;
}
let registry : t list ref = ref []
let register backend =
  registry := List.filter !registry ~f:(fun b -> not (String.equal b.id backend.id)) @ [backend]
let all () = !registry
let find_id id = List.find !registry ~f:(fun b -> String.equal b.id id)
let owns_key backend key =
  String.equal backend.storage_prefix (Reader.Backend.key_backend key)
let find key = List.find !registry ~f:(fun backend -> owns_key backend key)
let call name args = Value.funcallN (Value.intern name) args
let run name args = ignore (call name args : Value.t)
let s = Value.of_utf8_bytes
let alist fields = Value.list (List.map fields ~f:(fun (k,v) -> call "cons" [Value.intern k;v]))
let base ~id ~name ~threaded ~read = {
  id;storage_prefix=id;name;start=(fun()->());on_refresh=(fun()->());inline_attachments=true;
  normalize=Fn.id;group_key=(fun _ -> id);threaded;read;thread_contains_bodies=false;
  available=(fun _ -> true);attachment=None;controls=[];list_controls=[];composer=None;refile=(fun ~item:_ -> None);article_marks=(fun _->"");row_title=(fun row->Value.funcallN(Value.intern "alist-get")[Value.intern "title";row]);browser=None;entrances=(fun () -> []);followed=None;
  subscription_target=None;topic_tracking=None;subscription_option=None;can_subscribe_group=(fun ~source:_ ~group:_ -> false);delete=None;folder_move=None;events=[];
  reconcile=(fun ~source:_ ~existing:_ ~request:_ ~archived:_ message -> message)
}

(* Conversion at the Emacs presentation boundary; no JSON string round trip. *)
let rec of_json : Yojson.Safe.t -> Value.t = function
  | `Null | `Bool false -> Value.nil | `Bool true -> Value.intern "t"
  | `String text -> s text | `Int n -> Value.of_int_exn n | `Float f -> Value.of_float f
  | `Intlit n -> call "string-to-number" [s n]
  | `List xs -> Value.list (List.map xs ~f:of_json)
  | `Assoc xs -> alist (List.map xs ~f:(fun(k,v)->k,of_json v))
let rec to_json value : Yojson.Safe.t =
  if Value.eq value Value.nil then `Null
  else if Value.eq value (Value.intern "t") then `Bool true
  else if Value.eq value (Value.intern ":json-false") then `Bool false
  else if not(Value.eq (call "stringp" [value]) Value.nil) then `String(Value.to_utf8_bytes_exn value)
  else if not(Value.eq (call "integerp" [value]) Value.nil) then `Int(Value.to_int_exn value)
  else if not(Value.eq (call "floatp" [value]) Value.nil) then `Float(Value.to_float_exn value)
  else if not(Value.eq (call "vectorp" [value]) Value.nil) then
    `List(Value.to_list_exn (call "append" [value;Value.nil]) ~f:to_json)
  else `Assoc(Value.to_list_exn value ~f:(fun pair ->
    Value.to_utf8_bytes_exn(call "symbol-name" [call "car" [pair]]),to_json(call "cdr" [pair])))

let variable name default =
  ignore (Defvar.defvar (Symbol.intern name) [%here] ~docstring:"Backend configuration."
    ~type_:Value.Type.value ~initial_value:default () : Value.t Var.t)

let nil=Value.nil
let sym=Value.intern
let truth v=not(Value.eq v nil)
let get name=Current_buffer.symbol_value(Symbol.intern name)
let set name v=run "set"[sym name;v]
let local name v=run "make-local-variable"[sym name];set name v
let field name v=call "alist-get"[sym name;v]
let text v=if truth v then Value.to_utf8_bytes_exn v else ""
let string_field field_name entry =
  let v = field field_name entry in
  if truth (call "stringp" [v]) then text v else ""
let group_backend entry =
  find_id (string_field "backend" entry)
let resolve_group entry =
  let configured = truth (field "backend" entry) in
  let owner = if configured then group_backend entry else find (string_field "backend_key" entry) in
  match owner with
  | None -> invalid_arg "Specify a registered backend for this group"
  | Some backend ->
    let key =
      if configured then backend.group_key entry
      else string_field "backend_key" entry in
    if not (owns_key backend key) then invalid_arg "Group key belongs to another backend";
    let entry =
      if String.equal key (string_field "backend_key" entry) then entry
      else call "cons" [call "cons" [sym "backend_key";s key];entry] in
    backend.normalize entry
let firefox_cookie_provider () =
 let configured=get "camulus-firefox-profile" in
 let profile=if truth configured then Some(text(call "expand-file-name" [configured])) else None in
 Reader.Credentials.Firefox.create ?profile ()
let integer v=Value.to_int_exn v
let i=Value.of_int_exn
let b v=if v then sym "t"else nil
let values v=Value.to_list_exn v ~f:Fn.id
let configured_groups () =
  List.filter_map (values (get "camulus-groups")) ~f:(fun entry ->
    Option.bind (group_backend entry) ~f:(fun _ ->
      try Some (resolve_group entry) with Invalid_argument _ -> None))
let in_buffer value f=Current_buffer.set_temporarily Sync (Buffer.of_value_exn value) ~f
let live v=truth(call "buffer-live-p"[v])
let message fmt args=run "message"(s fmt::args)
let user_error reason=run "user-error"[s "%s";s reason]
let define0 ?(interactive=false) name f=Defun.defun_nullary_nil(Symbol.intern name)[%here]
 ~docstring:"Camulus operation." ?interactive:(if interactive then Some No_arg else None) f
let define1 name f=Defun.defun(Symbol.intern name)[%here] ~docstring:"Camulus callback." (Returns Value.Type.value)
 (let open Defun.Let_syntax in let%map_open arg=required "argument"value in f arg)
let optional ?(interactive=false) name f=Defun.defun(Symbol.intern name)[%here] ~docstring:"Camulus operation."
 ?interactive:(if interactive then Some No_arg else None) (Returns Value.Type.value)
 (let open Defun.Let_syntax in let%map_open arg=optional_with_nil "argument"value in f arg)
let lambda0 f=Function.to_value(Defun.lambda_nullary [%here](Returns Value.Type.value) f)
let lambda2 f=Function.to_value(Defun.lambda [%here](Returns Value.Type.value)
 (let open Defun.Let_syntax in let%map_open a=required "a"value and c=required "b"value in f a c))
let hook ?(local=false) name command=run "add-hook"[sym name;sym command;nil;b local]
let bind map key command=run "define-key"[map;call "kbd"[s key];sym command]
let mode name parent title header=
 ignore(Major_mode.define_derived_mode (Symbol.intern name) [%here] ~docstring:"Camulus backend view."
  ~parent:(Major_mode.find_or_wrap_existing [%here] (Symbol.intern parent)) ~mode_line:title
  ~initialize:(Returns Value.Type.unit,(fun()->local "header-line-format"(s header))) () : (module Major_mode.S))
let data_path env fallback name=
 let root=call "getenv"[s env] in
 call "expand-file-name"[s("camulus/"^name);if truth root then root else call "expand-file-name"[s fallback]]
let customization_group = lazy (Customization.Group.defgroup "camulus" [%here]
  ~docstring:"Read feeds and discussions." ~parents:[Customization.Group.emacs])
let custom ?(kind=Customization.Type.Sexp) name default docstring =
  ignore (Customization.defcustom (Symbol.intern name) [%here] ~docstring
    ~group:(Lazy.force customization_group) ~type_:Value.Type.value
    ~customization_type:kind ~standard_value:default () : Value.t Customization.t)

(* Local subscriptions are backend-owned Emacs options. Discovery reads the
   current option on the owner thread; no worker state or second file is involved. *)
let all_groups ~source:_ ~group:_ = true
let register_local_groups ?key_for_group ~option ~accepts backend =
  let key_for_group = Option.value key_for_group ~default:(fun _ -> backend.storage_prefix) in
  custom ~kind:Customization.Type.(Repeat String) option nil
    (backend.name^" groups included in the inbox. Configure with setopt or Customize; registering a browsing group does not subscribe it.");
  let accepts ~source ~group=owns_key backend source && accepts ~source ~group in
  let followed () =
    let groups=values(get option) |> List.map ~f:(fun value->
      let group=Value.to_utf8_bytes_exn value in
      if String.is_empty group || String.contains group '\000' then invalid_arg("Invalid subscription in "^option);
      group) |> List.dedup_and_sort ~compare:String.compare in
    List.map groups ~f:(fun group->
      let source=key_for_group group in
      if not(accepts ~source ~group)then invalid_arg("Unsupported subscription in "^option);
      alist["name",s(backend.name^" · "^group);"backend_key",s source;"group",s group]) in
  register {backend with group_key=(fun entry -> key_for_group (string_field "group" entry));
    can_subscribe_group=accepts;subscription_option=Some option;followed=Some followed}

(* Worker sessions contain only OCaml data and run on the Eio domain. *)
type worker = {
  storage_prefix : string;
  key_available : string -> bool;
  all_pages : bool;
  (* Local directory listings can authoritatively replace the view's
     membership. Remote pages supply observations to merge into it. *)
  refresh_membership : [ `Merge | `Replace ];
  (* A page can also supply complete, locally retained article presentations. *)
  page : source:string -> string -> after:string option -> Reader.Content.page * Yojson.Safe.t list;
  archive : source:string -> refresh:bool -> Reader.Content_state.target -> string -> native_thread:(string -> string) -> add:(Reader.Content_state.archived -> unit) -> bool;
  execute : Reader.Protocol.work -> observe:(source:string -> Reader.Content.message list -> unit) -> emit:(Reader.Protocol.payload -> unit) -> bool;
}
type host = {
  create : net:Eio_unix.Net.t -> now:(unit -> float) -> sleep:(float -> unit) ->
    groups:(source:string -> id:string -> string list) ->
    revisions:(source:string -> group:string -> id:string -> through:int -> string list) ->
    article:(source:string -> id:string -> Yojson.Safe.t option) -> worker;
  drain : unit -> Value.t list;
  close : unit -> unit;
}
let worker_factories : (notify:(unit -> unit) -> host option) list ref = ref []
let payload_renderers : (Reader.Protocol.payload -> (string * Yojson.Safe.t) option) list ref = ref []
let event_observers : (barrier:int -> Reader.Protocol.event -> Value.t list) list ref = ref []
let source_providers : (unit -> string option) list ref = ref []
let archive_extractors : (string -> (string -> int -> string) option) list ref = ref []
let configured_sources () = List.filter_map !source_providers ~f:(fun f -> f ())

(* Backend-owned operations may publish the common presentation format. *)
type Reader.Protocol.work += Subscription of string * string * string option
type Reader.Protocol.payload += Presentation of string * Yojson.Safe.t
let reply emit kind fields =
  emit (Presentation ("content-state", `Assoc (("kind", `String kind) :: fields)))
let render_payload = function
  | Presentation(name,data) -> name,data
  | payload -> List.find_map !payload_renderers ~f:(fun render -> render payload)
    |> Option.value_exn ~message:"Unknown backend event"
