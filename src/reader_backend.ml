(* Host-side capabilities registered by each backend. The shared UI knows neither
   backend names nor their request formats. This module is separate from the
   worker-side libraries so those stay usable without an Emacs runtime. *)
open! Core
open! Ecaml

type control = { key : string; hint : string; command : string }
type entrance = { label : string; open_ : unit -> unit }
type t = {
  name : string;
  start : unit -> unit;
  normalize : Value.t -> Value.t;
  on_entry : Value.t -> unit;
  matches : string -> bool;
  threaded : bool;
  thread_contains_bodies : bool;
  available : string -> bool;
  read : source:string -> id:string -> thread_id:string option -> group:string option -> Value.t;
  attachment : (source:string -> id:string -> group:string option -> part:int -> destination:string -> Value.t) option;
  controls : control list;
  list_controls : control list;
  delete : (entry:Value.t -> row:Value.t -> refresh:(unit -> unit) -> unit) option;
  article_marks : Value.t -> string;
  row_title : Value.t -> Value.t;
  browser : (string * string) option;
  entrances : unit -> entrance list;
  followed : (unit -> Value.t) option;
  group_subscription : (source:string -> group:string -> setting:string option -> Value.t) option;
  subscription_target : (source:string -> selected:(unit -> Value.t) -> string option) option;
  page : (Value.t -> unit) option;
  events : string list;
  reconcile : source:string -> existing:Value.t -> request:Value.t -> archived:bool -> Value.t -> Value.t;
}
let registry : t list ref = ref []
let register backend =
  registry := List.filter !registry ~f:(fun b -> not (String.equal b.name backend.name)) @ [backend]
let all () = !registry
let find source = List.find !registry ~f:(fun b -> b.matches source)
let call name args = Value.funcallN (Value.intern name) args
let run name args = ignore (call name args : Value.t)
let s = Value.of_utf8_bytes
let alist fields = Value.list (List.map fields ~f:(fun (k,v) -> call "cons" [Value.intern k;v]))
let base ~name ~matches ~threaded ~read = {
  name;start=(fun()->());normalize=Fn.id;on_entry=(fun _->());matches;threaded;read;thread_contains_bodies=false;
  available=(fun _ -> true);attachment=None;controls=[];list_controls=[];article_marks=(fun _->"");row_title=(fun row->Value.funcallN(Value.intern "alist-get")[Value.intern "title";row]);browser=None;entrances=(fun () -> []);followed=None;
  subscription_target=None;group_subscription=None;delete=None;page=None;events=[];
  reconcile=(fun ~source:_ ~existing:_ ~request:_ ~archived:_ message -> message)
}

(* Backends may opt into local group following when their service has no
   corresponding remote operation. Topic tracking remains a separate capability. *)
type local_group_operations = {
  followed_groups : backend:string -> int;
  subscribe_group : backend:string -> source:string -> group:string -> setting:string option -> int;
}
let local_group_operations = ref None
let register_local_groups backend =
  let operations () = Option.value_exn !local_group_operations in
  register { backend with
    followed=Some(fun () -> Value.of_int_exn ((operations ()).followed_groups ~backend:backend.name));
    group_subscription=Some(fun ~source ~group ~setting -> Value.of_int_exn (
      (operations ()).subscribe_group ~backend:backend.name ~source ~group ~setting)) }

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
let integer v=Value.to_int_exn v
let i=Value.of_int_exn
let b v=if v then sym "t"else nil
let values v=Value.to_list_exn v ~f:Fn.id
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
let settings : (string * (Yojson.Safe.t -> (string * Value.t) list)) list ref=ref []
let register_settings name f=settings:=(name,f)::List.Assoc.remove !settings name ~equal:String.equal
let json_field key=function `Assoc xs->Option.value(List.Assoc.find xs key ~equal:String.equal)~default:`Null|_->`Null
let string_settings fields value=
 match value with
 |`Null->[]
 |`Assoc xs->List.filter_map fields ~f:(fun(key,variable)->match List.Assoc.find xs key ~equal:String.equal with
   |None->None|Some(`String v)when not(String.contains v '\000')->Some(variable,s v)
   |_->failwith("Invalid backend setting: "^key))
 |_->failwith "Backend settings must be an object"
