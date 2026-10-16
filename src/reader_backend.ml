(* Host-side capabilities registered by each backend. The shared UI knows neither
   backend names nor their request formats. This module is separate from the
   worker-side libraries so those stay usable without an Emacs runtime. *)
open! Core
open! Ecaml

type control = { key : string; hint : string; command : string }
type entrance = { label : string; open_ : unit -> unit }
type t = {
  name : string;
  matches : string -> bool;
  threaded : bool;
  thread_contains_bodies : bool;
  available : string -> bool;
  read : source:string -> id:string -> thread_id:string option -> group:string option -> Value.t;
  attachment : (source:string -> id:string -> group:string option -> part:int -> destination:string -> Value.t) option;
  controls : control list;
  browser : (string * string) option;
  entrances : unit -> entrance list;
  followed : (unit -> Value.t) option;
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
  name;matches;threaded;read;thread_contains_bodies=false;
  available=(fun _ -> true);attachment=None;controls=[];browser=None;entrances=(fun () -> []);followed=None;
  subscription_target=None;page=None;events=[];
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
