open! Core
open! Ecaml
(** Resolve auth-source lazily on the Emacs owner thread. Return plain OCaml
    data so a backend can snapshot credentials before starting its worker. *)
type t = { user:string; secret:string }
let call name args=Value.funcallN(Symbol.function_exn(Symbol.intern name))args
let nil=Value.nil
let sym=Value.intern
let s=Value.of_utf8_bytes
let truth v=not(Value.eq v nil)
let get name=Current_buffer.symbol_value(Symbol.intern name)
let with_sources extra f =
  ignore(call "require"[sym "auth-source"]:Value.t);
  let sources=call "append"[Value.list(List.map extra ~f:s);get "auth-sources"]in
  Current_buffer.set_value_temporarily Sync (Var.create(Symbol.intern "auth-sources")Value.Type.value)sources ~f
let auth_source ?user ?port ?(require_user=false) host =
  ignore(call "require"[sym "auth-source"]:Value.t);
  let args=[sym ":host";s host] @ Option.value_map user ~default:[] ~f:(fun u->[sym ":user";s u]) @
    Option.value_map port ~default:[] ~f:(fun p->[sym ":port";s p]) @
    [sym ":require";Value.list((if require_user then [sym ":user"]else[])@[sym ":secret"]);sym ":max";Value.of_int_exn 1] in
  let entry=call "car"[call "auth-source-search" args]in
  if not(truth entry)then None else
    let value key=
      let v=call "plist-get"[entry;sym key]in
      let v=if truth(call "functionp"[v])then call "funcall"[v]else v in
      if truth v then Value.to_utf8_bytes_exn v else "" in
    Some{user=value ":user";secret=value ":secret"}
