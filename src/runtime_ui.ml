(* Emacs owns the notification process and timers. The Eio domain only publishes
   OCaml events; the reader receives them here directly on the owner thread. *)
module Os = Unix
open! Core
open! Ecaml
let s=Value.of_utf8_bytes
let sym=Value.intern
let nil=Value.nil
let call name args=Value.funcallN (sym name) args
let run name args=ignore(call name args : Value.t)
let get name=Current_buffer.symbol_value (Symbol.intern name)
let set name value=run "set" [sym name;value]
let truth value=not(Value.eq value nil)
let receive = ref (fun (_:Reader.Protocol.event) -> ())
let publishing = ref false
let publish () =
  if not !publishing && not(truth(get "camulus-probe--hold")) && not(String.equal (Application.status ()) "idle") then (
    publishing:=true;
    Exn.protect ~finally:(fun () -> publishing:=false) ~f:(fun () ->
      let publish_values=truth(get "camulus-probe-record-events") || truth(get "camulus-probe-event-hook") in
      let events=Application.drain ~receive:!receive ~publish_values () in
      List.iter events ~f:(fun event ->
        if not(truth(call "camulus-host-handle-event" [event])) then (
          if truth(get "camulus-probe-record-events") then set "camulus-probe--events" (call "cons" [event;get "camulus-probe--events"]);
          run "run-hook-with-args" [sym "camulus-probe-event-hook";event];
          ()))))
let await predicate timeout =
  let deadline=Os.gettimeofday () +. timeout in
  while not(truth(Value.funcallN predicate [])) do
    if Float.(Os.gettimeofday () > deadline) then failwith ("Camulus worker timeout: "^Application.status ());
    run "accept-process-output" [nil;Value.of_float 0.01]
  done
let start () =
  if not(String.equal (Application.status ()) "idle") then failwith "A Camulus session is already active";
  set "camulus-probe--events" nil;set "camulus-probe--wakeups" (Value.of_int_exn 0);set "camulus-probe--hold" nil;
  let process=call "make-pipe-process" [sym ":name";s "camulus-events";sym ":buffer";nil;
    sym ":coding";sym "binary";sym ":noquery";sym "t";sym ":filter";sym "camulus-probe--receive"] in
  set "camulus-probe--process" process;
  try Application.start process;
    set "camulus-probe--timer" (call "run-at-time" [Value.of_float 0.02;Value.of_float 0.02;sym "camulus-probe--publish"])
  with exn -> run "delete-process" [process];set "camulus-probe--process" nil;raise exn
let stop () =
  if not(String.equal (Application.status ()) "idle") then (
    Reader.Runtime.request_stop (Application.get_runtime ());
    let reap=Function.to_value (Defun.lambda_nullary [%here] (Returns Value.Type.bool) Application.reap) in
    await reap 5.);
  if truth(call "timerp" [get "camulus-probe--timer"]) then run "cancel-timer" [get "camulus-probe--timer"];
  run "camulus-host-cancel-all" [];
  if truth(call "process-live-p" [get "camulus-probe--process"]) then run "delete-process" [get "camulus-probe--process"];
  set "camulus-probe--timer" nil;set "camulus-probe--process" nil
let install ~on_event ~on_start ~on_stop =
  List.iter ["camulus-probe--process";"camulus-probe--timer";"camulus-probe--events";"camulus-probe--hold";"camulus-probe-record-events";"camulus-probe-event-hook"]
    ~f:(fun name->Reader_backend.variable name nil);
  Reader_backend.variable "camulus-probe--wakeups"(Value.of_int_exn 0);
  receive:=on_event;
  let define name f=Defun.defun_nullary_nil (Symbol.intern name) [%here] ~docstring:"Camulus event-loop operation." f in
  define "camulus-probe--publish" publish;
  define "camulus-probe-session-start" (fun () -> start ();on_start ());
  define "camulus-probe-session-stop" (fun () -> on_stop ();stop ());
  run "add-hook"[sym "kill-emacs-hook";sym "camulus-probe-session-stop"];
  Defun.defun (Symbol.intern "camulus-probe--receive") [%here] ~docstring:"Drain worker events after a pipe wakeup."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open _process=required "process" value and _bytes=required "bytes" value in
     set "camulus-probe--wakeups" (Value.of_int_exn (Value.to_int_exn(get "camulus-probe--wakeups")+1));publish ());
  Defun.defun (Symbol.intern "camulus-probe-await") [%here] ~docstring:"Wait while pumping Emacs events; used by cleanup and integration tests."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open predicate=required "predicate" value and timeout=optional_with_nil "timeout" value in
     let timeout=if truth timeout then Value.to_float_exn(call "float" [timeout]) else 5. in await predicate timeout)
