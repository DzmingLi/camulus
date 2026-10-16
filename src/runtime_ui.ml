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
  if not !publishing && not(truth(get "ef-probe--hold")) && not(String.equal (Application.status ()) "idle") then (
    publishing:=true;
    Exn.protect ~finally:(fun () -> publishing:=false) ~f:(fun () ->
      let publish_values=truth(get "ef-probe-record-events") || truth(get "ef-probe-event-hook") in
      let events=Application.drain ~receive:!receive ~publish_values () in
      List.iter events ~f:(fun event ->
        if not(truth(call "ef-host-handle-event" [event])) then (
          if truth(get "ef-probe-record-events") then set "ef-probe--events" (call "cons" [event;get "ef-probe--events"]);
          run "run-hook-with-args" [sym "ef-probe-event-hook";event];
          ()))))
let await predicate timeout =
  let deadline=Os.gettimeofday () +. timeout in
  while not(truth(Value.funcallN predicate [])) do
    if Float.(Os.gettimeofday () > deadline) then failwith ("Camulus worker timeout: "^Application.status ());
    run "accept-process-output" [nil;Value.of_float 0.01]
  done
let start () =
  if not(String.equal (Application.status ()) "idle") then failwith "A Camulus session is already active";
  set "ef-probe--events" nil;set "ef-probe--wakeups" (Value.of_int_exn 0);set "ef-probe--hold" nil;
  let process=call "make-pipe-process" [sym ":name";s "camulus-events";sym ":buffer";nil;
    sym ":coding";sym "binary";sym ":noquery";sym "t";sym ":filter";sym "ef-probe--receive"] in
  set "ef-probe--process" process;
  try Application.start process;
    set "ef-probe--timer" (call "run-at-time" [Value.of_float 0.02;Value.of_float 0.02;sym "ef-probe--publish"])
  with exn -> run "delete-process" [process];set "ef-probe--process" nil;raise exn
let stop () =
  if not(String.equal (Application.status ()) "idle") then (
    Reader.Runtime.request_stop (Application.get_runtime ());
    let reap=Function.to_value (Defun.lambda_nullary [%here] (Returns Value.Type.bool) Application.reap) in
    await reap 5.);
  if truth(call "timerp" [get "ef-probe--timer"]) then run "cancel-timer" [get "ef-probe--timer"];
  run "ef-host-cancel-all" [];
  if truth(call "process-live-p" [get "ef-probe--process"]) then run "delete-process" [get "ef-probe--process"];
  set "ef-probe--timer" nil;set "ef-probe--process" nil
let install ~on_event =
  List.iter ["ef-probe--process";"ef-probe--timer";"ef-probe--events";"ef-probe--hold";"ef-probe-record-events";"ef-probe-event-hook"]
    ~f:(fun name->Reader_backend.variable name nil);
  Reader_backend.variable "ef-probe--wakeups"(Value.of_int_exn 0);
  receive:=on_event;
  let define name f=Defun.defun_nullary_nil (Symbol.intern name) [%here] ~docstring:"Camulus event-loop operation." f in
  define "ef-probe--publish" publish;
  define "ef-probe-session-start" start;
  define "ef-probe-session-stop" stop;
  run "add-hook"[sym "kill-emacs-hook";sym "ef-probe-session-stop"];
  Defun.defun (Symbol.intern "ef-probe--receive") [%here] ~docstring:"Drain worker events after a pipe wakeup."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open _process=required "process" value and _bytes=required "bytes" value in
     set "ef-probe--wakeups" (Value.of_int_exn (Value.to_int_exn(get "ef-probe--wakeups")+1));publish ());
  Defun.defun (Symbol.intern "ef-probe-await") [%here] ~docstring:"Wait while pumping Emacs events; used by cleanup and integration tests."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open predicate=required "predicate" value and timeout=optional_with_nil "timeout" value in
     let timeout=if truth timeout then Value.to_float_exn(call "float" [timeout]) else 5. in await predicate timeout)
