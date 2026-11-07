open! Core
open! Ecaml
open Reader_backend
let jobs : (int,Value.t list) Hashtbl.t=Hashtbl.create(module Int)
let cancel id=
 match Hashtbl.find jobs id with None->()|Some ps->Hashtbl.remove jobs id;
 List.iter ps ~f:(fun p->
  run "set-process-sentinel"[p;sym "ignore"];if truth(call "process-live-p"[p])then run "delete-process"[p])
let cancel_all ()=List.iter(Hashtbl.keys jobs)~f:cancel
let start id argv=
 let chunks=ref [] and done_=ref false and err=ref nil in
 let complete status bytes=run "camulus-host-process-complete"[i id;i status;bytes]in
 let finish status=if not !done_ then (
  done_:=true;Hashtbl.remove jobs id;
  complete status (call "decode-coding-string"[call "concat"(List.rev !chunks);sym "utf-8-unix"]);
  if truth(call "process-live-p"[!err])then run "delete-process"[!err]) in
 try
  err:=call "make-pipe-process"[sym ":name";s "camulus-mail-stderr";sym ":noquery";sym "t";sym ":coding";sym "binary";
   sym ":filter";sym "ignore";sym ":sentinel";sym "ignore"];
  let p=call "make-process"[sym ":name";s "camulus-mail-command";sym ":command";Value.list argv;
   sym ":connection-type";sym "pipe";sym ":noquery";sym "t";sym ":coding";sym "binary";sym ":stderr";!err;
   sym ":filter";lambda2(fun _ bytes->chunks:=bytes::!chunks;nil);
   sym ":sentinel";lambda2(fun p _->let state=call "process-status"[p]in
    if List.exists["exit";"signal";"failed"]~f:(fun name->Value.eq state(sym name))then(
     finish(if Value.eq state(sym "exit")then integer(call "process-exit-status"[p])else -1));nil)]in
  Hashtbl.set jobs ~key:id ~data:[p;!err];
  (* A fast child may exit before stdin is closed; its sentinel owns the result. *)
  (try run "process-send-eof"[p] with ex->if truth(call "process-live-p"[p])then raise ex)
 with _->if not !done_ then(done_:=true;complete (-1)(s ""));cancel id;
  if truth(call "process-live-p"[!err])then run "delete-process"[!err]
let handle event=
 let kind=call "car"[event]and nth n=call "nth"[i n;event]in
 if Value.eq kind(sym "host-start")then(start(integer(nth 1))(values(nth 2));true)
 else if Value.eq kind(sym "host-cancel")then(cancel(integer(nth 1));true)else false
let install ()=
 define0 "camulus-host-cancel-all"cancel_all;
 define1 "camulus-host-handle-event"(fun event->b(handle event))
