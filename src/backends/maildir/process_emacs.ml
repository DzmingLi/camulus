open! Core
open! Ecaml
open Reader_backend
let jobs : (int,Value.t list) Hashtbl.t=Hashtbl.create(module Int)
let cancel id=
 match Hashtbl.find jobs id with None->()|Some ps->Hashtbl.remove jobs id;
 List.iter ps ~f:(fun p->if truth(call "processp"[p])then(
  run "set-process-sentinel"[p;sym "ignore"];if truth(call "process-live-p"[p])then run "delete-process"[p]))
let cancel_all ()=List.iter(Hashtbl.keys jobs)~f:cancel
let start id argv input_path limit=
 let chunks=ref []and size=ref 0 and done_=ref false and status=ref None and err_done=ref false and err=ref nil in
 let complete status bytes=run "ef-host-process-complete"[i id;i status;bytes]in
 let finish ()=match !status with Some status when not !done_ && !err_done->
  done_:=true;Hashtbl.remove jobs id;
  complete status (call "decode-coding-string"[call "concat"(List.rev !chunks);sym "utf-8-unix"]);
  if truth(call "process-live-p"[!err])then run "delete-process"[!err]
 |_->()in
 let fail ()=if not !done_ then(done_:=true;complete (-2) (s "");cancel id)in
 let count bytes= size:= !size+integer(call "length"[bytes]); !size<=limit in
 try
  err:=call "make-pipe-process"[sym ":name";s "camulus-mail-stderr";sym ":noquery";sym "t";sym ":coding";sym "binary";
   sym ":filter";lambda2(fun _ bytes->if not(count bytes)then fail();nil);
   sym ":sentinel";lambda2(fun p _->if not(truth(call "process-live-p"[p]))then(err_done:=true;finish());nil)];
  let command=if String.is_empty input_path then argv else
   [Option.value_map (let v=call "executable-find"[s "sh"]in if truth v then Some v else None)~default:(s "/bin/sh")~f:Fn.id;
    s "-c";s "file=$1; shift; exec \"$@\" < \"$file\"";s "camulus-mail-stdin";s input_path]@argv in
  let p=call "make-process"[sym ":name";s "camulus-mail-command";sym ":command";Value.list command;
   sym ":connection-type";sym "pipe";sym ":noquery";sym "t";sym ":coding";sym "binary";sym ":stderr";!err;
   sym ":filter";lambda2(fun _ bytes->if count bytes then chunks:=bytes::!chunks else fail();nil);
   sym ":sentinel";lambda2(fun p _->let state=call "process-status"[p]in
    if List.exists["exit";"signal";"failed"]~f:(fun name->Value.eq state(sym name))then(
     status:=Some(if Value.eq state(sym "exit")then integer(call "process-exit-status"[p])else -1);finish());nil)]in
  Hashtbl.set jobs ~key:id ~data:[p;!err];run "process-send-eof"[p]
 with _->if not !done_ then(done_:=true;complete (-1)(s ""));cancel id;
  if truth(call "process-live-p"[!err])then run "delete-process"[!err]
let handle event=
 let kind=call "car"[event]and nth n=call "nth"[i n;event]in
 if Value.eq kind(sym "host-start")then(start(integer(nth 1))(values(nth 2))(text(nth 3))(integer(nth 4));true)
 else if Value.eq kind(sym "host-cancel")then(cancel(integer(nth 1));true)else false
let install ()=
 define0 "ef-host-cancel-all"cancel_all;
 define1 "ef-host-handle-event"(fun event->b(handle event))
