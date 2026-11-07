open! Core
open! Ecaml
module P=Reader.Protocol
let last : P.request option ref=ref None
let serial=ref 0
let () =
 Douban_emacs.install_protocol ();
 Douban_emacs.install ~submit:(fun ~service work->Int.incr serial;
  last:=Some {P.id= !serial;owner=(if service then P.Service else P.View 0);work};!serial) ();
 let deliver event=List.iter !Reader_backend.event_observers ~f:(fun f->ignore(f ~barrier:100 event:Value.t list)) in
 Defun.defun_nullary (Symbol.intern "douban-test-request") [%here] ~docstring:"Inspect the synthetic request." (Returns Value.Type.value)
  (fun()->let r=Option.value_exn !last in match r.work with
   |Douban_emacs.Douban(Like(id,liked))->Value.list[Value.of_utf8_bytes id;Value.of_bool liked;Value.of_bool(Poly.equal r.owner P.Service)]
   |_->failwith "Unexpected request");
 Defun.defun (Symbol.intern "douban-test-finish") [%here] ~docstring:"Deliver a mock acknowledgement or error." (Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open success=required "success" bool in
   let r=Option.value_exn !last in
   if success then (match r.work with
    |Douban_emacs.Douban(Like(id,liked))->deliver(P.Data(r,Douban_emacs.Douban_liked(id,{liked;count=Some 7})))
    |_->());
   deliver(P.Finished(r,if success then P.Completed else P.Failed "test failure")));
 Defun.defun (Symbol.intern "douban-test-reconcile") [%here] ~docstring:"Reconcile mock article metadata." (Returns Value.Type.value)
  (let open Defun.Let_syntax in let%map_open message=required "message" value and request=required "request" value in
   (Option.value_exn(Reader_backend.find "douban")).reconcile ~source:"douban" ~existing:Value.nil ~request ~archived:false message);
 provide(Symbol.intern "douban_probe")
