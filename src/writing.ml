(** Shared Emacs composing sessions. All functions and completion callbacks run
    on the Emacs owner thread. Backends own draft identity, storage revisions,
    source formatting and submission protocols; this module owns buffer lifetime.

    A completion means the operation has finished, not merely been queued. A
    backend must never retry submission here, and must retain any durable guards
    against resending a draft whose remote outcome is uncertain. *)
open! Core
open! Ecaml
open Reader_backend

type sent = { message : string; close : bool }
type backend = {
  snapshot : unit -> string;
  save : source:string -> complete:((unit, string) Result.t -> unit) -> unit;
  send : complete:((sent, string) Result.t -> unit) -> unit;
}
type t = {
  buffer : Value.t;
  backend : backend;
  mutable timer : Value.t;
  mutable saving : bool;
  mutable preparing : bool;
  mutable sending : bool;
  mutable sent : bool;
  mutable error : string option;
}
let sessions : t list ref = ref []
let active t = live t.buffer && List.exists !sessions ~f:(phys_equal t)
let current () =
  let buffer=Buffer.to_value(Current_buffer.get()) in
  List.find !sessions ~f:(fun t->Value.eq t.buffer buffer)
let current_exn () = Option.value_exn (current ()) ~message:"Not a Camulus draft buffer"
let cancel_timer t =
  if truth(call "timerp" [t.timer]) then run "cancel-timer" [t.timer];
  t.timer<-nil
let modified () = truth(call "buffer-modified-p" [])

(* A successful save acknowledges only its captured buffer version. Edits made
   while the worker persists that snapshot stay modified and get another save. *)
let rec save t =
  cancel_timer t;
  if active t && not(t.saving || t.sending || t.sent) && modified () then (
    t.error<-None;
    let tick=call "buffer-chars-modified-tick" [] in
    let finished=ref false in
    t.saving<-true;
    let complete result =
      if not !finished then (
        finished:=true;
        if active t then in_buffer t.buffer (fun () ->
          t.saving<-false;
          match result with
          | Ok () ->
            t.error<-None;
            if Value.equal tick (call "buffer-chars-modified-tick" []) then
              run "set-buffer-modified-p" [nil];
            if modified () then schedule t
          | Error reason ->
            t.error<-Some reason;
            message "Draft not saved: %s" [s reason])) in
    try t.backend.save ~source:(t.backend.snapshot ()) ~complete
    with ex -> complete (Error(Exn.to_string ex)))
and schedule t =
  cancel_timer t;
  if active t && not(t.preparing || t.sending || t.sent) && Option.is_none t.error then
    t.timer<-call "run-at-time" [get "camulus-writing-autosave-delay";nil;
      lambda0(fun () ->
        if active t then in_buffer t.buffer (fun () -> save t);
        nil)]

let flush t =
  cancel_timer t;
  if not t.sent then (
    (* Explicit save/close/send may retry a failed save. Autosave stops on error
       so a persistent storage failure cannot produce an endless retry loop. *)
    t.error<-None;
    let now () = Value.to_float_exn(call "float-time" []) in
    let deadline=now () +. 15. in
    let rec wait () =
      if not(active t) then failwith "Draft buffer closed while saving";
      Option.iter t.error ~f:failwith;
      if not t.saving then save t;
      Option.iter t.error ~f:failwith;
      if t.saving || modified () then (
        if Float.(now () > deadline) then failwith "Timed out saving draft; buffer kept open";
        run "accept-process-output" [nil;Value.of_float 0.01];
        wait ()) in
    wait ();
    cancel_timer t)

let before_kill t =
  try
    if t.preparing || t.sending then failwith "Wait for the send result before closing";
    flush t;
    true
  with ex -> message "Keeping draft open: %s" [s(Exn.to_string ex)]; false

let send t =
  if t.sent then failwith "This draft was already sent; do not resend";
  if t.preparing || t.sending then failwith "Sending is already in progress";
  let read_only=get "buffer-read-only" in
  (* Reserve submission before pumping events during flush. Reentrant commands
     cannot submit the same draft or modify the snapshot being prepared. *)
  t.preparing<-true;
  set "buffer-read-only" (sym "t");
  try
    flush t;
    t.preparing<-false;
    t.sending<-true;
    let finished=ref false in
    let complete result =
      if not !finished then (
        finished:=true;
        if active t then in_buffer t.buffer (fun () ->
          t.sending<-false;
          match result with
          | Error reason ->
            set "buffer-read-only" read_only;
            message "Send failed: %s" [s reason]
          | Ok receipt ->
            (* Positive delivery acknowledgement is terminal even when local
               cleanup failed. Keep the buffer read-only and forbid resending. *)
            t.sent<-true;
            run "set-buffer-modified-p" [nil];
            set "header-line-format" (s receipt.message);
            message "%s" [s receipt.message];
            if receipt.close then run "kill-buffer" [t.buffer])) in
    (try t.backend.send ~complete with ex -> complete(Error(Exn.to_string ex)))
  with ex ->
    t.preparing<-false;
    set "buffer-read-only" read_only;
    raise ex

let flush_all () =
  List.iter !sessions ~f:(fun t -> if active t then in_buffer t.buffer (fun () ->
    if t.preparing || t.sending then failwith "Wait for the send result before exiting";
    flush t))

let installed=ref false
let install () =
  if not !installed then (
    installed:=true;
    custom ~kind:Customization.Type.Number "camulus-writing-autosave-delay"
      (Value.of_float 0.75) "Seconds without edits before persisting a Camulus draft.";
    define0 ~interactive:true "camulus-writing-save" (fun () -> save(current_exn ()));
    define0 ~interactive:true "camulus-writing-send" (fun () -> send(current_exn ()));
    define0 ~interactive:true "camulus-writing-close" (fun () ->
      let t=current_exn () in if before_kill t then run "kill-buffer" [t.buffer]);
    Defun.defun(Symbol.intern "camulus-writing--changed") [%here]
      ~docstring:"Schedule persistence of this composing session." (Returns Value.Type.unit)
      (let open Defun.Let_syntax in
       let%map_open _start=required "start" value
       and _end=required "end" value and _old_length=required "old-length" value in
       Option.iter (current ()) ~f:schedule);
    Defun.defun_nullary(Symbol.intern "camulus-writing--before-kill") [%here]
      ~docstring:"Keep a draft open when saving fails or submission is in progress."
      (Returns Value.Type.bool) (fun () -> Option.value_map (current ()) ~default:true ~f:before_kill);
    define0 "camulus-writing--detach" (fun () ->
      Option.iter (current ()) ~f:(fun t ->
        cancel_timer t;
        sessions:=List.filter !sessions ~f:(fun other->not(phys_equal t other))));
    define0 "camulus-writing--flush-all" flush_all;
    Defun.defun_nullary(Symbol.intern "camulus-writing--before-exit") [%here]
      ~docstring:"Veto normal Emacs exit when a draft cannot be saved."
      (Returns Value.Type.bool) (fun () ->
        try flush_all ();true
        with ex -> message "Keeping Emacs open: %s" [s(Exn.to_string ex)];false);
    hook "kill-emacs-query-functions" "camulus-writing--before-exit";
    hook "kill-emacs-hook" "camulus-writing--flush-all")

let attach backend =
  install ();
  if Option.is_some(current ()) then invalid_arg "A writing session is already attached";
  let t={buffer=Buffer.to_value(Current_buffer.get());backend;timer=nil;saving=false;
    preparing=false;sending=false;sent=false;error=None} in
  sessions:=t::!sessions;
  let original=call "current-local-map" [] in
  let map=if truth original then call "copy-keymap" [original] else call "make-sparse-keymap" [] in
  run "use-local-map" [map];
  List.iter ["C-x C-s","camulus-writing-save";"C-c C-s","camulus-writing-save";
    "C-c C-c","camulus-writing-send";"C-c C-k","camulus-writing-close"]
    ~f:(fun (key,command)->bind map key command);
  local "header-line-format" (s "Draft · autosave  C-c C-s: save  C-c C-c: send  C-c C-k: save and close");
  hook ~local:true "after-change-functions" "camulus-writing--changed";
  hook ~local:true "kill-buffer-query-functions" "camulus-writing--before-kill";
  hook ~local:true "kill-buffer-hook" "camulus-writing--detach"
