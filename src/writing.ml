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
    "C-c C-c","camulus-writing-send";"C-c C-d","camulus-writing-close";
    "C-c C-k","camulus-writing-close"]
    ~f:(fun (key,command)->bind map key command);
  local "header-line-format" (s "Draft · autosave  C-c C-c: send  C-c C-d: save and close");
  hook ~local:true "after-change-functions" "camulus-writing--changed";
  hook ~local:true "kill-buffer-query-functions" "camulus-writing--before-kill";
  hook ~local:true "kill-buffer-hook" "camulus-writing--detach"

let summary_window () =
  let selected=call "selected-window" [] in
  if not(truth(call "derived-mode-p" [sym "camulus-reader-article-mode"])) then selected
  else
    List.find (values(call "window-list" [])) ~f:(fun window ->
      in_buffer (call "window-buffer" [window]) (fun () ->
        truth(call "derived-mode-p" [sym "camulus-reader-feed-mode"])))
    |> Option.value ~default:selected

(* Wait for a positive backend receipt as well as task completion. *)
let request id ~expect ~complete =
  let receipt=ref None in
  let callback f = Function.to_value(Defun.lambda [%here] (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open data=required "data" value in f data;nil)) in
  run "puthash" [i id;callback(fun data ->
    if String.equal (text(field "kind" data)) expect then receipt:=Some data);
    get "camulus-reader--requests"];
  run "puthash" [i id;callback(fun event ->
    match !receipt with
    | Some data -> complete(Ok data)
    | None ->
      let reason=text(call "nth" [i 3;event]) in
      complete(Error(if String.is_empty reason then "No server receipt; check the destination before retrying" else reason)));
    get "camulus-reader--completions"]

let post_drafts : Value.t String.Table.t = String.Table.create ()
let post_headers source =
  String.split_lines source |> List.take_while ~f:(Fn.non String.is_empty)
let header_values headers name =
  let prefix=name^":" in
  List.filter_map headers ~f:(fun line ->
    Option.map (String.chop_prefix line ~prefix) ~f:String.strip)
let post_header name =
  text(call "buffer-substring-no-properties" [call "point-min" [];call "point-max" []])
  |> post_headers |> fun headers -> List.hd (header_values headers name)

let attach_completion ~separator ?delimiter candidates =
  let slice start stop=text(call "buffer-substring-no-properties" [i start;i stop]) in
  let length value=integer(call "length" [s value]) in
  let capf () =
    let point=integer(call "point" []) in
    let start=integer(call "line-beginning-position" []) in
    let stop=integer(call "line-end-position" []) in
    let prefix=slice (integer(call "point-min" [])) point in
    match String.lsplit2 (slice start stop) ~on:':',String.substr_index prefix ~pattern:separator with
    | Some (name,_),None when point>start+length name ->
      (match candidates name with
       | [] -> nil
       | choices ->
         let value_start=start+length name+1 in
         let first,last=match delimiter with
           | None -> value_start,stop
           | Some char ->
             let before=slice value_start point and after=slice point stop in
             let first=Option.value_map (String.rindex before char) ~default:value_start ~f:(fun index -> value_start+length(String.prefix before (index+1))) in
             let last=Option.value_map (String.index after char) ~default:stop ~f:(fun index -> point+length(String.prefix after index)) in
             first,last in
         let before=slice first point in
         let first=point-length (String.lstrip before) in
         Value.list [i first;i last;Value.list(List.map choices ~f:s);sym ":exclusive";sym "no"])
    | _ -> nil in
  run "add-hook" [sym "completion-at-point-functions";lambda0 capf;nil;sym "t"]

let post_draft ?(optional_fields=[]) ~destination ~identity ~fields ~complete_fields ~submit () =
  let window=summary_window () in
  let directory=data_path "XDG_STATE_HOME" "~/.local/state" "post-drafts/" in
  run "make-directory" [directory;sym "t"];
  run "set-file-modes" [directory;i 0o700];
  let name=Stdlib.Digest.(to_hex(string identity)) ^ ".draft" in
  let path=call "expand-file-name" [s name;directory] in
  let pending=s(text path^".sending") in
  let exists file=truth(call "file-exists-p" [file]) in
  let buffer=match Hashtbl.find post_drafts identity with
    | Some buffer when live buffer -> buffer
    | _ ->
      let buffer=Buffer.to_value(Buffer.create ~name:("*Camulus post: "^destination^"*")) in
      Hashtbl.set post_drafts ~key:identity ~data:buffer;
      in_buffer buffer (fun () ->
        run "text-mode" [];
        run "add-hook" [sym "kill-buffer-hook";lambda0(fun () -> Hashtbl.remove post_drafts identity;nil);nil;sym "t"];
        let snapshot ()=text(call "buffer-substring-no-properties" [call "point-min" [];call "point-max" []]) in
        let insert_fields fields =
          List.iter fields ~f:(fun (name,value) -> Point.insert (name^": "^value^"\n")) in
        if exists path then (
          run "insert-file-contents" [path];
          Point.goto_min ();
          let headers=post_headers (snapshot ()) in
          if truth(call "looking-at-p" [s "Subject:"]) &&
             List.for_all fields ~f:(fun (name,_) -> List.is_empty (header_values headers name)) then
            insert_fields fields;
          let missing=List.filter optional_fields ~f:(fun (name,_) -> List.is_empty (header_values headers name)) in
          if not(List.is_empty missing) then (
            Point.goto_min ();
            run "forward-line" [i (List.length fields)];
            insert_fields missing))
        else (insert_fields (fields @ optional_fields);Point.insert "Subject: \n\n");
        Point.goto_min ();run "forward-line" [i (List.length fields + List.length optional_fields)];run "end-of-line" [];
        run "set-buffer-modified-p" [nil];
        attach_completion ~separator:"\n\n" complete_fields;
        attach {
          snapshot;
          save=(fun ~source ~complete ->
            let temp=call "make-temp-file" [path] in
            Exn.protect ~finally:(fun () -> if exists temp then run "delete-file" [temp]) ~f:(fun () ->
              Current_buffer.set_value_temporarily Sync
                (Var.create (Symbol.intern "coding-system-for-write") Value.Type.value) (sym "utf-8-unix")
                ~f:(fun () -> run "write-region" [s source;nil;temp;nil;sym "silent"]);
              run "rename-file" [temp;path;sym "t"]);
            complete(Ok ()));
          send=(fun ~complete ->
            let source=snapshot () in
            let header_end=String.substr_index source ~pattern:"\n\n"
              |> Option.value_exn ~message:"Separate the post headers from the body with a blank line" in
            let headers=post_headers source in
            let header name =
              match header_values headers name with
              | [value] when not(String.is_empty value) -> value
              | _ -> invalid_arg ("Enter a single "^name^": value") in
            let optional name=match header_values headers name with
              | [] | [""] -> None
              | [value] -> Some(name,value)
              | _ -> invalid_arg ("Enter at most one "^name^": value") in
            let targets=List.map fields ~f:(fun (name,_) -> name,header name) @
              List.filter_map optional_fields ~f:(fun (name,_) -> optional name) in
            let subject=header "Subject" in
            let body=String.drop_prefix source (header_end+2) |> String.strip in
            if String.is_empty body then invalid_arg "Enter a post body";
            if exists pending then (
              if not(truth(call "yes-or-no-p"
                [s "Previous submission is unconfirmed. Have you checked the destination and want to resend? "])) then
                failwith "Draft kept";
              run "delete-file" [pending]);
            (* A marker survives Emacs exit or a missing receipt. *)
            run "write-region" [s "";nil;pending;nil;sym "silent";nil;sym "excl"];
            let finish = function
              | Error reason -> complete(Error reason)
              | Ok data ->
                let cleaned=try
                  if exists path then run "delete-file" [path];
                  run "delete-file" [pending];true
                with _ -> false in
                let notice=text(field "notice" data) in
                let message=if String.is_empty notice then "Post published" else notice in
                complete(Ok {close=cleaned;message=if cleaned then message else message^". Draft cleanup failed."}) in
            try submit ~targets ~subject ~body ~complete:finish
            with ex -> run "delete-file" [pending];raise ex)
        });
      buffer in
  run "select-window" [window];
  run "switch-to-buffer" [buffer]
