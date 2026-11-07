open! Core
open! Ecaml
open Reader_backend

type selection = { owner : Value.t; source : string; id : string; archived : bool; parts : Value.t list }
let pane_name = "*Camulus mail attachments*"
let selected : selection option ref = ref None
let hidden : (string * string) option ref = ref None
let download = ref (fun ~archived:_ ~source:_ ~id:_ ~part:_ ~destination:_ -> assert false)
let pane () = call "get-buffer" [s pane_name]
let close_windows () =
  if truth (pane ()) then List.iter (values (call "get-buffer-window-list" [pane ();nil;sym "t"]))
    ~f:(fun window -> run "delete-window" [window])
let close () =
  hidden:=Option.map !selected ~f:(fun x -> x.source,x.id);
  close_windows ()
let row () =
  let id=call "tabulated-list-get-id" [] in
  match !selected with
  | Some x when truth id ->
    (match List.find x.parts ~f:(fun part -> Value.equal id (field "part" part)) with
     | Some part -> x,part | None -> user_error "Choose an attachment";assert false)
  | _ -> user_error "Choose an attachment";assert false
let filename part =
  let name=call "file-name-nondirectory" [field "name" part] |> text in
  if String.is_empty name || List.mem [".";".."] name ~equal:String.equal then
    "attachment-"^Int.to_string(integer(field "part" part)) else name
let save x part destination open_after =
  let request= !download ~archived:x.archived ~source:x.source ~id:x.id
    ~part:(integer(field "part" part)) ~destination in
  let finished=Function.to_value(Defun.lambda [%here] (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open event=required "event" value in
     if Value.eq(call "nth" [i 2;event])(sym "completed") then (
       if open_after then run "find-file-other-window" [s destination]
       else message "Saved attachment: %s" [s destination])
     else message "Attachment: %s" [call "nth" [i 3;event]];
     nil)) in
  run "puthash" [request;finished;get "camulus-reader--completions"]
let save_selected () =
  let x,part=row () in
  let destination=call "read-file-name" [s "Save attachment: ";nil;nil;nil;s(filename part)] |> text in
  save x part destination false
let open_selected () =
  let x,part=row () in
  let directory=call "make-temp-file" [s "camulus-attachment-";sym "t"] in
  save x part (text(call "expand-file-name" [s(filename part);directory])) true
let save_all () =
  match !selected with
  | None -> user_error "No attachments"
  | Some x ->
    let directory=call "read-directory-name" [s "Save attachments in: "] in
    let reserved=Hash_set.create(module String) in
    List.iter x.parts ~f:(fun part ->
      let name=filename part in
      let path name=text(call "expand-file-name" [s name;directory]) in
      let rec available n =
        let candidate=path(if n=0 then name else Int.to_string n^"-"^name) in
        if Hash_set.mem reserved candidate || truth(call "file-exists-p" [s candidate]) then available(n+1)
        else (Hash_set.add reserved candidate;candidate) in
      save x part (available 0) false)
let refresh ?(force=false) () =
  let owner=Buffer.to_value(Current_buffer.get()) in
  if truth(call "derived-mode-p" [sym "camulus-reader-article-mode"]) then (
    let source=text(get "camulus-reader--source") in
    let id=call "get-text-property" [call "point" [];sym "camulus-reader-id"] in
    let id=if truth id then id else call "get-text-property" [call "point-min" [];sym "camulus-reader-id"] in
    let parts=if String.is_prefix source ~prefix:"maildir:" && truth id then
      values(call "gethash" [id;get "camulus-reader--attachments"]) else [] in
    if List.is_empty parts then (
      (match !selected with Some x when Value.equal owner x.owner -> selected:=None;close_windows () | _ -> ()))
    else if force || not(Option.equal (fun (a,b) (c,d) -> String.equal a c && String.equal b d) !hidden (Some(source,text id))) then (
      let unchanged=match !selected with Some x -> Value.equal owner x.owner && String.equal x.id(text id)
        && String.equal source x.source && List.equal Value.equal parts x.parts | None -> false in
      if force || not unchanged then (
        let x={owner;source;id=text id;archived=truth(get "camulus-reader--archive");parts} in
        selected:=Some x;
        let target=call "get-buffer-create" [s pane_name] in
        in_buffer target (fun () ->
          if not(truth(call "derived-mode-p" [sym "camulus-mail-attachments-mode"])) then
            run "camulus-mail-attachments-mode" [];
          local "tabulated-list-format" (call "vector" [
            Value.list[s "File";i 40;sym "t"];Value.list[s "Type";i 26;sym "t"];Value.list[s "Size";i 12;sym "t"]]);
          local "tabulated-list-entries" (Value.list(List.map parts ~f:(fun part ->
            Value.list[field "part" part;call "vector" [s(filename part);field "mime" part;
              call "file-size-human-readable" [field "size" part]]])));
          run "tabulated-list-init-header" [];run "tabulated-list-print" [sym "t"]);
        let window=call "get-buffer-window" [owner] in
        if truth window then (
          let previous=call "selected-window" [] in
          Exn.protect ~finally:(fun () -> if truth(call "window-live-p" [previous]) then run "select-window" [previous]) ~f:(fun () ->
            run "select-window" [window];
            ignore(call "display-buffer-in-direction" [target;alist["direction",sym "below";
              "window-height",i(Int.min 8 (Int.max 4 (List.length parts+2)))] ] : Value.t))))))
let owner_closed () =
  let owner=Buffer.to_value(Current_buffer.get()) in
  match !selected with Some x when Value.equal owner x.owner -> selected:=None;close_windows () | _ -> ()
let native_keys () =
  if truth(call "derived-mode-p" [sym "camulus-mail-attachments-mode"]) && truth(get "meow-mode") then run "meow-mode" [i(-1)]
let install ~save_attachment =
  download:=save_attachment;
  run "require" [sym "tabulated-list"];
  mode "camulus-mail-attachments-mode" "tabulated-list-mode" "Mail attachments"
    "RET/o: open  s: save  S: save all  q: close";
  define0 ~interactive:true "camulus-mail-attachments" (fun () -> hidden:=None;refresh ~force:true ());
  define0 "camulus-mail-attachments--refresh" (fun () -> refresh ());
  define0 "camulus-mail-attachments--owner-closed" owner_closed;
  define0 "camulus-mail-attachments--native-keys" native_keys;
  define0 ~interactive:true "camulus-mail-attachments-open" open_selected;
  define0 ~interactive:true "camulus-mail-attachments-save" save_selected;
  define0 ~interactive:true "camulus-mail-attachments-save-all" save_all;
  define0 ~interactive:true "camulus-mail-attachments-close" close;
  let setup () =
    hook ~local:true "post-command-hook" "camulus-mail-attachments--refresh";
    hook ~local:true "kill-buffer-hook" "camulus-mail-attachments--owner-closed" in
  define0 "camulus-mail-attachments--setup" setup;
  hook "camulus-reader-article-mode-hook" "camulus-mail-attachments--setup";
  hook "meow-mode-hook" "camulus-mail-attachments--native-keys";
  run "advice-add" [sym "camulus-reader--render";sym ":after";sym "camulus-mail-attachments--refresh"];
  let map=get "camulus-mail-attachments-mode-map" in
  List.iter ["RET","camulus-mail-attachments-open";"o","camulus-mail-attachments-open";
    "s","camulus-mail-attachments-save";"S","camulus-mail-attachments-save-all";"q","camulus-mail-attachments-close"]
    ~f:(fun(key,command)->bind map key command)
