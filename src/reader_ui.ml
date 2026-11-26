(* Emacs presentation lives on the owner thread. Values below are Emacs-owned
   buffer-local view state, never sent to the Eio worker. Existing backend adapters
   share these variables while their transport/compose APIs are migrated separately. *)
open! Core
open! Ecaml
open Reader_backend
module CS = Reader.Content_state

let list = Value.list
let str = Reader_backend.text
let optional_string x = if truth x then Some (str x) else None
let number x = if truth x then integer x else 0
let timestamp x = if truth x then Value.to_float_exn (call "float" [x]) else 0.
let text key value = str (field key value)
let cons a d = call "cons" [a;d]
let vector xs = call "vector" xs
let hash () = call "make-hash-table" [sym ":test";sym "equal"]
let find table key = call "gethash" [key;table]
let put table key value = run "puthash" [key;value;table]
let remove table key = run "remhash" [key;table]
let hash_values table = values (call "hash-table-values" [table])
let flag name = truth (get name)
let count table = number (call "hash-table-count" [table])
let member value xs = List.exists xs ~f:(Value.equal value)
let or_ a c = if truth a then a else c
let derived name = truth (call "derived-mode-p" [sym name])
let user_error message = run "user-error" [s "%s";s message]
let message text = run "message" [s "%s";s text]
let token () = call "make-symbol" [s "reader-view"]
let buffer () = Current_buffer.get ()
let bv = Buffer.to_value
let alive = Buffer.is_live
let in_buffer buffer f = Current_buffer.set_temporarily Sync buffer ~f
let writable f = Current_buffer.set_value_temporarily Sync (Var.create (Symbol.intern "inhibit-read-only") Value.Type.bool) true ~f
let point () = Position.to_int (Point.get ())
let min_point () = Position.to_int (Point.min ())
let max_point () = Position.to_int (Point.max ())
let goto p = Point.goto_char (Position.of_int_exn p)
let property p name = call "get-text-property" [i p;sym name]
let next_property p name limit = number (call "next-single-property-change" [i p;sym name;nil;i limit])
let properties start finish props = run "add-text-properties" [i start;i finish;list props]
let propertize value props = call "propertize" (value::props)
let styled text face = propertize (s text) [sym "face";sym face]
let insert_value v = Point.insert_text (Text.of_value_exn v)
let show_buffer name =
  let target = Buffer.find_or_create ~name in
  run "switch-to-buffer" [bv target]; target
let callback f =
  Function.to_value (Defun.lambda [%here] (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open value = required "value" value in f value))
let callback_unit f = callback (fun value -> f value;nil)
let callback0 f = Function.to_value (Defun.lambda_nullary_nil [%here] f)
let request ?(finished=nil) ?(receive=nil) operation =
  let id=i (Runtime_ui.request_state operation) in
  put (get "camulus-reader--requests") id (or_ receive (sym "ignore"));
  if truth finished then put (get "camulus-reader--completions") id finished;id
let request_ignore ?receive operation = ignore (request ?receive operation : Value.t)
let archive_target value = match str value with
  | "article" -> CS.Article | "thread" -> CS.Thread
  | _ -> invalid_arg "Invalid archive target"
let state source id = find (get "camulus-reader--states") (list [source;id])
let capability source = Reader_backend.find (str source)
let backend_label key =
  Option.value_map (capability key) ~default:(s (Reader.Backend.key_backend (str key)))
    ~f:(fun backend -> s backend.name)
let can_subscribe_entry entry =
  Option.exists (capability (field "backend_key" entry)) ~f:(fun backend ->
    backend.can_subscribe_group ~source:(text "backend_key" entry) ~group:(text "group" entry))
let single source = Option.exists (capability source) ~f:(fun backend -> not backend.threaded)
let entry_key entry = list [field "backend_key" entry;field "group" entry]
let inbox_entry row =
  List.find (values (get "camulus-reader--inbox-entries")) ~f:(fun e ->
    Value.equal (field "backend_key" e) (field "backend_key" row) && member (field "group" e) (values (field "groups" row)))
  |> Option.value ~default:nil
let format_time format time = call "format-time-string" [s format;call "seconds-to-time" [or_ time (i 0)]]
let inbox_row row = vector [s (if truth (field "unread" row) then "*" else "");
  (if Float.(timestamp (field "occurred" row) > 0.) then format_time "%m-%d %H:%M" (field "occurred" row) else s "");
  or_ (field "name" (inbox_entry row)) (backend_label (field "backend_key" row)); field "author" row;field "title" row]

type pending_delete = { source:string; group:string; identity:string; token:unit ref }
let pending_deletes = Hashtbl.create(module String)
let row_identity row = str(or_ (field "conversation_id" row) (or_ (field "thread_id" row) (field "id" row)))
let pending_marks () = get "camulus-reader--marks"
let article_mark_key id = list [get "camulus-reader--backend-key";id;s "article"]
let hidden_by_delete row =
  derived "camulus-reader-feed-mode" && Hashtbl.exists pending_deletes ~f:(fun pending ->
    String.equal pending.source (text "backend_key" row) && String.equal pending.identity (row_identity row) &&
    (if flag "camulus-reader--inbox" then member (s pending.group) (values(field "groups" row))
     else String.equal pending.group (text "group" (get "camulus-reader--entry"))))

let set_rows rows =
  (* Keep the latest authoritative rows separately. Pending backend operations
     hide only their presentation; neither read state nor dismissals are changed.
     Every cache/refresh result goes through this filter, including late results. *)
  set "camulus-reader--all-rows" rows;
  let rows=list(List.filter (values rows) ~f:(fun row->not(hidden_by_delete row))) in
  (* A removed ID cannot be restored by tabulated-list-print. Remember the
     displayed neighbours as well, so asynchronous refreshes keep our place. *)
  let old_ids=List.map (values (get "tabulated-list-entries")) ~f:(fun entry -> call "car" [entry]) in
  let anchor p =
    let id=call "tabulated-list-get-id" [i p] in
    let before,after=List.split_while old_ids ~f:(fun key -> not (Value.equal key id)) in
    after @ List.rev before in
  let saved=anchor (point ()) and column=call "current-column" [] in
  let windows=List.map (values (call "get-buffer-window-list" [bv (buffer ());nil;b true])) ~f:(fun window ->
    window,anchor (number (call "window-point" [window])),anchor (number (call "window-start" [window]))) in
  let table = hash () in
  set "camulus-reader--rows" table;
  let entries = List.map (values rows) ~f:(fun row ->
    let row = if single (field "backend_key" row) && not (truth (field "target" row))
      then cons (cons (sym "target") (s "article")) row else row in
    let favorite = derived "camulus-reader-folder-mode" in
    let key = list [field "backend_key" row;(if favorite then field "id" row else or_ (field "conversation_id" row) (field "id" row));field "target" row] in
    put table key row;
    let columns = if flag "camulus-reader--inbox" then inbox_row row
      else if favorite then vector [field "target" row;backend_label (field "backend_key" row);field "title" row]
      else vector [s (if truth (field "unread" row) then "*" else "");backend_label (field "backend_key" row);field "author" row;
        Option.value_map(capability(field "backend_key"row))~default:(field "title"row)~f:(fun backend->backend.row_title row)] in
    let face=if not favorite && truth(field "favorite" row) then Some "camulus-reader-favorite"
      else if truth (call "assq"[sym "unread";row]) && not(truth(field "unread"row)) then Some "camulus-reader-read"
      else None in
    let columns=match face with Some face ->
      vector(List.map (values(call "append"[columns;nil])) ~f:(fun column ->
        let copy=call "copy-sequence"[column] in
        run "add-face-text-property"[i 0;call "length"[copy];sym face;b true;copy];copy))
      | None -> columns in
    list [key;columns]) in
  set "tabulated-list-entries" (list entries);run "tabulated-list-print" [b true];
  let positions=hash () in
  Point.goto_min ();
  while point () < max_point () do
    let id=call "tabulated-list-get-id" [] in
    put positions id (i (point ()));
    if truth (pending_marks ()) then (
      let mark=find (pending_marks ()) id in
      if truth mark then run "tabulated-list-put-tag" [field "action" mark]);
    run "forward-line" [i 1]
  done;
  let restore candidates =
    List.find_map candidates ~f:(fun id -> let p=find positions id in if truth p then Some p else None)
    |> Option.value ~default:(i (min_point ())) in
  List.iter windows ~f:(fun (window,point_anchor,start_anchor) ->
    run "set-window-point" [window;restore point_anchor];
    run "set-window-start" [window;restore start_anchor;b true]);
  goto (number (restore saved));run "move-to-column" [column]

let render_delete_masks () =
  List.iter (Buffer.all_live ()) ~f:(fun target->in_buffer target (fun ()->
    if derived "camulus-reader-feed-mode" then
      let rows=if flag "camulus-reader--all-rows" then get "camulus-reader--all-rows"
        else if flag "camulus-reader--rows" then list(hash_values(get "camulus-reader--rows")) else nil in
      set_rows rows))
let cancel_deletions () = Hashtbl.clear pending_deletes;render_delete_masks ()

let selected () =
  if derived "camulus-reader-article-mode" then (
    let id = property (point ()) "camulus-reader-id" in
    if not (truth id) then user_error "Select an article";
    alist ["backend_key",get "camulus-reader--backend-key";"id",id;"target",s "article"])
  else let row = if flag "camulus-reader--rows" then find (get "camulus-reader--rows") (call "tabulated-list-get-id" []) else nil in
    if not (truth row) then user_error "Select a row"; row

let open_original () =
  let row = selected () in
  let article =
    if derived "camulus-reader-article-mode" then
      find (get "camulus-reader--messages") (field "id" row)
    else row in
  let url = text "url" article in
  if String.is_prefix url ~prefix:"https://" || String.is_prefix url ~prefix:"http://" then
    run "browse-url" [s url]
  else user_error "This article has no web URL"

let reading_layout () =
  local "line-spacing" (call "string-to-number" [s "0.2"]);
  local "shr-fill-text" nil;local "truncate-lines" nil;
  run "visual-line-mode" [i 1];
  if truth (call "require" [sym "olivetti";nil;b true]) then run "olivetti-mode" [i 1]

let header_text value =
  str value |> String.map ~f:(function '\n' | '\r' | '\t' -> ' ' | c -> c)

let heading m depth read =
  let start = point () in
  let title = if String.is_empty (text "title" m) then get "camulus-reader--title" else field "title" m in
  let date = if Float.(timestamp (field "created" m) > 0.) then str (format_time "%Y-%m-%d %H:%M" (field "created" m)) else "" in
  let marks () =
    let staged=find (pending_marks ()) (article_mark_key (field "id" m)) in
    if truth staged then Point.insert ("  ["^text "action" staged^": "^text "destination" staged^"]");
    if truth (field "liked" m) then Point.insert "  ♥";
    if truth (field "like-uncertain" m) then Point.insert " (like unconfirmed)";
    Option.iter(capability(get "camulus-reader--backend-key"))~f:(fun backend->Point.insert(backend.article_marks m)) in
  let author () =
    insert_value (styled (if truth (field "read" read) then "○ " else "● ") "shadow");
    insert_value (styled (header_text (field "author" m)) "font-lock-keyword-face") in
  if depth = 0 then (
    insert_value (styled "From: " "shadow");author ();marks ();Point.insert "\n";
    if not (String.is_empty (str title)) then (
      insert_value (styled "Subject: " "shadow");insert_value (styled (header_text title) "camulus-reader-heading");Point.insert "\n");
    if not (String.is_empty date) then (insert_value (styled "Date: " "shadow");Point.insert (date^"\n")))
  else (author ();if not (String.is_empty date) then Point.insert (" · "^date);marks ();Point.insert "\n";
        run "add-face-text-property" [i start;i (point () - 1);sym "camulus-reader-heading";b true]);
  insert_value (propertize (s " ") [sym "display";list [sym "space";sym ":align-to";list [sym "-";sym "right";i 1]];
                                    sym "face";sym "camulus-reader-separator";sym "rear-nonsticky";b true]);
  Point.insert "\n"

let anchor pos =
  let id = property pos "camulus-reader-id" in
  if not (truth id) then nil else
    let start = call "previous-single-property-change" [i (pos+1);sym "camulus-reader-id"] in
    cons id (i (pos - if truth start then number start else min_point ()))
let position anchor =
  if not (truth anchor) then nil else
    let id=call "car" [anchor] and offset=number (call "cdr" [anchor]) in
    let rec loop p =
      if p >= max_point () then nil
      else if Value.equal id (property p "camulus-reader-id") then
        i (Int.min (p+offset) (next_property p "camulus-reader-id" (max_point ()) - 1))
      else loop (next_property p "camulus-reader-id" (max_point ())) in
    loop (min_point ())

let shr_code dom =
  let start=point () in
  let renderer=match str (call "symbol-name" [call "dom-tag" [dom]]) with "pre"->"shr-tag-pre" | "tt"->"shr-tag-tt" | _->"shr-tag-code" in
  run renderer [dom];run "add-face-text-property" [i start;i (point ());sym "fixed-pitch"]

let with_bindings vars f =
  let rec loop = function
    | [] -> f ()
    | (name,value)::rest -> Current_buffer.set_value_temporarily Sync (Var.create (Symbol.intern name) Value.Type.value) value ~f:(fun () -> loop rest)
  in loop vars

let header_action label activate =
  let map=Keymap.create () in
  let action=Function.to_value (Defun.lambda_nullary_nil [%here] ~interactive:No_arg (fun () ->
     let event=get "last-input-event" in
     Selected_window.set (Window.of_value_exn (call "posn-window" [call "event-start" [event]]));
     activate ())) in
  run "define-key" [Keymap.to_value map;vector [sym "header-line";sym "mouse-1"];action];
  propertize (s label) [sym "face";sym "link";sym "mouse-face";sym "highlight";
    sym "help-echo";s label;sym "local-map";Keymap.to_value map]

let header_link label command =
  header_action label (fun () -> run "call-interactively" [sym command])

let with_image_downloads f =
  let target=buffer () and generation=get "camulus-reader--render-token" in
  (* SHR's download markers collapse to the beginning when we redraw. Scope
     this adapter to our rendering, and let only the current render insert. *)
  let advice=Function.to_value (Defun.lambda [%here] (Returns Value.Type.value)
    (let open Defun.Let_syntax in
     let%map_open original=required "original" value and args=rest "arguments" value in
     let args=match args with
       | url::finished::rest when Value.eq finished (sym "shr-image-fetched") ->
         let guarded=Function.to_value (Defun.lambda [%here] (Returns Value.Type.value)
           (let open Defun.Let_syntax in
            let%map_open status=required "status" value and args=rest "arguments" value in
            if alive target && in_buffer target (fun () ->
              derived "camulus-reader-article-mode" && Value.eq generation (get "camulus-reader--render-token"))
            then call "apply" [finished;list (status::args)]
            else (
              let response=buffer () in
              Exn.protect ~f:(fun () ->
                if not (truth (call "plist-get" [status;sym ":error"])) then
                  run "url-store-in-cache" [bv response])
                ~finally:(fun () -> if alive response then run "kill-buffer" [bv response]);nil))) in
         url::guarded::rest
       | _ -> args in
     call "apply" [original;list args])) in
  run "advice-add" [sym "url-queue-retrieve";sym ":around";advice];
  Exn.protect ~f ~finally:(fun () -> run "advice-remove" [sym "url-queue-retrieve";advice])

let body m indent =
  if String.is_empty (text "body" m) && truth (field "html" m) then (
    let dom=Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
      Point.insert (text "html" m);call "libxml-parse-html-region" [i (min_point ());i (max_point ())]) in
    let renderers=list (List.map ["pre";"code";"tt"] ~f:(fun tag -> cons (sym tag) (sym "camulus-reader--shr-code"))) in
    let index=ref 0 in
    let video=callback_unit(fun dom->
      Int.incr index;
      let window=call "get-buffer-window" [bv(buffer ());b true] in
      let width=Int.max 120 (number(call "window-body-width" [window;b true])-32-indent*8) in
      Reader_media.render ~source:(str(get "camulus-reader--backend-key")) ~id:(text "id" m)
        ~base:(str(or_ (field "link_url" m) (field "url" m))) ~index:!index ~width dom) in
    with_bindings ["shr-inhibit-images",nil;"shr-use-fonts",nil;"shr-fill-text",nil;
      "shr-external-rendering-functions",cons (cons (sym "video") video) (call "append" [renderers;get "shr-external-rendering-functions"]);
      "shr-width",i (Int.max 20 (number (call "window-body-width" [call "get-buffer-window" [bv (buffer ());b true]])-indent-2));
      "shr-base",or_ (field "link_url" m) (field "url" m)]
      (fun () -> with_image_downloads (fun () -> run "shr-insert-document" [dom])))
  else Point.insert (Reader.Content.normalize_newlines (text "body" m))

let render () =
  let current_anchor = anchor (point ()) in
  let windows = values (call "get-buffer-window-list" [bv (buffer ());nil;b true])
    |> List.map ~f:(fun w -> w,anchor (number (call "window-start" [w]))) in
  let messages = hash_values (get "camulus-reader--messages") |> List.sort ~compare:(fun a c ->
    let order=Float.compare (timestamp (field "created" a)) (timestamp (field "created" c)) in
    if order=0 then String.compare (text "id" a) (text "id" c) else order) in
  let seen=Hash_set.create (module String) and children=Hashtbl.create (module String) in
  List.iter (List.rev messages) ~f:(fun m -> Hashtbl.add_multi children ~key:(text "parent_id" m) ~data:m);
  let first_unread=ref None in
  local "camulus-reader--render-token" (token ());
  Reader_media.begin_render ();
  writable (fun () ->
    Current_buffer.erase ();
    (* Explicit traversal preserves chronological siblings without recursion limits. *)
    let render_roots roots =
      let stack=ref (List.map roots ~f:(fun m -> m,0)) in
      while not (List.is_empty !stack) do
        let m,depth=List.hd_exn !stack in stack:=List.tl_exn !stack;
        let id=field "id" m and id_string=text "id" m in
        if not (Hash_set.mem seen id_string) then (
          Hash_set.add seen id_string;
          let read=state (get "camulus-reader--backend-key") id in
          let start=point () and indent=number (get "camulus-reader-reply-indent") * Int.min depth 12 in
          if truth read && not (truth (field "read" read)) && Option.is_none !first_unread then first_unread:=Some start;
          heading m depth read;
          let body_start=point () in
          if truth (find (get "camulus-reader--loaded") id) then body m indent else Point.insert "Loading article…";
          Point.insert "\n";
          if Option.value_map (capability (get "camulus-reader--backend-key")) ~default:true
              ~f:(fun backend -> backend.inline_attachments) then (
          List.iter (values (find (get "camulus-reader--attachments") id)) ~f:(fun a ->
            run "insert-text-button" [s (Printf.sprintf "[Save %s · %d bytes]" (text "name" a) (number (field "size" a)));
              sym "action";sym "camulus-reader--attachment";sym "follow-link";b true;sym "backend_key";get "camulus-reader--backend-key";
              sym "message-id";id;sym "part";field "part" a;sym "filename";field "name" a;sym "archived";
              b (flag "camulus-reader--archive" && not (truth (field "download_from_source" a)))];
            Point.insert "\n");
          );
          Point.insert "\n";
          let prefix n=styled (String.make n ' ') "fixed-pitch" in
          properties start (point ()) [sym "camulus-reader-id";id;sym "camulus-reader-depth";i depth;
            sym "line-prefix";prefix indent;sym "wrap-prefix";prefix indent];
          properties body_start (point ()) [sym "line-prefix";prefix (indent+2);sym "wrap-prefix";prefix (indent+2)];
          stack:=List.map (Hashtbl.find_multi children id_string) ~f:(fun m -> m,depth+1) @ !stack)
      done in
    render_roots (List.filter messages ~f:(fun m -> not (truth (find (get "camulus-reader--messages") (field "parent_id" m)))));
    render_roots messages);
  Reader_media.finish_render ();
  let controls=Option.value_map (capability(get "camulus-reader--backend-key")) ~default:[] ~f:(fun backend->
    List.concat_map backend.controls ~f:(fun control->[s "  ";header_link ("["^control.hint^"]") control.command])) in
  set "header-line-format" (list ([s "b: original  C: compose  r: refile  m: choose folder  u: unmark  x: execute  n: next unread"] @ controls @
    [s (Printf.sprintf " — %d articles — %s" (List.length messages) (if flag "camulus-reader--complete" then "loaded" else "loading / partial"))]));
  (match !first_unread with
   | Some p when not (flag "camulus-reader--positioned") -> goto p;set "camulus-reader--positioned" (b true);
     List.iter windows ~f:(fun (w,_) -> run "set-window-start" [w;i p;b true])
   | _ -> let p=position current_anchor in goto (if truth p then number p else min_point ());
     List.iter windows ~f:(fun (w,a) -> let p=position a in if truth p then run "set-window-start" [w;p;b true]));
  run "camulus-reader--schedule-visible" [bv (buffer ())]

let next_unread () =
  let rec loop p = if p >= max_point () then message "No further loaded unread article"
    else let read=state (get "camulus-reader--backend-key") (property p "camulus-reader-id") in
      if truth read && not (truth (field "read" read)) then goto p
      else loop (next_property p "camulus-reader-id" (max_point ())) in
  loop (next_property (point ()) "camulus-reader-id" (max_point ()))

let load_archive target source id kind =
  let generation=get "camulus-reader--view-token" in
  request (CS.Archive(str source,archive_target kind,str id)) ~receive:(callback_unit (fun data ->
    if String.equal (text "kind" data) "archive" && alive target then in_buffer target (fun () ->
      if Value.eq generation (get "camulus-reader--view-token") then (
        List.iter (values (field "articles" data)) ~f:(fun article ->
          let m=field "message" article in let id=field "id" m in
          put (get "camulus-reader--messages") id m;put (get "camulus-reader--loaded") id (b true);
          put (get "camulus-reader--attachments") id (field "attachments" article));
        set "camulus-reader--complete" (b true);
        let ids=values (call "hash-table-keys" [get "camulus-reader--messages"]) |> List.map ~f:str in
        request_ignore (CS.States(str source,ids))
          ~receive:(callback_unit (fun _ -> if alive target then in_buffer target (fun () ->
            if Value.eq generation (get "camulus-reader--view-token") then render ())))))))

let open_article () =
  let row=selected () in
  let source=field "backend_key" row and id=field "id" row in
  let saved=derived "camulus-reader-folder-mode" in
  let entry=if flag "camulus-reader--inbox" then inbox_entry row else get "camulus-reader--entry" in
  let backend=capability source in
  let target=Buffer.find_or_create ~name:"*Reader thread*" in
  in_buffer target (fun () ->
    if flag "camulus-reader--active" then run "camulus-probe-cancel" [get "camulus-reader--active"];
    run "camulus-reader-article-mode" [];
    Option.iter backend ~f:(fun backend ->
      let map=call "copy-keymap" [call "current-local-map" []] in
      List.iter backend.controls ~f:(fun control -> run "define-key" [map;call "kbd" [s control.key];sym control.command]);
      run "use-local-map" [map]);
    set "camulus-reader--backend-key" source;set "camulus-reader--title" (field "title" row);set "camulus-reader--archive" (b saved);
    set "camulus-reader--view-token" (token ());set "camulus-reader--entry" entry;
    writable (fun () -> Current_buffer.erase ();Point.insert "Loading…");
    let active=if saved then load_archive target source id (field "target" row)
      else
        let work=Option.bind backend ~f:(fun backend ->
          if not(backend.available(str source)) then None else
          Some(backend.read ~source:(str source) ~id:(str id)
            ~thread_id:(optional_string (field "thread_id" row))
            ~group:(optional_string (field "group" entry)))) in
        let threaded=Option.value_map backend ~default:(not(single source)) ~f:(fun backend->backend.threaded) in
        i(Runtime_ui.read_content ~source:(str source) ~id:(str id)
          ~target:(if threaded then CS.Thread else CS.Article)
          ~thread_contains_bodies:(Option.exists backend ~f:(fun backend->backend.thread_contains_bodies)) work) in
    set "camulus-reader--active" active;
    if not saved then put (get "camulus-reader--loads") active (bv target));
  let window=call "display-buffer-in-direction" [bv target;alist ["direction",sym "right";"window-width",call "string-to-number" [s "0.6"]]] in
  if truth window then Selected_window.set (Window.of_value_exn window)

let browse_entry entry =
  run "camulus" [entry];
  let target=buffer () and cached=get "camulus-reader--list-request" in
  put (get "camulus-reader--completions") cached (callback_unit (fun _ ->
    if alive target then in_buffer target (fun () ->
      if Value.eq cached (get "camulus-reader--list-request") then
        run "camulus-reader--load-list" [b true])))
let choose_group_category () =
  let entry=get "camulus-reader--entry" in
  let menu=Option.bind (capability (field "backend_key" entry)) ~f:(fun b -> b.categories entry)
    |> Option.value_exn ~message:"This group does not offer categories" in
  let window=call "selected-window" [] and origin=buffer () in
  let current () = Value.eq window (call "selected-window" []) && alive origin &&
    Value.eq (bv origin) (call "window-buffer" [window]) &&
    in_buffer origin (fun () -> Value.eq entry (get "camulus-reader--entry")) in
  let listing base name group =
    let rest=List.filter (values base) ~f:(fun pair ->
      not (List.exists ["group";"name";"category_root"] ~f:(fun k -> Value.eq (call "car" [pair]) (sym k)))) in
    call "append" [alist ["group",s group;"name",s name];list rest] in
  let root=if truth (field "category_root" entry) then field "category_root" entry
    else if String.equal (text "group" entry) menu.all_topics.listing then entry
    else listing entry menu.all_topics.name menu.all_topics.listing in
  menu.load (function
    | Error error -> message ("Categories: "^error)
    | Ok categories ->
      let choose () =
        if current () then (
          let names=List.map categories ~f:(fun (category:Reader_backend.category) -> category.name) in
          let duplicates=String.Set.of_list (List.find_all_dups ("All categories"::names) ~compare:String.compare) in
          let choices=List.map categories ~f:(fun (category:Reader_backend.category) ->
            let label=if Set.mem duplicates category.name then category.name^" ["^category.listing^"]"
                      else category.name in
            label,Some category) in
          let choices=("All categories",None)::choices in
          let choice=str (call "completing-read" [s "Category: ";list (List.map choices ~f:(fun (label,_) -> s label));nil;b true]) in
          if current () then Option.iter (List.Assoc.find choices choice ~equal:String.equal) ~f:(function
            | None -> browse_entry root
            | Some category ->
              let target=listing root (text "name" root^" › "^category.name) category.listing in
              browse_entry (cons (cons (sym "category_root") root) target)));
        nil in
      run "run-at-time" [i 0;nil;lambda0 choose])

let button label action = run "insert-text-button" [s label;sym "follow-link";b true;sym "action";callback_unit (fun _ -> action ())]
let home_buffer () = Buffer.find_or_create ~name:"*Camulus*"
let home_views : Reader_config.view list ref = ref []
let home_actions : Reader_config.home_action list ref = ref []
let read_home_config () =
  let actions,views=Reader_config.home_config () in
  home_actions:=actions;home_views:=views
let visible_home_views () = List.filter !home_views ~f:(fun v -> v.show_on_home)
let find_view name =
  List.find !home_views ~f:(fun view -> String.equal view.name name)
let feed_key entry = text "backend_key" entry,text "group" entry
let any_or_member values value ~equal = List.is_empty values || List.mem values value ~equal
let view_entries entries (view:Reader_config.inbox) =
  List.filter entries ~f:(fun entry ->
    any_or_member view.subscriptions (feed_key entry) ~equal:Poly.equal &&
    (List.is_empty view.backends ||
     Option.exists (capability (field "backend_key" entry)) ~f:(fun backend ->
       List.mem view.backends backend.id ~equal:String.equal)))
let inbox_selection () =
  match find_view (str (get "camulus-reader--inbox")) with
  | Some {destination=Reader_config.Inbox view;_} -> Some view
  | _ -> None
let timeline_header () =
  set "header-line-format" (list [header_link "[Home · h]" "camulus";s "  ";
    header_link "[Views · V]" "camulus-open-view";
    s ("   "^str (get "camulus-reader--inbox")^
       "   RET: read  b: original  C: compose  r: refile  m: move  d: mark delete  u: unmark  x: execute  g: refresh  n: more")]);
  run "force-mode-line-update" []
let home_progress () =
  let pos=get "camulus-reader-home--progress-position" in
  if truth pos then Current_buffer.save_excursion Sync (fun () -> writable (fun () ->
    goto (number pos);
    run "delete-region" [pos;call "line-end-position" []];
    let finished=number(get "camulus-reader-home--done") and total=number(get "camulus-reader-home--total") in
    let failures=List.length (values (get "camulus-reader-home--errors")) in
    Point.insert (Printf.sprintf "%d/%d checked" finished total);
    if failures > 0 then (
      Point.insert (Printf.sprintf " · %d failed  " failures);
      button "[e] Show errors" (fun () -> run "camulus-reader-home-errors" []));
    Current_buffer.set_modified false))
let home_current buffer token = alive buffer && in_buffer buffer (fun () ->
  derived "camulus-home-mode" && Value.eq token (get "camulus-reader-home--token"))
let home_cancel () =
  if not (String.equal (str (call "camulus-probe-status" [])) "idle") then
    List.iter (values (get "camulus-reader-home--jobs")) ~f:(fun id -> run "camulus-probe-cancel" [id]);
  set "camulus-reader-home--jobs" nil;set "camulus-reader-home--queue" nil;set "camulus-reader-home--token" (token ())
let cancel_list () =
  let id=get "camulus-reader--list-request" in
  if truth id && truth (find (get "camulus-reader--requests") id)
    && not (String.equal (str (call "camulus-probe-status" [])) "idle") then
    run "camulus-probe-cancel" [id];
  set "camulus-reader--list-request" nil;set "camulus-reader--list-token" (token ())
let cancel () =
  if (flag "camulus-reader--inbox" && not (flag "camulus-reader--subscription-entry")) || derived "camulus-home-mode" then
    in_buffer (home_buffer ()) (fun () -> home_cancel ();home_progress ());
  cancel_list ()
let all_inbox_items = {CS.unread_only=false;favorite_only=false;days=None}
let inbox_cache () =
  let target=buffer () and generation=token () in
  let available=in_buffer (home_buffer ()) (fun () -> values (get "camulus-reader-home--entries")) in
  let source=get "camulus-reader--subscription-entry" in
  let entries,filter=if truth source then [source],all_inbox_items else match inbox_selection () with
    | Some view -> view_entries available view,view.filter
    | None -> [],all_inbox_items in
  set "camulus-reader--inbox-entries" (list entries);
  cancel_list ();
  set "camulus-reader--list-token" generation;
  let receive=callback_unit (fun data ->
    if alive target && String.equal (text "kind" data) "feed" then in_buffer target (fun () ->
      if flag "camulus-reader--inbox" && Value.eq generation (get "camulus-reader--list-token") then
        set_rows (field "rows" data))) in
  let id=request ~receive (CS.Timeline (List.map entries ~f:feed_key,filter)) in
  set "camulus-reader--list-request" id
let home_count_queries () =
  let entries=values (get "camulus-reader-home--entries") in
  List.map (visible_home_views ()) ~f:(fun (view:Reader_config.view) ->
    view.name,(match view.destination with
      | Inbox inbox -> CS.Inbox_view(List.map (view_entries entries inbox) ~f:feed_key,inbox.filter)
      | Feed entry | Group entry -> let source,group=feed_key entry in CS.Group_view(source,group)
      | Folder path -> CS.Folder_view path))
let rec home_counts () =
  let target=home_buffer () in
  in_buffer target (fun () ->
    if derived "camulus-home-mode" && not(String.equal (str (call "camulus-probe-status" [])) "idle") then (
      let pending=get "camulus-reader-home--count-request" in
      if truth (find (get "camulus-reader--requests") pending) then
        set "camulus-reader-home--count-dirty" (b true)
      else (
        set "camulus-reader-home--count-dirty" nil;
        let views= !home_views in
        let receive=callback_unit (fun data ->
          if alive target && String.equal (text "kind" data) "view-counts" then in_buffer target (fun () ->
            if derived "camulus-home-mode" && phys_equal views !home_views then (
              let counts=hash () in
              List.iter (values (field "rows" data)) ~f:(fun row -> put counts (field "name" row) row);
              set "camulus-reader-home--counts" counts;
              run "camulus-reader--render-home" []))) in
        let finished=callback_unit (fun _ ->
          if alive target then in_buffer target (fun () ->
            if flag "camulus-reader-home--count-dirty" then home_counts ())) in
        set "camulus-reader-home--count-request" (request ~receive ~finished (CS.View_counts (home_count_queries ()))))))
let redraw_inboxes () =
  List.iter (Buffer.all_live ()) ~f:(fun target -> in_buffer target (fun () ->
    if flag "camulus-reader--inbox" then inbox_cache ()))
let push_var name value = set name (cons value (get name))
let home_error name error = push_var "camulus-reader-home--errors" (cons name error)
let home_done () = set "camulus-reader-home--done" (i (number (get "camulus-reader-home--done")+1))
let rec home_pump more =
  let target=buffer () and generation=get "camulus-reader-home--token" in
  while flag "camulus-reader-home--queue" && List.length (values (get "camulus-reader-home--jobs")) < 4 do
    let entry=call "car" [get "camulus-reader-home--queue"] in
    set "camulus-reader-home--queue" (call "cdr" [get "camulus-reader-home--queue"]);
    let key=entry_key entry in
    try
      let receive=callback_unit (fun data ->
        if home_current target generation && String.equal (text "kind" data) "refreshed" then in_buffer target (fun () ->
          put (get "camulus-reader-home--cursors") key (field "after" data);redraw_inboxes ())) in
      let finished=callback_unit (fun event ->
        if home_current target generation then in_buffer target (fun () ->
          set "camulus-reader-home--jobs" (call "delq" [call "nth" [i 1;event];get "camulus-reader-home--jobs"]);home_done ();
          if not (Value.eq (call "nth" [i 2;event]) (sym "completed")) then
            home_error (field "name" entry) (or_ (call "nth" [i 3;event]) (s "Refresh failed"));
          home_pump more)) in
      let after=if more then optional_string (find (get "camulus-reader-home--cursors") key) else None in
      let source,group=feed_key entry in
      let id=request ~receive ~finished (CS.Refresh(`Cursor,source,group,after)) in
      push_var "camulus-reader-home--jobs" id
    with exn -> home_done ();home_error (field "name" entry) (s (Exn.to_string exn))
  done;
  home_progress ();
  if not (flag "camulus-reader-home--jobs") then home_counts ()
let home_merge subscribed_feeds =
  let configured=Reader_backend.configured_groups () in
  let seen=Hash_set.Poly.create () in
  List.filter_map subscribed_feeds ~f:(fun entry ->
    let key=feed_key entry in
    if Hash_set.mem seen key then None
    else (
      Hash_set.add seen key;
      let registered=List.find configured ~f:(fun e -> Poly.equal (feed_key e) key) in
      let base=Option.value registered ~default:entry in
      let label=or_ (field "name" entry) (field "name" base) in
      let label=if truth label then label else
        s (str (backend_label (field "backend_key" entry))^" · "^text "group" entry) in
      Some (if Value.equal label (field "name" base) then base
            else cons (cons (sym "name") label) base)))
let discover_subscribed_feeds ~on_error =
  List.concat_map (Reader_backend.all ()) ~f:(fun backend ->
    match backend.subscribed_feeds with
    | None -> []
    | Some subscribed_feeds -> try subscribed_feeds () with exn -> on_error backend exn)
let home_refresh ?(restart=false) () =
  let target=home_buffer () in
  if restart then read_home_config ();
  in_buffer target (fun () ->
    let running=flag "camulus-reader-home--jobs" in
    if restart || not running then (
      home_cancel ();set "camulus-reader-home--done" (i 0);set "camulus-reader-home--errors" nil;
      let previous=values (get "camulus-reader-home--entries") in
      let entries=discover_subscribed_feeds ~on_error:(fun backend exn ->
        home_error (s (backend.name^" feeds")) (s (Exn.to_string exn));
        List.filter previous ~f:(fun entry -> Reader_backend.owns_key backend (text "backend_key" entry)))
        |> home_merge in
      set "camulus-reader-home--ready" (b true);
      set "camulus-reader-home--entries" (list entries);set "camulus-reader-home--queue" (list entries);
      set "camulus-reader-home--cursors" (hash ());
      set "camulus-reader-home--total" (i (List.length entries));
      redraw_inboxes ();home_pump false))
let home_more () =
  let entries=values (get "camulus-reader--inbox-entries") in
  in_buffer (home_buffer ()) (fun () ->
    if flag "camulus-reader-home--jobs" then user_error "Wait for the current subscription refresh";
    let queue=List.filter entries ~f:(fun entry -> truth (find (get "camulus-reader-home--cursors") (entry_key entry))) in
    if List.is_empty queue then user_error "No further pages in this view";
    set "camulus-reader-home--done" (i 0);set "camulus-reader-home--errors" nil;
    set "camulus-reader-home--queue" (list queue);set "camulus-reader-home--total" (i (List.length queue));
    home_pump true)
let columns fields = vector (List.map fields ~f:(fun (name,width,sort) -> list [s name;i width;b sort]))
let home_setup () =
  let target=home_buffer () in
  in_buffer target (fun () ->
    if not (derived "camulus-home-mode") then run "camulus-home-mode" []);
  target
let list_controls map (backend:Reader_backend.t) =
  List.iter backend.list_controls ~f:(fun control -> Reader_backend.bind map control.key control.command);
  String.concat (List.map backend.list_controls ~f:(fun c -> "  "^c.hint))
let show_inbox ~name ~title ~subscription =
  let target=show_buffer name in
  in_buffer target (fun () ->
    if not (Value.equal (get "camulus-reader--inbox") (s title)) then (
      run "camulus-reader-feed-mode" [];
      set "camulus-reader--inbox" (s title);
      set "tabulated-list-format" (columns ["New",3,false;"Time",12,false;"Feed",28,true;"Author",18,true;"Thread",70,true]);
      run "tabulated-list-init-header" []);
    if not (Value.equal (entry_key subscription) (entry_key (get "camulus-reader--subscription-entry"))) then
      set "camulus-reader--cursor" nil;
    set "camulus-reader--subscription-entry" subscription;
    set "camulus-reader--backend-key" (field "backend_key" subscription);
    timeline_header ();
    let map=call "copy-keymap" [get "camulus-reader-feed-mode-map"] in
    Option.iter (capability (field "backend_key" subscription)) ~f:(fun backend ->
      let hints=list_controls map backend in
      if not (String.is_empty hints) then set "header-line-format" (list [get "header-line-format";s hints]));
    run "use-local-map" [map];
    inbox_cache ())
let open_inbox (view:Reader_config.view) =
  show_inbox ~name:("*Camulus view: "^view.name^"*") ~title:view.name ~subscription:nil

let reader entry =
  let entry=if truth entry then Reader_config.entry entry else nil in
  run "camulus-reader-start" [];
  if not (truth entry) then run "camulus-reader-home-open" [] else (
    ignore (show_buffer "*Reader*" : Buffer.t);
    run "camulus-reader-feed-mode" [];
    set "camulus-reader--entry" entry;set "camulus-reader--backend-key" (field "backend_key" entry);
    Option.iter (capability (field "backend_key" entry)) ~f:(fun backend ->
      let map=call "copy-keymap" [call "current-local-map" []] in
      run "define-key" [map;call "kbd" [s "S"];if can_subscribe_entry entry then sym "camulus-reader-group-subscription" else nil];
      let categories=Option.is_some (backend.categories entry) in
      if categories then Reader_backend.bind map "c" "camulus-group-categories";
      let controls=list_controls map backend in
      run "use-local-map" [map];
      let hints=(if can_subscribe_entry entry then "  S: group subscription settings" else "")^
        controls in
      if categories then set "header-line-format"
        (list [s (text "name" entry^"  c: categories  ");get "header-line-format";s hints])
      else if not(String.is_empty hints)then set "header-line-format" (list[get "header-line-format";s hints]));
    run "camulus-reader--load-list" []);
  let backends=if truth entry then Option.to_list (capability (field "backend_key" entry)) else Reader_backend.all () in
  List.iter backends ~f:(fun backend -> backend.on_refresh ())
(* Directory rows keep their identity across folding, refresh, and asynchronous counts. *)
let directory_folded = "camulus-reader-directory--folded"
let directory_section = "camulus-directory-section"
let directory_row = "camulus-directory-row"

let rec directory_render () =
  let subscriptions=derived "camulus-subscriptions-mode" in
  let selected=property (point ()) directory_row and section=property (point ()) directory_section in
  let selected_pos=ref None and section_pos=ref None in
  let entries=if subscriptions then values (get "camulus-reader-subscriptions--entries")
    else Reader_backend.registered_group_entries () |> List.map ~f:(fun entry ->
      try Reader_backend.resolve_group entry with Invalid_argument _ -> entry) in
  let counts=get "camulus-reader-subscriptions--counts" in
  let owner entry=match capability (field "backend_key" entry) with
    | Some backend -> backend.id
    | None -> text "backend" entry in
  let ids=List.map entries ~f:owner |> List.dedup_and_sort ~compare:String.compare in
  let row_map=call "make-sparse-keymap" [] in
  run "set-keymap-parent" [row_map;get "button-map"];
  List.iter ["TAB";"<tab>"] ~f:(fun key -> Reader_backend.bind row_map key "camulus-directory-toggle");
  let row ~id ~key ?(entry=nil) ~face label action =
    let start=point () in
    run "insert-text-button" [s label;sym "keymap";row_map;sym "face";sym face;sym "follow-link";b true;
      sym "action";callback_unit (fun _ -> action ())];
    Point.insert "\n";
    properties start (point ()) [sym directory_section;s id;sym directory_row;key;sym "camulus-reader-entry";entry];
    if Value.equal selected key then selected_pos:=Some start in
  writable (fun () ->
    Current_buffer.erase ();
    List.iter ids ~f:(fun id ->
      let backend=Reader_backend.find_id id in
      let label=Option.value_map backend ~default:"Unconfigured backend" ~f:(fun b -> b.name) in
      let folded=member (s id) (values (get directory_folded)) in
      if Value.equal section (s id) then section_pos:=Some (point ());
      row ~id ~key:(s id) ~face:"camulus-reader-heading" ((if folded then "[+] " else "[-] ")^label)
        (fun () -> directory_toggle_section (s id));
      if not folded then (
        List.filter entries ~f:(fun entry -> String.equal id (owner entry))
        |> List.iter ~f:(fun entry ->
          let unread=if truth counts then find counts (entry_key entry) else nil in
          let prefix=if subscriptions then Printf.sprintf "  %5s  " (if truth unread then Int.to_string (number unread) else "·") else "    " in
          let name=text "name" entry in
          row ~id ~entry ~key:(list [s id;entry_key entry;s name]) ~face:"default" (prefix^name) (fun () ->
            if subscriptions then show_inbox ~name:"*Camulus feed inbox*" ~title:name ~subscription:entry
            else browse_entry entry));
        if not subscriptions then Option.iter backend ~f:(fun backend ->
          List.iter (backend.entrances ()) ~f:(fun entrance ->
            row ~id ~key:(list [s id;s entrance.label]) ~face:"default" ("    "^entrance.label) entrance.open_))));
    if List.is_empty entries then Point.insert (if subscriptions then "No subscribed feeds.\n" else "No registered groups.\n");
    if subscriptions then List.iter (values (get "camulus-reader-subscriptions--errors")) ~f:(fun error ->
      Point.insert (str(call "car" [error])^": "^str(call "cdr" [error])^"\n"));
    goto (Option.value !selected_pos ~default:(Option.value !section_pos ~default:(min_point ()))));
  Current_buffer.set_modified false
and directory_toggle_section id =
  if truth id then (
    let folded=get directory_folded in
    set directory_folded (if member id (values folded) then call "remove" [id;folded] else cons id folded);
    directory_render ())
let directory_toggle () = directory_toggle_section (property (point ()) directory_section)
let directory_open () =
  let start=call "line-beginning-position" [] in
  if truth (call "button-at" [start]) then run "push-button" [start]
let directory_browse () =
  let choices=List.filter_map (Reader_backend.all ()) ~f:(fun backend -> backend.browser) in
  let backend=Reader_backend.find_id (str (property (point ()) directory_section)) in
  let choice=call "completing-read" [s "Browse with backend: ";list (List.map choices ~f:(fun (name,_) -> s name));
    nil;b true;nil;nil;Option.value_map (Option.bind backend ~f:(fun b -> b.browser)) ~default:nil ~f:(fun (name,_) -> s name)] in
  Option.iter (List.Assoc.find choices ~equal:String.equal (str choice)) ~f:(fun command ->
    run "call-interactively" [sym command])
let groups () =
  ignore (show_buffer "*Reader groups*" : Buffer.t);
  if not (derived "camulus-reader-groups-mode") then run "camulus-reader-groups-mode" [];
  directory_render ()
let subscriptions_refresh () =
  run "camulus-reader-start" [];
  set "camulus-reader-subscriptions--errors" nil;
  let entries=discover_subscribed_feeds ~on_error:(fun backend exn ->
    push_var "camulus-reader-subscriptions--errors" (cons (s backend.name) (s (Exn.to_string exn)));
    []) |> home_merge |> List.stable_sort ~compare:(fun a b -> String.compare (text "name" a) (text "name" b)) in
  let snapshot=list entries and target=buffer () in
  set "camulus-reader-subscriptions--entries" snapshot;
  set "camulus-reader-subscriptions--counts" nil;
  directory_render ();
  if not (List.is_empty entries) then (
    let queries=List.mapi entries ~f:(fun idx entry ->
      Int.to_string idx,CS.Inbox_view ([feed_key entry],all_inbox_items)) in
    let receive=callback_unit (fun data ->
      if alive target && String.equal (text "kind" data) "view-counts" then in_buffer target (fun () ->
        if Value.eq snapshot (get "camulus-reader-subscriptions--entries") then (
          let counts=hash () in
          List.iter2_exn entries (values (field "rows" data)) ~f:(fun entry row ->
            put counts (entry_key entry) (field "unread" row));
          set "camulus-reader-subscriptions--counts" counts;
          directory_render ()))) in
    request_ignore ~receive (CS.View_counts queries))
let subscriptions () =
  ignore(show_buffer "*Camulus subscribed feeds*" : Buffer.t);
  if not (derived "camulus-subscriptions-mode") then run "camulus-subscriptions-mode" [];
  subscriptions_refresh ()
let browse_source () =
  if not (flag "camulus-reader--inbox") then user_error "Open a subscription view first";
  let entry=inbox_entry (selected ()) in
  if not (truth entry) then user_error "No feed available for this item";
  reader entry
let home_errors () =
  let errors=in_buffer (home_buffer ()) (fun () -> values (get "camulus-reader-home--errors")) in
  if List.is_empty errors then message "No feed refresh errors" else (
    let target=Buffer.find_or_create ~name:"*Reader refresh errors*" in
    in_buffer target (fun () -> writable (fun () ->
      Current_buffer.erase ();List.iter (List.rev errors) ~f:(fun error ->
        Point.insert (str (call "car" [error])^": "^str (call "cdr" [error])^"\n\n")));
      run "special-mode" []);
    run "display-buffer" [bv target])


let load_list synchronize more =
  let subscription=get "camulus-reader--subscription-entry" in
  if truth subscription && synchronize then (
    let target=buffer () and generation=token () in
    let source,group=feed_key subscription in
    let after=if more then optional_string (get "camulus-reader--cursor") else None in
    if more && Option.is_none after then user_error "No further pages in this feed";
    cancel_list ();
    set "camulus-reader--list-token" generation;
    let receive=callback_unit (fun data ->
      if alive target && String.equal (text "kind" data) "refreshed" then in_buffer target (fun () ->
        if Value.eq generation (get "camulus-reader--list-token") then (
          set "camulus-reader--cursor" (field "after" data);inbox_cache ();home_counts ()))) in
    set "camulus-reader--list-request" (request ~receive (CS.Refresh(`Cursor,source,group,after))))
  else if flag "camulus-reader--inbox" then (if more then home_more () else if synchronize then home_refresh ~restart:true () else inbox_cache ()) else (
    let target=buffer () and folder=get "camulus-reader--folder" and entry=get "camulus-reader--entry" and generation=token () in
    let operation=if truth folder then Some (CS.Favorites(str folder))
      else if truth entry then
        let source=text "backend_key" entry and group=text "group" entry in
        Some (if synchronize then CS.Refresh(`Group_rows,source,group,
          if more then optional_string (get "camulus-reader--cursor") else None)
          else CS.Feed(source,group))
      else None in
    Option.iter operation ~f:(fun operation ->
      cancel_list ();
      set "camulus-reader--list-token" generation;
      let receive=callback_unit (fun data ->
        if alive target && List.mem ["feed";"favorites"] (text "kind" data) ~equal:String.equal then in_buffer target (fun () ->
          if Value.eq generation (get "camulus-reader--list-token") then (
            if synchronize then set "camulus-reader--cursor" (field "after" data);
            let rows=field "rows" data in
            set_rows rows;
            if synchronize && truth entry then (
              let subscribed=in_buffer (home_buffer ()) (fun () ->
                let subscribed=List.exists (values (get "camulus-reader-home--entries")) ~f:(fun candidate ->
                  Value.equal (entry_key candidate) (entry_key entry)) in
                if subscribed then put (get "camulus-reader-home--cursors") (entry_key entry) (field "after" data);
                subscribed) in
              if subscribed then redraw_inboxes ();home_counts ())))) in
      set "camulus-reader--list-request" (request ~receive operation)))
let refresh_source source =
  in_buffer (home_buffer ()) (fun () ->
    if derived "camulus-home-mode" then
      let target=buffer () in
      List.iter (values(get "camulus-reader-home--entries")) ~f:(fun entry ->
        if Value.equal source (field "backend_key" entry) then
          request_ignore (CS.Refresh(`Cursor,str source,text "group" entry,None))
            ~receive:(callback_unit(fun data ->
              if alive target && String.equal (text "kind" data) "refreshed" then in_buffer target(fun () ->
                if derived "camulus-home-mode" then (
                  put (get "camulus-reader-home--cursors") (entry_key entry) (field "after" data);
                  redraw_inboxes ();home_counts ()))))));
  List.iter (Buffer.all_live ()) ~f:(fun target -> in_buffer target (fun () ->
    if derived "camulus-reader-feed-mode" && Value.equal source (field "backend_key" (get "camulus-reader--entry")) then
      load_list true false))
let reload_lists () =
  List.iter (Buffer.all_live ()) ~f:(fun target -> in_buffer target (fun () ->
    if flag "camulus-reader--inbox"
       || ((derived "camulus-reader-feed-mode" || derived "camulus-reader-folder-mode")
            && not (flag "camulus-reader--list-request" && truth (find (get "camulus-reader--requests") (get "camulus-reader--list-request")))) then
      load_list false false));
  home_counts ()
let refresh_archives () =
  request_ignore CS.Refresh_archives ~receive:(callback_unit (fun data ->
    if truth (field "errors" data) then message ("Archive refresh: "^String.concat ~sep:"; " (List.map (values (field "errors" data)) ~f:str))))
let refresh () =
  load_list true false;
  let backends=if flag "camulus-reader--subscription-entry" then
      Option.to_list (capability (field "backend_key" (get "camulus-reader--subscription-entry")))
    else if flag "camulus-reader--inbox" then Reader_backend.all ()
    else Option.to_list (capability(field "backend_key" (get "camulus-reader--entry"))) in
  List.iter backends ~f:(fun backend -> backend.on_refresh ());
  refresh_archives ()
let more () =
  if not (flag "camulus-reader--inbox" || flag "camulus-reader--cursor") then user_error "No further page";
  if truth (find (get "camulus-reader--requests") (get "camulus-reader--list-request")) then
    user_error "Wait for the current load or cancel it with C-c C-k";
  load_list true true
let entries () =
  run "camulus-reader-start" [];
  let choices=Reader_backend.registered_group_entries () in
  if List.is_empty choices then user_error "No registered groups";
  let chosen=call "completing-read" [s "Group: ";list (List.map choices ~f:(field "name"));nil;b true] in
  Option.iter (List.find choices ~f:(fun entry -> Value.equal (field "name" entry) chosen)) ~f:reader
let delete row =
  if derived "camulus-reader-feed-mode" then (
    let home=flag "camulus-reader--inbox" in
    match Option.bind (capability (field "backend_key" row)) ~f:(fun backend -> backend.delete) with
    | Some delete ->
      let entry=if home then inbox_entry row else get "camulus-reader--entry" in
      let source=text "backend_key" row and group=text "group" entry and identity=row_identity row in
      let key=String.concat ~sep:"\000" [source;group;identity] in
      if Hashtbl.mem pending_deletes key then message "This item is already being removed" else (
        let token=ref () in
        Hashtbl.set pending_deletes ~key ~data:{source;group;identity;token};
        let active ()=Option.exists(Hashtbl.find pending_deletes key) ~f:(fun p->phys_equal p.token token) in
        let rollback reason=if active () then (
          Hashtbl.remove pending_deletes key;render_delete_masks ();
          message ("Removal did not complete; item restored: "^reason)) in
        let complete = function
          | Error reason->rollback reason
          | Ok removal->if active () then (
            (* Keep the mask until the new membership or dismissal is stored. Then force
               new view tokens, so older in-flight reads cannot resurrect it. *)
            let finished=callback_unit(fun event->if active () then (
              if Value.eq(call "nth"[i 2;event])(sym "completed") then (
                Hashtbl.remove pending_deletes key;
                List.iter(Buffer.all_live ()) ~f:(fun target->in_buffer target (fun ()->
                  let relevant entry=String.equal source (text "backend_key" entry) && String.equal group (text "group" entry) in
                  if derived "camulus-reader-feed-mode" &&
                    (if flag "camulus-reader--inbox" then List.exists(values(get "camulus-reader--inbox-entries")) ~f:relevant
                     else relevant(get "camulus-reader--entry")) then load_list false false)))
              else rollback "backend action finished, but updating the local list failed")) in
            let operation=match removal with
              | Reader_backend.Refresh_membership -> CS.Refresh(`Cursor,source,group,None)
              | Delete_inbox_article ->
                let id=text "id" row in
                if home then CS.Delete_rss_article(source,id)
                else CS.Dismiss(source,group,id,number(field "revision" row))
              | Reader_backend.Dismiss_notification ->
                CS.Dismiss_notification(source,group,text "id" row,number(field "revision" row)) in
            try ignore(request ~finished operation:Value.t)
            with ex->rollback(Exn.to_string ex)) in
        try render_delete_masks ();delete ~entry ~row ~complete
        with ex->rollback(Exn.to_string ex);raise ex)
    | None ->
      let source=text "backend_key" row and id=text "id" row and revision=number (field "revision" row) in
      request_ignore (if home then CS.Dismiss_timeline(source,id,revision)
        else CS.Dismiss(source,text "group" (get "camulus-reader--entry"),id,revision)))
  else if derived "camulus-reader-folder-mode" then
    request_ignore (CS.Unfavorite(text "backend_key" row,archive_target (field "target" row),text "id" row,str (get "camulus-reader--folder")))
  else user_error "Use d in the feed or a favorites folder"
let save_to row folder =
  request_ignore (CS.Favorite(text "backend_key" row,archive_target (or_ (field "target" row) (s "thread")),text "id" row,str folder))
let mark_key row =
  if derived "camulus-reader-article-mode" then article_mark_key (field "id" row)
  else call "tabulated-list-get-id" []
let redraw_marks () =
  if derived "camulus-reader-article-mode" then render ()
  else set_rows (get "camulus-reader--all-rows")
let apply_mark key tag ~advance =
  if derived "camulus-reader-article-mode" then render ()
  else if Value.equal key (call "tabulated-list-get-id" []) then (
    run "tabulated-list-put-tag" [tag];
    if advance && number (call "line-beginning-position" [i 2]) < max_point () then run "forward-line" [i 1])
  else redraw_marks ()
let stage key row ~action ~destination apply =
  let action=s action in
  put (pending_marks ()) key (alist ["key",key;"row",row;"action",action;
     "destination",s destination;"apply",callback0 apply]);
  apply_mark key action ~advance:true
let mark_delete () =
  let row=selected () in
  stage (mark_key row) row ~action:"D" ~destination:"" (fun () -> delete row)
let unmark () =
  let row=selected () in
  let key=mark_key row in
  remove (pending_marks ()) key;
  apply_mark key (s "") ~advance:false
let execute_marks () =
  let table=pending_marks () in
  let staged=hash_values table in
  if List.is_empty staged then user_error "No marked items";
  let article=derived "camulus-reader-article-mode" in
  let missing=ref 0 and errors=ref [] in
  List.iter staged ~f:(fun mark ->
    let key=field "key" mark and row=field "row" mark in
    let present=if article then truth(find (get "camulus-reader--messages") (field "id" row))
      else truth(find (get "camulus-reader--rows") key) in
    if not present then (Int.incr missing;remove table key)
    else try
      ignore (Value.funcallN (field "apply" mark) [] : Value.t);
      remove table key
    with exn -> errors:=Exn.to_string exn :: !errors);
  redraw_marks ();
  message (Printf.sprintf "Submitted %d marked items%s%s" (List.length staged - !missing - List.length !errors)
    (if !missing=0 then "" else Printf.sprintf " · %d unavailable" !missing)
    (if List.is_empty !errors then "" else " · "^String.concat ~sep:"; " (List.rev !errors)))
let refile_item row =
  let key=field "backend_key" row in
  let article=if derived "camulus-reader-article-mode" then
      find (get "camulus-reader--messages") (field "id" row) else row in
  let entry=if flag "camulus-reader--inbox" then inbox_entry row else get "camulus-reader--entry" in
  let groups=field "groups" row in
  list [sym ":id";field "id" row;
        sym ":backend";Option.value_map (capability key) ~default:nil ~f:(fun backend -> s backend.id);
        sym ":backend-key";key;
        sym ":group";field "group" entry;
        sym ":groups";if truth groups then groups else List.filter [field "group" entry] ~f:truth |> list;
        sym ":kind";sym (if String.equal (text "target" row) "article" then "article" else "thread");
        sym ":title";field "title" article;
        sym ":author";field "author" article;
        sym ":url";field "url" article]
let export_refile row item path format =
  let source=text "backend_key" row and id=text "id" row in
  let target=archive_target (or_ (field "target" row) (s "thread")) in
  let render =
    if Value.eq format (sym "html") then (fun messages ->
      let module Html=Reader.Html in
      let article m =
        let body =
          if truth (field "html" m) then
            Html.parse (text "html" m)
            |> Html.clean ~base:(text "url" m) ~drop:(fun n -> List.mem ["meta";"base";"link"] (Html.tag n) ~equal:String.equal)
            |> Option.value_map ~default:"" ~f:(fun tree ->
              let body=Option.value (Html.find (fun n -> String.equal (Html.tag n) "body") tree) ~default:tree in
              String.concat (List.map (Html.children body) ~f:Html.render))
          else "<pre>"^Html.escape (text "body" m)^"</pre>" in
        "<article><h2>"^Html.escape (text "title" m)^"</h2><p>"^Html.escape (text "author" m)^"</p>"^body^"</article>" in
      "<!doctype html>\n<html><head><meta charset=\"utf-8\"><title>"^
        Html.escape (str (call "plist-get" [item;sym ":title"]))^"</title></head><body>"^
        String.concat ~sep:"\n" (List.map messages ~f:article)^"</body></html>\n")
    else if not (truth format) || Value.eq format (sym "text") then (fun messages ->
      String.concat ~sep:"\n\n" (List.map messages ~f:(fun m ->
        String.concat ~sep:"\n" (List.filter_map ["title";"author";"url"] ~f:(fun key ->
          let value=text key m in if String.is_empty value then None else Some value))^"\n\n"^text "body" m))^"\n")
    else if truth (call "functionp" [format]) then (fun messages ->
      let output=Value.funcallN format [item;list messages] in
      if not (truth (call "stringp" [output])) then invalid_arg "Refile formatter must return a string";
      str output)
    else invalid_arg "Refile format must be text, html, or a formatter function" in
  fun () -> request_ignore (CS.Export_content(source,target,id))
    ~receive:(callback_unit(fun data ->
      if String.equal (text "kind" data) "export-content" then
        try
          let messages=List.map (values (field "articles" data)) ~f:(field "message") in
          request_ignore (CS.Write_export(path,render messages))
        with exn -> message ("Refile export: "^Exn.to_string exn)))
let refile () =
  let row=selected () in
  let item=refile_item row and key=mark_key row in
  let rule=get "camulus-refile-rule" in
  let choice=if truth (call "functionp" [rule]) then Value.funcallN rule [item] else rule in
  let destination,apply =
    if not (truth choice) then
      Option.bind (capability (field "backend_key" row)) ~f:(fun backend -> backend.refile ~item)
      |> Option.value_or_thunk ~default:(fun () ->
        user_error "No refile rule matched. Configure camulus-refile-rule or use m to choose a folder";
        assert false)
    else if truth (call "stringp" [choice]) && not (String.is_empty (str choice)) then
      str choice,(fun () -> save_to row choice)
    else if truth (call "consp" [choice]) then (
      let path=call "plist-get" [choice;sym ":file"] and format=call "plist-get" [choice;sym ":format"] in
      if not (truth (call "stringp" [path])) || String.is_empty (str path) then
        user_error "A refile export requires a nonempty :file path";
      let path=call "expand-file-name" [path] in
      if truth (call "file-remote-p" [path]) then user_error "Refile export requires a local file";
      str path,export_refile row item (str path) format)
    else (user_error "A refile rule must return a favorite folder, an export plist, or nil";assert false) in
  stage key row ~action:"r" ~destination apply;
  message ("Marked refile to "^destination)
let mark_move () =
  let row=call "copy-tree" [selected ()] in
  let key=mark_key row and target=buffer () and table=pending_marks () in
  let item=refile_item row in
  let backend=capability (field "backend_key" row) in
  let choose favorites backend_folders =
    let favorites=List.map (values favorites) ~f:(fun folder -> s ("Favorites: "^str folder)) in
    let choices=favorites @ List.map backend_folders ~f:(fun (label,_) -> s label) in
    run "run-at-time" [i 0;nil;callback0 (fun () ->
      if alive target && Value.eq (bv target) (call "window-buffer" [call "selected-window" []]) then
        in_buffer target (fun () ->
          if Value.eq table (pending_marks ()) then (
            let choice=str (call "completing-read" [s "Move to folder: ";list choices;nil;nil]) in
            if not (String.is_empty choice) && Value.eq table (pending_marks ()) then (
              let destination,apply=match List.Assoc.find backend_folders ~equal:String.equal choice with
                | Some apply -> choice,apply
                | None ->
                  let folder=String.chop_prefix ~prefix:"Favorites: " choice |> Option.value ~default:choice in
                  folder,(fun () -> save_to row (s folder)) in
              stage key row ~action:"m" ~destination apply;
              message ("Marked move to "^destination)))))] in
  request_ignore CS.Folders ~receive:(callback_unit (fun data ->
    if String.equal (text "kind" data) "folders" then
      let favorites=field "rows" data in
      match backend with
      | Some {name;folder_move=Some folders;_} ->
        folders.list ~item ~receive:(fun paths ->
          choose favorites (List.map paths ~f:(fun folder -> name^": "^folder,(fun () -> folders.move ~item ~folder))))
      | _ -> choose favorites []))
let open_folder folder =
  ignore(show_buffer ("*Reader folder: "^str folder^"*") : Buffer.t);
  run "camulus-reader-folder-mode" [];set "camulus-reader--folder" folder;load_list false false
let folders_load () =
  let target=buffer () and generation=token () in
  set "camulus-reader--list-token" generation;
  writable(fun () -> Current_buffer.erase ();Point.insert "Loading favorite folders…\n");
  request_ignore CS.Folders ~receive:(callback_unit (fun data ->
    if String.equal(text "kind" data)"folders" && alive target then in_buffer target (fun () ->
      if derived "camulus-reader-folders-mode" && Value.eq generation(get "camulus-reader--list-token") then (
        writable(fun () ->
          Current_buffer.erase ();Point.insert "Folders\n\n";
          let folders=values(field "rows" data) in
          List.iter folders ~f:(fun folder ->
            let parts=String.split (str folder) ~on:'/' in
            Point.insert (String.make (2*(List.length parts-1)) ' ');
            button (List.last_exn parts) (fun () -> open_folder folder);Point.insert "\n");
          if List.is_empty folders then Point.insert "No favorites yet. Press m to choose a folder, then x to file it.\n";
          Point.goto_min ();run "forward-line"[i 2]);
        Current_buffer.set_modified false))))
let folders () =
  run "camulus-reader-start" [];
  if derived "camulus-reader-article-mode" then (
    let windows=values(call "window-list"[]) in
    Option.iter (List.find windows ~f:(fun window ->
      let target=Buffer.of_value_exn(call "window-buffer"[window]) in
      in_buffer target(fun () -> derived "camulus-reader-feed-mode" || derived "camulus-reader-folder-mode" ||
        derived "camulus-reader-groups-mode" || derived "camulus-reader-folders-mode")))
      ~f:(fun window->Selected_window.set(Window.of_value_exn window)));
  ignore(show_buffer "*Camulus folders*" : Buffer.t);
  run "camulus-reader-folders-mode" [];folders_load ()
let delete_folder () =
  if not (flag "camulus-reader--folder") then user_error "Open a favorites folder first";
  request_ignore (CS.Remove_folder(str (get "camulus-reader--folder")))
    ~receive:(callback_unit (fun data -> if String.equal (text "kind" data) "changed" then reader nil))
let compose ~email =
  run "camulus-reader-start" [];
  let entry=if email || flag "camulus-reader--archive" then nil
    else if flag "camulus-reader--inbox" then get "camulus-reader--subscription-entry"
    else if derived "camulus-reader-groups-mode" then
      let entry=property (point ()) "camulus-reader-entry" in
      if truth entry then Reader_config.entry entry else nil
    else get "camulus-reader--entry" in
  let current=Option.bind (capability (field "backend_key" entry)) ~f:(fun backend ->
    Option.bind backend.composer ~f:(fun composer -> composer.in_group entry)) in
  match current with
  | Some open_ -> open_ ()
  | None ->
    match List.find_map (Reader_backend.all ()) ~f:(fun backend ->
      Option.bind backend.composer ~f:(fun composer -> composer.default ())) with
    | Some open_ -> open_ ()
    | None -> user_error "Configure a default mail account first"
let open_view (view:Reader_config.view) =
  match view.destination with
  | Inbox _ -> open_inbox view
  | Feed entry | Group entry -> browse_entry entry
  | Folder folder -> open_folder (s folder)
let render_home () =
  let previous=point () and anchor=property (point ()) "camulus-home-entry" in
  let selected=ref None in
  let map=call "copy-keymap" [get "camulus-home-mode-map"] in
  let counts=get "camulus-reader-home--counts" in
  let heading title =
    Point.insert "\n  ";
    insert_value (propertize (s title) [sym "face";list [sym "bold";sym "font-lock-type-face"]]);
    Point.insert "\n\n" in
  let row ~mnemonic id key label command =
    Point.insert "\t* ";
    let start=point () in
    let before,after=match key with
      | Some key when mnemonic && String.length key=1 && Char.is_alpha key.[0] ->
        (match String.lfindi label ~f:(fun idx c ->
           (idx=0 || Char.equal label.[idx-1] ' ') &&
           Char.equal (Char.lowercase c) (Char.lowercase key.[0])) with
         | Some idx -> String.prefix label idx,String.drop_prefix label (idx+1)
         | None -> "", " "^label)
      | Some _ -> "", " "^label
      | None -> "",label in
    let hint=Option.value_map key ~default:"" ~f:(Printf.sprintf "[%s]") in
    run "insert-text-button" [s (before^hint^after);sym "face";sym "default";
      sym "follow-link";b true;sym "action";callback_unit(fun _ -> run "call-interactively" [command])];
    Option.iter key ~f:(fun key ->
      let offset=start + number(call "length" [s before]) + 1 in
      properties offset (offset + number(call "length" [s key])) [sym "face";sym "highlight"];
      run "define-key" [map;call "kbd" [s key];command]);
    properties start (point ()) [sym "camulus-home-entry";id];
    if Value.equal anchor id then selected:=Some start in
  let section (a:Reader_config.home_action) =
    if List.exists ["camulus-reader-browse-groups";"camulus-groups";"camulus-reader-folders"]
         ~f:(fun name -> Value.eq a.command (sym name)) then `Places
    else if Value.eq a.command (sym "camulus-home-refresh") then `Misc
    else `Basics in
  let actions where=List.filter !home_actions ~f:(fun a -> Poly.equal (section a) where) in
  let action (a:Reader_config.home_action) =
    row ~mnemonic:true (cons (sym "action") (s a.shortcut)) (Some a.shortcut) a.label a.command;
    Point.insert "\n" in
  let visible=visible_home_views () in
  let bookmarks,places=List.partition_tf visible ~f:(fun v ->
    match v.destination with Reader_config.Inbox _ | Feed _ -> true | Group _ | Folder _ -> false) in
  let width v=number(call "string-width" [s v.Reader_config.name]) +
    Option.value_map v.key ~default:0 ~f:(fun k -> 3 + number(call "string-width" [s k])) in
  let widest=List.fold visible ~init:0 ~f:(fun n v -> Int.max n (width v)) in
  let view_command v=Function.to_value (Defun.lambda_nullary_nil [%here] ~interactive:No_arg (fun () -> open_view v)) in
  List.iter !home_views ~f:(fun v -> if not v.show_on_home then
    Option.iter v.key ~f:(fun key -> run "define-key" [map;call "kbd" [s key];view_command v]));
  let shortcut (v:Reader_config.view) =
    let name=s v.name and command=view_command v in
    row ~mnemonic:false (cons (sym "view") name) v.key v.name command;
    let count=if truth counts then find counts name else nil in
    if truth count then (
      Point.insert (String.make (2+widest-width v) ' ');
      let num key=number(field key count) in
      insert_value (propertize (s (Int.to_string (num "unread"))) [sym "face";sym "bold";sym "help-echo";s "Unread"]);
      Point.insert (Printf.sprintf "/%d" (num "total")));
    Point.insert "\n" in
  writable (fun () ->
    Current_buffer.erase ();
    Point.insert "* ";insert_value (styled "Camulus" "bold");
    insert_value (styled " — news and mail for Emacs\n" "font-lock-type-face");
    if not (List.is_empty (actions `Basics)) then (
      heading "Basics";List.iter (actions `Basics) ~f:action);
    if not (List.is_empty bookmarks) then (heading "Bookmarks";List.iter bookmarks ~f:shortcut);
    if not (List.is_empty places && List.is_empty (actions `Places)) then (
      heading "Groups and folders";List.iter places ~f:shortcut;List.iter (actions `Places) ~f:action);
    heading "Misc";
    List.iter (actions `Misc) ~f:action;
    List.iter ["C-c e","Configure home","camulus-edit-home-views";"q","Quit","quit-window"]
      ~f:(fun (k,label,cmd) -> row ~mnemonic:true (sym cmd) (Some k) label (sym cmd);Point.insert "\n");
    heading "Info";
    let info key=Point.insert "\t* ";insert_value (styled (Printf.sprintf "%-18s" key) "font-lock-type-face");Point.insert " : " in
    info "registered groups";Point.insert (Int.to_string (List.length (Reader_backend.registered_group_entries ()))^"\n");
    info "feed sync";set "camulus-reader-home--progress-position" (i (point ()));Point.insert "\n";
    goto (Option.value !selected ~default:(Int.min previous (max_point ()))));
  run "use-local-map" [map];
  Current_buffer.set_modified false;
  home_progress ()
let home_open () =
  read_home_config ();
  let target=home_setup () in
  run "switch-to-buffer" [bv target];
  render_home ();
  home_counts ();
  home_refresh ()
let choose_view name =
  run "camulus-reader-start" [];
  read_home_config ();
  let home=home_setup () in
  if not (in_buffer home (fun () -> flag "camulus-reader-home--ready")) then (
    home_refresh ();
    List.iter (Reader_backend.all ()) ~f:(fun backend -> backend.on_refresh ()));
  home_counts ();
  let name=if truth name then name else
    call "completing-read" [s "Camulus view: ";list(List.map !home_views ~f:(fun view -> s view.name));nil;b true] in
  match find_view (str name) with
  | Some view -> open_view view
  | None -> user_error "Choose a configured Camulus view"
let home_reload () =
  run "camulus-reader-start" [];
  home_refresh ~restart:true ();
  render_home ();
  List.iter (Reader_backend.all ()) ~f:(fun backend -> backend.on_refresh ());
  refresh_archives ()
let choose_subscription ?(label="Following policy") submit =
  let id=submit None in
  put (get "camulus-reader--requests") id (callback_unit (fun data ->
    if String.equal (text "kind" data) "subscription" then (
      let current=field "setting" data in
      let choice=call "completing-read" [s (label^" (currently "^str current^"): ");field "choices" data;nil;b true;nil;nil;current] in
      if not (Value.equal choice current) then (
        let id=submit (Some(str choice)) in
        put (get "camulus-reader--requests") id (callback_unit (fun result ->
          if String.equal (text "kind" result) "subscription" then (
            (* Reading and the management panel share the backend's policy. *)
            if truth (field "backend_key" result) then Reader_tracking_ui.changed (text "backend_key" result);
            List.iter (Buffer.all_live ()) ~f:(fun target -> in_buffer target (fun () ->
              if derived "camulus-subscriptions-mode" then subscriptions_refresh ()));
            message ("Following policy: "^text "setting" result))))))))
let feed_settings () =
  let entry=property (point ()) "camulus-reader-entry" in
  if not (truth entry) then user_error "Select a subscribed feed";
  let source=field "backend_key" entry in
  match capability source with
  | Some {topic_tracking=Some _;_} -> Reader_tracking_ui.open_ source
  | Some {subscription_option=Some option;_} -> Customization.customize_variable (Symbol.intern option)
  | _ -> user_error "This feed does not offer settings"

let group_subscription () =
  run "camulus-reader-start" [];
  let entry=if derived "camulus-reader-groups-mode" then property (point ()) "camulus-reader-entry"
    else if flag "camulus-reader--inbox" then inbox_entry (selected ()) else get "camulus-reader--entry" in
  if not(truth entry) then user_error "Select or open a registered group";
  let entry=Reader_config.entry entry in
  if not(can_subscribe_entry entry)then user_error "This group is browse-only; its backend does not offer group subscription settings";
  let source=field "backend_key" entry in
  match capability source with
  | Some backend -> (match backend.subscription_option with
    | Some option -> Customization.customize_variable(Symbol.intern option)
    | None -> user_error "This backend does not offer local group subscription options")
  | None -> user_error "This backend does not offer group subscription settings"
let subscription () =
  if derived "camulus-reader-groups-mode" then group_subscription () else (
    let source=if flag "camulus-reader--inbox" then field "backend_key" (selected ()) else get "camulus-reader--backend-key" in
    match Option.bind (capability source) ~f:(fun backend->backend.subscription_option) with
    | Some option -> Customization.customize_variable(Symbol.intern option)
    | None ->
    let target=Option.bind (capability source) ~f:(fun backend ->
      Option.bind backend.subscription_target ~f:(fun target -> target ~source:(str source) ~selected:(fun () -> selected ()))) in
    match target with
    | None -> group_subscription ()
    | Some target ->
      let tracking=Option.bind (capability source) ~f:(fun backend -> backend.topic_tracking) in
      choose_subscription ~label:(if Option.is_some tracking then "Topic notification level" else "Subscription policy") (fun setting -> match tracking with
        | Some tracking -> tracking.change ~source:(str source) ~target ~setting
        | None -> i (Runtime_ui.request_subscription (str source) target setting)))
let attachment button =
  let prop name=call "button-get" [button;sym name] in
  let source=prop "backend_key" and id=prop "message-id" and part=prop "part" in
  let destination=call "read-file-name" [s "Save attachment: ";nil;nil;nil;call "file-name-nondirectory" [prop "filename"]] in
  if truth (prop "archived") then
    request_ignore (CS.Attachment(str source,str id,number part,str destination))
  else match Option.bind (capability source) ~f:(fun backend -> backend.attachment) with
    | None -> user_error "This backend does not offer attachment downloads"
    | Some download ->
      let group=field "group" (get "camulus-reader--entry") in
      let id=download ~source:(str source) ~id:(str id) ~part:(number part) ~destination:(str destination)
        ~group:(optional_string group) in
      put (get "camulus-reader--requests") id (sym "ignore")
let mark_visible target =
  if alive target && not (String.equal (str (call "camulus-probe-status" [])) "idle") then in_buffer target (fun () ->
    set "camulus-reader--visibility-timer" nil;
    let ids=ref [] in
    List.iter (values (call "get-buffer-window-list" [bv target;nil;b true])) ~f:(fun window ->
      let p=ref (number (call "window-start" [window])) and finish=call "window-end" [window;b true] in
      if truth finish then while !p < number finish do
        let id=property !p "camulus-reader-id" in
        let key=list [get "camulus-reader--backend-key";id] and read=state (get "camulus-reader--backend-key") id in
        if truth id && truth read && truth (find (get "camulus-reader--loaded") id) && not (truth (field "read" read))
           && not (truth (find (get "camulus-reader--reading") key)) && truth (call "pos-visible-in-window-p" [i !p;window])
           && not (member id !ids) then ids:=id :: !ids;
        p:=next_property !p "camulus-reader-id" (number finish)
      done);
    if not (List.is_empty !ids) then (
      let id=request (CS.Read(str (get "camulus-reader--backend-key"),List.map !ids ~f:str,true)) in
      List.iter !ids ~f:(fun content -> put (get "camulus-reader--reading") (list [get "camulus-reader--backend-key";content]) id)))
let schedule_visible target =
  if alive target then in_buffer target (fun () ->
    if derived "camulus-reader-article-mode" && not (truth (call "timerp" [get "camulus-reader--visibility-timer"])) then
      set "camulus-reader--visibility-timer" (call "run-at-time" [call "string-to-number" [s "0.1"];nil;sym "camulus-reader--mark-visible";bv target]))

let receive data =
  let source=get "camulus-reader--backend-key" in
  let backend=capability source in
  let reconcile ~existing m = Option.value_map backend ~default:m ~f:(fun backend ->
    backend.reconcile ~source:(str source) ~existing ~request:(get "camulus-reader--active") ~archived:(flag "camulus-reader--archive") m) in
  match text "kind" data with
  | "cached-content" ->
    List.iter (values(field "articles" data)) ~f:(fun article ->
      let m=reconcile ~existing:nil (field "message" article) in
      let id=field "id" m in
      put (get "camulus-reader--messages") id m;
      put (get "camulus-reader--loaded") id (b true);
      put (get "camulus-reader--attachments") id (field "attachments" article));
    set "camulus-reader--complete" (field "complete" data);render ()
  | "page" ->
    set_rows (list (List.map (values (field "messages" data)) ~f:(fun m -> alist [
         "backend_key",source;"target",s (if single source then "article" else "thread");"id",field "id" m;
         "title",field "title" m;"author",field "author" m;"unread",b (not (truth (field "read" (state source (field "id" m)))))])))
  | "thread" ->
    List.iter (values (field "messages" data)) ~f:(fun m ->
      let id=field "id" m in
      let existing=find (get "camulus-reader--messages") id in
      let m=if truth existing then reconcile ~existing m else m in
      let m=if truth existing && truth(find (get "camulus-reader--loaded") id) &&
          not(truth(field "unavailable" m)) &&
          not(Option.exists backend ~f:(fun backend -> backend.thread_contains_bodies)) then
        List.fold ["body";"html"] ~init:m ~f:(fun m name ->
          cons (cons (sym name) (field name existing)) (call "assq-delete-all" [sym name;m]))
        else m in
      put (get "camulus-reader--messages") id m;
      if Option.exists backend ~f:(fun backend -> backend.thread_contains_bodies) then put (get "camulus-reader--loaded") id (b true));
    set "camulus-reader--complete" (field "complete" data);render ()
  | "article" ->
    if single source then set "camulus-reader--complete" (b true);
    let m=reconcile ~existing:nil (field "message" data) in
    let id=field "id" m in
    put (get "camulus-reader--messages") id m;put (get "camulus-reader--loaded") id (b true);
    put (get "camulus-reader--attachments") id (field "attachments" data);render ()
  | "error" -> message (text "message" data)
  | _ -> ()
let event event =
  let nth n=call "nth" [i n;event] in
  let id=nth 1 in
  let tag=str (call "symbol-name" [nth 0]) in
  match tag with
  | "content-state" ->
    let data=nth 2 in
    (match text "kind" data with
     | "states" ->
       let states=field "data" data in
       let source=field "backend_key" states in
       List.iter (values (field "items" states)) ~f:(fun read -> put (get "camulus-reader--states") (list [source;field "id" read]) read);
       List.iter (Buffer.all_live ()) ~f:(fun target -> in_buffer target (fun () ->
         if derived "camulus-reader-article-mode" && Value.equal source (get "camulus-reader--backend-key") && count (get "camulus-reader--messages") > 0 then render ()))
     | "changed" -> reload_lists ()
     | "saved" -> message ("Saved: "^str (or_ (field "folder" data) (field "path" data)))
     | "cached-content" ->
       let value=find (get "camulus-reader--loads") id in
       if truth value then let target=Buffer.of_value_exn value in
         if alive target then in_buffer target (fun () ->
           if Value.eq id (get "camulus-reader--active") then receive data)
     | _ -> ());
    let callback=find (get "camulus-reader--requests") id in
    if truth callback then ignore (Value.funcallN callback [data] : Value.t)
  | "finished" ->
    if (truth (find (get "camulus-reader--requests") id) || truth (find (get "camulus-reader--loads") id)) && Value.eq (nth 2) (sym "failed") then
      message ("Reader: "^str (nth 3));
    remove (get "camulus-reader--requests") id;remove (get "camulus-reader--loads") id;
    let callback=find (get "camulus-reader--completions") id in
    if truth callback then (remove (get "camulus-reader--completions") id;ignore (Value.funcallN callback [event] : Value.t));
    List.iter (values (call "hash-table-keys" [get "camulus-reader--reading"])) ~f:(fun key ->
      if Value.eq (find (get "camulus-reader--reading") key) id then remove (get "camulus-reader--reading") key)
  | tag when List.exists (Reader_backend.all ()) ~f:(fun backend -> List.mem backend.events tag ~equal:String.equal) ->
    let data=nth 2 in
    let callback=find (get "camulus-reader--requests") id in
    if truth callback then ignore (Value.funcallN callback [data] : Value.t);
    if truth (find (get "camulus-reader--requests") id) && String.equal (text "kind" data) "saved" then
      message ("Saved: "^text "path" data);
    let value=find (get "camulus-reader--loads") id in
    if truth value then let target=Buffer.of_value_exn value in
      if alive target then in_buffer target (fun () -> if Value.eq id (get "camulus-reader--active") then receive data)
  | _ -> ()
(* Registration is explicit, after backend adapters/configuration have loaded. *)
let define2 name f =
  Defun.defun (Symbol.intern name) [%here] ~docstring:"Native Ecaml reader operation." (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open a=required "first" value and b=required "second" value in f a b)
let local_default name default =
  if not (Current_buffer.variable_is_defined (Symbol.intern name)) then set name default;
  run "make-variable-buffer-local" [sym name]
let mode name parent title columns_ header init =
  let initialize () =
    Option.iter columns_ ~f:(fun cols -> set "tabulated-list-format" (columns cols));
    if Option.is_some columns_ then run "tabulated-list-init-header" [];
    local "header-line-format" (s header);
    init () in
  ignore (Major_mode.define_derived_mode (Symbol.intern name) [%here] ~docstring:"Native Ecaml reader view."
    ~parent:(Major_mode.find_or_wrap_existing [%here] (Symbol.intern parent)) ~mode_line:title
    ~initialize:(Returns Value.Type.unit,initialize) () : (module Major_mode.S))
let bind mode_name bindings =
  let map=Keymap.of_value_exn (get (mode_name^"-map")) in
  List.iter bindings ~f:(fun (key,command) -> run "define-key" [Keymap.to_value map;call "kbd" [s key];sym command])
let install_modes () =
  run "require" [sym "tabulated-list"];
  List.iter ["entries";"errors";"counts"] ~f:(fun name -> local_default ("camulus-reader-subscriptions--"^name) nil);
  local_default directory_folded nil;
  List.iter ["entries";"cursors";"token";"jobs";"queue";"errors";"ready";"counts";"progress-position";"count-request";"count-dirty"] ~f:(fun name -> local_default ("camulus-reader-home--"^name) nil);
  List.iter ["done";"total"] ~f:(fun name -> local_default ("camulus-reader-home--"^name) (i 0));
  mode "camulus-home-mode" "special-mode" "Camulus" None "" (fun () ->
    local "header-line-format" nil;
    List.iter ["kill-buffer-hook";"change-major-mode-hook"] ~f:(fun hook ->
      run "add-hook" [sym hook;sym "camulus-reader--cancel-home";nil;b true]));
  mode "camulus-reader-feed-mode" "tabulated-list-mode" "Feed"
    (Some ["New",3,false;"Backend",24,true;"Author",20,true;"Thread",70,true])
    "RET: read  b: original  C: compose  r: refile  m: choose folder  d: mark delete  u: unmark  x: execute  g: refresh  n: more"
    (fun () -> set "camulus-reader--marks" (hash ());set "tabulated-list-padding" (i 2);
      List.iter ["kill-buffer-hook";"change-major-mode-hook"] ~f:(fun hook ->
      run "add-hook" [sym hook;sym "camulus-reader--cancel-list";nil;b true]));
  mode "camulus-reader-folder-mode" "tabulated-list-mode" "Favorites"
    (Some ["Kind",8,false;"Backend",24,true;"Title",80,true])
    "RET: read saved copy  b: original  r: refile  m: choose folder  d: mark removal  u: unmark  x: execute  D: remove folder  g: reload  h: home"
    (fun () -> set "camulus-reader--marks" (hash ());set "tabulated-list-padding" (i 2));
  mode "camulus-reader-folders-mode" "special-mode" "Favorite folders" None
    "RET / click: open folder  TAB: next folder  g: refresh  h: home" ignore;
  mode "camulus-reader-groups-mode" "special-mode" "Reader groups" None
    "Groups · RET: open  TAB: fold  n/p: move  U: subscribed feeds  g: reload  b: browse…  s: settings  T: tracking  C: compose  C-c e: edit  h: home" ignore;
  mode "camulus-subscriptions-mode" "camulus-reader-groups-mode" "Subscribed feeds" None
    "Subscribed feeds · RET: open inbox  TAB: fold  n/p: move  B: groups  g: refresh  s: settings  T: tracking  C: compose  h: home" ignore;
  mode "camulus-reader-article-mode" "special-mode" "Reader thread" None "Loading…" (fun () ->
    local "camulus-reader--render-token" nil;
    reading_layout ();
    List.iter ["messages";"attachments";"loaded";"marks"] ~f:(fun name -> set ("camulus-reader--"^name) (hash ()));
    List.iter ["kill-buffer-hook";"change-major-mode-hook"] ~f:(fun hook->
      run "add-hook" [sym hook;callback0 Reader_media.cleanup;nil;b true]);
    run "add-hook" [sym "kill-buffer-hook";callback0 (fun () ->
      if truth (call "timerp" [get "camulus-reader--visibility-timer"]) then run "cancel-timer" [get "camulus-reader--visibility-timer"];
      if flag "camulus-reader--active" && not (String.equal (str (call "camulus-probe-status" [])) "idle") then run "camulus-probe-cancel" [get "camulus-reader--active"]);nil;b true]);
  List.iter ["camulus-reader-feed-mode";"camulus-reader-folder-mode"] ~f:(fun mode ->
    bind mode ["RET","camulus-reader-open";"b","camulus-reader-open-original";"d","camulus-reader-delete";"g","camulus-reader-refresh";"h","camulus"]);
  List.iter ["camulus-reader-feed-mode";"camulus-reader-folder-mode";"camulus-reader-article-mode"] ~f:(fun mode ->
    bind mode ["r","camulus-reader-refile";"m","camulus-reader-move";"u","camulus-reader-unmark";"x","camulus-reader-execute-marks"]);
  List.iter ["camulus-reader-feed-mode";"camulus-reader-article-mode";"camulus-reader-groups-mode"] ~f:(fun mode ->
    bind mode ["C","camulus-reader-compose";"V","camulus-open-view"]);
  List.iter ["camulus-reader-folder-mode";"camulus-reader-folders-mode"] ~f:(fun mode ->
    bind mode ["V","camulus-open-view"]);
  bind "camulus-home-mode" ["RET","push-button";"TAB","forward-button";"<backtab>","backward-button";
    "h","camulus";"q","quit-window";"e","camulus-reader-home-errors";
    "C-c e","camulus-edit-home-views";"C-c C-k","camulus-reader-cancel"];
  bind "camulus-reader-feed-mode" ["s","camulus-reader-subscription";"S","camulus-reader-group-subscription";"G","camulus-groups";"C-c e","camulus-reader-edit-config";"n","camulus-reader-more";"C-c C-k","camulus-reader-cancel";
    "U","camulus-subscriptions";"B","camulus-reader-browse-groups";"C-c b","camulus-reader-home-browse-source";"e","camulus-reader-home-errors"];
  bind "camulus-reader-folder-mode" ["D","camulus-reader-delete-folder"];
  bind "camulus-reader-folders-mode" ["RET","push-button";"TAB","forward-button";"<backtab>","backward-button";"g","camulus-reader-folders-refresh";"h","camulus"];
  bind "camulus-reader-article-mode" ["s","camulus-reader-subscription";"n","camulus-reader-next-unread";"b","camulus-reader-open-original";"h","camulus"];
  bind "camulus-subscriptions-mode" ["s","camulus-feed-settings";"S","camulus-feed-settings";"g","camulus-subscriptions-refresh";"B","camulus-reader-browse-groups";"U","camulus-subscriptions"];
  List.iter ["camulus-reader-feed-mode";"camulus-reader-article-mode";"camulus-reader-groups-mode"] ~f:(fun mode ->
    bind mode ["T","camulus-manage-subscriptions"]);
  bind "camulus-reader-groups-mode" ["RET","camulus-directory-open";"TAB","camulus-directory-toggle";"<tab>","camulus-directory-toggle";"n","forward-line";"p","previous-line";"b","camulus-directory-browse";"<backtab>","backward-button";"U","camulus-subscriptions";"s","camulus-reader-group-subscription";"S","camulus-reader-group-subscription";"h","camulus";"g","camulus-reader-groups-refresh";"B","camulus-reader-browse-groups";"C-c e","camulus-reader-edit-groups"]


let setup_variables () =
 List.iter["camulus-reader--requests";"camulus-reader--completions";"camulus-reader--loads";"camulus-reader--states";"camulus-reader--reading"]~f:(fun n->Reader_backend.variable n(call "make-hash-table"[sym ":test";sym "equal"]));
 List.iter["entry";"inbox";"inbox-entries";"subscription-entry";"cursor";"list-token";"list-request";"rows";"all-rows";"marks";"folder";"backend-key";"title";"messages";"attachments";"loaded";"complete";"archive";"active";"positioned";"view-token";"visibility-timer"]~f:(fun n->let name="camulus-reader--"^n in Reader_backend.variable name nil;run "make-variable-buffer-local"[sym name]);
 Reader_backend.variable "camulus-reader-reply-indent"(i 4);
 run "custom-declare-group"[sym "camulus";nil;s "Read feeds and discussions.";sym ":group";sym "applications"];
 Reader_backend.custom ~kind:Customization.Type.(Choice [Const nil;String;Function])
   "camulus-refile-rule" nil
   "A favorite folder or a function that decides where to refile an item. The function receives a plist with :id, :backend, :backend-key, :group, :groups, :kind, :title, :author, and :url. It can return a favorite folder, (:file PATH :format FORMAT), or nil to defer to the backend. FORMAT is text, html, or a function taking the item plist and the list of messages.";
 let read_attributes color=Value.list[sym ":inherit";sym "shadow";sym ":foreground";s color;sym ":weight";sym "normal"]in
 let background name color=Value.list[Value.list[Value.list[sym "background";sym name]];read_attributes color]in
 List.iter["camulus-reader-heading",Value.list[Value.list[sym "t";Value.list[sym ":weight";sym "bold"]]];
  "camulus-reader-separator",Value.list[Value.list[sym "t";Value.list[sym ":inherit";sym "shadow";sym ":underline";sym "t"]]];
  "camulus-reader-read",Value.list[background "light" "gray55";background "dark" "gray65";
    Value.list[sym "t";Value.list[sym ":inherit";sym "shadow"]]];
  "camulus-reader-favorite",Value.list[Value.list[sym "t";Value.list[sym ":foreground";s "red";sym ":weight";sym "normal"]]]]
 ~f:(fun(name,spec)->run "custom-declare-face"[sym name;spec;s "Reader presentation.";sym ":group";sym "camulus"])

let installed = ref false
let install () =
  if not !installed then (
  setup_variables ();
  Reader_media.install ();
  Reader_tracking_ui.install ~on_changed:(fun _ ->
    let home=home_buffer () in
    if in_buffer home (fun () -> derived "camulus-home-mode") then home_refresh ~restart:true ());
  define1 "camulus-reader--set-rows" (fun rows -> set_rows rows;nil);
  define0 "camulus-reader--reading-layout" reading_layout;
  define1 "camulus-reader--header-text" (fun value -> s (header_text value));
  define1 "camulus-reader--shr-code" (fun dom -> shr_code dom;nil);
  define1 "camulus-reader--anchor" (fun p -> anchor (number p));
  define1 "camulus-reader--position" position;
  Defun.defun_nullary (Symbol.intern "camulus-reader--selected") [%here] ~docstring:"Selected content." (Returns Value.Type.value) selected;
  define0 "camulus-reader--render" render;
  define0 ~interactive:true "camulus-reader-open" open_article;
  define0 ~interactive:true "camulus-reader-open-original" open_original;
  define0 ~interactive:true "camulus-mail-new" (fun () -> compose ~email:true);
  define0 ~interactive:true "camulus-reader-compose" (fun () -> compose ~email:false);
  define0 ~interactive:true "camulus-reader-next-unread" next_unread;
  define1 "camulus-reader--attachment" (fun button -> attachment button;nil);
  define1 "camulus-reader--refresh-backend-key" (fun source -> refresh_source source;nil);
  define0 "camulus-reader-home-open" home_open;
  define0 "camulus-reader--render-home" render_home;
  define0 "camulus-reader--cancel-list" cancel_list;
  define0 "camulus-reader--cancel-home" home_cancel;
  define0 ~interactive:true "camulus-home-refresh" home_reload;
  define0 ~interactive:true "camulus-edit-home-views" (fun () ->
    run "require" [sym "cus-edit"];
    let option name=list [sym name;sym "custom-variable"] in
    run "custom-buffer-create" [list (List.map ["camulus-home-actions";"camulus-home-views"] ~f:option);
      s "*Customize Camulus Home*"]);
  optional ~interactive:true "camulus-open-view" (fun name -> choose_view name;nil);
  define0 ~interactive:true "camulus-reader-home-browse-source" browse_source;
  define0 ~interactive:true "camulus-reader-home-errors" home_errors;
  define0 ~interactive:true "camulus-reader-browse-groups" groups;
  define0 ~interactive:true "camulus-reader-groups-refresh" directory_render;
  define0 ~interactive:true "camulus-directory-toggle" directory_toggle;
  define0 ~interactive:true "camulus-directory-open" directory_open;
  define0 ~interactive:true "camulus-directory-browse" directory_browse;
  define0 ~interactive:true "camulus-subscriptions" subscriptions;
  define0 ~interactive:true "camulus-subscriptions-refresh" subscriptions_refresh;
  define0 ~interactive:true "camulus-group-categories" choose_group_category;
  define1 "camulus-reader-browse-entry" (fun entry -> browse_entry entry;nil);
  optional ~interactive:true "camulus" (fun entry -> reader entry;nil);
  define1 "camulus-reader--single-article-source-p" (fun source -> b (single source));
  define2 "camulus-reader--state" state;
  define0 "camulus-reader--reload-lists" reload_lists;
  define0 ~interactive:true "camulus-reader-refresh" refresh;
  define0 ~interactive:true "camulus-reader-more" more;
  define0 ~interactive:true "camulus-reader-cancel" cancel;
  define0 ~interactive:true "camulus-groups" entries;
  define0 ~interactive:true "camulus-reader-delete" mark_delete;
  define0 ~interactive:true "camulus-reader-refile" refile;
  define0 ~interactive:true "camulus-reader-move" mark_move;
  define0 ~interactive:true "camulus-reader-unmark" unmark;
  define0 ~interactive:true "camulus-reader-execute-marks" execute_marks;
  define0 ~interactive:true "camulus-reader-folders" folders;
  define0 ~interactive:true "camulus-reader-folders-refresh" folders_load;
  define0 ~interactive:true "camulus-reader-delete-folder" delete_folder;
  define0 ~interactive:true "camulus-reader-subscription" subscription;
  define0 ~interactive:true "camulus-feed-settings" feed_settings;
  define0 ~interactive:true "camulus-reader-group-subscription" group_subscription;
  define2 "camulus-reader--window-scrolled" (fun window _ -> schedule_visible (Window.buffer_exn (Window.of_value_exn window));nil);
  define1 "camulus-reader--window-changed" (fun window -> schedule_visible (Window.buffer_exn (Window.of_value_exn window));nil);
  define1 "camulus-reader--receive" (fun data -> receive data;nil);
  define1 "camulus-reader--mark-visible" (fun target -> mark_visible (Buffer.of_value_exn target);nil);
  define1 "camulus-reader--schedule-visible" (fun target -> schedule_visible (Buffer.of_value_exn target);nil);
  Defun.defun (Symbol.intern "camulus-reader--load-list") [%here] ~docstring:"Refresh a native list view."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open synchronize=optional_with_nil "synchronize" value
     and more=optional_with_nil "more" value in load_list (truth synchronize) (truth more));
  install_modes ();
  run "camulus-reader-backends-install" [];
  run "add-hook" [sym "window-scroll-functions";sym "camulus-reader--window-scrolled"];
  run "add-hook" [sym "window-buffer-change-functions";sym "camulus-reader--window-changed"];
  installed := true)

let register () = define0 "camulus-reader-ui-install" install
