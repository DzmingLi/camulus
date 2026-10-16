(* Emacs presentation lives on the owner thread. Values below are Emacs-owned
   buffer-local view state, never sent to the Eio worker. Existing backend adapters
   share these variables while their transport/compose APIs are migrated separately. *)
open! Core
open! Ecaml

let nil = Value.nil
let s = Value.of_utf8_bytes
let sym = Value.intern
let i = Value.of_int_exn
let b x = if x then sym "t" else nil
let truth x = not (Value.eq x nil)
let call name args = Value.funcallN (sym name) args
let run name args = ignore (call name args : Value.t)
let list = Value.list
let values x = Value.to_list_exn x ~f:Fn.id
let str x = if truth x then Value.to_utf8_bytes_exn x else ""
let number x = if truth x then Value.to_int_exn x else 0
let timestamp x = if truth x then Value.to_float_exn (call "float" [x]) else 0.
let get name = Current_buffer.symbol_value (Symbol.intern name)
let set name value = run "set" [sym name;value]
let local name value = run "make-local-variable" [sym name]; set name value
let field key value = call "alist-get" [sym key;value]
let text key value = str (field key value)
let cons a d = call "cons" [a;d]
let alist pairs = list (List.map pairs ~f:(fun (k,v) -> cons (sym k) v))
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
let request_native fields receive finished =
  let id=i (Application.request_state (Reader.Content_state.operation_of_json (Reader_backend.to_json fields))) in
  put (get "ef-reader--requests") id (or_ receive (sym "ignore"));
  if truth finished then put (get "ef-reader--completions") id finished;id
let request ?(finished=nil) ?(receive=nil) fields = request_native (alist fields) receive finished
let request_ignore ?receive fields = ignore (request ?receive fields : Value.t)
let state source id = find (get "ef-reader--states") (list [source;id])
let capability source = Reader_backend.find (str source)
let single source = Option.exists (capability source) ~f:(fun backend -> not backend.threaded)
let entry_key entry = list [field "source" entry;field "group" entry]
let home_entry row =
  List.find (values (get "ef-reader-home--entries")) ~f:(fun e ->
    Value.equal (field "source" e) (field "source" row) && member (field "group" e) (values (field "groups" row)))
  |> Option.value ~default:nil
let format_time format time = call "format-time-string" [s format;call "seconds-to-time" [or_ time (i 0)]]
let home_row row = vector [s (if truth (field "unread" row) then "*" else "");
  (if Float.(timestamp (field "occurred" row) > 0.) then format_time "%m-%d %H:%M" (field "occurred" row) else s "");
  or_ (field "name" (home_entry row)) (field "source" row); field "author" row;field "title" row]

let set_rows rows =
  let table = hash () in
  set "ef-reader--rows" table;
  let entries = List.map (values rows) ~f:(fun row ->
    let row = if single (field "source" row) && not (truth (field "target" row))
      then cons (cons (sym "target") (s "article")) row else row in
    let favorite = derived "ef-reader-folder-mode" in
    let key = list [field "source" row;(if favorite then field "id" row else or_ (field "conversation_id" row) (field "id" row));field "target" row] in
    put table key row;
    let columns = if flag "ef-reader--home" then home_row row
      else if favorite then vector [field "target" row;field "source" row;field "title" row]
      else vector [s (if truth (field "unread" row) then "*" else "");field "source" row;field "author" row;field "title" row] in
    list [key;columns]) in
  set "tabulated-list-entries" (list entries);run "tabulated-list-print" [b true]

let selected () =
  if derived "ef-reader-article-mode" then (
    let id = property (point ()) "ef-reader-id" in
    if not (truth id) then user_error "Select an article";
    alist ["source",get "ef-reader--source";"id",id;"target",s "article"])
  else let row = if flag "ef-reader--rows" then find (get "ef-reader--rows") (call "tabulated-list-get-id" []) else nil in
    if not (truth row) then user_error "Select a row"; row

let reading_layout () =
  local "line-spacing" (call "string-to-number" [s "0.2"]);
  local "shr-fill-text" nil;local "truncate-lines" nil;
  run "visual-line-mode" [i 1];
  if truth (call "require" [sym "olivetti";nil;b true]) then run "olivetti-mode" [i 1]

let header_text value =
  str value |> String.map ~f:(function '\n' | '\r' | '\t' -> ' ' | c -> c)

let heading m depth read =
  let start = point () in
  let title = if String.is_empty (text "title" m) then get "ef-reader--title" else field "title" m in
  let date = if Float.(timestamp (field "created" m) > 0.) then str (format_time "%Y-%m-%d %H:%M" (field "created" m)) else "" in
  let marks () =
    if truth (field "liked" m) then Point.insert "  ♥";
    if truth (field "like-uncertain" m) then Point.insert " (like unconfirmed)" in
  let author () =
    insert_value (styled (if truth (field "read" read) then "○ " else "● ") "shadow");
    insert_value (styled (header_text (field "author" m)) "font-lock-keyword-face") in
  if depth = 0 then (
    insert_value (styled "From: " "shadow");author ();marks ();Point.insert "\n";
    if not (String.is_empty (str title)) then (
      insert_value (styled "Subject: " "shadow");insert_value (styled (header_text title) "ef-reader-heading");Point.insert "\n");
    if not (String.is_empty date) then (insert_value (styled "Date: " "shadow");Point.insert (date^"\n")))
  else (author ();if not (String.is_empty date) then Point.insert (" · "^date);marks ();Point.insert "\n";
        run "add-face-text-property" [i start;i (point () - 1);sym "ef-reader-heading";b true]);
  insert_value (propertize (s " ") [sym "display";list [sym "space";sym ":align-to";list [sym "-";sym "right";i 1]];
                                    sym "face";sym "ef-reader-separator";sym "rear-nonsticky";b true]);
  Point.insert "\n"

let anchor pos =
  let id = property pos "ef-reader-id" in
  if not (truth id) then nil else
    let start = call "previous-single-property-change" [i (pos+1);sym "ef-reader-id"] in
    cons id (i (pos - if truth start then number start else min_point ()))
let position anchor =
  if not (truth anchor) then nil else
    let id=call "car" [anchor] and offset=number (call "cdr" [anchor]) in
    let rec loop p =
      if p >= max_point () then nil
      else if Value.equal id (property p "ef-reader-id") then
        i (Int.min (p+offset) (next_property p "ef-reader-id" (max_point ()) - 1))
      else loop (next_property p "ef-reader-id" (max_point ())) in
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

let body m indent =
  if String.is_empty (text "body" m) && truth (field "html" m) then (
    let dom=Current_buffer.set_temporarily_to_temp_buffer Sync (fun () ->
      Point.insert (text "html" m);call "libxml-parse-html-region" [i (min_point ());i (max_point ())]) in
    let renderers=list (List.map ["pre";"code";"tt"] ~f:(fun tag -> cons (sym tag) (sym "ef-reader--shr-code"))) in
    with_bindings ["shr-inhibit-images",b true;"shr-use-fonts",nil;"shr-fill-text",nil;
      "shr-external-rendering-functions",call "append" [renderers;get "shr-external-rendering-functions"];
      "shr-width",i (Int.max 20 (number (call "window-body-width" [call "get-buffer-window" [bv (buffer ());b true]])-indent-2));
      "shr-base",or_ (field "link_url" m) (field "url" m)]
      (fun () -> run "shr-insert-document" [dom]))
  else Point.insert (text "body" m)

let render () =
  let current_anchor = anchor (point ()) in
  let windows = values (call "get-buffer-window-list" [bv (buffer ());nil;b true])
    |> List.map ~f:(fun w -> w,anchor (number (call "window-start" [w]))) in
  let messages = hash_values (get "ef-reader--messages") |> List.sort ~compare:(fun a c ->
    let order=Float.compare (timestamp (field "created" a)) (timestamp (field "created" c)) in
    if order=0 then String.compare (text "id" a) (text "id" c) else order) in
  let seen=Hash_set.create (module String) and children=Hashtbl.create (module String) in
  List.iter (List.rev messages) ~f:(fun m -> Hashtbl.add_multi children ~key:(text "parent_id" m) ~data:m);
  let first_unread=ref None in
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
          let read=state (get "ef-reader--source") id in
          let start=point () and indent=number (get "ef-reader-reply-indent") * Int.min depth 12 in
          if truth read && not (truth (field "read" read)) && Option.is_none !first_unread then first_unread:=Some start;
          heading m depth read;
          let body_start=point () in
          if truth (find (get "ef-reader--loaded") id) then body m indent else Point.insert "Loading article…";
          if single (get "ef-reader--source") && (String.is_prefix (text "url" m) ~prefix:"https://" || String.is_prefix (text "url" m) ~prefix:"http://") then (
            Point.insert "\n";
            run "insert-text-button" [s "Open original article";sym "follow-link";b true;sym "url";field "url" m;
              sym "action";callback_unit (fun button -> run "browse-url" [call "button-get" [button;sym "url"]])]);
          Point.insert "\n";
          List.iter (values (find (get "ef-reader--attachments") id)) ~f:(fun a ->
            run "insert-text-button" [s (Printf.sprintf "[Save %s · %d bytes]" (text "name" a) (number (field "size" a)));
              sym "action";sym "ef-reader--attachment";sym "follow-link";b true;sym "source";get "ef-reader--source";
              sym "message-id";id;sym "part";field "part" a;sym "filename";field "name" a;sym "archived";get "ef-reader--archive"];
            Point.insert "\n");
          Point.insert "\n";
          let prefix n=styled (String.make n ' ') "fixed-pitch" in
          properties start (point ()) [sym "ef-reader-id";id;sym "ef-reader-depth";i depth;
            sym "line-prefix";prefix indent;sym "wrap-prefix";prefix indent];
          properties body_start (point ()) [sym "line-prefix";prefix (indent+2);sym "wrap-prefix";prefix (indent+2)];
          stack:=List.map (Hashtbl.find_multi children id_string) ~f:(fun m -> m,depth+1) @ !stack)
      done in
    render_roots (List.filter messages ~f:(fun m -> not (truth (find (get "ef-reader--messages") (field "parent_id" m)))));
    render_roots messages);
  set "header-line-format" (s (Printf.sprintf "m: save this article  n: next unread%s — %d articles — %s"
    (Option.value_map (capability (get "ef-reader--source")) ~default:"" ~f:(fun backend ->
       String.concat (List.map backend.controls ~f:(fun control -> "  "^control.hint))))
    (List.length messages) (if flag "ef-reader--complete" then "loaded" else "loading / partial")));
  (match !first_unread with
   | Some p when not (flag "ef-reader--positioned") -> goto p;set "ef-reader--positioned" (b true);
     List.iter windows ~f:(fun (w,_) -> run "set-window-start" [w;i p;b true])
   | _ -> let p=position current_anchor in goto (if truth p then number p else min_point ());
     List.iter windows ~f:(fun (w,a) -> let p=position a in if truth p then run "set-window-start" [w;p;b true]));
  run "ef-reader--schedule-visible" [bv (buffer ())]

let next_unread () =
  let rec loop p = if p >= max_point () then message "No further loaded unread article"
    else let read=state (get "ef-reader--source") (property p "ef-reader-id") in
      if truth read && not (truth (field "read" read)) then goto p
      else loop (next_property p "ef-reader-id" (max_point ())) in
  loop (next_property (point ()) "ef-reader-id" (max_point ()))

let load_archive target source id kind =
  let generation=get "ef-reader--view-token" in
  request ["op",s "archive";"source",source;"id",id;"target",kind] ~receive:(callback_unit (fun data ->
    if String.equal (text "kind" data) "archive" && alive target then in_buffer target (fun () ->
      if Value.eq generation (get "ef-reader--view-token") then (
        List.iter (values (field "articles" data)) ~f:(fun article ->
          let m=field "message" article in let id=field "id" m in
          put (get "ef-reader--messages") id m;put (get "ef-reader--loaded") id (b true);
          put (get "ef-reader--attachments") id (field "attachments" article));
        set "ef-reader--complete" (b true);
        request_ignore ["op",s "states";"source",source;"ids",call "vconcat" [call "hash-table-keys" [get "ef-reader--messages"]]]
          ~receive:(callback_unit (fun _ -> if alive target then in_buffer target (fun () ->
            if Value.eq generation (get "ef-reader--view-token") then render ())))))))

let open_article () =
  let row=selected () in
  let source=field "source" row and id=field "id" row in
  let saved=derived "ef-reader-folder-mode" in
  let entry=if flag "ef-reader--home" then home_entry row else get "ef-reader--entry" in
  let backend=capability source in
  if not saved && not (Option.exists backend ~f:(fun backend -> backend.available (str source))) then
    user_error "Configure this source before opening live content";
  let target=Buffer.find_or_create ~name:"*Reader thread*" in
  in_buffer target (fun () ->
    if flag "ef-reader--active" then run "ef-probe-cancel" [get "ef-reader--active"];
    run "ef-reader-article-mode" [];
    Option.iter backend ~f:(fun backend ->
      let map=call "copy-keymap" [call "current-local-map" []] in
      List.iter backend.controls ~f:(fun control -> run "define-key" [map;call "kbd" [s control.key];sym control.command]);
      run "use-local-map" [map]);
    set "ef-reader--source" source;set "ef-reader--title" (field "title" row);set "ef-reader--archive" (b saved);
    set "ef-reader--view-token" (token ());set "ef-reader--entry" entry;
    writable (fun () -> Current_buffer.erase ();Point.insert "Loading…");
    let active=if saved then load_archive target source id (field "target" row)
      else let backend=Option.value_exn backend in
        backend.read ~source:(str source) ~id:(str id)
          ~thread_id:(if truth (field "thread_id" row) then Some (text "thread_id" row) else None)
          ~group:(if truth (field "group" entry) then Some (text "group" entry) else None) in
    set "ef-reader--active" active;
    if not saved then put (get "ef-reader--loads") active (bv target));
  let window=call "display-buffer-in-direction" [bv target;alist ["direction",sym "right";"window-width",call "string-to-number" [s "0.6"]]] in
  if truth window then Selected_window.set (Window.of_value_exn window)

let home_link label command =
  let map=Keymap.create () in
  let action=Function.to_value (Defun.lambda [%here] ~interactive:(Prompt "e") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open event=required "event" value in
     Selected_window.set (Window.of_value_exn (call "posn-window" [call "event-start" [event]]));
     run "call-interactively" [sym command])) in
  run "define-key" [Keymap.to_value map;vector [sym "header-line";sym "mouse-1"];action];
  propertize (s label) [sym "face";sym "link";sym "mouse-face";sym "highlight";
    sym "help-echo";s "Browse without subscribing";sym "local-map";Keymap.to_value map]
let home_header () =
  let errors=List.length (values (get "ef-reader-home--errors")) in
  let links=List.concat_map (Reader_backend.all ()) ~f:(fun backend ->
    match backend.browser with None -> [] | Some (label,command) -> [s "  ";home_link ("["^label^"]") command]) in
  set "header-line-format" (list ([s "Home  ";home_link "[Groups · B]" "ef-reader-browse-groups"] @ links @
    [s "  ";home_link "[Folders · f]" "ef-reader-folders";
    s (Printf.sprintf "   %d feeds · %d/%d refreshed%s   RET: read  d: remove  m: save  g: refresh  e: errors"
      (List.length (values (get "ef-reader-home--entries"))) (number (get "ef-reader-home--done"))
      (number (get "ef-reader-home--total")) (if errors=0 then "" else Printf.sprintf " · %d failed" errors))]));
  run "force-mode-line-update" []
let home_current buffer token = alive buffer && in_buffer buffer (fun () -> flag "ef-reader--home" && Value.eq token (get "ef-reader-home--token"))
let home_cancel () =
  if not (String.equal (str (call "ef-probe-status" [])) "idle") then
    List.iter (values (get "ef-reader-home--jobs")) ~f:(fun id -> run "ef-probe-cancel" [id]);
  set "ef-reader-home--jobs" nil;set "ef-reader-home--queue" nil;set "ef-reader-home--token" (token ())
let home_cache () =
  let target=buffer () and generation=token () in
  set "ef-reader--list-token" generation;
  let receive=callback_unit (fun data ->
    if alive target && String.equal (text "kind" data) "feed" then in_buffer target (fun () ->
      if flag "ef-reader--home" && Value.eq generation (get "ef-reader--list-token") then (
        set_rows (field "rows" data);home_header ()))) in
  let id=request ~receive ["op",s "timeline";"entries",call "vconcat" [get "ef-reader-home--entries"]] in
  set "ef-reader--list-request" id
let push_var name value = set name (cons value (get name))
let remove_job id = set "ef-reader-home--jobs" (call "delq" [id;get "ef-reader-home--jobs"])
let home_error name error = push_var "ef-reader-home--errors" (cons name error)
let home_done () = set "ef-reader-home--done" (i (number (get "ef-reader-home--done")+1))
let rec home_pump more =
  let target=buffer () and generation=get "ef-reader-home--token" in
  while flag "ef-reader-home--queue" && List.length (values (get "ef-reader-home--jobs")) < 4 do
    let entry=call "car" [get "ef-reader-home--queue"] in
    set "ef-reader-home--queue" (call "cdr" [get "ef-reader-home--queue"]);
    let key=entry_key entry in
    try
      let receive=callback_unit (fun data ->
        if home_current target generation && String.equal (text "kind" data) "feed" then in_buffer target (fun () ->
          put (get "ef-reader-home--cursors") key (field "after" data);run "ef-reader-home-load-cache" [])) in
      let finished=callback_unit (fun event ->
        if home_current target generation then in_buffer target (fun () ->
          remove_job (call "nth" [i 1;event]);home_done ();
          if not (Value.eq (call "nth" [i 3;event]) (sym "completed")) then
            home_error (field "name" entry) (or_ (call "nth" [i 4;event]) (s "Refresh failed"));
          home_pump more;home_header ())) in
      let id=request ~receive ~finished ["op",s "refresh";"source",field "source" entry;"group",field "group" entry;
        "after",(if more then find (get "ef-reader-home--cursors") key else nil)] in
      push_var "ef-reader-home--jobs" id
    with exn -> home_done ();home_error (field "name" entry) (s (Exn.to_string exn))
  done;
  home_header ()
let home_merge followed =
  let configured=values (get "ef-reader-entries") in
  let entries=List.filter configured ~f:(fun entry -> Value.eq (field "timeline" entry) (sym "t") && not (Option.exists (capability (field "source" entry)) ~f:(fun backend -> Option.is_some backend.followed))) in
  let entries=List.fold (values followed) ~init:entries ~f:(fun entries entry ->
    Option.value (List.find configured ~f:(fun e -> Value.equal (entry_key e) (entry_key entry))) ~default:entry :: entries) in
  let seen=Hash_set.create (module String) in
  List.filter entries ~f:(fun entry ->
    let key=text "source" entry ^ "\000" ^ text "group" entry in
    if Hash_set.mem seen key then false else (Hash_set.add seen key;true)) |> list
let home_begin entries more =
  set "ef-reader-home--entries" entries;
  let queue=if more then List.filter (values entries) ~f:(fun entry -> truth (find (get "ef-reader-home--cursors") (entry_key entry))) else values entries in
  set "ef-reader-home--queue" (list queue);set "ef-reader-home--total" (i (List.length queue));
  run "ef-reader-home-load-cache" [];home_pump more;
  if more && List.is_empty queue then message "No further pages in these feeds"
let home_refresh more =
  if more && flag "ef-reader-home--jobs" then user_error "Wait for the current feed refresh before loading more";
  home_cancel ();set "ef-reader-home--done" (i 0);set "ef-reader-home--errors" nil;
  if more then home_begin (get "ef-reader-home--entries") true else (
    run "ef-reader-load-config" [];
    let target=buffer () and generation=get "ef-reader-home--token" in
    let providers=List.filter_map (Reader_backend.all ()) ~f:(fun backend ->
      Option.map backend.followed ~f:(fun followed -> backend,followed)) in
    let remaining=ref (List.length providers) and entries=ref [] in
    let complete () =
      Int.decr remaining;
      if !remaining=0 then home_begin (home_merge (list !entries)) false in
    if List.is_empty providers then home_begin (home_merge nil) false;
    List.iter providers ~f:(fun (backend,followed) ->
      let failed reason =
        home_error (s (backend.name^" subscriptions")) reason;
        entries:=List.filter (values (get "ef-reader-home--entries")) ~f:(fun entry -> backend.matches (text "source" entry)) @ !entries in
      try
        let id=followed () in
        push_var "ef-reader-home--jobs" id;
        put (get "ef-reader--requests") id (callback_unit (fun data ->
          if home_current target generation && String.equal (text "kind" data) "entries" then
            entries:=values (field "rows" data) @ !entries));
        put (get "ef-reader--completions") id (callback_unit (fun event ->
          if home_current target generation then in_buffer target (fun () ->
            remove_job id;
            if not (Value.eq (call "nth" [i 3;event]) (sym "completed")) then failed (call "nth" [i 4;event]);
            complete ())))
      with exn -> failed (s (Exn.to_string exn));complete ()))
let columns fields = vector (List.map fields ~f:(fun (name,width,sort) -> list [s name;i width;b sort]))
let home_open () =
  ignore (show_buffer "*Reader*" : Buffer.t);
  if flag "ef-reader--home" then home_cancel ();
  run "ef-reader-feed-mode" [];
  set "ef-reader--home" (b true);set "ef-reader-home--cursors" (hash ());
  set "tabulated-list-format" (columns ["New",3,false;"Time",12,false;"Feed",28,true;"Author",18,true;"Thread",70,true]);
  run "tabulated-list-init-header" [];home_header ();
  run "add-hook" [sym "kill-buffer-hook";sym "ef-reader-home--cancel";nil;b true];home_refresh false
let reader entry =
  run "ef-reader-start" [];run "ef-reader-load-config" [];
  if not (truth entry) then home_open () else (
    ignore (show_buffer "*Reader*" : Buffer.t);
    if flag "ef-reader--home" then home_cancel ();
    run "ef-reader-feed-mode" [];
    set "ef-reader--entry" entry;set "ef-reader--source" (field "source" entry);
    run "ef-reader--load-list" [])
let browse_entry entry = run "camulus" [entry];run "ef-reader--load-list" [b true]
let button label action = run "insert-text-button" [s label;sym "follow-link";b true;sym "action";callback_unit (fun _ -> action ())]
let groups () =
  run "ef-reader-load-config" [];
  ignore (show_buffer "*Reader groups*" : Buffer.t);run "ef-reader-groups-mode" [];
  writable (fun () ->
    Current_buffer.erase ();
    Point.insert "Browse groups\n\nOpening a group does not subscribe to it or add it to Home.\n\n";
    List.iter (Reader_backend.all ()) ~f:(fun backend ->
      Option.iter backend.browser ~f:(fun (label,command) ->
        button (label^"…") (fun () -> run "call-interactively" [sym command]);Point.insert "   "));
    Point.insert "\n\n";
    List.iter (Reader_backend.all ()) ~f:(fun backend ->
      List.iter (backend.entrances ()) ~f:(fun entrance -> button entrance.label entrance.open_;Point.insert "\n"));
    let entries=values (get "ef-reader-entries") in
    let sources=List.map entries ~f:(field "source") |> List.stable_dedup ~compare:(fun a b -> String.compare (str a) (str b)) in
    List.iter sources ~f:(fun source ->
      Point.insert "\n";insert_value (styled (str source) "bold");Point.insert "\n";
      List.iter entries ~f:(fun entry -> if Value.equal source (field "source" entry) then (
        Point.insert "  ";button (text "name" entry) (fun () -> browse_entry entry);Point.insert ("  · "^text "group" entry^"\n"))));
    if List.is_empty entries then Point.insert "\nNo configured groups yet. Press C to add browsing entries.\n";
    Point.goto_min ());
  Current_buffer.set_modified false
let browse_source () =
  if not (flag "ef-reader--home") then user_error "Already browsing an entry";
  let entry=home_entry (selected ()) in
  if not (truth entry) then user_error "No source entry available";
  reader entry
let home_errors () =
  if not (flag "ef-reader--home") then user_error "Open the home timeline first";
  let errors=values (get "ef-reader-home--errors") in
  if List.is_empty errors then message "No home refresh errors" else (
    let target=Buffer.find_or_create ~name:"*Reader refresh errors*" in
    in_buffer target (fun () -> writable (fun () ->
      Current_buffer.erase ();List.iter (List.rev errors) ~f:(fun error ->
        Point.insert (str (call "car" [error])^": "^str (call "cdr" [error])^"\n\n")));
      run "special-mode" []);
    run "display-buffer" [bv target])


let unique_rows rows =
  let seen=Hash_set.create (module String) in
  List.rev rows |> List.filter ~f:(fun row -> let id=text "id" row in
    if Hash_set.mem seen id then false else (Hash_set.add seen id;true)) |> List.rev
let load_list synchronize more =
  if flag "ef-reader--home" then (if synchronize then home_refresh more else home_cache ()) else (
    let target=buffer () and folder=get "ef-reader--folder" and entry=get "ef-reader--entry" and generation=token () in
    let fields=if truth folder then ["op",s "favorites";"folder",folder]
      else if truth entry then ["op",s (if synchronize then "refresh" else "feed");"source",field "source" entry;
        "group",field "group" entry;"query",get "ef-reader--query";"after",(if more then get "ef-reader--cursor" else nil)] else [] in
    if not (List.is_empty fields) then (
      set "ef-reader--list-token" generation;
      let receive=callback_unit (fun data ->
        if alive target && List.mem ["feed";"favorites"] (text "kind" data) ~equal:String.equal then in_buffer target (fun () ->
          if Value.eq generation (get "ef-reader--list-token") then (
            if synchronize then set "ef-reader--cursor" (field "after" data);
            let rows=field "rows" data in
            let rows=if flag "ef-reader--query" then (
              let rows=if more then list (unique_rows (values (get "ef-reader--search-rows") @ values rows)) else rows in
              set "ef-reader--search-rows" rows;rows) else rows in
            set_rows rows))) in
      set "ef-reader--list-request" (request ~receive fields)))
let reload_lists () =
  List.iter (Buffer.all_live ()) ~f:(fun target -> in_buffer target (fun () ->
    if (derived "ef-reader-feed-mode" || derived "ef-reader-folder-mode") && not (flag "ef-reader--query")
      && not (flag "ef-reader--list-request" && truth (find (get "ef-reader--requests") (get "ef-reader--list-request"))) then load_list false false))
let refresh () =
  load_list true false;
  request_ignore ["op",s "refresh-archives"] ~receive:(callback_unit (fun data ->
    if truth (field "errors" data) then message ("Archive refresh: "^String.concat ~sep:"; " (List.map (values (field "errors" data)) ~f:str))))
let more () =
  if not (flag "ef-reader--home" || flag "ef-reader--cursor") then user_error "No further page";
  load_list true true
let search query =
  if flag "ef-reader--home" then user_error "Search is entry scoped: use G or b to open an entry first";
  set "ef-reader--query" (if String.is_empty query then nil else s query);load_list true false
let entries () =
  run "ef-reader-start" [];run "ef-reader-load-config" [];
  let providers=List.filter_map (Reader_backend.all ()) ~f:(fun backend -> backend.followed) in
  let left=ref (List.length providers) and discovered=ref [] in
  let choose () =
    let configured=values (get "ef-reader-entries") in
    let choices=configured @ List.filter !discovered ~f:(fun r -> not (List.exists configured ~f:(fun e ->
      Value.equal (field "name" e) (field "name" r) || Value.equal (entry_key e) (entry_key r)))) in
    if List.is_empty choices then user_error "No entries; use C to configure one";
    let chosen=call "completing-read" [s "Entry: ";list (List.map choices ~f:(field "name"));nil;b true] in
    Option.iter (List.find choices ~f:(fun entry -> Value.equal (field "name" entry) chosen)) ~f:reader in
  if List.is_empty providers then choose ();
  List.iter providers ~f:(fun provider ->
    let id=provider () in
    put (get "ef-reader--requests") id (callback_unit (fun data ->
      if String.equal (text "kind" data) "entries" then discovered:=values (field "rows" data) @ !discovered));
    put (get "ef-reader--completions") id (callback_unit (fun _ -> Int.decr left;if !left=0 then choose ())))
let delete () =
  let row=selected () in
  if derived "ef-reader-feed-mode" then (
    if flag "ef-reader--query" then user_error "Remove updates from the entry timeline, not search results";
    request_ignore ["op",s (if flag "ef-reader--home" then "dismiss-timeline" else "dismiss");"group",field "group" (get "ef-reader--entry");
      "source",field "source" row;"id",field "id" row;"revision",field "revision" row])
  else if derived "ef-reader-folder-mode" then
    request_ignore ["op",s "unfavorite";"source",field "source" row;"id",field "id" row;"target",field "target" row;"folder",get "ef-reader--folder"]
  else user_error "Use d in the feed or a favorites folder"
let save () =
  let row=call "copy-tree" [selected ()] in
  request_ignore ["op",s "folders"] ~receive:(callback_unit (fun data ->
    if String.equal (text "kind" data) "folders" then (
      let folder=call "completing-read" [s "Save to folder (new parent/child allowed): ";field "rows" data;nil;nil] in
      if not (String.is_empty (str folder)) then (
        request_ignore ["op",s "favorite";"source",field "source" row;"id",field "id" row;"target",or_ (field "target" row) (s "thread");"folder",folder];
        message "Downloading archive; it appears in the folder after saving succeeds"))))
let folders () =
  let origin=Selected_window.get () in
  request_ignore ["op",s "folders"] ~receive:(callback_unit (fun data ->
    if String.equal (text "kind" data) "folders" && Window.is_live origin then (
      let folder=call "completing-read" [s "Folder: ";field "rows" data;nil;b true] in
      if not (String.is_empty (str folder)) then (
        Selected_window.set origin;ignore (show_buffer ("*Reader folder: "^str folder^"*") : Buffer.t);
        run "ef-reader-folder-mode" [];set "ef-reader--folder" folder;load_list false false))))
let delete_folder () =
  if not (flag "ef-reader--folder") then user_error "Open a favorites folder first";
  request_ignore ["op",s "remove-folder";"folder",get "ef-reader--folder"]
    ~receive:(callback_unit (fun data -> if String.equal (text "kind" data) "changed" then reader nil))
let subscription () =
  let source=if flag "ef-reader--home" then field "source" (selected ()) else get "ef-reader--source" in
  let target=Option.bind (capability source) ~f:(fun backend ->
    Option.bind backend.subscription_target ~f:(fun target -> target ~source:(str source) ~selected:(fun () -> selected ()))) in
  match target with
  | None -> user_error "This backend does not offer subscription settings"
  | Some target ->
    let id=i (Application.request_subscription (str source) target None) in
    put (get "ef-reader--requests") id (callback_unit (fun data ->
      if String.equal (text "kind" data) "subscription" then (
        let current=field "setting" data in
        let choice=call "completing-read" [s ("Following policy (currently "^str current^"): ");field "choices" data;nil;b true;nil;nil;current] in
        if not (Value.equal choice current) then (
          let id=i (Application.request_subscription (str source) target (Some(str choice))) in
          put (get "ef-reader--requests") id (callback_unit (fun result ->
            if String.equal (text "kind" result) "subscription" then message ("Following policy: "^text "setting" result)))))))
let attachment button =
  let prop name=call "button-get" [button;sym name] in
  let source=prop "source" and id=prop "message-id" and part=prop "part" in
  let destination=call "read-file-name" [s "Save attachment: ";nil;nil;nil;call "file-name-nondirectory" [prop "filename"]] in
  if truth (prop "archived") then
    request_ignore ["op",s "attachment";"source",source;"id",id;"part",part;"destination",destination]
  else match Option.bind (capability source) ~f:(fun backend -> backend.attachment) with
    | None -> user_error "This backend does not offer attachment downloads"
    | Some download ->
      let group=field "group" (get "ef-reader--entry") in
      let id=download ~source:(str source) ~id:(str id) ~part:(number part) ~destination:(str destination)
        ~group:(if truth group then Some (str group) else None) in
      put (get "ef-reader--requests") id (sym "ignore")
let mark_visible target =
  if alive target && not (String.equal (str (call "ef-probe-status" [])) "idle") then in_buffer target (fun () ->
    set "ef-reader--visibility-timer" nil;
    let ids=ref [] in
    List.iter (values (call "get-buffer-window-list" [bv target;nil;b true])) ~f:(fun window ->
      let p=ref (number (call "window-start" [window])) and finish=call "window-end" [window;b true] in
      if truth finish then while !p < number finish do
        let id=property !p "ef-reader-id" in
        let key=list [get "ef-reader--source";id] and read=state (get "ef-reader--source") id in
        if truth id && truth read && truth (find (get "ef-reader--loaded") id) && not (truth (field "read" read))
           && not (truth (find (get "ef-reader--reading") key)) && truth (call "pos-visible-in-window-p" [i !p;window])
           && not (member id !ids) then ids:=id :: !ids;
        p:=next_property !p "ef-reader-id" (number finish)
      done);
    if not (List.is_empty !ids) then (
      let id=request ["op",s "read";"source",get "ef-reader--source";"ids",vector !ids;"value",b true] in
      List.iter !ids ~f:(fun content -> put (get "ef-reader--reading") (list [get "ef-reader--source";content]) id)))
let schedule_visible target =
  if alive target then in_buffer target (fun () ->
    if derived "ef-reader-article-mode" && not (truth (call "timerp" [get "ef-reader--visibility-timer"])) then
      set "ef-reader--visibility-timer" (call "run-at-time" [call "string-to-number" [s "0.1"];nil;sym "ef-reader--mark-visible";bv target]))

let receive data =
  let source=get "ef-reader--source" in
  let backend=capability source in
  let reconcile ~existing m = Option.value_map backend ~default:m ~f:(fun backend ->
    backend.reconcile ~source:(str source) ~existing ~request:(get "ef-reader--active") ~archived:(flag "ef-reader--archive") m) in
  match text "kind" data with
  | "page" ->
    (match Option.bind backend ~f:(fun backend -> backend.page) with
     | Some page -> page data
     | None -> set_rows (list (List.map (values (field "messages" data)) ~f:(fun m -> alist [
         "source",source;"target",s (if single source then "article" else "thread");"id",field "id" m;
         "title",field "title" m;"author",field "author" m;"unread",b (not (truth (field "read" (state source (field "id" m)))))]))))
  | "thread" ->
    List.iter (values (field "messages" data)) ~f:(fun m ->
      let id=field "id" m in
      let existing=find (get "ef-reader--messages") id in
      let m=if truth existing then reconcile ~existing m else m in
      put (get "ef-reader--messages") id m;
      if Option.exists backend ~f:(fun backend -> backend.thread_contains_bodies) then put (get "ef-reader--loaded") id (b true));
    set "ef-reader--complete" (field "complete" data);render ()
  | "article" ->
    if single source then set "ef-reader--complete" (b true);
    let m=reconcile ~existing:nil (field "message" data) in
    let id=field "id" m in
    put (get "ef-reader--messages") id m;put (get "ef-reader--loaded") id (b true);
    put (get "ef-reader--attachments") id (field "attachments" data);render ()
  | "error" -> message (text "message" data)
  | _ -> ()
let event event =
  let nth n=call "nth" [i n;event] in
  let id=nth 1 in
  let tag=str (call "symbol-name" [nth 0]) in
  match tag with
  | "content-state" ->
    let data=nth 3 in
    (match text "kind" data with
     | "states" ->
       let states=field "data" data in
       let source=field "source" states in
       List.iter (values (field "items" states)) ~f:(fun read -> put (get "ef-reader--states") (list [source;field "id" read]) read);
       List.iter (Buffer.all_live ()) ~f:(fun target -> in_buffer target (fun () ->
         if derived "ef-reader-article-mode" && Value.equal source (get "ef-reader--source") && count (get "ef-reader--messages") > 0 then render ()))
     | "changed" -> reload_lists ()
     | "saved" -> message ("Saved: "^str (or_ (field "folder" data) (field "path" data)))
     | _ -> ());
    let callback=find (get "ef-reader--requests") id in
    if truth callback then ignore (Value.funcallN callback [data] : Value.t)
  | "finished" ->
    if (truth (find (get "ef-reader--requests") id) || truth (find (get "ef-reader--loads") id)) && not (Value.eq (nth 3) (sym "completed")) then
      message ("Reader: "^str (nth 4));
    remove (get "ef-reader--requests") id;remove (get "ef-reader--loads") id;
    let callback=find (get "ef-reader--completions") id in
    if truth callback then (remove (get "ef-reader--completions") id;ignore (Value.funcallN callback [event] : Value.t));
    List.iter (values (call "hash-table-keys" [get "ef-reader--reading"])) ~f:(fun key ->
      if Value.eq (find (get "ef-reader--reading") key) id then remove (get "ef-reader--reading") key)
  | tag when List.exists (Reader_backend.all ()) ~f:(fun backend -> List.mem backend.events tag ~equal:String.equal) ->
    let data=nth 3 in
    if truth (find (get "ef-reader--requests") id) && String.equal (text "kind" data) "saved" then
      message ("Saved: "^text "path" data);
    let value=find (get "ef-reader--loads") id in
    if truth value then let target=Buffer.of_value_exn value in
      if alive target then in_buffer target (fun () -> if Value.eq id (get "ef-reader--active") then receive data)
  | _ -> ()
(* Registration is explicit, after backend adapters/configuration have loaded. *)
let define0 ?(interactive=false) name f =
  Defun.defun_nullary_nil (Symbol.intern name) [%here] ~docstring:"Native Ecaml reader command."
    ?interactive:(if interactive then Some Defun.Interactive.No_arg else None) f
let define1 name f =
  Defun.defun (Symbol.intern name) [%here] ~docstring:"Native Ecaml reader operation." (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open argument=required "argument" value in f argument)
let define2 name f =
  Defun.defun (Symbol.intern name) [%here] ~docstring:"Native Ecaml reader operation." (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open a=required "first" value and b=required "second" value in f a b)
let define_optional ?(interactive=false) name f =
  Defun.defun (Symbol.intern name) [%here] ~docstring:"Native Ecaml reader command."
    ?interactive:(if interactive then Some Defun.Interactive.No_arg else None) (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open argument=optional_with_nil "argument" value in f argument)

let local_default name default =
  if not (Current_buffer.variable_is_defined (Symbol.intern name)) then set name default;
  run "make-variable-buffer-local" [sym name]
let mode name parent title columns_ header init =
  let initialize () =
    Option.iter columns_ ~f:(fun cols -> set "tabulated-list-format" (columns cols));
    local "header-line-format" (s header);
    if Option.is_some columns_ then run "tabulated-list-init-header" [];
    init () in
  ignore (Major_mode.define_derived_mode (Symbol.intern name) [%here] ~docstring:"Native Ecaml reader view."
    ~parent:(Major_mode.find_or_wrap_existing [%here] (Symbol.intern parent)) ~mode_line:title
    ~initialize:(Returns Value.Type.unit,initialize) () : (module Major_mode.S))
let bind mode_name bindings =
  let map=Keymap.of_value_exn (get (mode_name^"-map")) in
  List.iter bindings ~f:(fun (key,command) -> run "define-key" [Keymap.to_value map;call "kbd" [s key];sym command])
let install_modes () =
  run "require" [sym "tabulated-list"];
  List.iter ["entries";"cursors";"token";"jobs";"queue";"errors"] ~f:(fun name -> local_default ("ef-reader-home--"^name) nil);
  List.iter ["done";"total"] ~f:(fun name -> local_default ("ef-reader-home--"^name) (i 0));
  mode "ef-reader-feed-mode" "tabulated-list-mode" "Feed"
    (Some ["New",3,false;"Source",24,true;"Author",20,true;"Thread",70,true])
    "RET: read  d: remove this update  m: save to folder  g: refresh  n: more  /: search  s: follow settings  G: entries  C: config  f: folders" ignore;
  mode "ef-reader-folder-mode" "tabulated-list-mode" "Favorites"
    (Some ["Kind",8,false;"Source",24,true;"Title",80,true])
    "RET: read saved copy  m: add folder  d: remove entry  D: remove folder  f: folders  g: reload  h: home" ignore;
  mode "ef-reader-groups-mode" "special-mode" "Reader groups" None
    "Groups · RET / click: browse  TAB: next entry  h: home  g: reload  C: configure" ignore;
  mode "ef-reader-article-mode" "special-mode" "Reader thread" None "Loading…" (fun () ->
    reading_layout ();
    List.iter ["messages";"attachments";"loaded"] ~f:(fun name -> set ("ef-reader--"^name) (hash ()));
    run "add-hook" [sym "kill-buffer-hook";callback0 (fun () ->
      if truth (call "timerp" [get "ef-reader--visibility-timer"]) then run "cancel-timer" [get "ef-reader--visibility-timer"];
      if flag "ef-reader--active" && not (String.equal (str (call "ef-probe-status" [])) "idle") then run "ef-probe-cancel" [get "ef-reader--active"]);nil;b true]);
  List.iter ["ef-reader-feed-mode";"ef-reader-folder-mode"] ~f:(fun mode ->
    bind mode ["RET","ef-reader-open";"d","ef-reader-delete";"m","ef-reader-save";"g","ef-reader-refresh";"f","ef-reader-folders";"h","camulus"]);
  bind "ef-reader-feed-mode" ["s","ef-reader-subscription";"G","ef-reader-entries";"C","ef-reader-edit-config";"n","ef-reader-more";"/","ef-reader-search";
    "B","ef-reader-browse-groups";"b","ef-reader-home-browse-source";"e","ef-reader-home-errors"];
  bind "ef-reader-folder-mode" ["D","ef-reader-delete-folder"];
  bind "ef-reader-article-mode" ["s","ef-reader-subscription";"m","ef-reader-save";"n","ef-reader-next-unread";"h","camulus"];
  bind "ef-reader-groups-mode" ["h","camulus";"g","ef-reader-browse-groups";"C","ef-reader-edit-config"]


let installed = ref false
let install () =
  if not !installed then (
  define1 "ef-reader--set-rows" (fun rows -> set_rows rows;nil);
  define0 "ef-reader--reading-layout" reading_layout;
  define1 "ef-reader--header-text" (fun value -> s (header_text value));
  define1 "ef-reader--shr-code" (fun dom -> shr_code dom;nil);
  define1 "ef-reader--anchor" (fun p -> anchor (number p));
  define1 "ef-reader--position" position;
  Defun.defun_nullary (Symbol.intern "ef-reader--selected") [%here] ~docstring:"Selected content." (Returns Value.Type.value) selected;
  define0 "ef-reader--render" render;
  define0 ~interactive:true "ef-reader-open" open_article;
  define0 ~interactive:true "ef-reader-next-unread" next_unread;
  define1 "ef-reader-home--key" entry_key;
  define1 "ef-reader-home-row-entry" home_entry;
  define1 "ef-reader-home-row" home_row;
  define0 "ef-reader-home--header" home_header;
  define0 "ef-reader-home-load-cache" home_cache;
  define0 "ef-reader-home--cancel" home_cancel;
  define1 "ef-reader-home--pump" (fun more -> home_pump (truth more);nil);
  define2 "ef-reader-home--begin" (fun entries more -> home_begin entries (truth more);nil);
  define1 "ef-reader-home--merge-entries" home_merge;
  define_optional "ef-reader-home-refresh" (fun more -> home_refresh (truth more);nil);
  define0 "ef-reader-home-open" home_open;
  define0 ~interactive:true "ef-reader-home-browse-source" browse_source;
  define0 ~interactive:true "ef-reader-home-errors" home_errors;
  define0 ~interactive:true "ef-reader-browse-groups" groups;
  define1 "ef-reader-browse-entry" (fun entry -> browse_entry entry;nil);
  define_optional ~interactive:true "camulus" (fun entry -> reader entry;nil);
  define1 "ef-reader--single-article-source-p" (fun source -> b (single source));
  define2 "ef-reader--state" state;
  define0 "ef-reader--reload-lists" reload_lists;
  define0 ~interactive:true "ef-reader-refresh" refresh;
  define0 ~interactive:true "ef-reader-more" more;
  define0 ~interactive:true "ef-reader-entries" entries;
  define0 ~interactive:true "ef-reader-delete" delete;
  define0 ~interactive:true "ef-reader-save" save;
  define0 ~interactive:true "ef-reader-folders" folders;
  define0 ~interactive:true "ef-reader-delete-folder" delete_folder;
  define0 ~interactive:true "ef-reader-subscription" subscription;
  define1 "ef-reader--attachment" (fun button -> attachment button;nil);
  define2 "ef-reader--window-scrolled" (fun window _ -> schedule_visible (Window.buffer_exn (Window.of_value_exn window));nil);
  define1 "ef-reader--window-changed" (fun window -> schedule_visible (Window.buffer_exn (Window.of_value_exn window));nil);
  define1 "ef-reader--receive" (fun data -> receive data;nil);
  define1 "ef-reader--mark-visible" (fun target -> mark_visible (Buffer.of_value_exn target);nil);
  define1 "ef-reader--schedule-visible" (fun target -> schedule_visible (Buffer.of_value_exn target);nil);
  Defun.defun (Symbol.intern "ef-reader-search") [%here] ~docstring:"Search only this group."
    ~interactive:(Prompt "sSearch this entry (empty clears): ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open query=required "query" string in search query);
  Defun.defun (Symbol.intern "ef-reader--load-list") [%here] ~docstring:"Refresh a native list view."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open synchronize=optional_with_nil "synchronize" value
     and more=optional_with_nil "more" value in load_list (truth synchronize) (truth more));
  install_modes ();
  run "ef-reader-backends-install" [];
  run "add-hook" [sym "window-scroll-functions";sym "ef-reader--window-scrolled"];
  run "add-hook" [sym "window-buffer-change-functions";sym "ef-reader--window-changed"];
  installed := true)

let register () = define0 "ef-reader-ui-install" install
