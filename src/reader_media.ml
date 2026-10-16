(* A real GTK WebKit view is placed over a reserved text glyph. Unlike the
   offscreen xwidget view, it can play media on PGTK. Everything runs on the
   Emacs owner thread; a redraw moves existing players without reloading them. *)
open! Core
open! Ecaml
let nil=Value.nil
let sym=Value.intern
let s=Value.of_utf8_bytes
let i=Value.of_int_exn
let call name args=Value.funcallN (sym name) args
let run name args=ignore(call name args:Value.t)
let truth v=not(Value.eq v nil)
let str v=if truth v then Value.to_utf8_bytes_exn v else ""
let number v=Value.to_int_exn v
let list=Value.list
let values v=Value.to_list_exn v ~f:Fn.id
let bv=Buffer.to_value
let current ()=Current_buffer.get ()
let in_buffer buffer f=Current_buffer.set_temporarily Sync buffer ~f
let writable f=Current_buffer.set_value_temporarily Sync
  (Var.create (Symbol.intern "inhibit-read-only") Value.Type.bool) true ~f
type media={key:string;url:string;poster:string;mutable marker:Value.t;
  preferred:int;
  mutable width:int;mutable height:int;mutable ratio:float;mutable widget:Value.t;
  mutable process:Value.t;mutable pending:string;mutable rectangle:Value.t;
  mutable frame:Value.t;
  mutable failed:bool}
type view={buffer:Buffer.t;items:media String.Table.t;seen:String.Hash_set.t}
let views=ref []
let view ()=
  let buffer=current () in
  match List.find !views ~f:(fun v->Value.eq (bv v.buffer) (bv buffer)) with
  | Some v->v
  | None->let v={buffer;items=String.Table.create ();seen=String.Hash_set.create ()} in views:=v::!views;v
let release m=
  if truth m.widget then (run "webkit--hide" [m.widget];run "webkit--destroy" [m.widget];m.widget<-nil);
  if truth m.process && truth(call "process-live-p" [m.process]) then run "delete-process" [m.process];
  m.process<-nil;m.frame<-nil;m.rectangle<-nil
let destroy m=release m;run "set-marker" [m.marker;nil]
let cleanup ()=
  let buffer=current () in
  List.iter !views ~f:(fun v->if Value.eq (bv v.buffer) (bv buffer) then Hashtbl.iter v.items ~f:destroy);
  views:=List.filter !views ~f:(fun v->not(Value.eq (bv v.buffer) (bv buffer)))
let begin_render ()=
  let v=view () in Hash_set.clear v.seen;
  Hashtbl.iter v.items ~f:(fun m->if truth m.widget then run "webkit--hide" [m.widget];m.rectangle<-nil)
let finish_render ()=
  let v=view () in
  Hashtbl.filter_inplace v.items ~f:(fun m->
    if Hash_set.mem v.seen m.key then true else (destroy m;false))
let glyph m=list [sym "space";sym ":width";list[i m.width];sym ":height";list[i m.height];sym ":ascent";i 100]
let update_glyph m=
  if truth(call "marker-buffer" [m.marker]) then writable(fun ()->
    let start=number(call "marker-position" [m.marker]) in
    run "put-text-property" [i start;i(start+1);sym "display";glyph m])
let receive v m bytes=
  m.pending<-m.pending^bytes;
  let rec packets start=
    match String.index_from m.pending start '\000' with
    | None->String.drop_prefix m.pending start
    | Some middle->(match String.index_from m.pending (middle+1) '\000' with
      | None->String.drop_prefix m.pending start
      | Some stop->
        let name=String.sub m.pending ~pos:start ~len:(middle-start) in
        let payload=String.sub m.pending ~pos:(middle+1) ~len:(stop-middle-1) in
        (* Only our fixed metadata message is accepted. Never intern or execute
           the names sent through the browser module's event channel. *)
        if String.equal name "camulusMedia" && Buffer.is_live v.buffer
          && Option.exists(Hashtbl.find v.items m.key) ~f:(phys_equal m) then (
          try
            let data=Yojson.Safe.from_string payload in
            let open Yojson.Safe.Util in
            let width=data |> member "width" |> to_int and height=data |> member "height" |> to_int in
            if width>0 && height>0 then (
              m.ratio<-Float.of_int height /. Float.of_int width;
              m.height<-Int.max 60 (Int.min 1200 (Float.to_int(Float.of_int m.width *. m.ratio)));
              in_buffer v.buffer (fun ()->update_glyph m);
              m.rectangle<-nil;
              run "force-window-update" [bv v.buffer];
              run "run-at-time" [i 0;nil;sym "camulus--media-layout"])
          with _->());
        packets (stop+1)) in
  m.pending<-packets 0;
  if String.length m.pending>65536 then m.pending<-""
let document m=
  let escape=Reader.Html.escape in
  Printf.sprintf "<!doctype html><html><head><meta charset='utf-8'><style>html,body{margin:0;padding:0;background:#111;scrollbar-width:none}body{width:%dpx;height:%dpx}video{display:block;object-fit:contain;width:100%%;height:100%%}</style></head><body><video controls playsinline preload='metadata'" m.width m.height^
  (if String.is_empty m.poster then "" else " poster=\""^escape m.poster^"\"")^
  " src=\""^escape m.url^"\"></video><script>const v=document.querySelector('video');v.addEventListener('loadedmetadata',()=>window.webkit.messageHandlers.camulusMedia.postMessage({width:v.videoWidth,height:v.videoHeight}));</script></body></html>"
let frame_id frame=
  let id=call "string-to-number" [call "frame-parameter" [frame;sym "window-id"]] in
  if Value.eq (call "window-system" [frame]) (sym "pgtk") then id else call "webkit--xid-to-pointer" [id]
let create v m frame=
  let filter=Function.to_value(Defun.lambda [%here] (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open _process=required "process" value and bytes=required "bytes" string in receive v m bytes)) in
  m.process<-call "make-pipe-process" [sym ":name";s "camulus-video";sym ":noquery";sym "t";sym ":filter";filter];
  m.widget<-call "webkit--new" [m.process;nil;frame_id frame];
  m.frame<-frame;
  run "webkit--hide" [m.widget];
  run "webkit--register-script-message" [m.widget;s "camulusMedia"];
  let html=call "encode-coding-string" [s(document m);sym "utf-8";sym "t"] in
  let uri="data:text/html;base64,"^str(call "base64-encode-string" [html;sym "t"]) in
  run "webkit--load-uri" [m.widget;s uri]
let layout ()=
  List.iter !views ~f:(fun v->if Buffer.is_live v.buffer then in_buffer v.buffer(fun ()->
    let windows=values(call "get-buffer-window-list" [bv v.buffer;nil;sym "t"]) in
    Hashtbl.iter v.items ~f:(fun m->
      let visible=List.find_map windows ~f:(fun window->
        let frame=call "window-frame" [window] in
        if not(truth(call "display-graphic-p" [frame])) || not(truth(call "frame-visible-p" [frame])) then None
        else let position=call "pos-visible-in-window-p" [m.marker;window;sym "t"] in
          if not(truth position) then None else Some(window,frame,position)) in
      match visible with
      | None->if truth m.widget then run "webkit--hide" [m.widget];m.rectangle<-nil
      | Some(window,frame,position) when not m.failed->
        (try
          (* GTK's fixed container uses frame coordinates, not screen coordinates. *)
          let edges=values(call "window-inside-pixel-edges" [window]) |> List.map ~f:number in
          let x=List.nth_exn edges 0+number(call "car" [position])
          and y=List.nth_exn edges 1+number(call "cadr" [position]) in
          let available=Int.max 120 (List.nth_exn edges 2-x) in
          let wanted=Int.min m.preferred available in
          if wanted<>m.width then (
            m.width<-wanted;m.height<-Int.max 60 (Int.min 1200 (Float.to_int(Float.of_int wanted *. m.ratio)));
            update_glyph m;m.rectangle<-nil);
          let left=Int.max x (List.nth_exn edges 0) and top=Int.max y (List.nth_exn edges 1) in
          let width=Int.min (x+m.width) (List.nth_exn edges 2)-left
          and height=Int.min (y+m.height) (List.nth_exn edges 3)-top in
          if width>0 && height>0 then (
            if not(truth m.widget) then create v m frame;
            let rectangle=list[frame;i left;i top;i width;i height;i(left-x);i(top-y);i m.width;i m.height] in
            if not(Value.equal rectangle m.rectangle) then (
              if not(Value.eq frame m.frame) then (run "webkit--move-to-frame" [m.widget;frame_id frame];m.frame<-frame);
              run "webkit--resize" [m.widget;i left;i top;i width;i height];
              run "webkit--execute-js" [m.widget;s(Printf.sprintf
                "document.body.style.width='%dpx';document.body.style.height='%dpx';window.scrollTo(%d,%d);"
                m.width m.height (left-x) (top-y))];
              m.rectangle<-rectangle);
            run "webkit--show" [m.widget])
         with ex->m.failed<-true;if truth m.widget then run "webkit--hide" [m.widget];
           run "message" [s "Inline video: %s";s(Exn.to_string ex)])
      | _->())))
let render ~source ~id ~base ~index ~width dom=
  let attr name node=str(call "alist-get" [sym name;call "cadr" [node]]) in
  let url=let own=attr "src" dom in
    if not(String.is_empty own) then own else
      let sources=values(call "dom-by-tag" [dom;sym "source"]) in
      Option.value_map(List.find sources ~f:(fun n->not(String.is_empty(attr "src" n)))) ~default:"" ~f:(attr "src") in
  match (if String.is_empty url then None else Reader.Html.safe_link ~base "src" url) with
  | None->()
  | Some url->
    let supported=truth(call "display-graphic-p" []) && truth(call "require" [sym "webkit-module";nil;sym "t"]) in
    if supported then (
      let requested=Int.of_string_opt(attr "width" dom) in
      let preferred=Option.value requested ~default:700 |> Int.max 120 |> Int.min 4096 in
      let width=Int.min width preferred |> Int.max 120 in
      let v=view () in let key=String.concat ~sep:"\000" [source;id;Int.to_string index;url] in
      let m=Hashtbl.find_or_add v.items key ~default:(fun ()->
        {key;url;poster=(let poster=attr "poster" dom in if String.is_empty poster then "" else Option.value(Reader.Html.safe_link ~base "src" poster) ~default:"");
         marker=call "make-marker" [];preferred;width;height=width*9/16;ratio=9./.16.;widget=nil;process=nil;pending="";rectangle=nil;frame=nil;failed=false}) in
      Hash_set.add v.seen key;
      m.width<-width;m.height<-Int.max 60 (Int.min 1200 (Float.to_int(Float.of_int width *. m.ratio)));
      run "set-marker" [m.marker;nil];m.marker<-call "point-marker" [];
      Point.insert_text(Text.of_value_exn(call "propertize" [s " ";sym "display";glyph m;sym "camulus-video";s key]));
      Point.insert "\n");
    run "insert-text-button" [s (if supported then "Open video externally" else "Play video");sym "follow-link";sym "t";
      sym "action";Function.to_value(Defun.lambda [%here] (Returns Value.Type.unit)
        (let open Defun.Let_syntax in let%map_open _=required "button" value in run "browse-url" [s url]))];
    Point.insert "\n"
let install ()=
  let hook name callback=
    run "add-hook" [sym name;Function.to_value(Defun.lambda [%here] (Returns Value.Type.unit)
      (let open Defun.Let_syntax in let%map_open args=rest "arguments" value in callback args))] in
  hook "pre-redisplay-functions" (fun _->layout ());
  hook "delete-frame-functions" (fun args->List.iter !views ~f:(fun v->Hashtbl.iter v.items ~f:(fun m->
    if Option.exists(List.hd args) ~f:(Value.eq m.frame) then release m)));
  Defun.defun_nullary_nil (Symbol.intern "camulus--media-layout") [%here] ~docstring:"Position visible inline players." layout
