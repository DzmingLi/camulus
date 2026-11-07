open! Core
open! Ecaml
open Reader_backend

type t = {buffer:Value.t;previous:Value.t;mutable closed:bool;on_close:unit->unit}
let available () =
  truth(call "display-graphic-p" []) &&
  truth(call "require" [sym "webkit-module";nil;sym "t"])
let closed view =
  if not view.closed then (view.closed<-true;view.on_close ())
let close view =
  closed view;
  if live view.buffer then begin
    let selected=Value.eq (call "window-buffer" [call "selected-window" []]) view.buffer in
    run "kill-buffer" [view.buffer];
    if selected && live view.previous then run "switch-to-buffer" [view.previous]
  end
let origin url =
  let u=Uri.of_string url in
  match Uri.scheme u,Uri.host u,Uri.userinfo u with
  | Some("https" as scheme),Some host,None
  | Some("http" as scheme),Some host,None ->
    scheme,host,Option.value (Uri.port u) ~default:(if String.equal scheme "https" then 443 else 80)
  | _->invalid_arg "WebKit page requires an HTTP(S) URL without user information"
let open_page ~title ~url ~channel ~javascript ~on_message ~on_close ~on_error =
  let expected_origin=origin url in
  if String.is_empty channel || not(String.for_all channel ~f:(function
    | 'a'..'z'|'A'..'Z'|'0'..'9'|'_'->true|_->false)) then invalid_arg "Invalid WebKit message channel";
  if not(available ()) || not(truth(call "require" [sym "webkit";nil;sym "t"])) then
    failwith "此功能需要图形 Emacs 和 emacs-webkit";
  let previous=call "current-buffer" [] in
  let variables=["webkit-cookie-file";"webkit-new-hook";"webkit-mode-hook";"webkit-own-window"] in
  let saved=List.map variables ~f:(fun name->name,get name) in
  let buffer=Exn.protect ~finally:(fun()->List.iter saved ~f:(fun(name,v)->set name v)) ~f:(fun()->
    List.iter variables ~f:(fun name->set name nil);
    call "webkit-new" [nil;s title;sym "t"]) in
  let view={buffer;previous;closed=false;on_close} in
  try
    in_buffer buffer (fun()->
      let widget=get "webkit--id" in
      let injected=ref false and pending=ref "" and failed=ref false in
      let fail reason=if not !failed then (failed:=true;on_error reason) in
      let same_origin ()=try Poly.equal expected_origin (origin(text(call "webkit--get-uri" [widget]))) with _->false in
      let receive bytes =
        if not view.closed && not !failed then begin
          pending:= !pending^bytes;
          if String.length !pending>65536 then (pending:="";fail "WebKit 消息过大")
          else begin
            let handle name payload =
              if view.closed || !failed then ()
              else if String.equal name channel then begin
                if !injected && same_origin () then on_message payload
                else fail "WebKit 消息来自未授权页面"
              end else match name with
              | "webkit--callback-unfocus"->run "webkit--unfocus" [widget]
              | "webkit--load-finished"->
                if not(same_origin ()) then fail "WebKit 页面离开了允许的站点"
                else if not !injected then begin
                  injected:=true;run "webkit--execute-js" [widget;s javascript]
                end
              | _->() in
            let rec packets start =
              match String.index_from !pending start '\000' with
              | None->String.drop_prefix !pending start
              | Some middle->(match String.index_from !pending (middle+1) '\000' with
                | None->String.drop_prefix !pending start
                | Some stop->
                  handle (String.sub !pending ~pos:start ~len:(middle-start))
                    (String.sub !pending ~pos:(middle+1) ~len:(stop-middle-1));
                  packets(stop+1)) in
            pending:=packets 0
          end
        end in
      let filter=Function.to_value(Defun.lambda [%here](Returns Value.Type.unit)
        (let open Defun.Let_syntax in let%map_open _process=required "process" value
          and bytes=required "bytes" string in
          try receive bytes with _->fail "WebKit 消息处理失败")) in
      (* Never intern page-controlled names or insert private messages into a
         process buffer. Only the declared channel and fixed UI events run. *)
      run "set-process-filter" [call "get-buffer-process" [buffer];filter];
      run "use-local-map" [call "copy-keymap" [call "current-local-map" []]];
      run "define-key" [call "current-local-map" [];call "kbd" [s "q"];
        Function.to_value(Defun.lambda_nullary [%here] ~interactive:No_arg
          (Returns Value.Type.unit)(fun()->close view))];
      run "add-hook" [sym "kill-buffer-hook";
        Function.to_value(Defun.lambda_nullary [%here](Returns Value.Type.unit)(fun()->closed view));nil;sym "t"];
      run "webkit--register-script-message" [widget;s channel];
      run "webkit--enable-javascript" [widget;sym "t"];
      run "webkit--load-uri" [widget;s url]);
    view
  with exn->close view;raise exn
