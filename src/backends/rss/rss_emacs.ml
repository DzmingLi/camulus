open! Core
open! Ecaml
open Reader_backend
let get name = Current_buffer.symbol_value (Symbol.intern name)
let set name value = run "set" [Value.intern name;value]
let refresh submit () =
  let active=get "ef-reader--active" in
  if not (Value.eq active Value.nil) then run "ef-probe-cancel" [active];
  let query=get "ef-reader--rss-query" in
  let url=String.drop_prefix (Value.to_utf8_bytes_exn (get "ef-reader--source")) 4 in
  let operation=if Value.eq query Value.nil then Rss_backend.List url else Search(url,Value.to_utf8_bytes_exn query) in
  let id=Value.of_int_exn (submit operation) in
  set "ef-reader--active" id;
  run "puthash" [id;Buffer.to_value (Current_buffer.get ());get "ef-reader--loads"]
let install ~submit ~followed () =
  Defun.defun (Symbol.intern "ef-reader-rss") [%here] ~docstring:"Browse an RSS or Atom feed without following it."
    ~interactive:(Prompt "sRSS / Atom feed URL: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
     run "ef-reader-start" [];
     run "switch-to-buffer" [Buffer.to_value (Buffer.find_or_create ~name:("*Reader feed: "^url^"*"))];
     run "ef-reader-rss-mode" [];set "ef-reader--source" (s ("rss:"^url));refresh submit ());
  Defun.defun_nullary_nil (Symbol.intern "ef-reader-rss-refresh") [%here] ~docstring:"Refresh this feed."
    ~interactive:No_arg (refresh submit);
  Defun.defun (Symbol.intern "ef-reader-rss-search") [%here] ~docstring:"Search this feed's published entries."
    ~interactive:(Prompt "sSearch this feed (empty clears): ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open query=required "query" string in
     set "ef-reader--rss-query" (if String.is_empty query then Value.nil else s query);refresh submit ());
  register { (base ~name:"RSS / Atom" ~matches:(String.is_prefix ~prefix:"rss:") ~threaded:false
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Rss_backend.Read(String.drop_prefix source 4,id))))) with
    attachment=Some (fun ~source ~id ~group ~part ~destination -> ignore group; Value.of_int_exn (submit (Rss_backend.Attachment(String.drop_prefix source 4,id,part,destination))));
    events=["rss"];subscription_target=Some (fun ~source ~selected:_ -> Some (String.drop_prefix source 4));
    browser=Some ("RSS / Atom", "ef-reader-rss");followed=Some (fun () -> Value.of_int_exn (followed ())) };
  ignore (Major_mode.define_derived_mode (Symbol.intern "ef-reader-rss-mode") [%here]
    ~docstring:"Browse one RSS or Atom feed." ~mode_line:"RSS / Atom"
    ~parent:(Major_mode.find_or_wrap_existing [%here] (Symbol.intern "tabulated-list-mode"))
    ~define_keys:(List.map ["s","ef-reader-subscription";"RET","ef-reader-open";"m","ef-reader-save";
      "g","ef-reader-rss-refresh";"/","ef-reader-rss-search";"h","camulus"] ~f:(fun (key,command)->key,Symbol.intern command))
    ~initialize:(Returns Value.Type.unit,(fun () ->
      run "set" [Value.intern "tabulated-list-format";call "vector" (List.map ["New",3;"Source",24;"Author",20;"Article",70]
        ~f:(fun (name,width)->Value.list [s name;Value.of_int_exn width;Value.intern "t"]))];
      run "set" [Value.intern "header-line-format";s "RET: read  m: save  /: search this feed  g: refresh  h: home"];
      run "tabulated-list-init-header" [])) () : (module Major_mode.S))
