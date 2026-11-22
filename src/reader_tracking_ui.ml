(* A remote subscription editor. Policies and requests belong to each backend;
   this buffer only holds the currently displayed page, never a saved authority. *)
open! Core
open! Ecaml
open Reader_backend

let prefix="camulus-tracking--"
let get_local name=get(prefix^name)
let set_local name value=local(prefix^name)value
let current ()=call "current-buffer" []
let table_put table key value=run "puthash" [key;value;get table]
let managed source=match Option.bind(find source)~f:(fun b->b.topic_tracking)with
 | Some manager->manager | None->user_error "This backend does not offer remote subscription management";assert false
let current_view buffer generation=
 live buffer && in_buffer buffer(fun()->
   truth(call "derived-mode-p" [sym "camulus-tracking-mode"]) && Value.eq generation(get_local "generation"))
let callback f=Function.to_value(Defun.lambda [%here](Returns Value.Type.unit)
 (let open Defun.Let_syntax in let%map_open value=required "value" value in f value))
let watch id receive finished=
 table_put "camulus-reader--requests" id (callback receive);
 table_put "camulus-reader--completions" id (callback finished)
let header ()=
 let manager=managed(text(get "camulus-reader--backend-key")) in
 let filter=text(get_local "filter")in
 let layout=manager.layout filter in
 let label=Option.value(List.Assoc.find manager.filters filter ~equal:String.equal)~default:filter in
 let status=if truth(get_local "error")then "Error: "^text(get_local "error")
   else if truth(get_local "busy")then "Updating…"
   else if truth(get_local "request")then "Loading…"
   else Printf.sprintf "%d %s%s" (List.length(values(get_local "rows"))) layout.units
     (if truth(get_local "after")then " · more available"else "")in
 let title=if String.equal layout.heading label then label else layout.heading^" · "^label in
 let shortcuts=List.map layout.shortcuts ~f:(fun control -> control.hint) in
 local "header-line-format" (s(title^" · "^status^"   "^String.concat ~sep:"  "
   (shortcuts@["RET: open";"s: change policy";"/: filter";"g: refresh";"n: more";"q: quit"])))
let render ?settings ()=
 let selected=call "tabulated-list-get-id" []in
 let table=call "make-hash-table" [sym ":test";sym "equal"]in
 let source=get "camulus-reader--backend-key"in
 let spec=((managed(text source)).layout(text(get_local "filter"))).columns in
 let columns=tracking_fields spec (Option.value_or_thunk settings ~default:(fun () -> get spec.option_name)) in
 let format=call "vector" (List.map columns ~f:(fun (_,label,width) ->
   Value.list[s label;i(Option.value width ~default:0);sym "t"])) in
 if not(Value.equal format (get "tabulated-list-format"))then(
   let sorted=call "car" [get "tabulated-list-sort-key"] in
   if truth sorted && not(List.exists columns ~f:(fun (_,label,_) -> Value.equal sorted (s label)))then
     local "tabulated-list-sort-key" nil;
   local "tabulated-list-format" format;
   run "tabulated-list-init-header" []);
 let last_index=List.length columns-1 in
 let entries=List.map(values(get_local "rows"))~f:(fun row->
   let id=field "id" row in
   let row=call "cons" [call "cons" [sym "backend_key";source];row]in
   run "puthash" [id;row;table];
   Value.list [id;call "vector" (List.mapi columns ~f:(fun at (key,_,width) ->
     let cell=field key row in
     (* Emacs always leaves the final tabulated-list column untruncated. *)
     match width with
     | Some n when at=last_index ->
       let clipped=call "truncate-string-to-width" [cell;i n;nil;nil;sym "t"] in
       if Value.equal cell clipped then cell else call "propertize" [cell;sym "display";clipped]
     | _ -> cell))])in
 local "camulus-reader--rows" table;
 local "tabulated-list-entries"(Value.list entries);
 run "tabulated-list-print" [sym "t"];
 (* tabulated-list-print preserves a surviving row; fall back to the first row. *)
 if not(List.is_empty entries) &&
    (not(truth selected) || not(truth(call "gethash" [selected;table])))then (
   run "goto-char" [call "point-min" []];run "forward-line" [i 1]);
 header ()
let cancel ()=
 let id=get_local "request"in
 if truth id then (
   List.iter ["camulus-reader--requests";"camulus-reader--completions"] ~f:(fun table->run "remhash" [id;get table]);
   ignore(call "camulus-probe-cancel" [id]:Value.t));
 set_local "request" nil;set_local "generation"(call "make-symbol" [s "tracking-view"])
let refresh more=
 if truth(get_local "busy")then user_error "Wait for the tracking change to finish";
 if more && truth(get_local "request")then user_error "Wait for the current page";
 if more && not(truth(get_local "after"))then user_error "No further topics";
 cancel ();
 let buffer=current () and generation=get_local "generation" in
 let source=text(get "camulus-reader--backend-key")and filter=text(get_local "filter")in
 let after=if more then Some(text(get_local "after"))else None in
 set_local "error" nil;
 let received=ref false in
 try
  let id=(managed source).list ~source ~filter ~after in
  set_local "request" id;header ();
  watch id (fun data->if current_view buffer generation && String.equal(text(field "kind" data))"subscription-page"then
    in_buffer buffer(fun()->
      received:=true;
      let rows=(if more then values(get_local "rows")else [])@values(field "rows" data)in
      let seen=Hash_set.create(module String)in
      set_local "rows"(Value.list(List.filter rows ~f:(fun row->
        let id=text(field "id" row)in if Hash_set.mem seen id then false else(Hash_set.add seen id;true))));
      set_local "after"(field "after" data);render ()))
   (fun event->if current_view buffer generation then in_buffer buffer(fun()->
      set_local "request" nil;
      if not(Value.eq(call "nth"[i 2;event])(sym "completed"))then set_local "error"(call "nth"[i 3;event])
      else if not !received then set_local "error"(s "Backend returned no subscription page");
      header ()))
 with exn->set_local "request" nil;set_local "error"(s(Exn.to_string exn));header ();raise exn
let changed source=
 List.iter(Buffer.all_live ())~f:(fun buffer->in_buffer(Buffer.to_value buffer)(fun()->
  if truth(call "derived-mode-p"[sym "camulus-tracking-mode"]) && String.equal(text(get "camulus-reader--backend-key"))source
     && not(truth(get_local "busy")) then refresh false))
let show source filter =
 let manager=managed source in
 let layout=manager.layout filter in
 let default=manager.layout(fst(List.hd_exn manager.filters)) in
 let name=if String.equal layout.heading default.heading then "*Camulus tracking: "^source^"*"
   else "*Camulus "^layout.heading^": "^source^"*" in
 let buffer=Buffer.find_or_create ~name in
 run "switch-to-buffer" [Buffer.to_value buffer];
 if not(truth(call "derived-mode-p"[sym "camulus-tracking-mode"]))then(
   run "camulus-tracking-mode" [];
   local "camulus-reader--backend-key"(s source);
   local "camulus-reader--entry"(alist["backend_key",s source]);
   let map=call "copy-keymap" [get "camulus-tracking-mode-map"] in
   bind map "RET" layout.visit;
   List.iter layout.shortcuts ~f:(fun control -> bind map control.key control.command);
   run "use-local-map" [map];
   local "tabulated-list-use-header-line" nil);
 if truth(get_local "busy")then user_error "Wait for the tracking change to finish";
 if not(Value.equal (get_local "filter") (s filter))then(
   cancel ();set_local "filter"(s filter);set_local "rows" nil;set_local "after" nil;render ());
 refresh false
let filter ()=
 if truth(get_local "busy")then user_error "Wait for the tracking change to finish";
 let filters=(managed(text(get "camulus-reader--backend-key"))).filters in
 let choices=List.map filters~f:(fun(key,label)->key^" · "^label)in
 let choice=text(call "completing-read"[s "Show topics: ";Value.list(List.map choices~f:s);nil;sym "t"])in
 let key=List.find_mapi filters~f:(fun n (key,_)->if String.equal choice(List.nth_exn choices n)then Some key else None)|>Option.value_exn in
 let key=(managed(text(get "camulus-reader--backend-key"))).resolve_filter key in
 show (text(get "camulus-reader--backend-key")) key
let change ~on_changed ~preferred target=
 if truth(get_local "busy")then user_error "A tracking change is already in progress";
 let source=text(get "camulus-reader--backend-key") in
 let target=if truth target then text target else text(field "id" (call "camulus-reader--selected" [])) in
 let manager=managed source and buffer=current ()in
 cancel ();
 let generation=get_local "generation" and selected=ref false in
 set_local "busy"(sym "t");set_local "error" nil;header ();
 let fail reason=if current_view buffer generation then in_buffer buffer(fun()->
   set_local "busy" nil;set_local "error" reason;header ())in
 try
  let id=manager.change ~source ~target ~setting:None in
  watch id (fun data->
    if current_view buffer generation && String.equal(text(field "kind" data))"subscription"then
    try in_buffer buffer(fun()->
      let previous=field "setting" data in
      let choice=call "completing-read"[s("Remote policy (currently "^text previous^"): ");field "choices" data;nil;sym "t";nil;nil;(if truth preferred then preferred else previous)]in
      if current_view buffer generation then (
       selected:=true;
       if Value.equal choice previous then (set_local "busy" nil;refresh false)else(
        let result=ref None in
        let write=manager.change ~source ~target ~setting:(Some(text choice))in
        watch write (fun result_data->if String.equal(text(field "kind"result_data))"subscription"then result:=Some result_data)
          (fun event->
            if Value.eq(call "nth"[i 2;event])(sym "completed")then
              match !result with
              | Some data->
                fail nil;
                changed source;on_changed source;
                message "Remote tracking policy: %s" [field "setting"data]
              | None->fail(s "Backend returned no confirmed tracking state")
            else fail(call "nth"[i 3;event])))))
    with exn->selected:=true;fail(s(Exn.to_string exn));raise exn)
    (fun event->if not !selected then fail(if Value.eq(call "nth"[i 2;event])(sym "completed")
      then s "No tracking policy selected"else call "nth"[i 3;event]))
 with exn->fail(s(Exn.to_string exn));raise exn
let open_ ?(view=nil) requested=
 run "camulus-reader-start" [];
 let sources=configured_groups ()|>List.map ~f:(fun entry->text(field "backend_key"entry))
   |>List.stable_dedup ~compare:String.compare
   |>List.filter ~f:(fun source->Option.exists(find source)~f:(fun b->Option.is_some b.topic_tracking))in
 let source=if truth requested then text requested else
   let entry=call "get-text-property"[call "point"[];sym "camulus-reader-entry"]in
   let candidate=if truth(field "backend_key" entry)then field "backend_key"entry else get "camulus-reader--backend-key"in
   if truth candidate && Option.exists(find(text candidate))~f:(fun b -> Option.is_some b.topic_tracking) then text candidate
   else if List.is_empty sources then(user_error "Register a source with remote subscription management first";assert false)
   else text(call "completing-read"[s "Manage subscriptions for: ";Value.list(List.map sources~f:s);nil;sym "t"])in
 let filter=if truth view then text view else fst(List.hd_exn (managed source).filters) in
 show source filter
let install ~on_changed=
 Defun.defun(Symbol.intern "camulus-tracking--update-columns")[%here]
   ~docstring:"Redraw open tracking tables that use OPTION with the proposed SETTINGS."
   (Returns Value.Type.unit)
   (let open Defun.Let_syntax in let%map_open option=required "option" value
    and settings=required "settings" value in
    List.iter(Buffer.all_live ())~f:(fun buffer -> in_buffer(Buffer.to_value buffer)(fun () ->
      if truth(call "derived-mode-p"[sym "camulus-tracking-mode"]) && truth(get_local "filter")then(
        let layout=(managed(text(get "camulus-reader--backend-key"))).layout(text(get_local "filter")) in
        if Value.eq option (sym layout.columns.option_name)then render ~settings ()))));
 hook "camulus-tracking--columns-changed-hook" "camulus-tracking--update-columns";
 List.iter["filter";"rows";"after";"request";"generation";"busy";"error"]~f:(fun name->
   variable(prefix^name)nil;run "make-variable-buffer-local"[sym(prefix^name)]);
 ignore(Major_mode.define_derived_mode(Symbol.intern "camulus-tracking-mode")[%here]
   ~docstring:"Manage the backend's remote topic tracking state." ~parent:(Major_mode.find_or_wrap_existing [%here](Symbol.intern "tabulated-list-mode"))
   ~mode_line:"Remote tracking"
   ~initialize:(Returns Value.Type.unit,(fun()->
     List.iter["kill-buffer-hook";"change-major-mode-hook"]~f:(fun hook->run "add-hook"[sym hook;lambda0(fun()->cancel ();nil);nil;sym "t"]))) ():(module Major_mode.S));
 Defun.defun(Symbol.intern "camulus-manage-subscriptions")[%here]
   ~docstring:"Open the remote subscription panel for SOURCE, optionally selecting VIEW."
   ~interactive:No_arg (Returns Value.Type.unit)
   (let open Defun.Let_syntax in let%map_open source=optional_with_nil "source" value
    and view=optional_with_nil "view" value in open_ ~view source);
 define0 ~interactive:true "camulus-tracking-refresh"(fun()->refresh false);
 define0 ~interactive:true "camulus-tracking-more"(fun()->refresh true);
 define0 ~interactive:true "camulus-tracking-filter" filter;
 Defun.defun(Symbol.intern "camulus-tracking-change")[%here]
   ~docstring:"Choose a remote policy for TARGET or the selected row."
   ~interactive:No_arg (Returns Value.Type.unit)
   (let open Defun.Let_syntax in let%map_open target=optional_with_nil "target" value
    and preferred=optional_with_nil "default-policy" value in change ~on_changed ~preferred target);
 let map=get "camulus-tracking-mode-map"in
 List.iter["RET","camulus-reader-open";"s","camulus-tracking-change";"/","camulus-tracking-filter";
   "g","camulus-tracking-refresh";"n","camulus-tracking-more";"q","quit-window"]~f:(fun(key,command)->bind map key command)
