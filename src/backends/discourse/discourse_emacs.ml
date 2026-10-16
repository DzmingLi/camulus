open! Core
open! Ecaml
open Reader_backend
let install ~submit () =
  register { (base ~name:"Discourse" ~matches:(String.is_prefix ~prefix:"discourse:") ~threaded:true
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (String.drop_prefix source 10) (Discourse_backend.Thread id)))) with
    attachment=Some (fun ~source ~id ~group ~part ~destination -> ignore group; Value.of_int_exn (submit (String.drop_prefix source 10) (Discourse_backend.Attachment(id,part,destination))));
    events=["discourse"];subscription_target=Some (fun ~source:_ ~selected ->
      Some (Value.to_utf8_bytes_exn (call "alist-get" [Value.intern "id";selected ()])));
    page=Some (fun data -> run "ef-discourse--page" [data]);
    controls=[{key="l";hint="l: like / C-u l: unlike";command="ef-reader-discourse-like"}];
    browser=Some ("Discourse", "ef-reader-discourse");
    entrances=(fun () ->
      Value.to_list_exn (Current_buffer.symbol_value (Symbol.intern "ef-discourse-sites")) ~f:Value.to_utf8_bytes_exn
      |> List.map ~f:(fun site -> {label=site^" · latest (c: categories)";open_=(fun () -> run "ef-reader-discourse" [s site;s "latest"])}));
    reconcile=(fun ~source ~existing ~request ~archived message ->
      if archived then message else (
        let message=if Value.eq existing Value.nil then message else
          call "cons" [call "cons" [Value.intern "liked";call "alist-get" [Value.intern "liked";existing]];message] in
        call "ef-discourse--apply-like" [message;s source;request])) }
