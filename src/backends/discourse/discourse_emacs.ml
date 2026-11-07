open! Core
open! Ecaml
open Reader_backend
let install ~submit () =
  Interaction_emacs.install ();
  register_settings "discourse"(fun j->match json_field "auth_sources"j with
    |`Null->[]|`List xs when List.for_all xs ~f:(function `String _->true|_->false)->["camulus-discourse-auth-sources",of_json(`List xs)]
    |_->failwith "Invalid discourse auth_sources");
  register { (base ~name:"Discourse" ~matches:(String.is_prefix ~prefix:"discourse:") ~threaded:true
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (String.drop_prefix source 10) (Discourse_backend.Thread id)))) with
    start=Interaction_emacs.setup;
    followed=Some(fun () -> Immediate(Value.list(
      values(get "camulus-discourse--authenticated-sites")
      |> List.dedup_and_sort ~compare:(fun a b -> String.compare(text a)(text b))
      |> List.filter ~f:(fun site -> List.exists(values(get "camulus-groups"))~f:(fun entry ->
        String.equal(text(field "source" entry))(Discourse_backend.source_of_site(text site))))
      |> List.map ~f:(fun site ->
        let source=Discourse_backend.source_of_site(text site)in
        let name=List.find_map(values(get "camulus-groups"))~f:(fun entry->
          if String.equal(text(field "source"entry))source then Some(text(field "name"entry))else None)
          |> Option.value ~default:(text site)in
        alist["name",s(name^" · 通知");"source",s source;"group",s "notifications"]))));
    normalize=(fun entry->let source=text(field "source"entry)in
      Interaction_emacs.assign "source"(s(Discourse_backend.source_of_site(String.drop_prefix source 10)))entry);
    on_entry=(fun entry->let site=s(String.drop_prefix(text(field "source"entry))10)in
      if not(List.exists(values(get "camulus-discourse-sites"))~f:(Value.equal site))then set "camulus-discourse-sites"(call "cons"[site;get "camulus-discourse-sites"]));
    attachment=Some (fun ~source ~id ~group ~part ~destination -> ignore group; Value.of_int_exn (submit (String.drop_prefix source 10) (Discourse_backend.Attachment(id,part,destination))));
    events=["discourse"];subscription_target=Some (fun ~source:_ ~selected ->
      let id=text(field "id"(selected ()))in
      if String.is_prefix id ~prefix:"topic:"then Some id else None);
    topic_tracking=Some {
      filters=["watching","所有回复通知";"tracking","追踪未读数量";"muted","免打扰"];
      list=(fun ~source ~filter ~after -> i(submit (String.drop_prefix source 10)
        (Discourse_backend.Subscriptions(Discourse_backend.tracking_of_string filter,after))));
      change=(fun ~source ~target ~setting -> i(submit (String.drop_prefix source 10)
        (Discourse_backend.Subscription_change(target,Option.map setting ~f:Discourse_backend.tracking_of_string)))) };
    list_controls=[{key="c";hint="c: categories";command="camulus-reader-discourse-categories"}];
    controls=[{key="l";hint="l: like / C-u l: unlike";command="camulus-reader-discourse-like"}];
    browser=Some ("Discourse", "camulus-reader-discourse");
    entrances=(fun () ->
      Value.to_list_exn (Current_buffer.symbol_value (Symbol.intern "camulus-discourse-sites")) ~f:Value.to_utf8_bytes_exn
      |> List.filter ~f:(fun site -> List.exists(values(get "camulus-groups"))~f:(fun entry ->
        String.equal(text(field "source" entry))(Discourse_backend.source_of_site site)))
      |> List.map ~f:(fun site -> {label=site^" · latest (c: categories)";open_=(fun () -> run "camulus-reader-discourse" [s site;s "latest"])}));
    reconcile=(fun ~source ~existing ~request ~archived message ->
      if archived then message else (
        let message=if Value.eq existing Value.nil then message else
          call "cons" [call "cons" [Value.intern "liked";call "alist-get" [Value.intern "liked";existing]];message] in
        call "camulus-discourse--apply-like" [message;s source;request])) }
