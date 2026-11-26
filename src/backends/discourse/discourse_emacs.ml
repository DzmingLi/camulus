open! Core
open! Ecaml
open Reader_backend
module Discourse = Discourse_backend
module Protocol = Reader.Protocol
module CS = Reader.Content_state
let discourse_configs = ref []
type Protocol.work += Discourse of string * Discourse.operation
let request_discourse ~submit site operation =
  submit (Discourse(site,operation))
let prepare ~notify:_ =
  let configs= !discourse_configs in
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ~revisions:_ ~article:_ ->
    let forums=Stdlib.Hashtbl.create 8 in
    List.iter configs ~f:(fun config -> Stdlib.Hashtbl.replace forums config.Discourse.site (Discourse.Live.create config));
    let forum site=match Stdlib.Hashtbl.find_opt forums site with
      | Some backend -> backend
      | None -> let backend=Discourse.Live.create(Discourse.config site) in Stdlib.Hashtbl.add forums site backend;backend in
    {storage_prefix="discourse";key_available=Discourse.is_source;all_pages=false;refresh_membership=`Merge;
     page=(fun ~source group ~after ->
       let backend=forum(Discourse.site_of_source source) and group=Discourse.group_of_string group in
       Discourse.Live.list backend group ~after,[]);
     archive=(fun ~source ~refresh:_ target id ~native_thread:_ ~add ->
       let backend=forum(Discourse.site_of_source source) in
       match target with
       | CS.Article -> add(Discourse.Live.archive_article backend (Discourse.Live.read backend id));true
       | CS.Thread ->
         let complete=ref false and seen=Stdlib.Hashtbl.create 32 in
         Discourse.Live.stream_thread backend id
           ~emit_article:(fun (a:Discourse.Decode.article) -> Stdlib.Hashtbl.replace seen a.message.id ();add(Discourse.Live.archive_article backend a))
           ~emit:(fun b ->
             List.iter b.messages ~f:(fun message ->
               if message.unavailable && not(Stdlib.Hashtbl.mem seen message.id) then
                 add {CS.message;attachments=`List [];raw=None;files=[]});
             complete:=b.complete);!complete);
     execute=(fun work ~observe ~emit -> match work with
       | Discourse(site,operation) ->
         Discourse.Live.execute (forum site) operation ~emit:(fun result ->
           let messages=match result with Discourse.Page p -> p.messages | Thread_batch b -> b.messages | Article a | Liked a | Sent a -> [a.message] | _ -> [] in
           if not(List.is_empty messages) then observe ~source:(Discourse.source_of_site site) messages;
           emit(Presentation("discourse",Discourse.result_json result)));true
       | Subscription(source,target,setting) when Discourse.is_source source ->
         let backend=forum(Discourse.site_of_source source) in
         Option.iter setting ~f:(fun s ->
           try Discourse.Live.Subscription.set backend target (Discourse.tracking_of_string s)
           with Discourse.Uncertain_write -> failwith "Tracking update outcome is uncertain; read remote state before retrying");
         let current=match Discourse.Live.Subscription.get backend target with Regular -> "regular" | Tracking -> "tracking" | Watching -> "watching" | Muted -> "muted" in
         reply emit "subscription" ["backend_key",`String source;"target",`String target;"setting",`String current;
           "choices",`List(List.map ["regular";"tracking";"watching";"muted"] ~f:(fun s -> `String s))];true
       | _ -> false)})}

let install_protocol ~ensure_ui ~idle ~submit =
  worker_factories := prepare :: !worker_factories;
  defun_nullary_nil (Symbol.intern "camulus-discourse-clear-config") [%here]
    ~docstring:"Clear Discourse session configuration before startup."
    (fun () -> ensure_ui ();if not(idle ()) then failwith "Stop runtime first";discourse_configs:=[]);
  defun (Symbol.intern "camulus-discourse-site") [%here]
    ~docstring:"Validate and normalize a Discourse forum base URL."
    (Returns Value.Type.string)
    (let open Defun.Let_syntax in let%map_open site=required "site" string in Discourse.Decode.site site);
  defun (Symbol.intern "camulus-discourse-configure") [%here]
    ~docstring:"Configure User API credentials before startup; empty key/client means anonymous."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open site=required "site" string
     and key=required "user-api-key" string
     and client=required "client-id" string in
     ensure_ui ();
     if not(idle ()) then failwith "Stop runtime before changing Discourse identity";
     let config=Discourse.config ~api_key:key ~client_id:client site in
     discourse_configs:=config::List.filter !discourse_configs ~f:(fun c -> not(String.equal c.site config.site)));
  defun (Symbol.intern "camulus-discourse-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     let j=Reader_backend.to_json request in request_discourse ~submit (Discourse.Decode.site (Discourse.Decode.str "site" j)) (Discourse.operation_of_json j))
let load_categories ~submit site complete =
  Writing.request (submit site Discourse.Categories) ~expect:"categories" ~complete:(fun result ->
    complete (Result.map result ~f:(fun data ->
      List.map (values(field "rows" data)) ~f:(fun row ->
        text(field "name" row),integer(field "id" row)))))

let composer ~submit =
  let authenticated ()=List.map (values(get "camulus-discourse--authenticated-sites")) ~f:text in
  let known_categories=String.Table.create () in
  let categories site complete =
    load_categories ~submit site (fun result ->
      complete (Result.map result ~f:(fun rows ->
        Hashtbl.set known_categories ~key:site ~data:(List.map rows ~f:snd);
        List.map rows ~f:(fun (name,id) -> Printf.sprintf "%s [%d]" name id,id)))) in
  let post site category label =
    Writing.post_draft ~destination:("Discourse · "^site^" · "^label)
      ~identity:(Discourse.source_of_site site^"/category:"^Int.to_string category)
      ~fields:["Discourse",site;"Category",Int.to_string category]
      ~complete_fields:(function
        | "Discourse" -> authenticated ()
        | "Category" ->
          let current_site=Writing.post_header "Discourse" in
          (match current_site with
           | None -> []
           | Some current ->
             let configured=List.filter_map (configured_groups ()) ~f:(fun entry ->
               if String.equal (text(field "backend_key" entry)) ("discourse:"^current) then
                 String.chop_prefix (text(field "group" entry)) ~prefix:"category:"
               else None) in
             let cached=Option.value (Hashtbl.find known_categories current) ~default:[] in
             let selected=if String.equal site current then [category] else [] in
             configured @ List.map (selected @ cached) ~f:Int.to_string
             |> List.dedup_and_sort ~compare:String.compare)
        | _ -> [])
      ~submit:(fun ~targets ~subject ~body ~complete ->
        let field name=List.Assoc.find_exn targets name ~equal:String.equal in
        let site=Discourse.Decode.site (field "Discourse") in
        let category=Discourse.Decode.number (field "Category") in
        if not(List.mem (authenticated ()) site ~equal:String.equal) then
          invalid_arg "Authorize the destination Discourse site before posting";
        Writing.request (submit site (Discourse.Send(New_topic{category;title=subject;text=body})))
          ~expect:"sent" ~complete) ();
    if not(Hashtbl.mem known_categories site) then categories site ignore in
  let choose_category site () =
    let window=call "selected-window" [] in
    let origin=call "window-buffer" [window] in
    categories site (function
      | Error reason -> message "Discourse categories: %s" [s reason]
      | Ok choices ->
        (* Open the picker outside the worker event drain. Only use the window
           that requested it if it still displays the original reader. *)
        let choose () =
          if truth(call "window-live-p" [window]) && Value.eq origin (call "window-buffer" [window]) then (
            run "select-window" [window];
            if List.is_empty choices then user_error "No categories available on this forum";
            let choice=text(call "completing-read" [s "Discourse category: ";
              Value.list(List.map choices ~f:(fun (label,_) -> s label));nil;sym "t"]) in
            Option.iter (List.Assoc.find choices choice ~equal:String.equal) ~f:(fun category ->
              post site category choice));
          nil in
        run "run-at-time" [i 0;nil;lambda0 choose]) in
  let in_group entry =
    let site=Discourse.site_of_source(text(field "backend_key" entry)) in
    if not(List.mem (authenticated ()) site ~equal:String.equal) then
      Some(fun () -> user_error "Authorize this Discourse site before posting")
    else match Discourse.group_of_string(text(field "group" entry)) with
    | Category category -> Some(fun () -> post site category (text(field "name" entry)))
    | Latest | Topic _ | Notifications -> Some(choose_category site) in
  {in_group;default=(fun () -> None)}

let install ~submit () =
  let submit=request_discourse ~submit in
  let topic_columns=define_tracking_columns "camulus-discourse-tracking-fields"
    ["setting","Policy",Some 12;"id","Topic ID",Some 16;"title","Topic",None] in
  Interaction_emacs.install ();
  define0 ~interactive:true "camulus-discourse-watch-topic"(fun()->
    let source=text(get "camulus-reader--backend-key") in
    if not(Discourse.is_source source && truth(call "derived-mode-p"[sym "camulus-tracking-mode"]))then
      user_error "Open the Discourse tracking panel for the forum first";
    let site=Discourse.site_of_source source in
    let input=text(call "read-string"[s ("Topic number or URL on "^site^": ")]) in
    let target=Discourse.Decode.topic_target ~site input in
    run "camulus-tracking-change"[s target;s "watching"]);
  register { (base ~id:"discourse" ~name:"Discourse" ~threaded:true
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Discourse(String.drop_prefix source 10,Discourse_backend.Thread id))) with
    group_key=Some (fun entry -> Discourse.source_of_site (string_field "site" entry));
    start=Interaction_emacs.setup;
    subscribed_feeds=Some(fun () ->
      Interaction_emacs.registered_sites ()
      |> List.filter ~f:(fun (site,_) -> List.exists(values(get "camulus-discourse--authenticated-sites")) ~f:(Value.equal(s site)))
      |> List.map ~f:(fun (site,entry) ->
        alist["name",s(text(field "name" entry)^" · Notifications");
          "backend_key",s(Discourse_backend.source_of_site site);"group",s "notifications"]));
    normalize=(fun entry ->
      let source=Discourse_backend.source_of_site(String.drop_prefix(text(field "backend_key" entry))10) in
      Interaction_emacs.assign "backend_key" (s source) (call "copy-alist" [entry]));

    events=["discourse"];subscription_target=Some (fun ~source:_ ~selected ->
      let id=text(field "id"(selected ()))in
      if String.is_prefix id ~prefix:"topic:"then Some id else None);
    topic_tracking=Some {
      layout=(fun _ -> topic_layout ~shortcuts:[
        {key="+";hint="+: watch topic";command="camulus-discourse-watch-topic"}]
        topic_columns);
      resolve_filter=Fn.id;
      filters=["watching","Watching";"tracking","Tracking";"muted","Muted"];
      list=(fun ~source ~filter ~after -> i(submit (String.drop_prefix source 10)
        (Discourse_backend.Subscriptions(Discourse_backend.tracking_of_string filter,after))));
      change=(fun ~source ~target ~setting -> i(submit (String.drop_prefix source 10)
        (Discourse_backend.Subscription_change(target,Option.map setting ~f:Discourse_backend.tracking_of_string)))) };
    composer=Some(composer ~submit);
    categories=(fun entry -> match Discourse.group_of_string (text(field "group" entry)) with
      | Latest | Category _ ->
        let site=Discourse.site_of_source (text(field "backend_key" entry)) in
        Some {all_topics={name=site;listing="latest"};load=(fun complete ->
          load_categories ~submit site (fun result -> complete (Result.map result ~f:(fun rows ->
            List.map rows ~f:(fun (name,id) -> {name;listing="category:"^Int.to_string id})))) )}
      | Topic _ | Notifications -> None);
    controls=[{key="l";hint="l: like / C-u l: unlike";command="camulus-reader-discourse-like"}];
    browser=Some ("Discourse", "camulus-reader-discourse");
    entrances=(fun () ->
      let groups=configured_groups () in
      Interaction_emacs.registered_sites () |> List.filter_map ~f:(fun (site,_) ->
        let key=s (Discourse.source_of_site site) in
        if List.exists groups ~f:(fun entry ->
          Value.equal key (field "backend_key" entry) && String.equal (string_field "group" entry) "latest")
        then None else Some {label=site^" · latest";open_=(fun () -> run "camulus-reader-discourse" [s site;s "latest"])}));
    reconcile=(fun ~source ~existing ~request ~archived message ->
      if archived then message else (
        let message=if Value.eq existing Value.nil then message else
          call "cons" [call "cons" [Value.intern "liked";call "alist-get" [Value.intern "liked";existing]];message] in
        call "camulus-discourse--apply-like" [message;s source;request])) }
