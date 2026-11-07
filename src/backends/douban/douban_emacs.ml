open! Core
open! Ecaml
open Reader_backend
module D=Douban_backend
module P=Reader.Protocol
module C=Reader.Content_state

type operation = Read of string | Attachment of string * int * string | Like of string * bool
type P.work += Douban of operation
type P.payload += Douban_content of Yojson.Safe.t | Douban_liked of string * D.reaction
let pending : int String.Table.t = String.Table.create ()
let confirmed : (int * D.reaction) String.Table.t = String.Table.create ()
let redraw () =
 List.iter(Buffer.all_live()) ~f:(fun buffer->Current_buffer.set_temporarily Sync buffer ~f:(fun()->
  if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"]) && String.equal(text(get "camulus-reader--source"))"douban" then
   run "camulus-reader--render" []))
let prepare ~notify:_ =
 let browser=firefox_cookie_provider () in
 Some {drain=(fun()->[]);close=(fun()->Hashtbl.clear pending;Hashtbl.clear confirmed);
  create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
   let backend=D.create_live ~cookies:(Reader.Firefox_cookies.get browser) () in
   {matches_source=String.equal ("douban");
    page=(fun ~source:_ group ~after ~query->match query with None->D.Live.list backend group ~after|Some query->D.Live.search backend group ~query ~after);
    archive=(fun ~source:_ ~refresh:_ target id ~native_thread:_ ~add->
     match target with
     | C.Article->D.Live.invalidate backend id;add(D.Live.archive backend "" id);true
     | C.Thread->let complete=ref false in
       D.Live.stream_thread backend id ~emit_article:(fun a->add(D.Live.archive_article backend a)) ~emit:(fun b->complete:=b.complete);!complete);
    execute=(fun work ~observe ~emit -> let observe=observe ~source:("douban") in match work with
     | Douban operation ->
       (match operation with
        | Read id->D.Live.stream_thread backend id
          ~emit_article:(fun a->observe[a.message];emit(Douban_content(D.Live.article_json backend a)))
          ~emit:(fun b->emit(Douban_content(`Assoc["kind",`String "thread";"thread",`String b.thread;
            "messages",`List(List.map b.messages ~f:(D.Live.message_json backend));"complete",`Bool b.complete])))
        | Attachment(id,part,destination)->D.Live.attachment backend "" id part destination;
          emit(Douban_content(`Assoc["kind",`String "saved";"path",`String destination]))
        | Like(id,liked)->D.Live.set_like backend ~id ~liked;
          emit(Douban_liked(id,Option.value_exn(D.Live.reaction backend id))));true
     | _->false)})}
let overlay message reaction =
 let fields=Value.list(List.map (D.reaction_fields(Some reaction)) ~f:(fun(k,v)->call "cons"[sym k;of_json v])) in
 call "append"[fields;message]
let install_protocol () =
 worker_factories:=prepare::!worker_factories;
 source_providers:=(fun()->Some "douban")::!source_providers;
 payload_renderers:=(function
  | Douban_content data->Some("rss",data)
  | Douban_liked(id,r)->Some("douban-liked",`Assoc(("id",`String id)::D.reaction_fields(Some r)))
  | _->None)::!payload_renderers;
 event_observers:=(fun ~barrier event->
  (match event with
   | P.Data(request,Douban_liked(id,reaction)) when Option.equal Int.equal (Hashtbl.find pending id)(Some request.id)->
     Hashtbl.set confirmed ~key:id ~data:(barrier,reaction);
     List.iter(Buffer.all_live()) ~f:(fun buffer->Current_buffer.set_temporarily Sync buffer ~f:(fun()->
      if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"]) && String.equal(text(get "camulus-reader--source"))"douban" && not(truth(get "camulus-reader--archive")) then
       let table=get "camulus-reader--messages" in let item=call "gethash"[s id;table] in
       if truth item then run "puthash"[s id;overlay item reaction;table]));
     redraw ();message (if reaction.liked then "豆瓣：已赞" else "豆瓣：已取消赞") []
   | P.Finished(request,outcome)->(match request.work with
     | Douban(Like(id,_)) when Option.equal Int.equal(Hashtbl.find pending id)(Some request.id)->
       Hashtbl.remove pending id;
       (match outcome with P.Completed->()|P.Cancelled->message "豆瓣点赞已取消；请刷新确认远端状态" []
        |P.Failed reason->message "豆瓣点赞失败：%s" [s reason])
     |_->())
   | _->());[])::!event_observers
let install ~submit () =
 let like liked =
  if not(truth(call "derived-mode-p"[sym "camulus-reader-article-mode"])) || not(String.equal(text(get "camulus-reader--source"))"douban") || truth(get "camulus-reader--archive") then
   user_error "请打开豆瓣的在线文章或评论";
  let id=text(field "id"(call "camulus-reader--selected"[])) in
  ignore(D.like_target id liked : string * (string * string) list * bool);
  let item=call "gethash"[s id;get "camulus-reader--messages"] in
  if not(truth(call "gethash"[s id;get "camulus-reader--loaded"])) || truth(field "unavailable" item) then user_error "请等待该楼内容加载完成";
  if Hashtbl.mem pending id then user_error "该楼的点赞请求仍在提交";
  let request=submit ~service:true (Douban(Like(id,liked))) in
  Hashtbl.set pending ~key:id ~data:request;
  message "正在提交豆瓣点赞操作…" [] in
 define0 ~interactive:true "camulus-douban-like" (fun()->like true);
 define0 ~interactive:true "camulus-douban-unlike" (fun()->like false);
 register_local_groups ~accepts:all_groups { (base ~name:"douban" ~matches:(String.equal "douban") ~threaded:true
  ~read:(fun ~source:_ ~id ~thread_id:_ ~group:_->i(submit ~service:false (Douban(Read id))))) with
  attachment=Some(fun ~source:_ ~id ~group:_ ~part ~destination->i(submit ~service:true (Douban(Attachment(id,part,destination)))));
  controls=[{key="l";hint="赞 · l";command="camulus-douban-like"};{key="L";hint="取消赞 · L";command="camulus-douban-unlike"}];
  article_marks=(fun item->if truth(field "like-count" item) then " · "^text(call "number-to-string"[field "like-count" item])^" 赞" else "");
  reconcile=(fun ~source:_ ~existing:_ ~request ~archived item->
    if archived then item else match Hashtbl.find confirmed (text(field "id" item)) with
     |Some(barrier,reaction) when not(truth request) || integer request<=barrier->overlay item reaction
     |_->item);
  events=["rss"] }
