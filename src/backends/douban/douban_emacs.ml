open! Core
open! Ecaml
open Reader_backend
module D=Douban_backend
module P=Reader.Protocol
module C=Reader.Content_state

type operation = Read of string | Attachment of string * int * string | Like of string * bool
 | Categories of string
 | Publish of D.destination * string * string
type P.work += Douban of operation
type P.payload += Douban_liked of string * D.reaction
let pending : int String.Table.t = String.Table.create ()
let confirmed : (int * D.reaction) String.Table.t = String.Table.create ()
let redraw () =
 List.iter(Buffer.all_live()) ~f:(fun buffer->Current_buffer.set_temporarily Sync buffer ~f:(fun()->
  if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"]) && String.equal(text(get "camulus-reader--backend-key"))"douban" then
   run "camulus-reader--render" []))
let prepare ~notify:_ =
 let cookies=if truth(get "camulus-firefox-profile") then
   Reader.Credentials.Firefox.get (firefox_cookie_provider ())
  else fun _ -> [] in
 Some {drain=(fun()->[]);close=(fun()->Hashtbl.clear pending;Hashtbl.clear confirmed);
  create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ~revisions:_ ~article:_ ->
   let backend=D.create_live ~cookies () in
   {storage_prefix="douban";key_available=String.equal "douban";all_pages=false;refresh_membership=`Merge;
    page=(fun ~source:_ group ~after->D.Live.list backend group ~after,[]);
    archive=(fun ~source:_ ~refresh:_ target id ~native_thread:_ ~add->
     match target with
     | C.Article->D.Live.invalidate backend id;add(D.Live.archive backend "" id);true
     | C.Thread->let complete=ref false in
       D.Live.stream_thread backend id ~emit_article:(fun a->add(D.Live.archive_article backend a)) ~emit:(fun b->complete:=b.complete);!complete);
    execute=(fun work ~observe ~emit -> let observe=observe ~source:("douban") in match work with
     | Douban operation ->
       (match operation with
        | Read id->D.Live.stream_thread backend id
          ~emit_article:(fun a->observe[a.message];emit(Presentation("rss",D.Live.article_json backend a)))
          ~emit:(fun b->emit(Presentation("rss",`Assoc["kind",`String "thread";"thread",`String b.thread;
            "messages",`List(List.map b.messages ~f:(D.Live.message_json backend));"complete",`Bool b.complete])))
        | Attachment(id,part,destination)->D.Live.attachment backend "" id part destination;
          emit(Presentation("rss",`Assoc["kind",`String "saved";"path",`String destination]))
        | Like(id,liked)->D.Live.set_like backend ~id ~liked;
          emit(Douban_liked(id,Option.value_exn(D.Live.reaction backend id)))
        | Categories slug ->
          let rows=List.map (D.Live.categories backend slug) ~f:(fun (name,id) ->
            `Assoc["name",`String name;"id",`String id]) in
          emit(Presentation("douban",`Assoc["kind",`String "categories";"rows",`List rows]))
        | Publish(destination,title,body) ->
          let id,notice=D.Live.publish backend destination ~title ~text:body in
          emit(Presentation("douban",`Assoc["kind",`String "published";"id",`String id;"notice",`String notice])));true
     | _->false)})}
let overlay message reaction =
 let fields=Value.list(List.map (D.reaction_fields(Some reaction)) ~f:(fun(k,v)->call "cons"[sym k;of_json v])) in
 call "append"[fields;message]
let install_protocol () =
 worker_factories:=prepare::!worker_factories;
 source_providers:=(fun()->Some "douban")::!source_providers;
 payload_renderers:=(function
  | Douban_liked(id,r)->Some("douban-liked",`Assoc(("id",`String id)::D.reaction_fields(Some r)))
  | _->None)::!payload_renderers;
 event_observers:=(fun ~barrier event->
  (match event with
   | P.Data(request,Douban_liked(id,reaction)) when Option.equal Int.equal (Hashtbl.find pending id)(Some request.id)->
     Hashtbl.set confirmed ~key:id ~data:(barrier,reaction);
     List.iter(Buffer.all_live()) ~f:(fun buffer->Current_buffer.set_temporarily Sync buffer ~f:(fun()->
      if truth(call "derived-mode-p"[sym "camulus-reader-article-mode"]) && String.equal(text(get "camulus-reader--backend-key"))"douban" && not(truth(get "camulus-reader--archive")) then
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
let load_categories ~submit slug complete =
 Writing.request (submit (Douban(Categories slug))) ~expect:"categories" ~complete:(fun result ->
   complete (Result.map result ~f:(fun data ->
     List.map (values(field "rows" data)) ~f:(fun row -> text(field "name" row),text(field "id" row)))))

let composer ~submit =
 let group_name entry =
   if not(String.equal (text(field "backend_key" entry)) "douban") then None else
   Option.map (D.forum_group (text(field "group" entry))) ~f:(fun (slug,_) ->
     let root=field "category_root" entry in
     text(field "name" (if truth root then root else entry)),slug) in
 let in_group entry = Some(fun () ->
   let current=group_name entry in
   let groups () = Option.to_list current @ List.filter_map (configured_groups ()) ~f:group_name in
   let slug target =
     match List.filter_map (groups ()) ~f:(fun (name,slug) -> if String.equal name target then Some slug else None)
       |> List.dedup_and_sort ~compare:String.compare with
     | [slug] -> slug
     | [] -> String.drop_prefix (D.group_of_target target) 6
     | _ -> invalid_arg "存在同名豆瓣小组，请填写小组 ID 或链接" in
   let group_target target =
     if String.equal target "广播" then None else
     match String.rsplit2 target ~on:'/' with
     | Some (name,tag) when String.equal (String.strip tag) "广播" -> Some(slug (String.strip name),true)
     | _ -> Some(slug target,false) in
   let known_categories = String.Table.create () in
   let categories slug =
     match Hashtbl.find known_categories slug with
     | Some rows -> Option.value rows ~default:[]
     | None ->
       Hashtbl.set known_categories ~key:slug ~data:None;
       load_categories ~submit slug (function
         | Error _ -> Hashtbl.remove known_categories slug
         | Ok rows ->
           Hashtbl.set known_categories ~key:slug ~data:(Some rows));
       [] in
   let complete_fields = function
     | "Douban" -> "广播" :: List.concat_map (groups ()) ~f:(fun (name,_) -> [name;name^" / 广播"])
       |> List.dedup_and_sort ~compare:String.compare
     | "Category" ->
       let current=Option.bind (Writing.post_header "Douban") ~f:(fun target ->
         try group_target target with Invalid_argument _ -> None) in
       Option.value_map current ~default:[] ~f:(fun (slug,_) ->
         List.concat_map (categories slug) ~f:(fun (name,id) -> [name;id]))
     | _ -> [] in
   let name,identity=match current with
     | None -> "广播","douban/broadcast"
     | Some (name,slug) -> name,"douban/group:"^slug in
   let category=Option.bind (D.forum_group (text(field "group" entry))) ~f:snd in
   let identity=Option.value_map category ~default:identity ~f:(fun id -> identity^"/category:"^id) in
   Writing.post_draft ~destination:("豆瓣 · "^name) ~identity
     ~fields:["Douban",name] ~optional_fields:["Category",Option.value category ~default:""] ~complete_fields
     ~submit:(fun ~targets ~subject ~body ~complete ->
       let dest=List.Assoc.find_exn targets "Douban" ~equal:String.equal in
       let destination=match group_target dest with
         | None -> D.Broadcast
         | Some (slug,also_broadcast) ->
           let category=List.Assoc.find targets "Category" ~equal:String.equal in
           Option.iter (Option.join (Hashtbl.find known_categories slug)) ~f:(fun rows ->
             Option.iter category ~f:(fun value -> ignore(D.category_id rows value : string)));
           D.Group {slug;category;also_broadcast} in
       Writing.request (submit (Douban(Publish(destination,subject,body)))) ~expect:"published" ~complete) ();
   Option.iter current ~f:(fun (_,slug) -> ignore(categories slug : (string * string) list))) in
 {in_group;default=(fun () -> None)}
let install ~submit () =
 let like liked =
  if not(truth(call "derived-mode-p"[sym "camulus-reader-article-mode"])) || not(String.equal(text(get "camulus-reader--backend-key"))"douban") || truth(get "camulus-reader--archive") then
   user_error "请打开豆瓣的在线文章或评论";
  let id=text(field "id"(call "camulus-reader--selected"[])) in
  ignore(D.like_target id liked : string * (string * string) list * bool);
  let item=call "gethash"[s id;get "camulus-reader--messages"] in
  if not(truth(call "gethash"[s id;get "camulus-reader--loaded"])) || truth(field "unavailable" item) then user_error "请等待该楼内容加载完成";
  if Hashtbl.mem pending id then user_error "该楼的点赞请求仍在提交";
  let request=submit (Douban(Like(id,liked))) in
  Hashtbl.set pending ~key:id ~data:request;
  message "正在提交豆瓣点赞操作…" [] in
 define0 ~interactive:true "camulus-douban-like" (fun()->like true);
 define0 ~interactive:true "camulus-douban-unlike" (fun()->like false);
 Defun.defun (Symbol.intern "camulus-douban-group") [%here]
  ~docstring:"Browse a Douban group by URL, ID or slug."
  ~interactive:(Prompt "豆瓣小组链接或 ID: ") (Returns Value.Type.unit)
  (let open Defun.Let_syntax in let%map_open target=required "target" string in
   let group=D.group_of_target target in
   run "camulus-reader-browse-entry" [alist["name",s ("豆瓣小组 · "^String.drop_prefix group 6);
     "backend",s "douban";"group",s group]]);
 register_local_feeds ~option:"camulus-douban-subscriptions" ~accepts:all_groups { (base ~id:"douban" ~name:"douban" ~threaded:true
  ~read:(fun ~source:_ ~id ~thread_id:_ ~group:_->Douban(Read id))) with
  browser=Some("豆瓣","camulus-douban-group");
  composer=Some(composer ~submit);
  categories=(fun entry -> Option.map (D.forum_group (text(field "group" entry))) ~f:(fun (slug,_) ->
    let root="group."^slug in
    {all_topics={listing=root;name="豆瓣小组 · "^slug};load=(fun complete ->
      load_categories ~submit slug (fun result -> complete (Result.map result ~f:(fun rows ->
        List.map rows ~f:(fun (name,id) -> {name;listing=root^".category."^id})))))}));
  attachment=Some(fun ~source:_ ~id ~group:_ ~part ~destination->i(submit (Douban(Attachment(id,part,destination)))));
  controls=[{key="l";hint="赞 · l";command="camulus-douban-like"};{key="L";hint="取消赞 · L";command="camulus-douban-unlike"}];
  article_marks=(fun item->if truth(field "like-count" item) then " · "^text(call "number-to-string"[field "like-count" item])^" 赞" else "");
  reconcile=(fun ~source:_ ~existing:_ ~request ~archived item->
    if archived then item else match Hashtbl.find confirmed (text(field "id" item)) with
     |Some(barrier,reaction) when not(truth request) || integer request<=barrier->overlay item reaction
     |_->item);
  events=["rss";"douban"] }
