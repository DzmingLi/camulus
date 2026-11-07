open! Core
open Reader.Service
module Discourse = Discourse_backend
module Protocol = Reader.Protocol
module CS = Reader.Content_state
type Protocol.work += Discourse of string * Discourse.operation
let create configs ~net:_ ~now:_ ~sleep:_ ~groups:_ =
    let forums=Stdlib.Hashtbl.create 8 in
    List.iter configs ~f:(fun config -> Stdlib.Hashtbl.replace forums config.Discourse.site (Discourse.Live.create config));
    let forum site=match Stdlib.Hashtbl.find_opt forums site with
      | Some backend -> backend
      | None -> let backend=Discourse.Live.create(Discourse.config site) in Stdlib.Hashtbl.add forums site backend;backend in
    {matches_source=Discourse.is_source;
     page=(fun ~source group ~after ~query ->
       let backend=forum(Discourse.site_of_source source) and group=Discourse.group_of_string group in
       match query with None -> Discourse.Live.list backend group ~after | Some query -> Discourse.Live.search backend group ~query ~after);
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
           try Discourse.Subscription.set backend target (Discourse.tracking_of_string s)
           with Discourse.Uncertain_write -> failwith "Tracking update outcome is uncertain; read remote state before retrying");
         let current=match Discourse.Subscription.get backend target with Regular -> "regular" | Tracking -> "tracking" | Watching -> "watching" | Muted -> "muted" in
         reply emit "subscription" ["source",`String source;"target",`String target;"setting",`String current;
           "choices",`List(List.map ["regular";"tracking";"watching";"muted"] ~f:(fun s -> `String s))];true
       | _ -> false)}
