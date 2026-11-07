open! Core
open! Ecaml
open Reader_backend
module Z=Zhihu_backend
module Protocol=Reader.Protocol

type operation = Read of string | Attachment of string * int * string
type Protocol.work += Zhihu of operation
type Protocol.payload += Zhihu_content of Yojson.Safe.t

let prepare ~notify:_ =
  let browser=firefox_cookie_provider () in
  Some {drain=(fun()->[]);close=(fun()->());
    create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
      let backend=Z.create_live ~cookies:(Reader.Firefox_cookies.get browser) () in
      {matches_source=String.equal ("zhihu");
       page=(fun ~source:_ group ~after ~query->match query with
         | None->Z.Live.list backend group ~after
         | Some query->Z.Live.search backend group ~query ~after);
       archive=(fun ~source:_ ~refresh:_ target id ~native_thread:_ ~add->
         if Poly.equal target Reader.Content_state.Thread then failwith "Zhihu publications do not have threads";
         Z.Live.invalidate backend id;add(Z.Live.archive backend "" id);true);
       execute=(fun work ~observe ~emit -> let observe=observe ~source:("zhihu") in match work with
         | Zhihu operation ->
           (match operation with
            | Read id ->
              let article=Z.Live.read backend "" id in observe [article.message];
              emit(Zhihu_content(Rss_backend.article_json article))
            | Attachment(id,part,destination) ->
              Z.Live.attachment backend "" id part destination;
              emit(Zhihu_content(`Assoc["kind",`String "saved";"path",`String destination])));
           true
         | _->false)})}

let install_protocol () =
  worker_factories:=prepare::!worker_factories;
  source_providers:=(fun()->Some "zhihu")::!source_providers;
  payload_renderers:=(function
    | Zhihu_content data->Some("rss",data)
    | _->None)::!payload_renderers

let install ~submit () =
  register_local_groups ~accepts:all_groups { (base ~name:"zhihu" ~matches:(String.equal "zhihu") ~threaded:false
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group:_ -> i(submit ~service:false (Zhihu(Read id))))) with
    attachment=Some(fun ~source:_ ~id ~group:_ ~part ~destination ->i(submit ~service:true (Zhihu(Attachment(id,part,destination)))));
    events=["rss"] }
