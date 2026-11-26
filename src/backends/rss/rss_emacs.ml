open! Core
open! Ecaml
open Reader_backend
module Protocol = Reader.Protocol
module CS = Reader.Content_state
module R = Rss_backend

type operation = Read of string * string

(* RSS and JSON Feed share article presentation, while their codecs and cursors
   remain owned by their respective backend modules. *)
module type Feed = sig
  type t
  val id : string
  val all_pages : bool
  val is_source : string -> bool
  val source_of_url : string -> string
  val url_of_source : string -> string
  val create : unit -> t
  val page : t -> string -> after:string option -> Reader.Content.page * Yojson.Safe.t list
  val read : t -> string -> string -> R.Decode.article
  val archive : t -> string -> string -> CS.archived
  val refresh : t -> string -> Reader.Content.page
  val local_articles : bool
end
let article_presentation ~local (article:R.Decode.article) =
  let json=R.article_json article in
  match local,article.message.activity,json with
  | true,Some revision,`Assoc fields -> `Assoc (("revision",`String revision)::fields)
  | _ -> json

module Connect (F:Feed) = struct
  type Protocol.work += Request of operation
  let prepare ~notify:_ =
    Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ~revisions:_ ~article ->
      let backend=F.create () in
      let cached url id =
        if F.local_articles then Option.map (article ~source:(F.source_of_url url) ~id) ~f:R.article_of_json
        else None in
      {storage_prefix=F.id;key_available=F.is_source;all_pages=F.all_pages;refresh_membership=`Merge;
       page=(fun ~source group ~after ->
         if not(String.equal (F.url_of_source source) group) then invalid_arg "Feed source and entry URL differ";
         F.page backend group ~after);
       archive=(fun ~source ~refresh target id ~native_thread:_ ~add ->
         if Poly.equal target CS.Thread then failwith "Feed articles do not have threads";
         let url=F.url_of_source source in
         if refresh then ignore(F.refresh backend url : Reader.Content.page);
         add(match cached url id with
             | Some article -> R.Article.archive ~download:R.Live.download_url article
             | None -> F.archive backend url id);
         true);
       execute=(fun work ~observe ~emit -> match work with
         | Request (Read(url,id)) ->
           let article=F.read backend url id in
           observe ~source:(F.source_of_url url) [article.message];
           emit(Presentation("rss",article_presentation ~local:F.local_articles article));true
         | _ -> false)})}
  let install_protocol () = worker_factories := prepare :: !worker_factories
end
module Connection = Connect(struct
  include R.Live
  let create ()=R.Live.create ()
  let id="rss"
  let all_pages=false
  let local_articles=true
  let page t url ~after =
    let articles=list_articles t url ~after in
    {Reader.Content.messages=List.map articles ~f:(fun article -> article.R.Decode.message);after=None},
    List.map articles ~f:(article_presentation ~local:true)
  let is_source=R.is_source
  let source_of_url=R.source_of_url
  let url_of_source=R.url_of_source
  let refresh t url=list t url ~after:None
end)
let install_protocol = Connection.install_protocol

(* Publication backends reuse RSS article presentation; each backend supplies
   its own network operations and group rules when registering this worker. *)
type Protocol.work += Publication_read of string * string * string option
  | Publication_attachment of string * string * string option * int * string
let publication_backend ~source ~threaded =
  { (base ~id:source ~name:source ~threaded
      ~read:(fun ~source:_ ~id ~thread_id:_ ~group ->
        Publication_read(source,id,group))) with
    events=["rss"] }
let publication_worker ?(all_pages=false) ~source ~group ~page ~read ~archive ?attachment ~refresh
    ?thread () =
  {storage_prefix=source;key_available=String.equal source;all_pages;refresh_membership=`Merge;
   page=(fun ~source:_ group ~after -> page group ~after);
   archive=(fun ~source:_ ~refresh:reload target id ~native_thread:_ ~add ->
     let group=group id None in
     if reload then refresh group id;
     match target,thread with
     | CS.Thread,Some(stream,archive_article) ->
       let complete=ref false in
       stream id ~emit_article:(fun a -> add(archive_article a)) ~emit:(fun (b:Reader.Content.thread_batch) -> complete:=b.complete);!complete
     | Thread,None -> failwith "Publication articles do not have threads"
     | Article,_ -> add(archive group id);true);
   execute=(fun work ~observe ~emit ->
     let article (a:R.Decode.article) = observe ~source [a.message];emit(Presentation("rss",R.article_json a)) in
     match work with
     | Publication_read(s,id,requested) when String.equal s source ->
       (match thread with
        | Some(stream,_) -> stream id ~emit_article:article ~emit:(fun (b:Reader.Content.thread_batch) ->
          emit(Presentation("rss",`Assoc["kind",`String "thread";"thread",`String b.thread;
            "messages",`List(List.map b.messages ~f:CS.message_json);"complete",`Bool b.complete])))
        | None -> article(read (group id requested) id));true
     | Publication_attachment(s,id,requested,part,destination) when String.equal s source ->
       (match attachment with
        | None -> false
        | Some save -> save (group id requested) id part destination;
          reply emit "saved" ["path",`String destination];true)
     | _ -> false)}

let render_media message parts =
  let html=text (field "html" message) in
  let base=text (field "url" message) in
  let seen=Hash_set.create (module String) in
  let resolve url=if String.is_empty url then None else Reader.Html.safe_link ~base "href" url in
  if not (List.is_empty parts || String.is_empty html) then (
    let tree=Reader.Html.parse html in
    let urls=Reader.Html.media tree @
      List.map (Reader.Html.select (fun node -> String.equal (Reader.Html.tag node) "a") tree)
        ~f:(Reader.Html.attr "href") in
    List.iter urls ~f:(fun url -> Option.iter (resolve url) ~f:(Hash_set.add seen)));
  List.iter parts ~f:(fun part ->
    match resolve (text (field "url" part)) with
    | Some url when not (Hash_set.mem seen url) ->
      Hash_set.add seen url;
      let name=text (field "name" part) in
      run "insert-text-button" [s (if String.is_empty name then url else name);
        sym "action";sym "camulus-feed-open-media";sym "follow-link";b true;sym "url";s url;
        sym "archived";b (truth (get "camulus-reader--archive") && not (truth (field "download_from_source" part)));
        sym "source";get "camulus-reader--backend-key";sym "message-id";field "id" message;sym "part";field "part" part];
      Point.insert "\n"
    | _ -> ())

let install ~request_state () =
  define1 "camulus-feed-open-media" (fun button ->
    let prop key=call "button-get" [button;sym key] in
    if truth (prop "archived") then (
      let dir=call "make-temp-file" [s "camulus-media-";b true] in
      let ext=Stdlib.Filename.extension (Uri.path (Uri.of_string (text (prop "url")))) in
      let path=text (call "expand-file-name" [s ("media"^ext);dir]) in
      let id=request_state (CS.Attachment(text(prop "source"),text(prop "message-id"),integer(prop "part"),path)) in
      Writing.request id ~expect:"saved" ~complete:(function
        | Ok _ -> run "browse-url-of-file" [s path]
        | Error reason -> message "Media: %s" [s reason]))
    else run "browse-url" [prop "url"];
    nil);
  Defun.defun (Symbol.intern "camulus-reader-rss") [%here] ~docstring:"Browse an RSS or Atom feed without following it."
    ~interactive:(Prompt "RSS / Atom feed URL: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
     run "camulus-reader-browse-entry" [alist ["name",s ("RSS / Atom: "^url);
       "backend",s "rss";"group",s url]]);
  register_local_feeds ~option:"camulus-rss-subscriptions" ~key_for_group:R.source_of_url
    ~accepts:(fun ~source ~group->String.equal (R.url_of_source source) group) { (base ~id:"rss" ~name:"RSS / Atom" ~threaded:false
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Connection.Request(Read(String.drop_prefix source 4,id)))) with
    render_media;
    events=["rss"];
    delete=Some (fun ~entry:_ ~row:_ ~complete -> complete (Ok Delete_inbox_article));
    browser=Some ("RSS / Atom", "camulus-reader-rss") }
