open! Core
open! Ecaml
open Reader_backend
module Protocol = Reader.Protocol
module CS = Reader.Content_state
module R = Rss_backend

type operation = Read of string * string | Attachment of string * string * int * string

(* RSS and JSON Feed share article presentation, while their codecs and cursors
   remain owned by their respective backend modules. *)
module type Feed = sig
  type t
  val all_pages : bool
  val is_source : string -> bool
  val source_of_url : string -> string
  val url_of_source : string -> string
  val create : unit -> t
  val list : t -> string -> after:string option -> Reader.Content.page
  val read : t -> string -> string -> R.Decode.article
  val archive : t -> string -> string -> CS.archived
  val attachment : t -> string -> string -> int -> string -> unit
  val refresh : t -> string -> Reader.Content.page
end
module Connect (F:Feed) = struct
  type Protocol.work += Request of operation
  let request ~submit operation = submit (Request operation)
  let prepare ~notify:_ =
    Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
      let backend=F.create () in
      {matches_source=F.is_source;all_pages=F.all_pages;
       page=(fun ~source group ~after ->
         if not(String.equal (F.url_of_source source) group) then invalid_arg "Feed source and entry URL differ";
         F.list backend group ~after);
       archive=(fun ~source ~refresh target id ~native_thread:_ ~add ->
         if Poly.equal target CS.Thread then failwith "Feed articles do not have threads";
         let url=F.url_of_source source in
         if refresh then ignore(F.refresh backend url : Reader.Content.page);
         add(F.archive backend url id);true);
       execute=(fun work ~observe ~emit -> match work with
         | Request (Read(url,id)) ->
           let article=F.read backend url id in
           observe ~source:(F.source_of_url url) [article.message];
           emit(Presentation("rss",R.article_json article));true
         | Request (Attachment(url,id,part,destination)) ->
           F.attachment backend url id part destination;
           reply emit "saved" ["path",`String destination];true
         | _ -> false)})}
  let install_protocol () = worker_factories := prepare :: !worker_factories
end
module Connection = Connect(struct
  include R.Live
  let create ()=R.Live.create ()
  let all_pages=false
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
let publication_backend ~submit ~source ~threaded =
  { (base ~name:source ~matches:(String.equal source) ~threaded
      ~read:(fun ~source:_ ~id ~thread_id:_ ~group ->
        i(submit (Publication_read(source,id,group))))) with
    attachment=Some(fun ~source:_ ~id ~group ~part ~destination ->
      i(submit (Publication_attachment(source,id,group,part,destination))));
    events=["rss"] }
let publication_worker ?(all_pages=false) ~source ~group ~page ~read ~archive ~attachment ~refresh
    ?thread () =
  {matches_source=String.equal source;all_pages;
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
       attachment (group id requested) id part destination;reply emit "saved" ["path",`String destination];true
     | _ -> false)}

let install ~submit () =
  let submit=Connection.request ~submit in
  Defun.defun (Symbol.intern "camulus-reader-rss") [%here] ~docstring:"Browse an RSS or Atom feed without following it."
    ~interactive:(Prompt "RSS / Atom feed URL: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
     run "camulus-reader-browse-entry" [alist ["name",s ("RSS / Atom: "^url);
       "source",s (R.source_of_url url);"group",s url]]);
  register_local_groups ~option:"camulus-rss-subscriptions" ~source:R.source_of_url
    ~accepts:(fun ~source ~group->String.equal (R.url_of_source source) group) { (base ~name:"RSS / Atom" ~matches:(String.is_prefix ~prefix:"rss:") ~threaded:false
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Read(String.drop_prefix source 4,id))))) with
    attachment=Some (fun ~source ~id ~group ~part ~destination -> ignore group; Value.of_int_exn (submit (Attachment(String.drop_prefix source 4,id,part,destination))));
    events=["rss"];
    browser=Some ("RSS / Atom", "camulus-reader-rss") }
