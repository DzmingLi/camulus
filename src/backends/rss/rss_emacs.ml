open! Core
open! Ecaml
open Reader_backend
module Protocol = Reader.Protocol
module CS = Reader.Content_state
module R = Rss_backend

(* RSS and JSON Feed share article presentation, while their codecs and cursors
   remain owned by their respective backend modules. *)
module type Feed = sig
  type t
  val name : string
  val label : string
  val is_source : string -> bool
  val source_of_url : string -> string
  val url_of_source : string -> string
  val create : ?subscriptions_file:string -> unit -> t
  val list : t -> string -> after:string option -> Reader.Content.page
  val search : t -> string -> query:string -> after:string option -> Reader.Content.page
  val read : t -> string -> string -> R.Decode.article
  val archive : t -> string -> string -> CS.archived
  val attachment : t -> string -> string -> int -> string -> unit
  val refresh : t -> string -> Reader.Content.page
  val followed : t -> string list
  val get : t -> string -> bool
  val set : t -> string -> bool -> unit
end
module Connect (F:Feed) = struct
  type Protocol.work += Request of R.operation | Followed
  let subscriptions_file = ref None
  let request ~submit operation = submit ~service:(match operation with R.Attachment _ -> true | _ -> false) (Request operation)
  let followed ~submit () = submit ~service:true Followed
  let prepare ~notify:_ =
    let file= !subscriptions_file in
    Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
      let backend=F.create ?subscriptions_file:file () in
      {matches_source=F.is_source;
       page=(fun ~source group ~after ~query ->
         if not(String.equal (F.url_of_source source) group) then invalid_arg "Feed source and entry URL differ";
         match query with None -> F.list backend group ~after | Some query -> F.search backend group ~query ~after);
       archive=(fun ~source ~refresh target id ~native_thread:_ ~add ->
         if Poly.equal target CS.Thread then failwith "Feed articles do not have threads";
         let url=F.url_of_source source in
         if refresh then ignore(F.refresh backend url : Reader.Content.page);
         add(F.archive backend url id);true);
       execute=(fun work ~observe ~emit -> match work with
         | Followed ->
           reply emit "entries" ["rows",`List(List.map (F.followed backend) ~f:(fun url ->
             `Assoc["name",`String(F.label^": "^url);"source",`String(F.source_of_url url);"group",`String url]))];true
         | Subscription(source,target,setting) when F.is_source source ->
           if not(String.equal (F.url_of_source source) target) then invalid_arg "Feed subscription target differs from source";
           Option.iter setting ~f:(fun s -> F.set backend target (match s with "subscribed" -> true | "unsubscribed" -> false | _ -> invalid_arg "Invalid subscription setting"));
           reply emit "subscription" ["source",`String source;"target",`String target;
             "setting",`String(if F.get backend target then "subscribed" else "unsubscribed");
             "choices",`List[`String "unsubscribed";`String "subscribed"]];true
         | Request operation ->
           let url=R.operation_url operation in
           let observe=observe ~source:(F.source_of_url url) in
           let result data=emit(Presentation("rss",data)) in
           (match operation with
            | R.List _ | Search _ ->
              let page=match operation with Search(_,query) -> F.search backend url ~query ~after:None | _ -> F.list backend url ~after:None in
              observe page.messages;
              result(`Assoc["kind",`String "page";"messages",`List(List.map page.messages ~f:CS.message_json)])
            | Read(_,id) -> let article=F.read backend url id in observe [article.message];result(R.article_json article)
            | Attachment(_,id,part,destination) -> F.attachment backend url id part destination;reply emit "saved" ["path",`String destination]);true
         | _ -> false)})}
  let install_protocol ~ensure_ui ~idle ~submit =
    worker_factories := prepare :: !worker_factories;
    Defun.defun (Symbol.intern ("camulus-"^F.name^"-subscriptions-configure")) [%here]
      ~docstring:"Set backend subscription state before startup." (Returns Value.Type.unit)
      (let open Defun.Let_syntax in let%map_open path=required "path" string in
       ensure_ui ();if not(idle ()) then failwith "Stop runtime before changing subscription storage";
       if Stdlib.Filename.is_relative path then invalid_arg "Use an absolute state path";
       subscriptions_file:=Some path);
    Defun.defun (Symbol.intern ("camulus-"^F.name^"-submit")) [%here]
      ~docstring:"Submit a feed operation." (Returns Value.Type.int)
      (let open Defun.Let_syntax in let%map_open value=required "request" value in
       request ~submit (R.operation_of_json (to_json value)));
    Defun.defun_nullary (Symbol.intern ("camulus-"^F.name^"-followed")) [%here]
      ~docstring:"Discover this backend's subscriptions." (Returns Value.Type.int) (followed ~submit)
end
module Connection = Connect(struct
  include R.Live
  let create ?subscriptions_file ()=R.Live.create ?subscriptions_file ()
  let name="rss"
  let label="RSS"
  let is_source=R.is_source
  let source_of_url=R.source_of_url
  let url_of_source=R.url_of_source
  let get=Subscription.get
  let set=Subscription.set
  let refresh t url=list t url ~after:None
  let archive t url id=let a,files=archive t url id in
    {CS.message=a.message;attachments=R.Decode.attachments a;raw=None;files}
end)
let install_protocol = Connection.install_protocol

(* Publication backends reuse RSS article presentation; each backend supplies
   its own network operations and group rules when registering this worker. *)
type Protocol.work += Publication_read of string * string * string option
  | Publication_attachment of string * string * string option * int * string
let publication_commands ~submit source =
  (fun id group -> submit ~service:false (Publication_read(source,id,group))),
  (fun id group part destination -> submit ~service:true (Publication_attachment(source,id,group,part,destination)))
let publication_worker ~source ~group ~page ~read ~archive ~attachment ~refresh
    ?stream ?archive_article () =
  {matches_source=String.equal source;
   page=(fun ~source:_ group ~after ~query -> page group ~after ~query);
   archive=(fun ~source:_ ~refresh:reload target id ~native_thread:_ ~add ->
     let group=group id None in
     if reload then refresh group id;
     match target,stream,archive_article with
     | CS.Thread,Some stream,Some archive_article ->
       let complete=ref false in
       stream id ~emit_article:(fun a -> add(archive_article a)) ~emit:(fun (b:Reader.Content.thread_batch) -> complete:=b.complete);!complete
     | Thread,_,_ -> failwith "Publication articles do not have threads"
     | Article,_,_ -> add(archive group id);true);
   execute=(fun work ~observe ~emit ->
     let article (a:R.Decode.article) = observe ~source [a.message];emit(Presentation("rss",R.article_json a)) in
     match work with
     | Publication_read(s,id,requested) when String.equal s source ->
       (match stream with
        | Some stream -> stream id ~emit_article:article ~emit:(fun (b:Reader.Content.thread_batch) ->
          emit(Presentation("rss",`Assoc["kind",`String "thread";"thread",`String b.thread;
            "messages",`List(List.map b.messages ~f:CS.message_json);"complete",`Bool b.complete])))
        | None -> article(read (group id requested) id));true
     | Publication_attachment(s,id,requested,part,destination) when String.equal s source ->
       attachment (group id requested) id part destination;reply emit "saved" ["path",`String destination];true
     | _ -> false)}

let get name = Current_buffer.symbol_value (Symbol.intern name)
let set name value = run "set" [Value.intern name;value]
let refresh submit () =
  let active=get "camulus-reader--active" in
  if not (Value.eq active Value.nil) then run "camulus-probe-cancel" [active];
  let query=get "camulus-reader--rss-query" in
  let url=String.drop_prefix (Value.to_utf8_bytes_exn (get "camulus-reader--source")) 4 in
  let operation=if Value.eq query Value.nil then Rss_backend.List url else Search(url,Value.to_utf8_bytes_exn query) in
  let id=Value.of_int_exn (submit operation) in
  set "camulus-reader--active" id;
  run "puthash" [id;Buffer.to_value (Current_buffer.get ());get "camulus-reader--loads"]
let install ~submit () =
  let followed=Connection.followed ~submit and submit=Connection.request ~submit in
  Defun.defun (Symbol.intern "camulus-reader-rss") [%here] ~docstring:"Browse an RSS or Atom feed without following it."
    ~interactive:(Prompt "RSS / Atom feed URL: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
     run "camulus-reader-start" [];
     run "switch-to-buffer" [Buffer.to_value (Buffer.find_or_create ~name:("*Reader feed: "^url^"*"))];
     run "camulus-reader-rss-mode" [];set "camulus-reader--source" (s ("rss:"^url));refresh submit ());
  Defun.defun_nullary_nil (Symbol.intern "camulus-reader-rss-refresh") [%here] ~docstring:"Refresh this feed."
    ~interactive:No_arg (refresh submit);
  Defun.defun (Symbol.intern "camulus-reader-rss-search") [%here] ~docstring:"Search this feed's published entries."
    ~interactive:(Prompt "Search this feed (empty clears): ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open query=required "query" string in
     set "camulus-reader--rss-query" (if String.is_empty query then Value.nil else s query);refresh submit ());
  custom ~kind:Customization.Type.File "camulus-reader-rss-subscriptions-file" (data_path "XDG_STATE_HOME" "~/.local/state" "rss-subscriptions.json") "Backend state file.";
  register { (base ~name:"RSS / Atom" ~matches:(String.is_prefix ~prefix:"rss:") ~threaded:false
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Rss_backend.Read(String.drop_prefix source 4,id))))) with
    attachment=Some (fun ~source ~id ~group ~part ~destination -> ignore group; Value.of_int_exn (submit (Rss_backend.Attachment(String.drop_prefix source 4,id,part,destination))));
    start=(fun()->run "camulus-rss-subscriptions-configure"[get "camulus-reader-rss-subscriptions-file"]);
    events=["rss"];can_subscribe_group=(fun ~source ~group -> String.equal group (String.drop_prefix source 4));
    subscription_target=Some (fun ~source ~selected:_ -> Some (String.drop_prefix source 4));
    browser=Some ("RSS / Atom", "camulus-reader-rss");followed=Some (fun () -> Pending(Value.of_int_exn (followed ()))) };
  ignore (Major_mode.define_derived_mode (Symbol.intern "camulus-reader-rss-mode") [%here]
    ~docstring:"Browse one RSS or Atom feed." ~mode_line:"RSS / Atom"
    ~parent:(Major_mode.find_or_wrap_existing [%here] (Symbol.intern "tabulated-list-mode"))
    ~define_keys:(List.map ["s","camulus-reader-subscription";"RET","camulus-reader-open";"m","camulus-reader-save";
      "g","camulus-reader-rss-refresh";"/","camulus-reader-rss-search";"h","camulus"] ~f:(fun (key,command)->key,Symbol.intern command))
    ~initialize:(Returns Value.Type.unit,(fun () ->
      run "set" [Value.intern "tabulated-list-format";call "vector" (List.map ["New",3;"Source",24;"Author",20;"Article",70]
        ~f:(fun (name,width)->Value.list [s name;Value.of_int_exn width;Value.intern "t"]))];
      run "set" [Value.intern "header-line-format";s "RET: read  m: save  /: search this feed  g: refresh  h: home"];
      run "tabulated-list-init-header" [])) () : (module Major_mode.S))
