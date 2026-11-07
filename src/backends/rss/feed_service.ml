open! Core
open Reader.Service
module Protocol = Reader.Protocol
module CS = Reader.Content_state
module R = Rss_backend
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
  let create ?subscriptions_file:file () ~net:_ ~now:_ ~sleep:_ ~groups:_ =
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
         | _ -> false)}
end
