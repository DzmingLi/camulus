module Http=Http
module Decode=Decode
module D=Decode
module C=Reader.Content
let source="discourse"
let source_of_site site="discourse:"^D.site site
let is_source=String.starts_with ~prefix:"discourse:"
let site_of_source source=
  if not(is_source source) then invalid_arg "Not a Discourse source";
  D.site(String.sub source 10 (String.length source-10))
type config={site:string;api_key:string;client_id:string}
let config ?(api_key="") ?(client_id="") site =
  let safe s=String.for_all(fun c -> (c>='a' && c<='z') || (c>='A' && c<='Z') || (c>='0' && c<='9') || c='-' || c='_') s in
  if not(safe api_key && safe client_id) || (api_key="")<>(client_id="") then invalid_arg "Invalid Discourse User API credentials";
  {site=D.site site;api_key;client_id}
type group=Latest | Category of int | Topic of int | Watched_topics | Tracked_topics | Notifications | Replies
type cursor=string
let group_of_string = function
  | "latest" -> Latest
  | "notifications" -> Notifications | "replies" -> Replies
  | "watching" -> Watched_topics | "tracking" -> Tracked_topics
  | s when String.starts_with ~prefix:"category:" s -> Category(D.number(String.sub s 9 (String.length s-9)))
  | s when String.starts_with ~prefix:"topic:" s -> Topic(D.number(String.sub s 6 (String.length s-6)))
  | _ -> invalid_arg "Use latest, category:ID, topic:ID, watching or tracking"
let group_string=function Notifications->"notifications" | Replies->"replies" | Watched_topics->"watching" | Tracked_topics->"tracking" | Latest -> "latest" | Category n -> "category:"^string_of_int(D.positive n) | Topic n -> D.topic_id n
type draft=Reply of {parent:string;text:string} | New_topic of {category:int;title:string;text:string}
type receipt=D.article
type tracking=Regular | Tracking | Watching | Muted
let tracking_code=function Muted->0 | Regular->1 | Tracking->2 | Watching->3
let tracking_of_string=function "regular"->Regular | "tracking"->Tracking | "watching"->Watching | "muted"->Muted | _->invalid_arg "Invalid Discourse tracking level"
type operation=Categories | List of group*cursor option | Search of group*string*cursor option
 | Thread of string | Read of string | Like of string*bool | Send of draft
 | Track of string*tracking | Topic_state of string | Attachment of string*int*string
let service=function Like _|Send _|Track _|Attachment _ -> true | _ -> false
type result=Page of C.page | Thread_batch of C.thread_batch | Article of D.article
 | Category_list of Yojson.Safe.t | Liked of D.article | Sent of D.article
 | Tracked of string*tracking | Topic_state_result of string*int | Saved of string
exception Uncertain_write
let error_write ()=failwith "Discourse write outcome is uncertain; check the forum before retrying"
module Make(H:Http.S)=struct
 type t={config:config;categories:(int,string) Hashtbl.t;lock:Eio.Mutex.t}
 type nonrec group=group
 type nonrec cursor=cursor
 let source=source
 let create config={config;categories=Hashtbl.create 16;lock=Eio.Mutex.create ()}
 let headers t=["Accept","application/json"] @
   (if t.config.api_key="" then [] else ["User-Api-Key",t.config.api_key;"User-Api-Client-Id",t.config.client_id])
 let call t ?(meth=Http.Get) path params =
   let writing=meth<>Http.Get in
   if writing && t.config.api_key="" then failwith "Authorize this forum with discourse-auth-login before writing";
   let url=t.config.site^path in
   let uri=Uri.of_string url in
   let url=if writing then url else Uri.add_query_params' uri params |> Uri.to_string in
   let body=if writing then Uri.encoded_of_query(List.map(fun(k,v)->k,[v])params) else "" in
   let r=try H.request {meth;url;body;limit=16*1024*1024;headers=headers t @ (if writing then ["Content-Type","application/x-www-form-urlencoded"] else [])}
     with Http.Transport_error _ -> if writing then raise Uncertain_write else failwith "Discourse network failure" in
   if writing && (r.status>=500 || r.status=408) then raise Uncertain_write;
   if r.status=429 then failwith "Discourse rate limit reached; retry later";
   if r.status=401 || r.status=403 then failwith "Discourse authorization or access denied";
   if r.status<200 || r.status>=300 then failwith(Printf.sprintf "Discourse HTTP %d (redirects are not followed for API credentials)" r.status);
   try let j=Yojson.Safe.from_string r.body in
     if D.field "errors" j<>`Null then failwith "Discourse rejected the operation";j
   with Yojson.Json_error _ -> if writing then raise Uncertain_write else failwith "Invalid Discourse JSON"
 let categories t =
   let j=call t "/categories.json" ["include_subcategories","true"] in
   let xs=D.list "categories" (D.field "category_list" j) in
   List.iter(fun c -> Hashtbl.replace t.categories (D.positive(D.num "id" c)) (D.str "slug" c)) xs;
   `List(List.map(fun c -> `Assoc["id",`Int(D.num "id" c);"name",`String(D.str "name" c);"group",`String(group_string(Category(D.num "id" c)))])xs)
 let category_slug t n = match Hashtbl.find_opt t.categories n with Some s->s | None ->
   let j=call t (Printf.sprintf "/c/%d/show.json" (D.positive n)) [] |> D.field "category" in
   if D.num "id" j<>n then failwith "Unexpected Discourse category";
   let slug=D.str "slug" j in if slug="" then failwith "Missing category slug";
   Hashtbl.replace t.categories n slug;slug
 let cursor_scope group query=Yojson.Safe.to_string(`List[`String(group_string group);`String query])
 let page_number group query after =match after with None->0 | Some s ->
   let j=try Yojson.Safe.from_string s with _->invalid_arg "Invalid Discourse cursor" in
   let n=D.num "page" j in
   if D.str "scope" j<>cursor_scope group query || n<1 || n>10000 then invalid_arg "Cursor belongs to another Discourse group or query";n
 let cursor group query n = Some(Yojson.Safe.to_string(`Assoc["scope",`String(cursor_scope group query);"page",`Int(n+1)]))
 let personal_list t group ~query ~after =
   if t.config.api_key="" then failwith "Personal Discourse entries require authorization";
   let n=page_number group query after in
   let j=call t "/latest.json" (["state",group_string group;"page",string_of_int n] @
     (if query="" then [] else ["search",query])) |> D.field "topic_list" in
   {C.messages=List.map(D.summary ~site:t.config.site)(D.list "topics" j);notifications=[];
    after=(if D.str "more_topics_url" j<>"" then cursor group query n else None)}
 let notification_list t group ~after =
   if t.config.api_key="" then failwith "Discourse notifications require authorization";
   let page=page_number group "" after in
   let j=call t "/notifications.json" ["offset",string_of_int(page*60);"limit","60";"silent","true"] in
   let notifications=D.list "notifications" j in
   let messages=List.filter_map(fun n ->
     let kind=D.num "notification_type" n in
     if not(List.mem kind (if group=Replies then [2] else [1;2;3])) || D.num "topic_id" n<=0 || D.num "post_number" n<=0 then None else
     let topic=D.positive(D.num "topic_id" n) and floor=D.positive(D.num "post_number" n) in
     let data=match D.field "data" n with `String s -> Yojson.Safe.from_string s | x->x in
     let title=match D.str "topic_title" data with ""->"Topic "^string_of_int topic | title->title in
     let author=D.str "display_username" data in
     Some{(D.base_message ~site:t.config.site ~topic ~floor ~title ~author ~created:(D.date(D.str "created_at" n))) with
       activity=Some("notification:"^string_of_int(D.positive(D.num "id" n)))}) notifications in
   {C.messages;notifications=[];after=(if (page+1)*60 < D.num "total_rows_notifications" j then cursor group "" page else None)}
 let list t group ~after =
   let n=page_number group "" after in
   match group with
   | Notifications | Replies -> notification_list t group ~after
   | Watched_topics | Tracked_topics -> personal_list t group ~query:"" ~after
   | Topic id ->
     if after<>None then invalid_arg "Single topic has no list cursor";
     let j=call t (Printf.sprintf "/t/%d.json" (D.positive id)) [] in
     if D.num "id" j<>id then failwith "Unexpected Discourse topic";
     {C.messages=[D.summary ~site:t.config.site j];notifications=[];after=None}
   | Latest | Category _ ->
     let path=match group with Latest->"/latest.json" | Category id -> Printf.sprintf "/c/%s/%d.json" (Uri.pct_encode(category_slug t id)) id | _->assert false in
     let j=call t path ["page",string_of_int n;"no_subcategories","true"] |> D.field "topic_list" in
     let topics=D.list "topics" j in
     let topics=List.filter(fun j -> match group with Category id -> D.num "category_id" j=id | _->true) topics in
     {C.messages=List.map(D.summary ~site:t.config.site) topics;notifications=[];
      after=(if D.str "more_topics_url" j<>"" then cursor group "" n else None)}
 let search t group ~query ~after =
   if group=Notifications || group=Replies then failwith "Discourse notification entries do not provide remote search";
   if group=Watched_topics || group=Tracked_topics then personal_list t group ~query ~after else
   let n=page_number group query after in
   let scoped=query^(match group with Watched_topics|Tracked_topics|Notifications|Replies->assert false | Latest->"" | Category id->" category:"^string_of_int id | Topic id->" topic:"^string_of_int id) in
   let j=call t "/search.json" ["q",scoped;"page",string_of_int(n+1)] in
   let topics=D.optional_list "topics" j in
   let hits=D.optional_list "posts" j in
   let seen=Hashtbl.create 32 in
   let messages=List.filter_map(fun hit ->
     let topic=D.num "topic_id" hit in
     let meta=List.find_opt(fun x -> D.num "id" x=topic) topics in
     match meta with None->failwith "Search hit has no topic metadata" | Some meta ->
     if Hashtbl.mem seen topic || not(match group with Watched_topics|Tracked_topics|Notifications|Replies->assert false | Latest->true | Category n->D.num "category_id" meta=n | Topic n->topic=n) then None else begin
       Hashtbl.add seen topic ();
       Some(D.summary ~site:t.config.site meta)
     end) hits in
   {C.messages;notifications=[];after=(if D.field "more_full_page_results" (D.field "grouped_search_result" j)=`Bool true then cursor group query n else None)}
 let thread t id ~emit_article ~emit =
   let topic,_=D.parse_id id in
   let j=call t (Printf.sprintf "/t/%d.json" topic) [] in
   if D.num "id" j<>topic then failwith "Unexpected Discourse topic";
   let title=D.str "title" j in if title="" then failwith "Missing Discourse title";
   let stream=D.field "post_stream" j in
   let ids=D.list "stream" stream |> List.map(fun n -> D.positive(D.integer n)) in
   if ids=[] || List.length ids>20000 || List.length(List.sort_uniq compare ids)<>List.length ids then failwith "Invalid or oversized Discourse post stream";
   let known=Hashtbl.create 64 and floors=Hashtbl.create 64 in
   let bytes=ref 0 in
   let ingest posts =
     let articles=List.map(D.post ~site:t.config.site ~topic ~title) posts in
     List.iter(fun a ->
       if not(List.mem a.D.post_id ids) || Hashtbl.mem known a.post_id || Hashtbl.mem floors a.floor then failwith "Unexpected or duplicate Discourse post";
       bytes:= !bytes+String.length(Option.value a.message.html ~default:"");
       if !bytes>128*1024*1024 then failwith "Discourse topic exceeds 128 MiB";
       Hashtbl.add known a.post_id ();Hashtbl.add floors a.floor a;
       emit_article a) articles;
     let remaining=List.filter(fun id -> not(Hashtbl.mem known id)) ids in
     let placeholders=Hashtbl.create 16 in
     Hashtbl.iter(fun _ a ->
       let _,parent=match a.D.message.parent_id with Some p -> D.parse_id p | None -> topic,None in
       Option.iter(fun floor -> if not(Hashtbl.mem floors floor) then Hashtbl.replace placeholders floor ()) parent) floors;
     let missing=Hashtbl.fold(fun floor () acc ->
       { (D.base_message ~site:t.config.site ~topic ~floor ~title ~author:"Unavailable parent" ~created:0.) with
         unavailable=true;body="Parent post is not loaded, deleted, or inaccessible." }::acc) placeholders [] in
     emit {C.thread=D.topic_id topic;messages=missing @ List.map(fun a->a.D.message) articles;
       gaps=(if remaining=[] then [] else [{C.parent=D.article_id topic 1;count=List.length remaining}]);complete=remaining=[]};
     remaining in
   let rec load remaining = match remaining with []->() | _ ->
     let rec split n acc xs=if n=0 then List.rev acc,xs else match xs with []->List.rev acc,[] | x::xs->split(n-1)(x::acc)xs in
     let batch,_=split 20 [] remaining in
     let j=call t (Printf.sprintf "/t/%d/posts.json" topic) (List.map(fun n->"post_ids[]",string_of_int n)batch) in
     let posts=D.list "posts" (D.field "post_stream" j) in
     if List.sort compare (List.map(D.num "id")posts)<>List.sort compare batch then failwith "Incomplete Discourse page; refresh to reconcile deleted or inaccessible posts";
     load(ingest posts) in
   load(ingest(D.list "posts" stream))
 let read t id =
   let topic,floor=D.parse_id id in let floor=Option.value floor ~default:1 in
   let j=call t (Printf.sprintf "/t/%d/%d.json" topic floor) [] in
   if D.num "id" j<>topic then failwith "Unexpected Discourse topic";
   let posts=D.list "posts" (D.field "post_stream" j) in
   let post=match List.find_opt(fun p -> D.num "post_number" p=floor) posts with Some p->p | None->failwith "Discourse article unavailable" in
   D.post ~site:t.config.site ~topic ~title:(D.str "title" j) post
 let set_like_article t ~id ~liked =
   Eio.Mutex.lock t.lock;
   Fun.protect ~finally:(fun ()->Eio.Mutex.unlock t.lock) @@ fun () ->
   if t.config.api_key="" then failwith "Authorize this forum before liking";
   let a=read t id in if a.liked=liked then a else
   let meth,path,params=if liked then Http.Post,"/post_actions.json",["id",string_of_int a.post_id;"post_action_type_id","2"]
     else Http.Delete,Printf.sprintf "/post_actions/%d.json" a.post_id,["post_action_type_id","2"] in
   let j=call t ~meth path params in
   try let topic,_=D.parse_id id in let b=D.post ~site:t.config.site ~topic ~title:a.message.title j in
     if b.post_id<>a.post_id || b.liked<>liked then raise Uncertain_write;b
   with _ -> raise Uncertain_write
 let set_like t ~id ~liked=ignore(set_like_article t ~id ~liked)
 let send t draft =
   let nonempty s=if String.trim s="" then invalid_arg "Empty Discourse post" in
   let params,expected,title=match draft with
     | Reply{parent;text} -> nonempty text;let topic,floor=D.parse_id parent in let floor=Option.value floor ~default:1 in
       ["topic_id",string_of_int topic;"reply_to_post_number",string_of_int floor;"raw",text],Some(topic,floor),""
     | New_topic{category;title;text} -> nonempty title;nonempty text;
       ["category",string_of_int(D.positive category);"title",title;"raw",text],None,title in
   let j=call t ~meth:Http.Post "/posts.json" params in
   try let topic=D.positive(D.num "topic_id" j) in
     (match expected with Some(t,f) when t<>topic || D.num "reply_to_post_number" j<>f -> raise Uncertain_write | _ -> ());
     let a=D.post ~site:t.config.site ~topic ~title j in
     if expected=None && a.floor<>1 then raise Uncertain_write;a
   with _ -> raise Uncertain_write
 let track t id level =
   let topic,_=D.parse_id id in
   let j=call t ~meth:Http.Post (Printf.sprintf "/t/%d/notifications.json" topic) ["notification_level",string_of_int(tracking_code level)] in
   if D.str "success" j<>"OK" && D.field "success" j<>`Bool true then raise Uncertain_write
 let topic_state t id =
   let topic,_=D.parse_id id in
   let j=call t (Printf.sprintf "/t/%d.json" topic) [] in
   if D.num "id" j<>topic then failwith "Unexpected Discourse topic";
   match D.field "notification_level" (D.field "details" j) with `Int n when n>=0 && n<=3 -> n | _->failwith "Discourse tracking state is unavailable"
 module Subscription = struct
   type nonrec t = t
   type target = string
   type settings = tracking
   let get t id = match topic_state t id with
     | 0 -> Muted | 1 -> Regular | 2 -> Tracking | 3 -> Watching
     | _ -> assert false
   let set = track
 end
 let download t ?(limit=128*1024*1024) url =
   let rec fetch visited url =
     ignore(Http.validate url);
     if List.length visited>=6 || List.mem url visited then failwith "Discourse attachment redirect limit";
     let uri=Uri.of_string url in
     let mount=Uri.path(Uri.of_string t.config.site) in
     let path=Uri.path uri in
     let authenticated=Http.same_origin t.config.site url && (mount="" || String.starts_with ~prefix:(mount^"/") path) in
     let r=H.request {Http.meth=Get;url;body="";limit;headers=(if authenticated then headers t else [])} in
     if List.mem r.status [301;302;303;307;308] then
       match List.assoc_opt "location" r.headers with Some next->fetch(url::visited)(Http.resolve url next) | None->failwith "Attachment redirect missing location"
     else if r.status<>200 then failwith(Printf.sprintf "Discourse attachment HTTP %d" r.status) else r.body in
   fetch [] url
 let archive_article t a =
   let total=ref(String.length(Option.value a.D.message.html ~default:"")) in
   let files=List.map(fun file ->
     let data=download t ~limit:(128*1024*1024 - !total) file.D.url in
     total:= !total+String.length data;
     if !total>128*1024*1024 then failwith "Discourse archive exceeds 128 MiB";
     file.part,data) a.attachments in
   {Reader.Content_state.message=a.message;attachments=D.attachments_json a;raw=None;files}
 let attachment t id part destination =
   let a=read t id in
   let file=match List.find_opt(fun f->f.D.part=part)a.attachments with Some f->f | None->invalid_arg "No such attachment" in
   let data=download t file.url in
   Eio_unix.run_in_systhread(fun () ->
     let fd=Unix.openfile destination [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_EXCL;Unix.O_CLOEXEC] 0o600 in
     let ch=Unix.out_channel_of_descr fd in
     try output_string ch data;flush ch;Unix.fsync fd;close_out ch
     with ex->close_out_noerr ch;(try Unix.unlink destination with _->());raise ex)
 let execute t operation ~emit =
   try match operation with
   | Categories -> emit(Category_list(categories t))
   | List(g,c) -> emit(Page(list t g ~after:c))
   | Search(g,q,c) -> emit(Page(search t g ~query:q ~after:c))
   | Thread id -> thread t id ~emit_article:(fun a->emit(Article a)) ~emit:(fun b->emit(Thread_batch b))
   | Read id -> emit(Article(read t id))
   | Like(id,liked) -> emit(Liked(set_like_article t ~id ~liked))
   | Send draft -> emit(Sent(send t draft))
   | Track(id,level) -> Subscription.set t id level;emit(Tracked(id,level))
   | Topic_state id -> emit(Topic_state_result(id,tracking_code(Subscription.get t id)))
   | Attachment(id,part,path) -> attachment t id part path;emit(Saved path)
   with Uncertain_write -> error_write ()
 let stream_thread=thread
 let thread t id ~emit=stream_thread t id ~emit_article:ignore ~emit
end
module Live=Make(Curl_transport)
module Subscription = Live.Subscription
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor = Live
module Threading : Reader.Backend.Threading with type t=Live.t = Live
module Liking : Reader.Backend.Liking with type t=Live.t = Live
module Writing = struct type t=Live.t type nonrec draft=draft type nonrec receipt=receipt let send=Live.send end
let operation_of_json j =
 let str k=let s=D.str k j in if s="" then invalid_arg("Missing "^k);s in
 let id ()=let id=str "id" in ignore(D.parse_id id);id in
 let after=match D.field "after" j with `String s->Some s | `Null->None | _->invalid_arg "Invalid cursor" in
 match str "op" with
 | "categories"->Categories
 | "list"->List(group_of_string(str "group"),after)
 | "search"->Search(group_of_string(str "group"),str "query",after)
 | "thread"->Thread(id()) | "read"->Read(id())
 | "like"->let liked=match D.field "liked" j with `Bool b->b | _->invalid_arg "Missing liked boolean" in Like(id(),liked)
 | "reply"->Send(Reply{parent=id();text=str "text"})
 | "submit"->Send(New_topic{category=D.positive(D.num "category" j);title=str "title";text=str "text"})
 | "track"->Track(id(),tracking_of_string(str "level"))
 | "topic-state"->Topic_state(id())
 | "attachment"->let part=match D.field "part" j with `Int n when n>=0->n | _->invalid_arg "Invalid attachment part" in Attachment(id(),part,str "destination")
 | _->invalid_arg "Unknown Discourse operation"
let result_json = function
 | Page p -> `Assoc["kind",`String "page";"messages",`List(List.map Reader.Content_state.message_json p.messages);"after",(match p.after with None->`Null | Some s->`String s)]
 | Thread_batch b -> `Assoc["kind",`String "thread";"thread",`String b.thread;"messages",`List(List.map Reader.Content_state.message_json b.messages);"complete",`Bool b.complete;
   "gaps",`List(List.map(fun g->`Assoc["parent",`String g.C.parent;"count",`Int g.count])b.gaps)]
 | Article a -> D.article_json a
 | Liked a | Sent a as result -> `Assoc["kind",`String(match result with Liked _->"liked" | _->"sent");"article",D.article_json a]
 | Category_list rows -> `Assoc["kind",`String "categories";"rows",rows]
 | Tracked(id,level)->`Assoc["kind",`String "tracked";"id",`String id;"level",`Int(tracking_code level)]
 | Topic_state_result(id,level)->`Assoc["kind",`String "topic-state";"id",`String id;"level",`Int level]
 | Saved path -> `Assoc["kind",`String "saved";"path",`String path]
