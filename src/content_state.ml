module Q = Sqlite3
module M = Content
type t = Q.db
type target = Article | Thread
type view_filter = { unread_only : bool; favorite_only : bool; days : int option }
type view_query = Inbox_view of (string * string) list * view_filter
                | Group_view of string * string | Folder_view of string
type archived = { message : M.message; attachments : Yojson.Safe.t; raw : string option; files : (int * string) list }
let text s = Q.Data.TEXT s
let int i = Q.Data.INT (Int64.of_int i)
let opt = function None -> Q.Data.NULL | Some s -> text s
let check db rc = if rc <> Q.Rc.OK && rc <> Q.Rc.DONE then failwith ("Content state: " ^ Q.errmsg db)
let exec db sql = check db (Q.exec db sql)
let query db sql args =
  let st = Q.prepare db sql in
  Fun.protect ~finally:(fun () -> ignore (Q.finalize st)) @@ fun () ->
  check db (Q.bind_values st args);
  let rec rows acc = match Q.step st with
    | Q.Rc.ROW -> rows (Array.init (Q.column_count st) (Q.column st) :: acc)
    | Q.Rc.DONE -> List.rev acc
    | rc -> check db rc; assert false in
  rows []
let run db sql args = ignore (query db sql args)
let s = function Q.Data.TEXT s | Q.Data.BLOB s -> s | _ -> ""
let n = function Q.Data.INT n -> Int64.to_int n | _ -> 0
let transaction db f =
  exec db "BEGIN IMMEDIATE";
  match f () with
  | result -> (try exec db "COMMIT"; result with ex -> ignore(Q.exec db "ROLLBACK");raise ex)
  | exception ex -> ignore(Q.exec db "ROLLBACK"); raise ex
let rec directory path =
  if Sys.file_exists path then (if not (Sys.is_directory path) then failwith "State parent is not a directory")
  else (directory (Filename.dirname path); try Unix.mkdir path 0o700 with Unix.Unix_error(Unix.EEXIST,_,_) -> ())
let rss_source = String.starts_with ~prefix:"rss:"
let atlantic_source = String.equal "atlantic"
(* Standalone feeds can publish a new version even after the article was archived. *)
let refreshable_article source = rss_source source || atlantic_source source
(* Archive blobs are indexed by enclosure part number. Keep their manifest
   consistent with the presentation when revisions change the attachment list. *)
let resource_manifest article =
  Yojson.Safe.Util.member "attachments" article |> Yojson.Safe.to_string
  |> Digest.string |> Digest.to_hex
let read_article row = Yojson.Safe.from_string (s row.(1))
let rss_deleted db ~source ~id =
  rss_source source &&
  query db "SELECT 1 FROM rss_deletions WHERE backend_key=? AND id=?" [text source;text id] <> []
let initialize db =
  Q.busy_timeout db 2000;
  exec db "PRAGMA foreign_keys=ON";
  transaction db (fun () ->
    match query db "PRAGMA user_version" [], query db "PRAGMA application_id" [] with
    | [version],[app] when n version.(0)=1 && n app.(0)=0x43414D55 -> ()
    | [version],[app] when n version.(0)=0 && n app.(0)=0 &&
        query db "SELECT 1 FROM sqlite_schema WHERE name NOT LIKE 'sqlite_%' LIMIT 1" []=[] ->
      exec db "CREATE TABLE items(backend_key TEXT NOT NULL,id TEXT NOT NULL,parent TEXT,thread TEXT,title TEXT NOT NULL,author TEXT NOT NULL,url TEXT NOT NULL,created REAL NOT NULL,is_read INTEGER NOT NULL DEFAULT 0,PRIMARY KEY(backend_key,id));
CREATE TABLE item_links(backend_key TEXT NOT NULL,child TEXT NOT NULL,ancestor TEXT NOT NULL,PRIMARY KEY(backend_key,child,ancestor));
CREATE TABLE activities(seq INTEGER PRIMARY KEY AUTOINCREMENT,backend_key TEXT NOT NULL,grp TEXT NOT NULL,id TEXT NOT NULL,revision TEXT NOT NULL,occurred REAL NOT NULL,UNIQUE(backend_key,grp,id,revision));
CREATE TABLE entry_members(backend_key TEXT NOT NULL,grp TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(backend_key,grp,id));
CREATE TABLE dismissals(backend_key TEXT NOT NULL,grp TEXT NOT NULL,id TEXT NOT NULL,through_seq INTEGER NOT NULL,PRIMARY KEY(backend_key,grp,id));
CREATE TABLE folders(path TEXT PRIMARY KEY,parent TEXT REFERENCES folders(path) ON DELETE CASCADE);
CREATE TABLE favorites(folder TEXT NOT NULL REFERENCES folders(path) ON DELETE CASCADE,backend_key TEXT NOT NULL,kind TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(folder,backend_key,kind,id));
CREATE TABLE archives(backend_key TEXT NOT NULL,id TEXT NOT NULL,payload TEXT NOT NULL,raw BLOB, resources_complete INTEGER NOT NULL DEFAULT 1,PRIMARY KEY(backend_key,id));
CREATE TABLE archive_files(backend_key TEXT NOT NULL,id TEXT NOT NULL,part INTEGER NOT NULL,data BLOB NOT NULL,PRIMARY KEY(backend_key,id,part),FOREIGN KEY(backend_key,id) REFERENCES archives(backend_key,id) ON DELETE CASCADE);
CREATE TABLE cached_threads(backend_key TEXT NOT NULL,id TEXT NOT NULL,complete INTEGER NOT NULL,PRIMARY KEY(backend_key,id));
CREATE TABLE rss_deletions(backend_key TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(backend_key,id));
INSERT INTO folders VALUES('',NULL);
PRAGMA application_id=0x43414D55;
PRAGMA user_version=1"
    | _ -> failwith "Unsupported Camulus database format")
let close_db db = ignore (Q.db_close db)
let open_db path =
  if Filename.is_relative path then invalid_arg "State database path must be absolute";
  directory (Filename.dirname path);
  let fd = Unix.openfile path [Unix.O_CREAT;Unix.O_RDWR;Unix.O_CLOEXEC] 0o600 in
  Unix.close fd;
  let db = Q.db_open path in
  match initialize db with
  | () -> db
  | exception ex -> close_db db; raise ex

let validate s = if s="" || String.contains s '\000' then invalid_arg "Empty or invalid state identity"
let observe ?(replace=false) db ~source ~group messages =
  validate source;
  transaction db (fun () ->
    (match group with Some group when replace ->
      run db "DELETE FROM entry_members WHERE backend_key=? AND grp=?" [text source;text group] | _ -> ());
    List.iter (fun (m:M.message) ->
      validate m.id;
      if not (rss_deleted db ~source ~id:m.id) then begin
        run db "INSERT INTO items(backend_key,id,parent,thread,title,author,url,created) VALUES(?,?,?,?,?,?,?,?) ON CONFLICT(backend_key,id) DO UPDATE SET parent=coalesce(excluded.parent,items.parent),thread=coalesce(excluded.thread,items.thread),title=excluded.title,author=excluded.author,url=excluded.url,created=excluded.created"
          [text source;text m.id;opt m.parent_id;opt m.thread_id;text m.title;text m.author;text m.url;Q.Data.FLOAT m.created];
        List.iter(fun ancestor -> run db "INSERT OR IGNORE INTO item_links VALUES(?,?,?)" [text source;text m.id;text ancestor]) m.references;
        Option.iter (fun group ->
          validate group;
          run db "INSERT OR IGNORE INTO entry_members VALUES(?,?,?)" [text source;text group;text m.id];
          (* The backend's activity token is an opaque revision key, not a time.
             A repeated (source, group, message ID, token) keeps its local sequence,
             so refreshing an already dismissed update cannot resurrect it. A new
             token creates another sequence that can fall beyond the dismissal
             boundary. With no token, the message ID identifies its only activity.
             Display order uses the remote activity or creation time, never this sequence or
             the time at which we fetched the message. *)
          run db "INSERT INTO activities(backend_key,grp,id,revision,occurred) VALUES(?,?,?,?,?) ON CONFLICT(backend_key,grp,id,revision) DO UPDATE SET occurred=excluded.occurred"
            [text source;text group;text m.id;text (Option.value m.activity ~default:m.id);Q.Data.FLOAT (Option.value m.activity_at ~default:m.created)]
        ) group
      end) messages)
let mark_read db ~source ids value =
  transaction db (fun () -> List.iter (fun id ->
    run db "UPDATE items SET is_read=? WHERE backend_key=? AND id=?" [int(if value then 1 else 0);text source;text id]) ids)
(* An internal connected component, not a native thread ID. Unknown ancestors are
   included so replies merge as missing headers arrive. No subject-based merging. *)
let components db =
  let parents=Hashtbl.create 128 in
  let rec root k = match Hashtbl.find_opt parents k with
    | None -> Hashtbl.add parents k k;k
    | Some p when p=k -> k
    | Some p -> let r=root p in Hashtbl.replace parents k r;r in
  let union a b = let a=root a and b=root b in if a<>b then
    let first,last=if compare a b < 0 then a,b else b,a in Hashtbl.replace parents last first in
  let rows=query db "SELECT backend_key,id,parent,thread,title,author,url,created,is_read FROM items" [] in
  List.iter(fun r -> let key=s r.(0),s r.(1) in ignore(root key);
    List.iter(fun i -> if s r.(i)<>"" then union key (s r.(0),s r.(i))) [2;3]) rows;
  List.iter(fun r -> union (s r.(0),s r.(1)) (s r.(0),s r.(2)))
    (query db "SELECT backend_key,child,ancestor FROM item_links" []);
  root,rows
let favorite_components db root =
  let saved=Hashtbl.create 32 in
  List.iter(fun r -> Hashtbl.replace saved (root(s r.(0),s r.(1))) ())
    (query db "SELECT backend_key,id FROM favorites" []);
  saved
let snapshots db ~source ids =
  let root,_=components db in let saved=favorite_components db root in
  `Assoc ["backend_key",`String source;"items",`List(List.map (fun id ->
    let read = match query db "SELECT is_read FROM items WHERE backend_key=? AND id=?" [text source;text id] with
      | [r] -> n r.(0)<>0 | _ -> false in
    `Assoc["id",`String id;"read",`Bool read;"favorite",`Bool(Hashtbl.mem saved (root(source,id)))]) ids)]
let dismiss db ~source ~group ~id ~through =
  if through<0 then invalid_arg "Negative feed revision";
  run db "INSERT INTO dismissals VALUES(?,?,?,?) ON CONFLICT(backend_key,grp,id) DO UPDATE SET through_seq=max(through_seq,excluded.through_seq)" [text source;text group;text id;int through]
let unread_components root items =
  let unread=Hashtbl.create 128 in
  List.iter (fun row ->
    if n row.(8)=0 then Hashtbl.replace unread (root(s row.(0),s row.(1))) ()) items;
  unread
let rows_for_activities db ~dismissals ~events ~groups =
  let root,items=components db in
  let groups=groups root in
  let saved=favorite_components db root and unread=unread_components root items in
  let meta=Hashtbl.create 128 and last=Hashtbl.create 128 and removed=Hashtbl.create 32 in
  List.iter(fun r -> Hashtbl.replace meta (s r.(0),s r.(1)) r) items;
  List.iter(fun r -> let k=root(s r.(0),s r.(1)) in
    Hashtbl.replace removed k (max (n r.(2)) (Option.value (Hashtbl.find_opt removed k) ~default:0)))
    dismissals;
  let time=function Q.Data.FLOAT f -> f | Q.Data.INT n -> Int64.to_float n | _ -> 0. in
  List.iter(fun r -> let k=root(s r.(1),s r.(2)) in
    let latest=match Hashtbl.find_opt last k with
      | Some(_,old) when time old.(3)>time r.(3) -> old | _ -> r in
    let previous=match Hashtbl.find_opt last k with Some(revision,_) -> revision | None -> 0 in
    Hashtbl.replace last k (max previous (n r.(0)),latest)) events;
  Hashtbl.fold(fun key (revision,event) acc ->
    if revision<=Option.value(Hashtbl.find_opt removed key) ~default:0 then acc else
    match Hashtbl.find_opt meta (s event.(1),s event.(2)) with None -> acc | Some m ->
    let unread=Hashtbl.mem unread key in
    ((time event.(3),revision),`Assoc["backend_key",`String(s event.(1));"id",`String(s event.(2));
      "thread_id",(if s m.(3)="" then `Null else `String(s m.(3)));
      "title",`String(s m.(4));"author",`String(s m.(5));"unread",`Bool unread;
      "url",`String(s m.(6));
      "favorite",`Bool(Hashtbl.mem saved key);
      "conversation_id",`String(snd key);"occurred",`Float(time event.(3));
      "groups",`List(List.map (fun s -> `String s) (groups key));
      "revision",`Int revision])::acc) last []
  |> List.sort(fun(a,_)(b,_)->compare b a) |> List.map snd |> fun xs -> `List xs

let feed db ~source ~group =
  rows_for_activities db
    ~dismissals:(query db "SELECT backend_key,id,through_seq FROM dismissals WHERE backend_key=? AND grp=?" [text source;text group])
    ~events:(query db "SELECT seq,backend_key,id,occurred FROM activities WHERE backend_key=? AND grp=? AND id IN (SELECT id FROM entry_members WHERE backend_key=? AND grp=?) ORDER BY seq" [text source;text group;text source;text group])
    ~groups:(fun _ _ -> [group])

(* A private view scope, never a backend group or subscription. Reuse the
   existing dismissal format without changing the database schema. *)
let timeline_scope = "\001reader-home"
let dismiss_timeline db ~source ~id ~through =
  dismiss db ~source ~group:timeline_scope ~id ~through
let dismiss_notification db ~source ~group ~id ~through =
  transaction db (fun () ->
    dismiss db ~source ~group ~id ~through;
    dismiss_timeline db ~source ~id ~through)

let revisions db ~source ~group ~id ~through =
  query db "SELECT revision FROM activities WHERE backend_key=? AND grp=? AND id=? AND seq<=? AND seq>coalesce((SELECT through_seq FROM dismissals WHERE backend_key=? AND grp=? AND id=?),0)"
    [text source;text group;text id;int through;text source;text group;text id]
  |> List.map(fun row -> s row.(0))

let timeline db ~entries filter =
  let eligible=Hashtbl.create 128 and origins=Hashtbl.create 128 in
  List.iter (fun (source,group) ->
    List.iter (fun r ->
      let id=s r.(0) in
      Hashtbl.replace eligible (source,id,s r.(1)) ();
      let old=Option.value(Hashtbl.find_opt origins (source,id)) ~default:[] in
      Hashtbl.replace origins (source,id) (group::old))
      (query db "SELECT a.id,a.revision FROM activities a JOIN entry_members m ON m.backend_key=a.backend_key AND m.grp=a.grp AND m.id=a.id WHERE a.backend_key=? AND a.grp=?" [text source;text group]))
    (List.sort_uniq compare entries);
  (* The same revision discovered through a second entry must retain its first
     sequence number. Otherwise pagination or adding an entry revives an update
     already dismissed on the home timeline. *)
  let events=query db "SELECT min(seq),backend_key,id,min(occurred),revision FROM activities GROUP BY backend_key,id,revision ORDER BY min(seq)" []
    |> List.filter (fun r -> Hashtbl.mem eligible (s r.(1),s r.(2),s r.(4))) in
  let groups root =
    let index=Hashtbl.create (Hashtbl.length origins) in
    Hashtbl.iter (fun identity groups ->
      let thread=root identity in
      let old=Option.value (Hashtbl.find_opt index thread) ~default:[] in
      Hashtbl.replace index thread (List.rev_append groups old)) origins;
    fun key -> Option.value (Hashtbl.find_opt index key) ~default:[] |> List.sort_uniq String.compare in
  let rows=rows_for_activities db
    ~dismissals:(query db "SELECT backend_key,id,through_seq FROM dismissals WHERE grp=?" [text timeline_scope])
    ~events ~groups in
  let open Yojson.Safe.Util in
  let since=Option.map (fun days -> Unix.gettimeofday () -. (float_of_int days *. 86400.)) filter.days in
  `List (List.filter (fun row ->
    (not filter.unread_only || member "unread" row = `Bool true) &&
    (not filter.favorite_only || member "favorite" row = `Bool true) &&
    Option.fold ~none:true ~some:(fun cutoff -> to_number (member "occurred" row) >= cutoff) since)
    (to_list rows))

let folder_parts path =
  let parts=String.split_on_char '/' path in
  if List.exists(fun p -> p="" || p="." || p=".." || String.contains p '\000') parts then invalid_arg "Invalid folder path";
  parts
let mkdir db path =
  let parts=folder_parts path in
  transaction db (fun () -> ignore(List.fold_left(fun parent name ->
    let child=if parent="" then name else parent^"/"^name in
    run db "INSERT OR IGNORE INTO folders VALUES(?,?)" [text child;text parent];child) "" parts))
let folders db = query db "SELECT path FROM folders WHERE path<>'' ORDER BY path" [] |> List.map(fun r -> s r.(0))
let kind = function Article -> "article" | Thread -> "thread"
let message_json (m:M.message) =
  let optional = function None -> `Null | Some s -> `String s in
  `Assoc["id",`String m.id;"parent_id",optional m.parent_id;"thread_id",optional m.thread_id;
    "title",`String m.title;"author",`String m.author;"body",`String m.body;"html",optional m.html;
    "url",`String m.url;"link_url",optional m.link_url;"created",`Float m.created;
    "unavailable",`Bool m.unavailable;"score",`Int m.score;
    "my_vote",`Int(match m.my_vote with Upvote->1|Downvote-> -1|No_vote->0)]
let write_files db source a =
  run db "DELETE FROM archive_files WHERE backend_key=? AND id=?" [text source;text a.message.id];
  List.iter(fun (part,data) -> run db "INSERT INTO archive_files VALUES(?,?,?,?)"
    [text source;text a.message.id;int part;Q.Data.BLOB data]) a.files
let stored_article db ~source ~id =
  query db "SELECT id,payload,resources_complete FROM archives WHERE backend_key=? AND id=?" [text source;text id]
let local_article db ~source ~id =
  if rss_deleted db ~source ~id then failwith "Article was deleted";
  match stored_article db ~source ~id with
  | [row] -> Some (read_article row)
  | _ -> None
let delete_rss_article db ~source ~id =
  if not (rss_source source) then invalid_arg "Expected an RSS article";
  validate source;validate id;
  transaction db (fun () ->
    let args=[text source;text id] in
    run db "INSERT OR IGNORE INTO rss_deletions VALUES(?,?)" args;
    List.iter (fun table -> run db ("DELETE FROM " ^ table ^ " WHERE backend_key=? AND id=?") args)
      ["archives";"favorites";"entry_members";"activities";"dismissals";"items"])
let write_archive db source a =
  let id=a.message.id in
  let article=`Assoc["message",message_json a.message;"attachments",a.attachments] in
  let payload=
    if refreshable_article source then begin
      let article=match local_article db ~source ~id with
        | Some current ->
          if Yojson.Safe.Util.member "attachments" current <> a.attachments then
            failwith "Article enclosures changed during archiving; try again";
          current
        | None -> article in
      Yojson.Safe.to_string article
    end else Yojson.Safe.to_string article in
  run db "INSERT INTO archives(backend_key,id,payload,raw,resources_complete) VALUES(?,?,?,?,1) ON CONFLICT(backend_key,id) DO UPDATE SET payload=excluded.payload,raw=excluded.raw,resources_complete=1"
    [text source;text id;text payload;(match a.raw with None -> Q.Data.NULL | Some raw -> Q.Data.BLOB raw)];
  write_files db source a
let archived_resources_match db ~source ~id =
  match stored_article db ~source ~id with
  | [row] -> n row.(2)=1
  | _ -> false
let embedded_attachment article part =
  let open Yojson.Safe.Util in
  match member "html" (member "message" article) with
  | `String html ->
    let parts=member "attachments" article |> to_list in
    Option.bind (List.find_opt (fun attachment -> member "part" attachment=`Int part) parts)
      (fun attachment ->
        let url=member "url" attachment |> to_string in
        Option.bind (Html.embedded_image ~url html) Html.embedded_image_bytes)
  | _ -> None
let attachment_archive db ~source ~id ~part =
  let archived () =
    match query db "SELECT data FROM archive_files WHERE backend_key=? AND id=? AND part=?" [text source;text id;int part] with
    | [r] -> Some(s r.(0)) | _ -> None in
  if atlantic_source source then
    (match local_article db ~source ~id with
     | Some article -> (match embedded_attachment article part with
        | Some _ as image -> image
        | None -> if archived_resources_match db ~source ~id then archived () else None)
     | None -> None)
  else if rss_source source && not (archived_resources_match db ~source ~id) then None
  else archived ()
let save_archive db ~source ~target ~id ~folder articles =
  if articles=[] then invalid_arg "Cannot archive an empty result";
  mkdir db folder;
  observe db ~source ~group:None (List.map(fun a -> a.message) articles);
  transaction db (fun () ->
    List.iter (write_archive db source) articles;
    run db "INSERT OR IGNORE INTO favorites VALUES(?,?,?,?)" [text folder;text source;text(kind target);text id])
let memberships db = query db "SELECT backend_key,kind,id,folder FROM favorites ORDER BY folder,backend_key,id" []
  |> List.map(fun r -> s r.(0),(if s r.(1)="thread" then Thread else Article),s r.(2),s r.(3))
let covered root memberships source id = List.exists(fun (src,target,anchor,_) -> src=source &&
  (match target with Article -> anchor=id | Thread -> root(src,anchor)=root(source,id))) memberships
let remove_favorite db ~source ~target ~id ~folder = transaction db (fun () ->
  run db "DELETE FROM favorites WHERE folder=? AND backend_key=? AND kind=? AND id=?" [text folder;text source;text(kind target);text id])
let remove_folder db path =
  ignore(folder_parts path);
  transaction db (fun () -> run db "DELETE FROM folders WHERE path=?" [text path])
let favorites db ~folder =
  let root,items=components db in
  let unread=unread_components root items in
  query db "SELECT f.backend_key,f.kind,f.id,i.title,i.url,i.is_read FROM favorites f LEFT JOIN items i ON i.backend_key=f.backend_key AND i.id=f.id WHERE folder=? ORDER BY f.backend_key,f.id,f.kind" [text folder]
  |> List.map(fun r ->
    let source=s r.(0) and id=s r.(2) in
    let is_unread=if s r.(1)="thread" then Hashtbl.mem unread (root(source,id)) else n r.(5)=0 in
    `Assoc["backend_key",`String source;"target",`String(s r.(1));"id",`String id;
      "title",`String(s r.(3));"url",`String(s r.(4));"favorite",`Bool true;"unread",`Bool is_unread])
  |> fun xs -> `List xs
let view_counts db views =
  `List (List.map (fun (name,view) ->
    let rows=match view with
      | Inbox_view (entries,filter) -> timeline db ~entries filter
      | Group_view (source,group) -> feed db ~source ~group
      | Folder_view folder -> favorites db ~folder in
    let rows=Yojson.Safe.Util.to_list rows in
    let unread=List.fold_left (fun count row ->
      if Yojson.Safe.Util.member "unread" row = `Bool true then count+1 else count) 0 rows in
    `Assoc ["name",`String name;"unread",`Int unread;"total",`Int (List.length rows)]) views)

let stored_content db ~source ~target ~id =
  match target with
  | Article -> stored_article db ~source ~id
  | Thread ->
    let root,_=components db in
    (* Filter identities before loading payloads. A persistent cache may hold
       years of unrelated bodies from the same source. *)
    query db "SELECT id FROM archives WHERE backend_key=? ORDER BY id" [text source]
    |> List.filter(fun r -> root(source,s r.(0))=root(source,id))
    |> List.concat_map(fun r -> query db "SELECT id,payload,resources_complete FROM archives WHERE backend_key=? AND id=?" [text source;r.(0)])
let load_archive db ~source ~target ~id =
  let root,_=components db in let members=memberships db in
  stored_content db ~source ~target ~id
  |> List.filter(fun r -> (n r.(2)=1 || refreshable_article source) && covered root members source (s r.(0)))
  |> List.map(fun r ->
      let article=read_article r in
      if n r.(2)=1 then article else
      let open Yojson.Safe.Util in
      let attachments=member "attachments" article |> to_list
        |> List.map (fun part -> `Assoc (("download_from_source",`Bool true) :: to_assoc part)) in
      `Assoc (("attachments",`List attachments) :: List.remove_assoc "attachments" (to_assoc article))) |> fun xs -> `List xs
let export_articles db ~source ~target ~id =
  match target with
  | Article -> Option.to_list (local_article db ~source ~id)
  | Thread ->
    if query db "SELECT 1 FROM favorites WHERE backend_key=? AND kind='thread' AND id=?" [text source;text id] = []
    then []
    else Yojson.Safe.Util.to_list (load_archive db ~source ~target ~id)
let store_article ~refresh db ~source article =
  let open Yojson.Safe.Util in
  let id=article |> member "message" |> member "id" |> to_string in
  validate source;validate id;
  if refreshable_article source then begin
    if rss_deleted db ~source ~id then failwith "Article was deleted";
    if refresh || Option.is_none (local_article db ~source ~id) then begin
      let manifest=resource_manifest article in
      let preserve_resources=match stored_article db ~source ~id with
        | [row] -> n row.(2)=1 && resource_manifest (read_article row)=manifest
        | _ -> false in
      if not preserve_resources then
        run db "DELETE FROM archive_files WHERE backend_key=? AND id=?" [text source;text id];
      let payload=Yojson.Safe.to_string article in
      run db "INSERT INTO archives(backend_key,id,payload,raw,resources_complete) VALUES(?,?,?,NULL,?) ON CONFLICT(backend_key,id) DO UPDATE SET payload=excluded.payload,resources_complete=excluded.resources_complete WHERE archives.payload<>excluded.payload OR archives.resources_complete<>excluded.resources_complete"
        [text source;text id;text payload;int(if preserve_resources then 1 else 0)]
    end
  end else
    (* Metadata-only thread batches must never enter this function. Preserve
       resource-complete archive payloads and their resource part numbers. *)
    run db "INSERT INTO archives(backend_key,id,payload,raw,resources_complete) VALUES(?,?,?,NULL,0) ON CONFLICT(backend_key,id) DO UPDATE SET payload=excluded.payload WHERE archives.resources_complete=0"
      [text source;text id;text(Yojson.Safe.to_string article)]
let cache_article db ~source article =
  if refreshable_article source then transaction db (fun () -> store_article ~refresh:false db ~source article)
  else store_article ~refresh:false db ~source article
let cache_page_articles db ~source articles =
  transaction db (fun () ->
    List.iter (fun article ->
      let id=Yojson.Safe.Util.(article |> member "message" |> member "id" |> to_string) in
      if not (rss_deleted db ~source ~id) then store_article ~refresh:true db ~source article) articles)
let cache_thread db ~source ~id complete =
  run db "INSERT INTO cached_threads VALUES(?,?,?) ON CONFLICT(backend_key,id) DO UPDATE SET complete=excluded.complete"
    [text source;text id;int(if complete then 1 else 0)]
let cached_content db ~source ~target ~id =
  if rss_deleted db ~source ~id then failwith "Article was deleted";
  let articles=stored_content db ~source ~target ~id |> List.map read_article in
  let complete=match target with
    | Article -> articles<>[]
    | Thread ->
      let root,_=components db in
      List.exists(fun (src,target,anchor,_) -> src=source && target=Thread && root(src,anchor)=root(source,id)) (memberships db) ||
      List.exists(fun r -> n r.(1)=1 && root(source,s r.(0))=root(source,id))
        (query db "SELECT id,complete FROM cached_threads WHERE backend_key=?" [text source]) in
  articles,complete
let raw_archive db ~source ~id =
  match query db "SELECT raw FROM archives WHERE backend_key=? AND id=?" [text source;text id] with
  | [r] when r.(0)<>Q.Data.NULL -> s r.(0)
  | _ -> failwith "No archived MIME for this message"

type operation =
  | States of string * string list | Read of string * string list * bool
  | Feed of string * string
  | Timeline of (string * string) list * view_filter
  | View_counts of (string * view_query) list
  | Dismiss_timeline of string * string * int
  | Dismiss_notification of string * string * string * int
  | Refresh of [ `Cursor | `Group_rows ] * string * string * string option | Refresh_archives
  | Dismiss of string * string * string * int
  | Delete_rss_article of string * string
  | Folders | Remove_folder of string | Favorites of string
  | Favorite of string * target * string * string
  | Unfavorite of string * target * string * string
  | Archive of string * target * string
  | Export_content of string * target * string
  | Write_export of string * string
  | Attachment of string * string * int * string
let native_thread db ~source ~id = match query db "SELECT thread FROM items WHERE backend_key=? AND id=?" [text source;text id] with
  | [r] when s r.(0)<>"" -> Some(s r.(0)) | _ -> None
let copy_favorite db ~source ~target ~id ~folder =
  let parts=folder_parts folder in
  transaction db (fun () ->
    let available=match target with
      | Article when refreshable_article source -> archived_resources_match db ~source ~id
      | Article -> query db "SELECT 1 FROM archives WHERE backend_key=? AND id=? AND resources_complete=1" [text source;text id]<>[]
      | Thread -> query db "SELECT 1 FROM favorites WHERE backend_key=? AND kind='thread' AND id=?" [text source;text id]<>[] in
    if not available then false else begin
      ignore(List.fold_left(fun parent name ->
        let child=if parent="" then name else parent^"/"^name in
        run db "INSERT OR IGNORE INTO folders VALUES(?,?)" [text child;text parent];child) "" parts);
      run db "INSERT OR IGNORE INTO favorites VALUES(?,?,?,?)" [text folder;text source;text(kind target);text id];true
    end)
let refresh_archive db ~source ~target ~id articles =
  observe db ~source ~group:None (List.map(fun a -> a.message) articles);
  transaction db (fun () ->
    (* A concurrent folder removal must never be undone by a late download. *)
    if query db "SELECT 1 FROM favorites WHERE backend_key=? AND kind=? AND id=?" [text source;text(kind target);text id]<>[] then
      List.iter (write_archive db source) articles)

let groups_for_item db ~source ~id =
 query db "SELECT DISTINCT grp FROM activities WHERE backend_key=? AND id=? ORDER BY grp" [text source;text id]
 |> List.map(fun r -> s r.(0))
