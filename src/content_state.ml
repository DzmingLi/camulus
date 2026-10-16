module Q = Sqlite3
module M = Content
type t = Q.db
type target = Article | Thread
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
let with_db path f =
  if Filename.is_relative path then invalid_arg "State database path must be absolute";
  directory (Filename.dirname path);
  let fd = Unix.openfile path [Unix.O_CREAT;Unix.O_RDWR;Unix.O_CLOEXEC] 0o600 in
  Unix.close fd;
  let db = Q.db_open path in
  Fun.protect ~finally:(fun () -> ignore(Q.db_close db)) @@ fun () ->
  Q.busy_timeout db 2000;
  exec db "PRAGMA foreign_keys=ON";
  let version = match query db "PRAGMA user_version" [] with [r] -> n r.(0) | _ -> -1 in
  if version < 0 || version > 4 then failwith "Unsupported content-state schema version";
  if version = 0 then transaction db (fun () ->
    exec db "CREATE TABLE IF NOT EXISTS items(source TEXT NOT NULL,id TEXT NOT NULL,parent TEXT,thread TEXT,title TEXT NOT NULL,author TEXT NOT NULL,url TEXT NOT NULL,created REAL NOT NULL,is_read INTEGER NOT NULL DEFAULT 0,PRIMARY KEY(source,id));
CREATE TABLE IF NOT EXISTS item_links(source TEXT NOT NULL,child TEXT NOT NULL,ancestor TEXT NOT NULL,PRIMARY KEY(source,child,ancestor));
CREATE TABLE IF NOT EXISTS subscriptions(source TEXT NOT NULL,grp TEXT NOT NULL,PRIMARY KEY(source,grp));
CREATE TABLE IF NOT EXISTS activities(seq INTEGER PRIMARY KEY AUTOINCREMENT,source TEXT NOT NULL,id TEXT NOT NULL,revision TEXT NOT NULL,occurred REAL NOT NULL,UNIQUE(source,id,revision));
CREATE TABLE IF NOT EXISTS dismissals(source TEXT NOT NULL,id TEXT NOT NULL,through_seq INTEGER NOT NULL,PRIMARY KEY(source,id));
CREATE TABLE IF NOT EXISTS folders(path TEXT PRIMARY KEY,parent TEXT REFERENCES folders(path) ON DELETE CASCADE);
INSERT OR IGNORE INTO folders VALUES('',NULL);
CREATE TABLE IF NOT EXISTS favorites(folder TEXT NOT NULL REFERENCES folders(path) ON DELETE CASCADE,source TEXT NOT NULL,kind TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(folder,source,kind,id));
CREATE TABLE IF NOT EXISTS archives(source TEXT NOT NULL,id TEXT NOT NULL,payload TEXT NOT NULL,raw BLOB,PRIMARY KEY(source,id));
PRAGMA user_version=1");
  if version < 2 then transaction db (fun () ->
    exec db "CREATE TABLE IF NOT EXISTS archive_files(source TEXT NOT NULL,id TEXT NOT NULL,part INTEGER NOT NULL,data BLOB NOT NULL,PRIMARY KEY(source,id,part),FOREIGN KEY(source,id) REFERENCES archives(source,id) ON DELETE CASCADE); PRAGMA user_version=2");
  if version < 3 then transaction db (fun () ->
    (* Preserve old data for explicit export; it no longer drives any feed. *)
    exec db "ALTER TABLE subscriptions RENAME TO legacy_subscriptions;
ALTER TABLE activities RENAME TO legacy_activities;
ALTER TABLE dismissals RENAME TO legacy_dismissals;
CREATE TABLE activities(seq INTEGER PRIMARY KEY AUTOINCREMENT,source TEXT NOT NULL,grp TEXT NOT NULL,id TEXT NOT NULL,revision TEXT NOT NULL,occurred REAL NOT NULL,UNIQUE(source,grp,id,revision));
CREATE TABLE entry_members(source TEXT NOT NULL,grp TEXT NOT NULL,id TEXT NOT NULL,PRIMARY KEY(source,grp,id));
CREATE TABLE dismissals(source TEXT NOT NULL,grp TEXT NOT NULL,id TEXT NOT NULL,through_seq INTEGER NOT NULL,PRIMARY KEY(source,grp,id));
INSERT INTO activities(source,grp,id,revision,occurred)
 SELECT a.source,s.grp,a.id,a.revision,a.occurred FROM legacy_activities a
 JOIN legacy_subscriptions s ON s.source=a.source ORDER BY a.seq,s.grp;
INSERT INTO dismissals(source,grp,id,through_seq)
 SELECT d.source,s.grp,d.id,coalesce(max(n.seq),0) FROM legacy_dismissals d
 JOIN legacy_subscriptions s ON s.source=d.source
 LEFT JOIN legacy_activities a ON a.source=d.source AND a.seq<=d.through_seq
 LEFT JOIN activities n ON n.source=a.source AND n.grp=s.grp AND n.id=a.id AND n.revision=a.revision
 GROUP BY d.source,s.grp,d.id;
PRAGMA user_version=3");
  if version < 4 then transaction db (fun () ->
    (* v3 substituted fetch time for a changed revision. Repair cached display
       times from source metadata without changing activity IDs or dismissals.
       The next group refresh replaces these with its exact source timestamp. *)
    exec db "UPDATE activities SET occurred=coalesce((SELECT i.created FROM items i WHERE i.source=activities.source AND i.id=activities.id),0);
PRAGMA user_version=4");
  f db
let validate s = if s="" || String.contains s '\000' then invalid_arg "Empty or invalid state identity"
let observe ?(replace=false) db ~source ~group messages =
  validate source;
  transaction db (fun () ->
    (match group with Some group when replace ->
      run db "DELETE FROM entry_members WHERE source=? AND grp=?" [text source;text group] | _ -> ());
    List.iter (fun (m:M.message) ->
      validate m.id;
      run db "INSERT INTO items(source,id,parent,thread,title,author,url,created) VALUES(?,?,?,?,?,?,?,?) ON CONFLICT(source,id) DO UPDATE SET parent=coalesce(excluded.parent,items.parent),thread=coalesce(excluded.thread,items.thread),title=excluded.title,author=excluded.author,url=excluded.url,created=excluded.created"
        [text source;text m.id;opt m.parent_id;opt m.thread_id;text m.title;text m.author;text m.url;Q.Data.FLOAT m.created];
      List.iter(fun ancestor -> run db "INSERT OR IGNORE INTO item_links VALUES(?,?,?)" [text source;text m.id;text ancestor]) m.references;
      Option.iter (fun group ->
        validate group;
        run db "INSERT OR IGNORE INTO entry_members VALUES(?,?,?)" [text source;text group;text m.id];
        (* Sequence numbers track discovery/dismissal, while display order uses
           source time. Updating a known revision must retain its sequence. *)
        run db "INSERT INTO activities(source,grp,id,revision,occurred) VALUES(?,?,?,?,?) ON CONFLICT(source,grp,id,revision) DO UPDATE SET occurred=excluded.occurred"
          [text source;text group;text m.id;text (Option.value m.activity ~default:m.id);Q.Data.FLOAT m.created]
      ) group) messages)
let mark_read db ~source ids value =
  transaction db (fun () -> List.iter (fun id ->
    run db "UPDATE items SET is_read=? WHERE source=? AND id=?" [int(if value then 1 else 0);text source;text id]) ids)
let snapshots db ~source ids = `Assoc ["source",`String source;"items",`List(List.map (fun id ->
  let read = match query db "SELECT is_read FROM items WHERE source=? AND id=?" [text source;text id] with
    | [r] -> n r.(0)<>0 | _ -> false in
  `Assoc["id",`String id;"read",`Bool read]) ids)]
let legacy_groups db = query db "SELECT source,grp FROM legacy_subscriptions ORDER BY source,grp" []
  |> List.map(fun r -> s r.(0),s r.(1))
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
  let rows=query db "SELECT source,id,parent,thread,title,author,url,created,is_read FROM items" [] in
  List.iter(fun r -> let key=s r.(0),s r.(1) in ignore(root key);
    List.iter(fun i -> if s r.(i)<>"" then union key (s r.(0),s r.(i))) [2;3]) rows;
  List.iter(fun r -> union (s r.(0),s r.(1)) (s r.(0),s r.(2)))
    (query db "SELECT source,child,ancestor FROM item_links" []);
  root,rows
let dismiss db ~source ~group ~id ~through =
  if through<0 then invalid_arg "Negative feed revision";
  run db "INSERT INTO dismissals VALUES(?,?,?,?) ON CONFLICT(source,grp,id) DO UPDATE SET through_seq=max(through_seq,excluded.through_seq)" [text source;text group;text id;int through]
let rows_for_activities db ~dismissals ~events ~groups =
  let root,items=components db in
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
    let unread=List.exists(fun r -> root(s r.(0),s r.(1))=key && n r.(8)=0) items in
    ((time event.(3),revision),`Assoc["source",`String(s event.(1));"id",`String(s event.(2));
      "thread_id",(if s m.(3)="" then `Null else `String(s m.(3)));
      "title",`String(s m.(4));"author",`String(s m.(5));"unread",`Bool unread;
      "conversation_id",`String(snd key);"occurred",`Float(time event.(3));
      "groups",`List(List.map (fun s -> `String s) (groups root key));
      "revision",`Int revision])::acc) last []
  |> List.sort(fun(a,_)(b,_)->compare b a) |> List.map snd |> fun xs -> `List xs

let feed db ~source ~group =
  rows_for_activities db
    ~dismissals:(query db "SELECT source,id,through_seq FROM dismissals WHERE source=? AND grp=?" [text source;text group])
    ~events:(query db "SELECT seq,source,id,occurred FROM activities WHERE source=? AND grp=? AND id IN (SELECT id FROM entry_members WHERE source=? AND grp=?) ORDER BY seq" [text source;text group;text source;text group])
    ~groups:(fun _ _ -> [group])

(* A private view scope, never a backend group or subscription. Reuse the
   existing dismissal format without changing the database schema. *)
let timeline_scope = "\001reader-home"
let dismiss_timeline db ~source ~id ~through =
  dismiss db ~source ~group:timeline_scope ~id ~through

let timeline db ~entries =
  let eligible=Hashtbl.create 128 and origins=Hashtbl.create 128 in
  List.iter (fun (source,group) ->
    List.iter (fun r ->
      let id=s r.(0) in
      Hashtbl.replace eligible (source,id,s r.(1)) ();
      let old=Option.value(Hashtbl.find_opt origins (source,id)) ~default:[] in
      Hashtbl.replace origins (source,id) (group::old))
      (query db "SELECT a.id,a.revision FROM activities a JOIN entry_members m ON m.source=a.source AND m.grp=a.grp AND m.id=a.id WHERE a.source=? AND a.grp=?" [text source;text group]))
    (List.sort_uniq compare entries);
  (* The same revision discovered through a second entry must retain its first
     sequence number. Otherwise pagination or adding an entry revives an update
     already dismissed on the home timeline. *)
  let events=query db "SELECT min(seq),source,id,min(occurred),revision FROM activities GROUP BY source,id,revision ORDER BY min(seq)" []
    |> List.filter (fun r -> Hashtbl.mem eligible (s r.(1),s r.(2),s r.(4))) in
  rows_for_activities db
    ~dismissals:(query db "SELECT source,id,through_seq FROM dismissals WHERE grp=?" [text timeline_scope])
    ~events
    ~groups:(fun root key -> Hashtbl.fold (fun identity groups acc ->
      if root identity=key then groups @ acc else acc) origins [] |> List.sort_uniq String.compare)
(* Search results share identities/read state, but never enter the cached timeline. *)
let search_rows db ~source messages =
  let root,items=components db in
  let seen=Hashtbl.create 32 in
  `List(List.filter_map(fun (m:M.message) ->
    let key=root(source,m.id) in
    if Hashtbl.mem seen key then None else begin
      Hashtbl.add seen key ();
      let unread=List.exists(fun r -> root(s r.(0),s r.(1))=key && n r.(8)=0) items in
      Some(`Assoc["source",`String source;"id",`String m.id;"thread_id",(match m.thread_id with None->`Null | Some s->`String s);"title",`String m.title;
        "author",`String m.author;"unread",`Bool unread])
    end) messages)
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
  run db "DELETE FROM archive_files WHERE source=? AND id=?" [text source;text a.message.id];
  List.iter(fun (part,data) -> run db "INSERT INTO archive_files VALUES(?,?,?,?)"
    [text source;text a.message.id;int part;Q.Data.BLOB data]) a.files
let attachment_archive db ~source ~id ~part =
  match query db "SELECT data FROM archive_files WHERE source=? AND id=? AND part=?" [text source;text id;int part] with
  | [r] -> Some(s r.(0)) | _ -> None
let save_archive db ~source ~target ~id ~folder articles =
  if articles=[] then invalid_arg "Cannot archive an empty result";
  mkdir db folder;
  observe db ~source ~group:None (List.map(fun a -> a.message) articles);
  transaction db (fun () ->
    List.iter(fun a ->
      let payload=`Assoc["message",message_json a.message;"attachments",a.attachments] |> Yojson.Safe.to_string in
      run db "INSERT INTO archives VALUES(?,?,?,?) ON CONFLICT(source,id) DO UPDATE SET payload=excluded.payload,raw=excluded.raw"
        [text source;text a.message.id;text payload;(match a.raw with None -> Q.Data.NULL | Some raw -> Q.Data.BLOB raw)];write_files db source a) articles;
    run db "INSERT OR IGNORE INTO favorites VALUES(?,?,?,?)" [text folder;text source;text(kind target);text id])
let memberships db = query db "SELECT source,kind,id,folder FROM favorites ORDER BY folder,source,id" []
  |> List.map(fun r -> s r.(0),(if s r.(1)="thread" then Thread else Article),s r.(2),s r.(3))
let covered root memberships source id = List.exists(fun (src,target,anchor,_) -> src=source &&
  (match target with Article -> anchor=id | Thread -> root(src,anchor)=root(source,id))) memberships
let prune db =
  let root,_=components db in let members=memberships db in
  query db "SELECT source,id FROM archives" [] |> List.iter(fun r ->
    let source=s r.(0) and id=s r.(1) in if not(covered root members source id) then
    run db "DELETE FROM archives WHERE source=? AND id=?" [text source;text id])
let remove_favorite db ~source ~target ~id ~folder = transaction db (fun () ->
  run db "DELETE FROM favorites WHERE folder=? AND source=? AND kind=? AND id=?" [text folder;text source;text(kind target);text id];prune db)
let remove_folder db path =
  ignore(folder_parts path);
  transaction db (fun () -> run db "DELETE FROM folders WHERE path=?" [text path];prune db)
let favorites db ~folder =
  query db "SELECT f.source,f.kind,f.id,i.title FROM favorites f LEFT JOIN items i ON i.source=f.source AND i.id=f.id WHERE folder=? ORDER BY f.source,f.id,f.kind" [text folder]
  |> List.map(fun r -> `Assoc["source",`String(s r.(0));"target",`String(s r.(1));"id",`String(s r.(2));"title",`String(s r.(3))]) |> fun xs -> `List xs
let load_archive db ~source ~target ~id =
  let root,_=components db in let members=memberships db in
  query db "SELECT id,payload FROM archives WHERE source=?" [text source]
  |> List.filter(fun r -> covered root members source (s r.(0)) &&
    (match target with Article -> s r.(0)=id | Thread -> root(source,s r.(0))=root(source,id)))
  |> List.map(fun r -> Yojson.Safe.from_string(s r.(1))) |> fun xs -> `List xs
let raw_archive db ~source ~id =
  match query db "SELECT raw FROM archives WHERE source=? AND id=?" [text source;text id] with
  | [r] when r.(0)<>Q.Data.NULL -> s r.(0)
  | _ -> failwith "No archived MIME for this message"

type operation =
  | States of string * string list | Read of string * string list * bool
  | Legacy_groups | Feed of string * string
  | Timeline of (string * string) list
  | Dismiss_timeline of string * string * int
  | Refresh of string * string * string option * string option | Refresh_archives
  | Dismiss of string * string * string * int
  | Folders | Mkdir of string | Remove_folder of string | Favorites of string
  | Favorite of string * target * string * string
  | Unfavorite of string * target * string * string
  | Archive of string * target * string
  | Attachment of string * string * int * string
let operation_of_json j =
  let get k=match j with `Assoc xs -> Option.value(List.assoc_opt k xs) ~default:`Null | _ -> `Null in
  let string k=match get k with `String s when s<>"" -> validate s;s | _ -> invalid_arg("Missing "^k) in
  let boolean k=match get k with `Bool b -> b | _ -> invalid_arg("Missing boolean "^k) in
  let integer k=match get k with `Int n when n>=0 -> n | _ -> invalid_arg("Invalid "^k) in
  let ids ()=match get "ids" with `List xs when List.length xs<=5000 -> List.map(function `String s -> validate s;s | _ -> invalid_arg "Invalid ID") xs | _ -> invalid_arg "Invalid ID list" in
  let target ()=match string "target" with "article" -> Article | "thread" -> Thread | _ -> invalid_arg "Invalid target" in
  match string "op" with
  | "states" -> States(string "source",ids())
  | "read" -> Read(string "source",ids(),boolean "value")
  | "legacy-groups" -> Legacy_groups
  | "feed" -> Feed(string "source",string "group")
  | "timeline" ->
    let entries=match get "entries" with
      | `List xs when List.length xs<=1000 -> List.map (function
          | `Assoc xs ->
            let field k=match List.assoc_opt k xs with Some(`String s) -> validate s;s | _ -> invalid_arg "Invalid timeline entry" in
            field "source",field "group"
          | _ -> invalid_arg "Invalid timeline entry") xs
      | _ -> invalid_arg "Invalid timeline entries" in
    Timeline entries
  | "dismiss-timeline" -> Dismiss_timeline(string "source",string "id",integer "revision")
  | "refresh-archives" -> Refresh_archives
  | "refresh" ->
    let optional k=match get k with `Null->None | `String s when s<>"" -> Some s | _->invalid_arg("Invalid "^k) in
    Refresh(string "source",string "group",optional "after",optional "query")
  | "dismiss" -> Dismiss(string "source",string "group",string "id",integer "revision")
  | "folders" -> Folders | "mkdir" -> Mkdir(string "folder")
  | "remove-folder" -> Remove_folder(string "folder") | "favorites" -> Favorites(string "folder")
  | "favorite" -> Favorite(string "source",target(),string "id",string "folder")
  | "unfavorite" -> Unfavorite(string "source",target(),string "id",string "folder")
  | "archive" -> Archive(string "source",target(),string "id")
  | "attachment" -> Attachment(string "source",string "id",integer "part",string "destination")
  | _ -> invalid_arg "Unknown content-state operation"
let native_thread db ~source ~id = match query db "SELECT thread FROM items WHERE source=? AND id=?" [text source;text id] with
  | [r] when s r.(0)<>"" -> Some(s r.(0)) | _ -> None
let copy_favorite db ~source ~target ~id ~folder =
  let parts=folder_parts folder in
  transaction db (fun () ->
    let available=match target with
      | Article -> query db "SELECT 1 FROM archives WHERE source=? AND id=?" [text source;text id]<>[]
      | Thread -> query db "SELECT 1 FROM favorites WHERE source=? AND kind='thread' AND id=?" [text source;text id]<>[] in
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
    if query db "SELECT 1 FROM favorites WHERE source=? AND kind=? AND id=?" [text source;text(kind target);text id]<>[] then
      List.iter(fun a ->
        let payload=`Assoc["message",message_json a.message;"attachments",a.attachments] |> Yojson.Safe.to_string in
        run db "INSERT INTO archives VALUES(?,?,?,?) ON CONFLICT(source,id) DO UPDATE SET payload=excluded.payload,raw=excluded.raw"
          [text source;text a.message.id;text payload;(match a.raw with None -> Q.Data.NULL | Some raw -> Q.Data.BLOB raw)];write_files db source a) articles)

let groups_for_item db ~source ~id =
 query db "SELECT DISTINCT grp FROM activities WHERE source=? AND id=? ORDER BY grp" [text source;text id]
 |> List.map(fun r -> s r.(0))
