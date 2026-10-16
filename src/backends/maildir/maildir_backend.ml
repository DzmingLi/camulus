module Process = Process
module Mime = Mime
module Smtp = Smtp
module M = Reader.Content
type account_settings = { prefix : string; address : string; smtp_account : string }
type config = {
  root : string; muhome : string; mu : string;
  sender : string; account : string; page_size : int; max_headers : int;
  accounts : account_settings list; smtp_profiles : (string * Smtp.config) list;
}
let config ~root ~muhome ~sender =
  {root;muhome;sender;mu="mu";account="";page_size=50;max_headers=5000;accounts=[];smtp_profiles=[]}
exception Error of string
let error s = raise (Error s)
let () = Printexc.register_printer (function Error s -> Some ("Mail: "^s) | _ -> None)
let clean s = not (String.exists (fun c -> Char.code c<32 || c='\127') s)
let address s =
  if s="" || not (clean s) || String.exists (fun c -> List.mem c [' ';'<';'>';',';';';'"';'\\']) s
     || not (String.contains s '@') || s.[0]='-' then error "Expected a single email address";
  s
let validate_config c =
  if Filename.is_relative c.root || Filename.is_relative c.muhome then error "Mail paths must be absolute";
  if c.page_size<1 || c.page_size>200 || c.max_headers<c.page_size || c.max_headers>20000 then
    error "Invalid mail page bounds";
  ignore(address c.sender);
  List.iter (fun a -> if a.prefix="" || not(Store.valid_group a.prefix) then error "Invalid account prefix"; ignore(address a.address)) c.accounts
let get j key = match j with `Assoc xs -> Option.value (List.assoc_opt key xs) ~default:`Null | _ -> `Null
let str = function `String s -> s | _ -> ""
let strings = function `List xs -> List.filter_map (function `String s -> Some s | _ -> None) xs | _ -> []
let number = function `Int i -> float i | `Float f -> f | _ -> 0.
let quote s =
  if not (clean s) then error "Invalid query value";
  "\"" ^ String.concat "" (List.map (function '"' -> "\\\"" | '\\' -> "\\\\" | c -> String.make 1 c) (List.of_seq (String.to_seq s))) ^ "\""
type header = { message : M.message; path : string; refs : string list; maildir : string }
type group = string
type cursor = string
type attachment = Mime.attachment
type article = { message : M.message; attachments : attachment list }
type draft = { to_ : string list; cc : string list; subject : string; body : string; reply_to : string option }
type receipt = { message_id : string }
type saved_draft = { key : string; group : string; revision : string; source_text : string; indexed : bool }
type move_result = { destination : string; moved : string list; failed : int }
type operation = List of group * cursor option | Search of group * string * cursor option
  | Thread of string | Read of string | Attachment of string * int * string | Send of draft
  | Draft_new of string * string option | Draft_open of string
  | Draft_save of string * string * string | Draft_list of string
  | Draft_send of string * string
  | Archive of string * string * bool
  | Trash of string * string * bool
type result = Page of M.page * bool | Thread_batch of M.thread_batch | Article of article
  | Saved of string | Sent of receipt
  | Draft of saved_draft | Drafts of saved_draft list | Archived of move_result | Trashed of move_result
  | Draft_sent of string * bool
type process_result = { status : int; stdout : string }
type run = ?input:string -> string list -> process_result
type submit = profile:string -> sender:string -> recipients:string list -> string -> unit
type snapshot = { group : string; query : string; rows : M.message list; truncated : bool }
type t = { config : config; run : run; submit : submit; headers : (string,header) Hashtbl.t;
           snapshots : (string,snapshot) Hashtbl.t; mutable serial : int; mutable truncated : bool; mutations : Eio.Mutex.t }
let source="mail"
let create ~config ~run ~submit =
  validate_config config;
  {config;run;submit;headers=Hashtbl.create 256;snapshots=Hashtbl.create 8;serial=0;truncated=false;mutations=Eio.Mutex.create ()}

let header j =
  let path=str(get j "path") and msgid=str(get j "message-id") in
  let id=if msgid="" then "mail-file:" ^ Digest.to_hex(Digest.string path) else "mail:"^msgid in
  let refs=strings(get j "references") |> List.map (fun s -> "mail:"^s) in
  let author=match get j "from" with
    | `List xs -> String.concat ", " (List.map (fun j -> let n=str(get j "name") and e=str(get j "email") in
        if n="" then e else n^" <"^e^">") xs) | _ -> "" in
  {path;refs;maildir=str(get j "maildir");message={
    M.id;thread_id=None;references=refs;parent_id=(match List.rev refs with p::_ when p<>id -> Some p | _ -> None);
    title=str(get j "subject");author;body="";html=None;url="";link_url=None;
    created=number(get j "date-unix");unavailable=false;score=0;activity=None;my_vote=M.No_vote}}

let find t ?(related=false) query =
  let argv=[t.config.mu;"find";"--muhome";t.config.muhome;"--format=json2";
    "--sortfield=date";"--reverse";"--maxnum";string_of_int (t.config.max_headers+1)] @
    (if related then ["--include-related"] else []) @ ["--";query] in
  let r=t.run argv in
  if r.status=2 then [] else begin
    if r.status<>0 then error ("mu find failed (status "^string_of_int r.status^")");
    let json=try Yojson.Safe.from_string r.stdout with _ -> error "Invalid mu JSON output" in
    match json with `List xs -> List.map header xs | _ -> error "mu did not return an array"
  end

let remember t (h:header) = Hashtbl.replace t.headers h.message.id h

(* Union message IDs and references, including missing ancestors. No subject-only
   merging and no persistent synthetic thread identifier. *)
let summaries (headers:header list) =
  let parents=Hashtbl.create 256 in
  let rec root x = match Hashtbl.find_opt parents x with
    | None -> Hashtbl.add parents x x;x | Some p when p=x -> x
    | Some p -> let r=root p in Hashtbl.replace parents x r;r in
  List.iter (fun (h:header) -> List.iter (fun id ->
    let a=root h.message.id and b=root id in if a<>b then Hashtbl.replace parents a b) h.refs) headers;
  let groups : (string,M.message) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun (h:header) -> let key=root h.message.id in
    match Hashtbl.find_opt groups key with
    | Some old when old.M.created>=h.message.created -> ()
    | _ -> Hashtbl.replace groups key h.message) headers;
  Hashtbl.fold(fun _ m acc -> m::acc) groups [] |>
  List.sort(fun (a:M.message) (b:M.message) -> let c=Float.compare b.M.created a.M.created in if c=0 then String.compare a.id b.id else c)

let search t group ~query ~after =
  if group="" || group.[0]<>'/' then error "Group must be a Maildir path beginning with /";
  let key,offset,snap = match after with
    | Some cursor ->
      (match String.split_on_char ':' cursor with
       | [key;off] ->
         let offset=try int_of_string off with _ -> error "Invalid mail cursor" in
         let snap=match Hashtbl.find_opt t.snapshots key with Some x -> x | None -> error "Expired mail cursor; refresh" in
         if snap.group<>group || snap.query<>query || offset<0 || offset>List.length snap.rows then error "Cursor belongs to another search";
         key,offset,snap
       | _ -> error "Invalid mail cursor")
    | None ->
      let scope="maildir:"^quote group in
      let q=if String.trim query="" then scope else "("^scope^") AND ("^query^")" in
      (* Defense in depth: enforce the group after mu query parsing as well. *)
      let found=find t q in
      let truncated=List.length found>t.config.max_headers in
      let found=List.filter(fun (h:header) -> h.maildir=group) found in
      List.iter (remember t) found;
      let rows=summaries found in
      t.serial<-t.serial+1;
      let key=string_of_int t.serial in
      if Hashtbl.length t.snapshots>=8 then begin
        let oldest=Hashtbl.fold(fun k _ n -> min n (int_of_string k)) t.snapshots max_int in
        Hashtbl.remove t.snapshots (string_of_int oldest)
      end;
      let snap={group;query;rows;truncated} in Hashtbl.add t.snapshots key snap;key,0,snap in
  let rows=List.filteri(fun i _ -> i>=offset && i<offset+t.config.page_size) snap.rows in
  let next=offset+List.length rows in
  t.truncated<-snap.truncated;
  {M.messages=rows;notifications=[];after=(if next<List.length snap.rows then Some(key^":"^string_of_int next) else None)}
let list t group ~after = search t group ~query:"" ~after

let resolve t id =
  (* Resolve fresh paths from the index, so Maildir flag renames do not change IDs. *)
  if String.starts_with ~prefix:"mail:" id then begin
    let msgid=String.sub id 5 (String.length id-5) in
    let hs=find t ("msgid:"^quote msgid) in
    match List.find_opt(fun (h:header) -> h.message.id=id) hs with
    | Some h -> remember t h;h
    | None -> error "Mail is not present in the local index"
  end else match Hashtbl.find_opt t.headers id with
    | Some h -> h | None -> error "Unknown message; refresh its group"

let checked_path t (h:header) =
  let root=Unix.realpath t.config.root and path=Unix.realpath h.path in
  if not (String.starts_with ~prefix:(root^"/") path) then error "Message path is outside the configured Maildir";
  path
let raw t (h:header) =
  let path=checked_path t h in
  let ch=open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ch) @@ fun () ->
  let size=in_channel_length ch in
  if size>32*1024*1024 then error "Mail exceeds the 32 MiB limit";
  really_input_string ch size

let read_header t (h:header) =
  let parsed=Mime.parse(raw t h) in
  if String.starts_with ~prefix:"mail:" h.message.id && "mail:"^parsed.message_id<>h.message.id then
    error "Mail changed since indexing; reindex before opening it";
  let parent=match Mime.ids parsed.in_reply_to with
    | [p] -> Some("mail:"^p)
    | _ -> (match List.rev(Mime.ids parsed.references) with p::_ -> Some("mail:"^p) | _ -> h.message.parent_id) in
  let html=Mime.body "text/html" parsed in
  {message={h.message with parent_id=(if parent=Some h.message.id then None else parent);
    references=List.map(fun id -> "mail:"^id)(Mime.ids parsed.references);
    body=Mime.body "text/plain" parsed; html=(if html="" then None else Some html);
    title=(if parsed.subject="" then h.message.title else parsed.subject)};attachments=parsed.attachments}
let read t id = read_header t (resolve t id)

let raw_message t id =
  let h=resolve t id in
  let source=raw t h in
  let parsed=Mime.parse source in
  if String.starts_with ~prefix:"mail:" id && "mail:"^parsed.message_id<>id then
    error "Mail changed since indexing";
  source

let thread_full t id ~article_emit ~emit =
  let seed=resolve t id in
  let hs=if String.starts_with ~prefix:"mail:" id then
      find t ~related:true ("msgid:"^quote(String.sub id 5 (String.length id-5))) else [seed] in
  let seen=Hashtbl.create 128 in
  let hs=List.filter(fun (h:header) -> if Hashtbl.mem seen h.message.id then false else (Hashtbl.add seen h.message.id ();true)) hs in
  let complete=List.length hs<=t.config.max_headers in
  let hs=List.filteri(fun i _ -> i<t.config.max_headers) hs in
  List.iter(remember t) hs;
  (* Metadata first, then one decoded article at a time. *)
  emit {M.thread=id;messages=List.map(fun (h:header) -> h.message) hs;gaps=[];complete=false};
  List.iter(fun (h:header) ->
    Eio.Fiber.yield ();
    let article=read_header t h in
    article_emit article;
    emit {M.thread=id;messages=[article.message];gaps=[];complete=false}) hs;
  emit {M.thread=id;messages=[];gaps=(if complete then [] else [{M.parent=id;count=1}]);complete}

let thread t id ~emit = thread_full t id ~article_emit:(fun _ -> ()) ~emit

let save_attachment t id part destination =
  let h=resolve t id in
  let bytes=raw t h in
  let parsed=Mime.parse bytes in
  if not(List.exists(fun a -> a.Mime.part=part) parsed.attachments) then error "Not an attachment part";
  let data=Mime.extract bytes part in
  (* User chooses the destination; MIME filenames never become filesystem paths. *)
  let fd=Unix.openfile destination [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_EXCL;Unix.O_CLOEXEC] 0o600 in
  let ch=Unix.out_channel_of_descr fd in
  try output_string ch data;flush ch;Unix.fsync fd;close_out ch
  with ex -> close_out_noerr ch;(try Unix.unlink destination with _ -> ());raise ex

let random_id () =
  let ch=open_in_bin "/dev/urandom" in
  let bytes=Fun.protect ~finally:(fun () -> close_in_noerr ch)(fun () -> really_input_string ch 20) in
  String.concat "" (List.init 20 (fun i -> Printf.sprintf "%02x" (Char.code bytes.[i]))) ^ "@camulus.local"
let account_for t group =
  if not(Store.valid_group group) then error "Invalid Maildir group";
  let matches=List.filter(fun a -> group=a.prefix || String.starts_with ~prefix:(a.prefix^"/") group) t.config.accounts in
  match List.sort(fun a b -> Int.compare (String.length b.prefix) (String.length a.prefix)) matches with
  | a::_ -> a
  | [] ->
    let parts=List.filter((<>) "") (String.split_on_char '/' group) in
    let prefix=match parts with _::_::_ -> "/"^List.hd parts | _ -> "" in
    {prefix;address=t.config.sender;smtp_account=t.config.account}
let special_folder t group name = (account_for t group).prefix ^ "/" ^ name
let with_mutation t f =
  Eio.Mutex.lock t.mutations;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.mutations) f
let index_file t path =
  try (t.run [t.config.mu;"add";"--muhome";t.config.muhome;"--quiet";"--";path]).status=0
  with Eio.Cancel.Cancelled _ as ex -> raise ex | _ -> false
let draft_info t key indexed =
  let group,_,path=Store.draft_path ~root:t.config.root key in
  let source_text=Store.read path in
  {key;group;revision=Store.revision source_text;source_text;indexed}
let open_draft t key = draft_info t key true
let save_draft t key expected source_text = with_mutation t @@ fun () ->
  if String.length source_text>1024*1024 then error "Draft exceeds 1 MiB";
  let group,dir,path=Store.draft_path ~root:t.config.root key in
  let revision=try Store.write_atomic ~dir ~path ~expected:(Some expected) source_text
    with Store.Conflict -> error "Draft changed in another editor; reopen before saving" in
  Hashtbl.clear t.snapshots;
  let indexed=index_file t path in
  {key;group;revision;source_text;indexed}
let new_draft t group reply_to = with_mutation t @@ fun () ->
  let account=account_for t group in
  let folder=special_folder t group "Drafts" in
  let dir=Store.folder ~root:t.config.root folder in
  let id=random_id () in
  let headers,body=match reply_to with
    | None -> ["To: ";"Subject: "],""
    | Some id ->
      if not(String.starts_with ~prefix:"mail:" id) then error "Reply target needs a Message-ID";
      let candidates=find t ("msgid:"^quote(String.sub id 5 (String.length id-5))) |>
        List.filter(fun (h:header) -> h.message.id=id && (account_for t h.maildir).prefix=account.prefix) in
      let h=match List.find_opt(fun (h:header) -> h.maildir=group) candidates,candidates with
        | Some h,_ -> h | None,h::_ -> h | _ -> error "Reply target is not in this account" in
      let a=read_header t h and p=Mime.parse(raw t h) in
      let subject=if String.starts_with ~prefix:"Re:" a.message.title then a.message.title else "Re: "^a.message.title in
      let refs=Mime.ids p.references @ [p.message_id] in
      if not(clean subject && clean a.message.author && clean p.message_id) ||
         List.exists (fun s -> not(clean s)) refs then error "Invalid reply headers";
      ["To: "^a.message.author;"Subject: "^subject;"In-Reply-To: <"^p.message_id^">";
       "References: "^String.concat " " (List.map(fun s -> "<"^s^">") refs)],
      String.concat "\n" (List.map(fun line -> "> "^line)(String.split_on_char '\n' a.message.body)) in
  let source_text=String.concat "\n" (["From: "^account.address] @ headers @
    ["Message-ID: <"^id^">";"MIME-Version: 1.0";"Content-Type: text/plain; charset=UTF-8";
     "Content-Transfer-Encoding: 8bit";"";body]) in
  let key=folder^"/cur/ecaml-"^id^":2,D" in
  let _,_,path=Store.draft_path ~root:t.config.root key in
  let revision=Store.write_atomic ~dir ~path ~expected:None source_text in
  let indexed=index_file t path in
  {key;group=folder;revision;source_text;indexed}
let list_drafts t group =
  let folder=special_folder t group "Drafts" in
  let dir=Store.folder ~root:t.config.root folder in
  Sys.readdir(Filename.concat dir "cur") |> Array.to_list |> List.sort String.compare |>
  List.filter(fun name -> not(String.starts_with ~prefix:"." name)) |>
  List.map(fun name -> draft_info t (folder^"/cur/"^name) true)
let move_to t group id whole_thread folder = with_mutation t @@ fun () ->
  if not(Store.valid_group group) then error "Invalid Maildir group";
  if Filename.basename group="Drafts" then error "Drafts must stay in Drafts while being edited";
  let destination=special_folder t group folder in
  if group=destination then {destination;moved=[];failed=0} else begin
    if not(String.starts_with ~prefix:"mail:" id) then error "Moving mail requires a Message-ID";
    let query="msgid:"^quote(String.sub id 5 (String.length id-5)) in
    let all=find t ~related:whole_thread query in
    if List.length all>t.config.max_headers then error "Thread exceeds move limit; move individual messages";
    let hs=List.filter(fun (h:header) -> h.maildir=group) all in
    if hs=[] then error "Mail is no longer in this group; refresh";
    ignore(Store.folder ~root:t.config.root destination);
    (* mu move validates destination against indexed maildirs. Index new empty
       folders before moving; existing folders incur no full reindex. *)
    let probe=t.run [t.config.mu;"move";"--dry-run";"--change-name";"--muhome";t.config.muhome;
                     "--";(List.hd hs).path;destination] in
    if probe.status<>0 then begin
      let r=t.run [t.config.mu;"index";"--muhome";t.config.muhome;"--quiet"] in
      if r.status<>0 then error "Cannot update mu index; another mu process may hold its write lock"
    end;
    let moved=ref [] and failed=ref 0 in
    List.iter(fun (h:header) ->
      ignore(checked_path t h);
      let r=t.run [t.config.mu;"move";"--change-name";"--muhome";t.config.muhome;"--";h.path;destination] in
      if r.status=0 then moved:=h.message.id::!moved else incr failed) hs;
    Hashtbl.clear t.snapshots;
    {destination;moved=List.rev !moved;failed= !failed}
  end
let archive t group id whole_thread = move_to t group id whole_thread "Archive"
let trash t group id whole_thread =
  if Filename.basename group="Trash" then error "Already in Trash; d does not permanently delete mail";
  move_to t group id whole_thread "Trash"
let prepare_internal ?message_id ?(references=[]) t d =
  if d.to_=[] || List.length d.to_+List.length d.cc>100 then error "Supply 1..100 recipients";
  List.iter(fun s -> ignore(address s)) (d.to_@d.cc);
  if not(clean d.subject) || String.length d.subject>900 then error "Invalid subject";
  if String.length d.body>1024*1024 then error "Draft body exceeds 1 MiB";
  let refs=match d.reply_to with None -> references | Some id ->
    let h=resolve t id in
    let p=Mime.parse(raw t h) in
    if p.message_id="" || not(clean p.message_id) then error "Parent has no usable Message-ID";
    let xs=Mime.ids p.references @ [p.message_id] in
    List.iter(fun s -> if not(clean s) || String.contains s '<' || String.contains s '>' then error "Invalid parent references") xs;
    xs in
  let id=Option.value message_id ~default:(random_id()) and tm=Unix.gmtime(Unix.time()) in
  let date=Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d +0000"
    [|"Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"|].(tm.Unix.tm_wday)
    tm.tm_mday [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|].(tm.tm_mon)
    (1900+tm.tm_year) tm.tm_hour tm.tm_min tm.tm_sec in
  id,Mime.compose ~sender:t.config.sender ~to_:d.to_ ~cc:d.cc ~subject:d.subject
    ~date ~message_id:id ~references:refs ~body:d.body

let prepare t d = prepare_internal t d

let send t d =
  let id,input=prepare t d in
  (* Exactly one attempt; failures cannot prove the peer did not accept DATA. *)
  (try t.submit ~profile:t.config.account ~sender:t.config.sender ~recipients:(d.to_@d.cc) input
   with Eio.Cancel.Cancelled _ as ex -> raise ex
      | _ -> error "Send failed or outcome uncertain; do not automatically retry");
  {message_id="mail:"^id}

let send_draft t key expected = with_mutation t @@ fun () ->
  let group,dir,path=Store.draft_path ~root:t.config.root key in
  let source=Store.read path in
  if Store.revision source<>expected then error "Draft changed; save or reopen before sending";
  let parsed=Mime.parse source in
  if parsed.attachments<>[] || Mime.body "text/html" parsed<>"" ||
     List.exists (fun line -> String.starts_with ~prefix:"<#" line) (String.split_on_char '\n' source) then
    error "This compose slice sends plain text only; attachments/HTML must not be silently discarded";
  let account=account_for t group in
  let from,to_,cc=Mime.envelope source in
  if List.map String.lowercase_ascii from<>[String.lowercase_ascii account.address] then
    error "From does not match this Maildir account";
  let scoped={t with config={t.config with sender=account.address;account=account.smtp_account}} in
  let references=Mime.ids parsed.references in
  let references=if references=[] then Mime.ids parsed.in_reply_to else references in
  let message_id=if parsed.message_id="" then random_id() else parsed.message_id in
  if not(clean message_id) || String.contains message_id '<' || String.contains message_id '>' then error "Invalid draft Message-ID";
  List.iter(fun id -> if not(clean id) then error "Invalid reply references") references;
  let id,input=prepare_internal ~message_id ~references scoped
    {to_;cc;subject=parsed.subject;body=Mime.body "text/plain" parsed;reply_to=None} in
  let states=dir^"/.ecaml-send-state" in
  if not(Sys.file_exists states) then Unix.mkdir states 0o700;
  if Unix.realpath states<>states then error "Unexpected send-state directory";
  let marker=states^"/"^Filename.basename path in
  if Sys.file_exists marker then error "This draft was already submitted or has an uncertain send result; it will not be sent again";
  ignore(Store.write_atomic ~dir ~path:marker ~expected:None "submitting");
  (try t.submit ~profile:account.smtp_account ~sender:account.address ~recipients:(to_@cc) input
   with Eio.Cancel.Cancelled _ as ex -> raise ex
      | _ -> error "Send failed or outcome uncertain; draft retained and resubmission blocked");
  ignore(Store.write_atomic ~dir ~path:marker ~expected:(Some(Store.revision "submitting")) "accepted");
  (* Persist Sent before removing the draft. A later failure must never replay SMTP. *)
  let cleaned=try
    let sent_group=special_folder t group "Sent" in
    let sent_dir=Store.folder ~root:t.config.root sent_group in
    let sent_path=sent_dir^"/cur/ecaml-"^random_id()^":2,S" in
    ignore(Store.write_atomic ~dir:sent_dir ~path:sent_path ~expected:None input);
    ignore(index_file t sent_path);
    let unchanged=Sys.file_exists path && Store.revision(Store.read path)=expected in
    if unchanged then begin Unix.unlink path;Store.fsync_dir(Filename.dirname path) end;
    (try ignore(t.run [t.config.mu;"index";"--muhome";t.config.muhome;"--quiet"]) with _ -> ());
    unchanged
  with _ -> false in
  Hashtbl.clear t.snapshots;
  id,cleaned

module Readable : Reader.Backend.S with type t=t and type group=group and type cursor=cursor = struct
  type nonrec t=t type nonrec group=group type nonrec cursor=cursor
  let source=source let list=list let search=search
end
module Threading : Reader.Backend.Threading with type t=t = struct
  type nonrec t = t
  let thread = thread
end
module Writing : Reader.Backend.Writing with type t=t and type draft=draft and type receipt=receipt = struct
  type nonrec t=t type nonrec draft=draft type nonrec receipt=receipt let send=send
end

let operation_of_json j =
  let required k=let s=str(get j k) in if s="" then error ("Missing "^k) else s in
  let after=match get j "after" with `String s -> Some s | _ -> None in
  match required "op" with
  | "list" -> List(required "group",after)
  | "search" -> Search(required "group",required "query",after)
  | "thread" -> Thread(required "id") | "read" -> Read(required "id")
  | "attachment" ->
    let part=match get j "part" with `Int n when n>0 -> n | _ -> error "Invalid attachment part" in
    Attachment(required "id",part,required "destination")
  | "draft-new" -> Draft_new(required "group",(match get j "reply_to" with `String s -> Some s | _ -> None))
  | "draft-open" -> Draft_open(required "key")
  | "draft-save" -> Draft_save(required "key",required "revision",(match get j "source" with `String s -> s | _ -> error "Missing draft source"))
  | "draft-send" -> Draft_send(required "key",required "revision")
  | "draft-list" -> Draft_list(required "group")
  | "archive" -> Archive(required "group",required "id", get j "thread" = `Bool true)
  | "trash" -> Trash(required "group",required "id", get j "thread" = `Bool true)
  | "send" -> Send {to_=strings(get j "to");cc=strings(get j "cc");subject=str(get j "subject");
    body=str(get j "body");reply_to=(match get j "reply_to" with `String s -> Some s | _ -> None)}
  | _ -> error "Unknown mail operation"
let message_json (m:M.message) = `Assoc ["id",`String m.id;"parent_id",(match m.parent_id with Some s -> `String s | None -> `Null);
  "title",`String m.title;"author",`String m.author;"body",`String m.body;
  "html",(match m.html with Some s -> `String s | None -> `Null);"created",`Float m.created]
let attachment_json (a:attachment) = `Assoc ["part",`Int a.part;"name",`String a.name;"mime",`String a.mime;"cid",`String a.cid;"size",`Int a.size]
let draft_json ~include_source d =
  let subject=try (Mime.parse d.source_text).subject with _ -> "" in
  `Assoc (["key",`String d.key;"group",`String d.group;"revision",`String d.revision;
           "subject",`String subject;"indexed",`Bool d.indexed] @
          if include_source then ["source",`String d.source_text] else [])
let result_json = function
  | Page(p,truncated) -> `Assoc["kind",`String "page";"messages",`List(List.map message_json p.messages);
    "after",(match p.after with Some s -> `String s | None -> `Null);"truncated",`Bool truncated]
  | Thread_batch b -> `Assoc["kind",`String "thread";"thread",`String b.thread;"messages",`List(List.map message_json b.messages);"complete",`Bool b.complete]
  | Article a -> `Assoc["kind",`String "article";"message",message_json a.message;"attachments",`List(List.map attachment_json a.attachments)]
  | Saved path -> `Assoc["kind",`String "saved";"path",`String path]
  | Draft_sent(id,cleaned) -> `Assoc["kind",`String "draft-sent";"id",`String id;"cleaned",`Bool cleaned]
  | Draft d -> `Assoc["kind",`String "draft";"draft",draft_json ~include_source:true d]
  | Drafts ds -> `Assoc["kind",`String "drafts";"drafts",`List(List.map(draft_json ~include_source:false) ds)]
  | (Archived r | Trashed r) as result -> `Assoc["kind",`String (match result with Trashed _->"trashed"|_->"archived");"destination",`String r.destination;
                        "moved",`List(List.map(fun s -> `String s) r.moved);"failed",`Int r.failed]
  | Sent r -> `Assoc["kind",`String "sent";"id",`String r.message_id]
let execute t op ~emit = match op with
  | List(g,a) -> let p=list t g ~after:a in emit(Page(p,t.truncated))
  | Search(g,q,a) -> let p=search t g ~query:q ~after:a in emit(Page(p,t.truncated))
  | Thread id -> thread_full t id ~article_emit:(fun a -> emit(Article a)) ~emit:(fun b -> emit(Thread_batch b))
  | Read id -> emit(Article(read t id))
  | Attachment(id,part,destination) -> save_attachment t id part destination;emit(Saved destination)
  | Send d -> emit(Sent(send t d))

  | Draft_new(g,id) -> emit(Draft(new_draft t g id))
  | Draft_open key -> emit(Draft(open_draft t key))
  | Draft_save(key,rev,src) -> emit(Draft(save_draft t key rev src))
  | Draft_list g -> emit(Drafts(list_drafts t g))
  | Archive(g,id,whole) -> emit(Archived(archive t g id whole))
  | Trash(g,id,whole) -> emit(Trashed(trash t g id whole))

  | Draft_send(key,rev) -> let id,cleaned=send_draft t key rev in emit(Draft_sent(id,cleaned))
