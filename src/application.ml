module Os = Unix
module Host_thread = Thread
open! Core
open! Ecaml
module Runtime = Reader.Runtime
module Protocol = Reader.Protocol
module Voting = Reader.Voting
module CS = Reader.Content_state
module Discourse = Discourse_backend
type publication_operation = Publication_read of string * string option | Publication_attachment of string * string option * int * string
type Protocol.work += Reddit of Reddit_backend.operation | Mail of Maildir_backend.operation | Local of CS.operation | Rss of Rss_backend.operation | Discourse of string * Discourse.operation | Subscription of string * string * string option | Rss_followed | Jsonfeed_followed | Jsonfeed of Rss_backend.operation | Publication of string * publication_operation | Csrin of Csrin_backend.operation
type Protocol.payload += Reddit_result of Reddit_backend.result | Mail_result of Maildir_backend.result | State_result of Yojson.Safe.t | Rss_result of Yojson.Safe.t | Discourse_result of Discourse.result
let reddit_service = function
  | Reddit_backend.Vote _ | Reply _ | Submit _ | Authorize | Complete_authorization _ -> true
  | _ -> false
let mail_service = function
  | Maildir_backend.Send _ | Attachment _ | Draft_new _ | Draft_save _ | Draft_send _ | Archive _ -> true
  | _ -> false

external open_channel : Value.t -> Os.file_descr = "ecaml_probe_open_channel"

let owner_thread = Host_thread.id (Host_thread.self ())
let ensure_ui () =
  if Host_thread.id (Host_thread.self ()) <> owner_thread then
    failwith "effects-probe: must be called on the Emacs owner thread"

(* These references are only accessed from the UI thread. No Lisp value enters Runtime. *)
let runtime = ref None
let wake_fd = ref None
let current_generation = ref 0
let next_id = ref 0
let dropped_stale = ref 0
let reddit_config = ref None
let mail_config = ref None
let discourse_configs = ref []
let process_bridge = ref None
let votes = Voting.create ()
let state_file = ref None
let rss_subscriptions_file = ref None
let jsonfeed_subscriptions_file = ref None
let firefox_profile = ref None
let csrin_site = ref "https://cs.rin.ru/forum/"
let publication_source s = List.mem ["atlantic";"janestreet";"zhihu";"douban";"netease";"public-inbox";"blogger";"gcores";"csrin"] s ~equal:String.equal

let vote_of_int = function
  | 1 -> Reader.Content.Upvote | -1 -> Downvote | 0 -> No_vote
  | _ -> invalid_arg "Vote must be -1, 0 or 1"
let vote_code = function Reader.Content.Upvote -> 1 | Downvote -> -1 | No_vote -> 0
let vote_status = function
  | Voting.Pending -> "pending", "" | Confirmed -> "confirmed", ""
  | Rejected message -> "rejected", message | Uncertain message -> "uncertain", message

let get_runtime () =
  ensure_ui ();
  match !runtime with Some t -> t | None -> failwith "effects-probe is not started"

let start process =
  ensure_ui ();
  if Option.is_some !runtime then failwith "effects-probe already started";
  Reader.Host.snapshot_signals ();
  let fd = open_channel process in
  Value.Expert.raise_if_emacs_signaled ();
  Os.set_nonblock fd;
  Os.set_close_on_exec fd;
  let notify_ui () = ignore (Reader.Host.notify fd : bool) in
  try
    let bridge = Option.map !mail_config ~f:(fun _ -> Maildir_backend.Process.create ~notify:notify_ui ()) in
    let extension ~net ~now ~sleep =
      let reddit = Option.map !reddit_config ~f:(fun config -> Reddit_backend.Live.create ~config ~now ~sleep) in
      let run ?(input="") argv =
        let r=Maildir_backend.Process.run (Option.value_exn bridge) ~now ~sleep ~input argv in
        {Maildir_backend.status=r.status;stdout=r.stdout} in
      let mail = Option.map !mail_config ~f:(fun config ->
        let submit ~profile ~sender ~recipients source =
          let smtp=match List.Assoc.find config.Maildir_backend.smtp_profiles profile ~equal:String.equal with
            | Some smtp -> smtp | None -> failwith "Configure an SMTP profile before sending" in
          Maildir_backend.Smtp.send ~net ~sleep smtp ~sender ~recipients source in
        Maildir_backend.create ~config ~run ~submit) in
      let rss = Rss_backend.Live.create ?subscriptions_file:!rss_subscriptions_file () in
      let jsonfeed = Jsonfeed_backend.Live.create ?subscriptions_file:!jsonfeed_subscriptions_file () in
      let atlantic=Atlantic_backend.Live.create () and janestreet=Janestreet_backend.Live.create () in
      let cookies url=match !firefox_profile with
        | Some profile->Reader.Browser_cookies.get ~profile url
        | None->failwith "Configure backend_settings.browser.firefox_profile for this backend" in
      let zhihu=Zhihu_backend.Live.create ~cookies () and douban=Douban_backend.Live.create ~cookies () in
      let netease=Netease_backend.Live.create() and public_inbox=Public_inbox_backend.Live.create() in
      let blogger=Blogger_backend.Live.create() and gcores=Gcores_backend.Live.create() in
      let csrin=Csrin_backend.Live.create ~base:!csrin_site
        ~cookies:(fun url -> match !firefox_profile with Some _ -> cookies url | None -> []) () in
      let publication_list source group ~after = match source with
        | "atlantic"->Atlantic_backend.Live.list atlantic group ~after
        | "janestreet"->Janestreet_backend.Live.list janestreet group ~after
        | "zhihu"->Zhihu_backend.Live.list zhihu group ~after
        | "douban"->Douban_backend.Live.list douban group ~after
        | "netease"->Netease_backend.Live.list netease group ~after
        | "public-inbox"->Public_inbox_backend.Live.list public_inbox group ~after
        | "blogger"->Blogger_backend.Live.list blogger group ~after
        | "gcores"->Gcores_backend.Live.list gcores group ~after
        | "csrin"->Csrin_backend.Live.list csrin group ~after
        | _->invalid_arg "Unknown publication" in
      let publication_search source group ~query ~after = match source with
        | "atlantic"->Atlantic_backend.Live.search atlantic group ~query ~after
        | "janestreet"->Janestreet_backend.Live.search janestreet group ~query ~after
        | "zhihu"->Zhihu_backend.Live.search zhihu group ~query ~after
        | "douban"->Douban_backend.Live.search douban group ~query ~after
        | "netease"->Netease_backend.Live.search netease group ~query ~after
        | "public-inbox"->Public_inbox_backend.Live.search public_inbox group ~query ~after
        | "blogger"->Blogger_backend.Live.search blogger group ~query ~after
        | "gcores"->Gcores_backend.Live.search gcores group ~query ~after
        | "csrin"->Csrin_backend.Live.search csrin group ~query ~after
        | _->invalid_arg "Unknown publication" in
      let publication_read source group id = match source with
        | "atlantic"->Atlantic_backend.Live.read atlantic group id
        | "janestreet"->Janestreet_backend.Live.read janestreet group id
        | "zhihu"->Zhihu_backend.Live.read zhihu group id
        | "douban"->Douban_backend.Live.read douban group id
        | "netease"->Netease_backend.Live.read netease group id
        | "public-inbox"->Public_inbox_backend.Live.read public_inbox group id
        | "blogger"->Blogger_backend.Live.read blogger group id
        | "gcores"->Gcores_backend.Live.read gcores group id
        | "csrin"->Csrin_backend.Live.read csrin group id
        | _->invalid_arg "Unknown publication" in
      let publication_archive source group id = match source with
        | "atlantic"->Atlantic_backend.Live.archive atlantic group id
        | "janestreet"->Janestreet_backend.Live.archive janestreet group id
        | "zhihu"->Zhihu_backend.Live.archive zhihu group id
        | "douban"->Douban_backend.Live.archive douban group id
        | "netease"->Netease_backend.Live.archive netease group id
        | "public-inbox"->Public_inbox_backend.Live.archive public_inbox group id
        | "blogger"->Blogger_backend.Live.archive blogger group id
        | "gcores"->Gcores_backend.Live.archive gcores group id
        | "csrin"->Csrin_backend.Live.archive csrin group id
        | _->invalid_arg "Unknown publication" in
      let publication_attachment source group id part destination = match source with
        | "atlantic"->Atlantic_backend.Live.attachment atlantic group id part destination
        | "janestreet"->Janestreet_backend.Live.attachment janestreet group id part destination
        | "zhihu"->Zhihu_backend.Live.attachment zhihu group id part destination
        | "douban"->Douban_backend.Live.attachment douban group id part destination
        | "netease"->Netease_backend.Live.attachment netease group id part destination
        | "public-inbox"->Public_inbox_backend.Live.attachment public_inbox group id part destination
        | "blogger"->Blogger_backend.Live.attachment blogger group id part destination
        | "gcores"->Gcores_backend.Live.attachment gcores group id part destination
        | "csrin"->Csrin_backend.Live.attachment csrin group id part destination
        | _->invalid_arg "Unknown publication" in
      let forums=Stdlib.Hashtbl.create 8 in
      List.iter !discourse_configs ~f:(fun config ->
        Stdlib.Hashtbl.replace forums config.Discourse.site (Discourse.Live.create config));
      let forum site = match Stdlib.Hashtbl.find_opt forums site with
        | Some backend -> backend
        | None -> let backend=Discourse.Live.create(Discourse.config site) in
          Stdlib.Hashtbl.add forums site backend;backend in
      let db_lock = Eio.Mutex.create () in
      (* A late response must not replace a newer entry snapshot in storage.
         Atomics are read by the storage system thread under db_lock. *)
      let entry_versions = Stdlib.Hashtbl.create 16 in
      let storage f =
        let path=Option.value_exn !state_file in
        Eio.Mutex.lock db_lock;
        Exn.protect ~finally:(fun () -> Eio.Mutex.unlock db_lock) ~f:(fun () ->
          Eio_unix.run_in_systhread (fun () -> CS.with_db path f)) in
      let rsource=Option.map !reddit_config ~f:(fun c -> "reddit:"^c.Reddit_backend.username) in
      let msource=Option.map !mail_config ~f:(fun c -> "maildir:"^c.Maildir_backend.root) in
      let is_source expected source=Option.value_map expected ~default:false ~f:(String.equal source) in
      let reply emit kind fields = emit (State_result (`Assoc (("kind",`String kind)::fields))) in
      let observe ?(replace=false) emit source group messages =
        if Option.is_some !state_file then begin
          let data=storage(fun db -> CS.observe ~replace db ~source ~group messages;
            CS.snapshots db ~source (List.map messages ~f:(fun m -> m.Reader.Content.id))) in
          reply emit "states" ["data",data]
        end in
      let publication_group source id requested = match requested with
        | Some group -> group
        | None when String.equal source "janestreet" -> "posts"
        | None when List.mem ["zhihu";"douban";"public-inbox";"gcores";"csrin"] source ~equal:String.equal -> ""
        | None -> storage(fun db -> CS.groups_for_item db ~source ~id) |> List.hd
          |> Option.value_or_thunk ~default:(fun () -> failwith "Open this publication's entry before reading the article") in
      let archived source target id =
        let complete=ref false and bytes=ref 0 in
        let articles=Stdlib.Hashtbl.create 64 in
        let add a =
          bytes := !bytes + String.length a.CS.message.body +
            Option.value_map a.message.html ~default:0 ~f:String.length +
            Option.value_map a.raw ~default:0 ~f:String.length +
            List.sum (module Int) a.files ~f:(fun (_,data) -> String.length data);
          if !bytes > 128 * 1024 * 1024 then failwith "Archive exceeds the 128 MiB operation limit";
          Stdlib.Hashtbl.replace articles a.message.id a in
        if is_source rsource source then begin
          let root=if String.is_prefix id ~prefix:"t3_" then id else
            storage(fun db -> CS.native_thread db ~source ~id) |> Option.value_exn in
          Reddit_backend.Live.thread (Option.value_exn reddit) root ~emit:(fun batch ->
            complete:=batch.complete;
            List.iter batch.messages ~f:(fun message ->
              if Poly.equal target CS.Thread || String.equal message.id id then
                add {CS.message;attachments=`List [];raw=None;files=[]}))
        end else if is_source msource source then begin
          let backend=Option.value_exn mail in
          let capture (a:Maildir_backend.article) =
            let attachments=match Maildir_backend.result_json (Article a) with
              | `Assoc fields -> List.Assoc.find_exn fields ~equal:String.equal "attachments" | _ -> assert false in
            add {CS.message=a.message;attachments;raw=Some(Maildir_backend.raw_message backend a.message.id);files=[]} in
          (match target with
           | CS.Article -> capture (Maildir_backend.read backend id);complete:=true
           | CS.Thread -> Maildir_backend.execute backend (Thread id) ~emit:(function
               | Article a -> capture a | Thread_batch b -> complete:=b.complete | _ -> ()))
        end else if Discourse.is_source source then begin
          let backend=forum(Discourse.site_of_source source) in
          (match target with
           | CS.Article -> add(Discourse.Live.archive_article backend (Discourse.Live.read backend id));complete:=true
           | CS.Thread -> Discourse.Live.stream_thread backend id
               ~emit_article:(fun a -> add(Discourse.Live.archive_article backend a))
               ~emit:(fun b ->
                 List.iter b.messages ~f:(fun message ->
                   if message.unavailable && not(Stdlib.Hashtbl.mem articles message.id) then
                     add {CS.message;attachments=`List [];raw=None;files=[]});
                 complete:=b.complete))
        end else if String.equal source "public-inbox" && Poly.equal target CS.Thread then begin
          Public_inbox_backend.Live.stream_thread public_inbox id
            ~emit_article:(fun a->add(Public_inbox_backend.Live.archive_article public_inbox a))
            ~emit:(fun b->complete:=b.complete)
        end else if String.equal source "douban" && Poly.equal target CS.Thread then begin
          Douban_backend.Live.stream_thread douban id
            ~emit_article:(fun a->add(Douban_backend.Live.archive_article douban a))
            ~emit:(fun b->complete:=b.complete)
        end else if String.equal source "csrin" && Poly.equal target CS.Thread then begin
          Csrin_backend.Live.stream_thread csrin id
            ~emit_article:(fun a->add(Csrin_backend.Live.archive_article csrin a))
            ~emit:(fun b->complete:=b.complete)
        end else if publication_source source then begin
          if Poly.equal target CS.Thread then failwith "Publication articles do not have threads";
          add(publication_archive source (publication_group source id None) id);
          complete:=true
        end else if Jsonfeed_backend.is_source source then begin
          if Poly.equal target CS.Thread then failwith "JSON Feed articles do not have threads";
          add(Jsonfeed_backend.Live.archive jsonfeed (Jsonfeed_backend.url_of_source source) id);
          complete:=true
        end else if Rss_backend.is_source source then begin
          if Poly.equal target CS.Thread then failwith "RSS articles do not have threads";
          let a,files=Rss_backend.Live.archive rss (Rss_backend.url_of_source source) id in
          add {CS.message=a.message;attachments=Rss_backend.Decode.attachments a;raw=None;files};
          complete:=true
        end else failwith "Configure this source before downloading its archive";
        if Poly.equal target CS.Thread && not !complete then failwith "Thread is incomplete; archive was not committed";
        let articles=Stdlib.Hashtbl.fold(fun _ a xs -> a::xs) articles [] in
        if List.is_empty articles then failwith "Requested article is unavailable";
        articles in
      let state operation emit =
        let changed ()=reply emit "changed" [] in
        match operation with
        | CS.States(source,ids) -> reply emit "states" ["data",storage(fun db -> CS.snapshots db ~source ids)]
        | Read(source,ids,value) ->
          let data=storage(fun db -> CS.mark_read db ~source ids value;CS.snapshots db ~source ids) in
          reply emit "states" ["data",data];changed()
        | Feed(source,group) -> reply emit "feed" ["rows",storage(fun db -> CS.feed db ~source ~group)]
        | Timeline entries -> reply emit "feed" ["rows",storage(fun db -> CS.timeline db ~entries)]
        | Dismiss_timeline(source,id,through) -> storage(fun db -> CS.dismiss_timeline db ~source ~id ~through);changed()
        | Dismiss(source,group,id,through) -> storage(fun db -> CS.dismiss db ~source ~group ~id ~through);changed()
        | Legacy_groups -> reply emit "groups" ["rows",`List(storage CS.legacy_groups |> List.map ~f:(fun(source,group)->
            `Assoc["source",`String source;"group",`String group]))]
        | Folders -> reply emit "folders" ["rows",`List(List.map (storage CS.folders) ~f:(fun s -> `String s))]
        | Mkdir folder -> storage(fun db -> CS.mkdir db folder);changed()
        | Remove_folder folder -> storage(fun db -> CS.remove_folder db folder);changed()
        | Favorites folder -> reply emit "favorites" ["folder",`String folder;"rows",storage(fun db -> CS.favorites db ~folder)]
        | Favorite(source,target,id,folder) ->
          if not(storage(fun db -> CS.copy_favorite db ~source ~target ~id ~folder)) then begin
            let articles=archived source target id in
            storage(fun db -> CS.save_archive db ~source ~target ~id ~folder articles)
          end;
          reply emit "saved" ["folder",`String folder;"id",`String id];changed()
        | Unfavorite(source,target,id,folder) ->
          storage(fun db -> CS.remove_favorite db ~source ~target ~id ~folder);changed()
        | Archive(source,target,id) ->
          reply emit "archive" ["source",`String source;"id",`String id;
            "articles",storage(fun db -> CS.load_archive db ~source ~target ~id)]
        | Attachment(source,id,part,destination) ->
          let data=match storage(fun db -> CS.attachment_archive db ~source ~id ~part) with
            | Some data -> data
            | None -> let raw=storage(fun db -> CS.raw_archive db ~source ~id) in
              Maildir_backend.Mime.extract raw part in
          Eio_unix.run_in_systhread(fun () ->
            let fd=Os.openfile destination [Os.O_WRONLY;Os.O_CREAT;Os.O_EXCL;Os.O_CLOEXEC] 0o600 in
            let ch=Os.out_channel_of_descr fd in
            try Stdlib.output_string ch data;Stdlib.flush ch;Os.fsync fd;Stdlib.close_out ch
            with ex -> Stdlib.close_out_noerr ch;(try Os.unlink destination with _ -> ());raise ex);
          reply emit "saved" ["path",`String destination]
        | Refresh(source,group,after,query) ->
          let version=match Stdlib.Hashtbl.find_opt entry_versions (source,group) with
            | Some v -> v
            | None -> let v=Atomic.make 0 in Stdlib.Hashtbl.add entry_versions (source,group) v;v in
          let epoch=Atomic.fetch_and_add version 1 + 1 in
          let page = if is_source rsource source then
              let group=if String.equal group "notifications" then Reddit_backend.Notifications
                else if String.is_prefix group ~prefix:"post:" then Reddit_backend.Post(String.drop_prefix group 5)
                else Reddit_backend.Subreddit group in
              (match query with None -> Reddit_backend.Live.list (Option.value_exn reddit) group ~after
               | Some query -> Reddit_backend.Live.search (Option.value_exn reddit) group ~query ~after)
            else if is_source msource source then
              (match query with None -> Maildir_backend.list (Option.value_exn mail) group ~after
               | Some query -> Maildir_backend.search (Option.value_exn mail) group ~query ~after)
            else if publication_source source then
              (match query with None -> publication_list source group ~after
               | Some query -> publication_search source group ~query ~after)
            else if Jsonfeed_backend.is_source source then begin
              if not(String.equal (Jsonfeed_backend.url_of_source source) group) then invalid_arg "JSON Feed source and entry URL differ";
              match query with None -> Jsonfeed_backend.Live.list jsonfeed group ~after
              | Some query -> Jsonfeed_backend.Live.search jsonfeed group ~query ~after
            end else if Rss_backend.is_source source then begin
              if not(String.equal (Rss_backend.url_of_source source) group) then invalid_arg "RSS source and entry URL differ";
              match query with None -> Rss_backend.Live.list rss group ~after
              | Some query -> Rss_backend.Live.search rss group ~query ~after
            end else if Discourse.is_source source then
              let backend=forum(Discourse.site_of_source source) in
              let group=Discourse.group_of_string group in
              (match query with None -> Discourse.Live.list backend group ~after
               | Some query -> Discourse.Live.search backend group ~query ~after)
            else failwith "Source is not configured" in
          let result=storage(fun db ->
            if Atomic.get version <> epoch then None else begin
              CS.observe ~replace:(Option.is_none after) db ~source
                ~group:(if Option.is_none query then Some group else None) page.messages;
              let states=CS.snapshots db ~source (List.map page.messages ~f:(fun m -> m.Reader.Content.id)) in
              let rows=match query with None -> CS.feed db ~source ~group
                | Some _ -> CS.search_rows db ~source page.messages in
              Some(states,rows)
            end) in
          (match result with
           | None -> reply emit "superseded" []
           | Some(states,rows) ->
             reply emit "states" ["data",states];
             reply emit "feed" ["rows",rows;"after",(match page.after with None -> `Null | Some c -> `String c)])
        | Refresh_archives ->
          let errors=ref [] in
          let rss_refreshed=Stdlib.Hashtbl.create 16 in
          (* Refresh each saved target once, then update every existing folder copy. *)
          let done_=Stdlib.Hashtbl.create 32 in
          List.iter (storage CS.memberships) ~f:(fun(source,target,id,_) ->
            if not(Stdlib.Hashtbl.mem done_ (source,target,id)) then begin
              Stdlib.Hashtbl.add done_ (source,target,id) ();
              try
                if Jsonfeed_backend.is_source source && not(Stdlib.Hashtbl.mem rss_refreshed source) then begin
                  ignore(Jsonfeed_backend.Live.refresh jsonfeed (Jsonfeed_backend.url_of_source source) : Reader.Content.page);
                  Stdlib.Hashtbl.replace rss_refreshed source ()
                end;
                if Rss_backend.is_source source && not(Stdlib.Hashtbl.mem rss_refreshed source) then begin
                  ignore(Rss_backend.Live.list rss (Rss_backend.url_of_source source) ~after:None : Reader.Content.page);
                  Stdlib.Hashtbl.replace rss_refreshed source ()
                end;
                if String.equal source "gcores" then Gcores_backend.Live.invalidate gcores id;
                if String.equal source "blogger" then begin
                  let group=publication_group source id None in
                  let key=source^":"^group in
                  if not(Stdlib.Hashtbl.mem rss_refreshed key) then begin
                    ignore(Blogger_backend.Live.refresh blogger group : Reader.Content.page);
                    Stdlib.Hashtbl.replace rss_refreshed key ()
                  end
                end;
                if List.mem ["atlantic";"janestreet";"netease"] source ~equal:String.equal then begin
                  let group=publication_group source id None in
                  if not(Stdlib.Hashtbl.mem rss_refreshed (source^group)) then begin
                    ignore(publication_list source group ~after:None : Reader.Content.page);
                    Stdlib.Hashtbl.replace rss_refreshed (source^group) ()
                  end
                end;
                if String.equal source "zhihu" then Zhihu_backend.Live.invalidate zhihu id;
                if String.equal source "douban" then Douban_backend.Live.invalidate douban id;
                let articles=archived source target id in
                storage(fun db -> CS.refresh_archive db ~source ~target ~id articles)
              with ex -> errors:=(source^" archive: "^Exn.to_string ex)::!errors
            end);
          reply emit "archives-refreshed" ["errors",`List(List.map !errors ~f:(fun s -> `String s))]
      in
      fun work ~emit -> match work with
      | Local operation -> state operation emit
      | Csrin operation ->
        (match operation with
         | Csrin_backend.Forums -> reply emit "csrin-forums" ["rows",`List(List.map (Csrin_backend.Live.forums csrin) ~f:(fun(group,name)->`Assoc["group",`String group;"name",`String name]))]
         | Login(username,password) -> Csrin_backend.Live.login csrin ~username ~password;reply emit "csrin-login" []
         | Prepare(group,id) ->
           let d=Csrin_backend.Live.prepare csrin group id in
           reply emit "csrin-compose" ["forum",`String d.forum;"topic",(match d.topic with None->`Null|Some s->`String s);"subject",`String d.subject]
         | Send draft -> let url=Csrin_backend.Live.send csrin draft in reply emit "csrin-sent" ["url",`String url])
      | Publication(source,operation) ->
        (match operation with
         | Publication_read(id,group) ->
           let group=publication_group source id group in
           if String.equal source "douban" || String.equal source "public-inbox" || String.equal source "csrin" then
             (if String.equal source "douban" then Douban_backend.Live.stream_thread douban id
              else if String.equal source "csrin" then Csrin_backend.Live.stream_thread csrin id
              else Public_inbox_backend.Live.stream_thread public_inbox id)
               ~emit_article:(fun a->observe emit source None [a.message];emit(Rss_result(Rss_backend.article_json a)))
               ~emit:(fun b->emit(Rss_result(`Assoc["kind",`String "thread";"thread",`String b.thread;
                 "messages",`List(List.map b.messages ~f:CS.message_json);"complete",`Bool b.complete])))
           else let a=publication_read source group id in
             observe emit source None [a.message];emit(Rss_result(Rss_backend.article_json a))
         | Publication_attachment(id,group,part,destination) ->
           let group=publication_group source id group in
           publication_attachment source group id part destination;
           reply emit "saved" ["path",`String destination])
      | Jsonfeed_followed ->
        reply emit "entries" ["rows",`List(List.map (Jsonfeed_backend.followed jsonfeed) ~f:(fun url ->
          `Assoc["name",`String("JSON Feed: "^url);"source",`String(Jsonfeed_backend.source_of_url url);"group",`String url]))]
      | Jsonfeed operation ->
        let url=Rss_backend.operation_url operation in
        let source=Jsonfeed_backend.source_of_url url in
        (match operation with
         | Rss_backend.List _ | Search _ ->
           let page=match operation with Search(_,query)->Jsonfeed_backend.Live.search jsonfeed url ~query ~after:None
             | _->Jsonfeed_backend.Live.list jsonfeed url ~after:None in
           observe emit source None page.messages;
           emit(Rss_result(`Assoc["kind",`String "page";"messages",`List(List.map page.messages ~f:CS.message_json)]))
         | Read(_,id) -> let a=Jsonfeed_backend.Live.read jsonfeed url id in
           observe emit source None [a.message];emit(Rss_result(Rss_backend.article_json a))
         | Attachment(_,id,part,destination) -> Jsonfeed_backend.Live.attachment jsonfeed url id part destination;
           reply emit "saved" ["path",`String destination])
      | Rss_followed ->
        reply emit "entries" ["rows",`List(List.map (Rss_backend.followed rss) ~f:(fun url ->
          `Assoc["name",`String("RSS: "^url);"source",`String(Rss_backend.source_of_url url);"group",`String url]))]
      | Subscription(source,target,setting) ->
        let current,choices = if Discourse.is_source source then begin
          let backend=forum(Discourse.site_of_source source) in
          Option.iter setting ~f:(fun s ->
            try Discourse.Subscription.set backend target (Discourse.tracking_of_string s)
            with Discourse.Uncertain_write -> failwith "Tracking update outcome is uncertain; read remote state before retrying");
          let current=match Discourse.Subscription.get backend target with
            | Regular->"regular" | Tracking->"tracking" | Watching->"watching" | Muted->"muted" in
          current,["regular";"tracking";"watching";"muted"]
        end else if Jsonfeed_backend.is_source source then begin
          if not(String.equal (Jsonfeed_backend.url_of_source source) target) then invalid_arg "JSON Feed subscription target differs from source";
          Option.iter setting ~f:(fun s -> Jsonfeed_backend.Subscription.set jsonfeed target
            (match s with "subscribed"->true | "unsubscribed"->false | _->invalid_arg "Invalid JSON Feed subscription setting"));
          (if Jsonfeed_backend.Subscription.get jsonfeed target then "subscribed" else "unsubscribed"),["unsubscribed";"subscribed"]
        end else if Rss_backend.is_source source then begin
          if not(String.equal (Rss_backend.url_of_source source) target) then invalid_arg "RSS subscription target differs from source";
          Option.iter setting ~f:(fun s -> Rss_backend.Subscription.set rss target
            (match s with "subscribed"->true | "unsubscribed"->false | _->invalid_arg "Invalid RSS subscription setting"));
          (if Rss_backend.Subscription.get rss target then "subscribed" else "unsubscribed"),["unsubscribed";"subscribed"]
        end else failwith "This backend does not provide the Subscription capability" in
        reply emit "subscription" ["source",`String source;"target",`String target;"setting",`String current;
          "choices",`List(List.map choices ~f:(fun s -> `String s))]

      | Discourse(site,operation) ->
        Discourse.Live.execute (forum site) operation ~emit:(fun result ->
          let source=Discourse.source_of_site site in
          let messages=match result with Discourse.Page p->p.messages | Thread_batch b->b.messages | Article a|Liked a|Sent a->[a.message] | _->[] in
          let group=None in
          if not(List.is_empty messages) then observe emit source group messages;
          emit(Discourse_result result))
      | Rss operation ->
        let url=Rss_backend.operation_url operation in
        let source=Rss_backend.source_of_url url in
        (match operation with
         | Rss_backend.List _ | Search _ ->
           let page=match operation with Search(_,query) -> Rss_backend.Live.search rss url ~query ~after:None
             | _ -> Rss_backend.Live.list rss url ~after:None in
           let group=None in
           observe emit source group page.messages;
           emit (Rss_result (`Assoc["kind",`String "page";"messages",`List(List.map page.messages ~f:CS.message_json)]))
         | Read(_,id) ->
           let a=Rss_backend.Live.read rss url id in
           observe emit source None [a.message];emit(Rss_result(Rss_backend.article_json a))
         | Attachment(_,id,part,destination) ->
           Rss_backend.Live.attachment rss url id part destination;
           reply emit "saved" ["path",`String destination])
      | Reddit operation ->
        (try Reddit_backend.Live.execute (Option.value_exn reddit) operation
               ~emit:(fun result ->
                 let messages=match result with Reddit_backend.Page p -> p.messages | Thread_batch b -> b.messages | _ -> [] in
                 let group=None in
                 if not(List.is_empty messages) then observe emit (Option.value_exn rsource) group messages;
                 emit (Reddit_result result))
         with Reddit_backend.Error error as ex ->
           emit (Reddit_result (Reddit_backend.Backend_error error)); raise ex)
      | Mail operation -> Maildir_backend.execute (Option.value_exn mail) operation
                            ~emit:(fun result ->
                              let messages=match result with Maildir_backend.Page(p,_) -> p.messages | Thread_batch b -> b.messages | Article a -> [a.message] | _ -> [] in
                              let group=None in
                              if not(List.is_empty messages) then observe emit (Option.value_exn msource) group messages;
                              emit (Mail_result result))
      | _ -> failwith "Unknown backend task"
    in
    let t = Runtime.create ~extension ~notify_ui () in
    process_bridge := bridge;
    runtime := Some t; wake_fd := Some fd;
    Voting.clear votes;
    current_generation := 0; dropped_stale := 0
  with ex -> Os.close fd; raise ex

let submit milliseconds steps service fail allocate =
  let t = get_runtime () in
  if milliseconds < 0 || milliseconds > 60_000 || steps < 1 || steps > 1000 then
    invalid_arg "delay must be 0..60000ms and steps 1..1000";
  if allocate < 0 || allocate > 10000 then invalid_arg "allocation rounds 0..10000";
  let id = !next_id + 1 in
  let owner = if service then Protocol.Service else Protocol.View !current_generation in
  let work =
    if fail then Protocol.Fail
    else if allocate > 0 then Protocol.Allocate allocate
    else Protocol.Delay { seconds = Float.of_int milliseconds /. 1000.; steps }
  in
  if not (Runtime.submit t (Protocol.Submit { id; owner; work })) then
    failwith "request not accepted: queue full or runtime stopping";
  next_id := id;
  id

let event_value =
  let int = Value.of_int_exn in
  let sym = Value.intern in
  let owner = function Protocol.Service -> Value.nil | View generation -> int generation in
  function
  | Protocol.Ready domain -> Value.list [sym "ready"; int domain]
  | Progress (r, step) -> Value.list [sym "progress"; int r.id; owner r.owner; int step]
  | Data (r, Reddit_result result) ->
    Value.list [sym "reddit";int r.id;owner r.owner;
      Reader_backend.of_json (Reddit_backend.result_json result)]
  | Data (r, Mail_result result) ->
    Value.list [sym "mail";int r.id;owner r.owner;
      Reader_backend.of_json (Maildir_backend.result_json result)]
  | Data (r, Discourse_result result) ->
    Value.list [sym "discourse";int r.id;owner r.owner;
      Reader_backend.of_json (Discourse.result_json result)]
  | Data (r, Rss_result result) ->
    Value.list [sym "rss";int r.id;owner r.owner;Reader_backend.of_json result]
  | Data (r, State_result result) ->
    Value.list [sym "content-state";int r.id;owner r.owner;
      Reader_backend.of_json result]
  | Data _ -> failwith "Unknown backend event"
  | Finished (r, outcome) ->
    let state, message = match outcome with
      | Completed -> "completed", ""
      | Cancelled -> "cancelled", ""
      | Failed message -> "failed", message
    in
    Value.list [sym "finished"; int r.id; owner r.owner; sym state; Value.of_utf8_bytes message]

let vote_update_value (update : Voting.update) =
  let state, message = vote_status update.status in
  Value.list [Value.intern "reddit-vote";Value.of_int_exn update.request;
    Value.of_utf8_bytes update.id;Value.of_int_exn (vote_code update.direction);
    Value.intern state;Value.of_utf8_bytes message]

let update_votes event =
  let finish (r : Protocol.request) status = match r.work with
    | Reddit (Vote (id,_)) -> Option.to_list (Voting.finish votes ~id ~request:r.id ~barrier:!next_id ~status)
    | _ -> [] in
  let observe request messages =
    List.filter_map messages ~f:(fun (m : Reader.Content.message) ->
      Voting.observe votes ~id:m.id ~request ~direction:m.my_vote) in
  match event with
  | Protocol.Data (r,Reddit_result (Voted _)) -> finish r Voting.Confirmed
  | Data (r,Reddit_result (Backend_error error)) ->
    let message = Reddit_backend.to_string error in
    let status = match error with
      | Reddit_backend.Uncertain_write | Invalid_response _ -> Voting.Uncertain message
      | _ -> Voting.Rejected message in
    finish r status
  | Finished (r,outcome) ->
    let message = match outcome with
      | Protocol.Completed -> "No vote confirmation received; refresh to check"
      | Cancelled -> "Vote cancelled; remote outcome is unknown"
      | Failed message -> message in
    finish r (Voting.Uncertain message)
  | Data (r,Reddit_result (Page page)) -> observe r.id page.messages
  | Data (r,Reddit_result (Thread_batch batch)) -> observe r.id batch.messages
  | _ -> []

let drain ?(receive=ignore) ?(publish_values=true) () =
  let t = get_runtime () in
  let rec loop remaining acc =
    if remaining = 0 then List.rev acc
    else match Runtime.take_event t with
    | None -> List.rev acc
    | Some event ->
      let stale = match event with
        | Protocol.Ready _ -> false
        | Progress (r, _) | Finished (r, _) | Data (r, _) ->
          (match r.owner with Service -> false | View g -> g <> !current_generation)
      in
      if stale then (Int.incr dropped_stale; loop (remaining - 1) acc)
      else
        let update = update_votes event in
        receive event;
        let acc = if publish_values then event_value event :: acc else acc in
        let acc = List.fold (if publish_values then update else []) ~init:acc ~f:(fun acc update -> vote_update_value update :: acc) in
        loop (remaining - 1) acc
  in
  let processes = match !process_bridge with
    | None -> []
    | Some bridge -> List.map (Maildir_backend.Process.drain bridge) ~f:(function
      | Maildir_backend.Process.Start r -> Value.list [Value.intern "host-start";Value.of_int_exn r.id;
          Value.list (List.map r.argv ~f:Value.of_utf8_bytes);Value.of_utf8_bytes r.input_path;Value.of_int_exn r.limit]
      | Cancel id -> Value.list [Value.intern "host-cancel";Value.of_int_exn id]) in
  processes @ loop 64 []

let status () =
  ensure_ui ();
  match !runtime with
  | None -> "idle"
  | Some t ->
    match Runtime.status t with
    | Starting -> "starting" | Running -> "running" | Stopped -> "stopped"
    | Crashed reason -> "crashed: " ^ reason

let reap () =
  ensure_ui ();
  match !runtime with
  | None -> true
  | Some t ->
    if not (Runtime.reap t) then false
    else begin
      Option.iter !wake_fd ~f:Os.close;
      wake_fd := None; runtime := None; process_bridge := None; true
    end


(* Typed operations submitted directly by backend capabilities and native UI. *)
let submit_work ~service work =
  let t=get_runtime () in
  let id= !next_id+1 in
  let owner=if service then Protocol.Service else Protocol.View !current_generation in
  if not(Runtime.submit t (Protocol.Submit {id;owner;work})) then failwith "Request not accepted: worker stopping or queue full";
  next_id:=id;id
let request_state operation =
  if Option.is_none !state_file then failwith "Configure content state first";
  submit_work ~service:true (Local operation)
let request_rss operation = submit_work ~service:(match operation with Rss_backend.Attachment _->true | _->false) (Rss operation)
let request_jsonfeed operation = submit_work ~service:(match operation with Rss_backend.Attachment _->true | _->false) (Jsonfeed operation)
let request_discourse site operation = submit_work ~service:(Discourse.service operation) (Discourse(site,operation))
let request_reddit operation =
  if Option.is_none !reddit_config then failwith "Configure Reddit first";
  submit_work ~service:(reddit_service operation) (Reddit operation)
let request_mail operation =
  if Option.is_none !mail_config then failwith "Configure mail first";
  submit_work ~service:(mail_service operation) (Mail operation)
let request_publication source operation =
  if not(publication_source source) then invalid_arg "Unknown publication backend";
  submit_work ~service:(match operation with Publication_read _->false | _->true) (Publication(source,operation))
let request_subscription source target setting = submit_work ~service:true (Subscription(source,target,setting))
let followed_rss () = submit_work ~service:true Rss_followed
let followed_jsonfeed () = submit_work ~service:true Jsonfeed_followed
let configured_sources () =
  Option.to_list (Option.map !reddit_config ~f:(fun c -> "reddit:"^c.Reddit_backend.username)) @
  Option.to_list (Option.map !mail_config ~f:(fun c -> "maildir:"^c.Maildir_backend.root))

let request_csrin operation = submit_work ~service:true (Csrin operation)
