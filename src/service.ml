module Os = Unix
open! Core
module CS = Content_state

type Protocol.work += Local of CS.operation | Group_followed of string | Group_subscription of Local_subscriptions.target * string option
  | Subscription of string * string * string option
type Protocol.payload += State_result of Yojson.Safe.t | Presentation of string * Yojson.Safe.t

type worker = {
  matches_source : string -> bool;
  page : source:string -> string -> after:string option -> query:string option -> Content.page;
  archive : source:string -> refresh:bool -> CS.target -> string -> native_thread:(string -> string) -> add:(CS.archived -> unit) -> bool;
  execute : Protocol.work -> observe:(source:string -> Content.message list -> unit) -> emit:(Protocol.payload -> unit) -> bool;
}
type factory = net:Eio_unix.Net.t -> now:(unit -> float) -> sleep:(float -> unit) -> groups:(source:string -> id:string -> string list) -> worker
let reply emit kind fields = emit (Presentation ("content-state", `Assoc (("kind", `String kind) :: fields)))

(* Shared state operations. Neither factories nor emitted data contain frontend values. *)
let create ~state_file ~subscriptions_file ~subscribable ~factories ~archive_extractors ~net ~now ~sleep =
      let group_subscriptions=Option.map subscriptions_file ~f:Local_subscriptions.create in
      let groups () = Option.value_exn ~message:"Configure group subscriptions state before startup" group_subscriptions in
      let db_lock = Eio.Mutex.create () in
      (* A late response must not replace a newer entry snapshot in storage.
         Atomics are read by the storage system thread under db_lock. *)
      let entry_versions = Stdlib.Hashtbl.create 16 in
      let storage f =
        let path=Option.value_exn state_file in
        Eio.Mutex.lock db_lock;
        Exn.protect ~finally:(fun () -> Eio.Mutex.unlock db_lock) ~f:(fun () ->
          Eio_unix.run_in_systhread (fun () -> CS.with_db path f)) in
      let reply emit kind fields = emit (State_result (`Assoc (("kind",`String kind)::fields))) in
      let observe ?(replace=false) emit source group messages =
        if Option.is_some state_file then begin
          let data=storage(fun db -> CS.observe ~replace db ~source ~group messages;
            CS.snapshots db ~source (List.map messages ~f:(fun m -> m.Content.id))) in
          reply emit "states" ["data",data]
        end in
      let workers=List.map factories ~f:(fun create -> create ~net ~now ~sleep
        ~groups:(fun ~source ~id -> storage(fun db -> CS.groups_for_item db ~source ~id))) in
      let worker source=List.find workers ~f:(fun w -> w.matches_source source)
        |> Option.value_exn ~message:"Source is not configured" in
      let archived ~refresh source target id =
        let complete=ref false and bytes=ref 0 in
        let articles=Stdlib.Hashtbl.create 64 in
        let add a =
          bytes := !bytes + String.length a.CS.message.body +
            Option.value_map a.message.html ~default:0 ~f:String.length +
            Option.value_map a.raw ~default:0 ~f:String.length +
            List.sum (module Int) a.files ~f:(fun (_,data) -> String.length data);
          if !bytes > 128 * 1024 * 1024 then failwith "Archive exceeds the 128 MiB operation limit";
          Stdlib.Hashtbl.replace articles a.message.id a in
        complete:=(worker source).archive ~source ~refresh target id ~add
          ~native_thread:(fun id -> storage(fun db -> CS.native_thread db ~source ~id) |> Option.value_exn);
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
            let articles=archived ~refresh:false source target id in
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
              let extract=List.find_map archive_extractors ~f:(fun f -> f source)
                |> Option.value_exn ~message:"No backend can extract this archived attachment" in
              extract raw part in
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
          let page=(worker source).page ~source group ~after ~query in
          let result=storage(fun db ->
            if Atomic.get version <> epoch then None else begin
              CS.observe ~replace:(Option.is_none after) db ~source
                ~group:(if Option.is_none query then Some group else None) page.messages;
              let states=CS.snapshots db ~source (List.map page.messages ~f:(fun m -> m.Content.id)) in
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
          (* Refresh each saved target once, then update every existing folder copy. *)
          let done_=Stdlib.Hashtbl.create 32 in
          List.iter (storage CS.memberships) ~f:(fun(source,target,id,_) ->
            if not(Stdlib.Hashtbl.mem done_ (source,target,id)) then begin
              Stdlib.Hashtbl.add done_ (source,target,id) ();
              try
                let articles=archived ~refresh:true source target id in
                storage(fun db -> CS.refresh_archive db ~source ~target ~id articles)
              with ex -> errors:=(source^" archive: "^Exn.to_string ex)::!errors
            end);
          reply emit "archives-refreshed" ["errors",`List(List.map !errors ~f:(fun s -> `String s))]
      in
      fun work ~emit -> match work with
      | Local operation -> state operation emit
      | Group_followed backend ->
        reply emit "entries" ["rows",`List(List.map (List.filter (Local_subscriptions.followed (groups ()) ~backend) ~f:subscribable) ~f:(fun target ->
          `Assoc["name",`String(target.source^" · "^target.group);"source",`String target.source;"group",`String target.group]))]
      | Group_subscription(target,setting) ->
        if not(subscribable target)then invalid_arg "This backend does not offer a subscription for this group";
        Option.iter setting ~f:(fun setting -> Local_subscriptions.set (groups ()) target
          (match setting with "subscribed"->true | "unsubscribed"->false | _->invalid_arg "Invalid group subscription setting"));
        let subscribed=Local_subscriptions.get (groups ()) target in
        reply emit "subscription" ["source",`String target.source;"target",`String target.group;
          "setting",`String(if subscribed then "subscribed" else "unsubscribed");
          "choices",`List[`String "unsubscribed";`String "subscribed"]]
      | work ->
        if not(List.exists workers ~f:(fun worker ->
          worker.execute work ~observe:(fun ~source messages -> observe emit source None messages) ~emit)) then
          failwith "Unknown backend task"
