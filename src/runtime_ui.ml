module Os = Unix
module Host_thread = Thread
open! Core
open! Ecaml
module Runtime = Reader.Runtime
module Protocol = Reader.Protocol
module CS = Reader.Content_state
type Protocol.work += Local of CS.operation | Group_followed of string | Group_subscription of Reader.Local_subscriptions.target * string option
type Protocol.payload += State_result of Yojson.Safe.t
external open_channel : Value.t -> Os.file_descr = "ecaml_probe_open_channel"

let owner_thread = Host_thread.id (Host_thread.self ())
let ensure_ui () =
  if Host_thread.id (Host_thread.self ()) <> owner_thread then
    failwith "Camulus: must be called on the Emacs owner thread"

(* These references are only accessed from the UI thread. No Lisp value enters Runtime. *)
let runtime = ref None
let wake_fd = ref None
let current_generation = ref 0
let next_id = ref 0
let dropped_stale = ref 0
let backend_hosts : Reader_backend.host list ref = ref []
let state_file = ref None
let group_subscriptions_file = ref None

let get_runtime () =
  ensure_ui ();
  match !runtime with Some t -> t | None -> failwith "Camulus is not started"

let start_runtime process =
  ensure_ui ();
  if Option.is_some !runtime then failwith "Camulus already started";
  Reader.Host.snapshot_signals ();
  let fd = open_channel process in
  Value.Expert.raise_if_emacs_signaled ();
  Os.set_nonblock fd;
  Os.set_close_on_exec fd;
  let notify_ui () = ignore (Reader.Host.notify fd : bool) in
  try
    let hosts=List.filter_map !Reader_backend.worker_factories ~f:(fun prepare -> prepare ~notify:notify_ui) in
    let group_rules = !Reader_backend.local_group_rules in
    let subscribable (target : Reader.Local_subscriptions.target) =
      Option.value_map (List.Assoc.find group_rules target.backend ~equal:String.equal) ~default:false
        ~f:(fun accepts -> accepts ~source:target.source ~group:target.group) in
    let extension ~net ~now ~sleep =
      let group_subscriptions=Option.map !group_subscriptions_file ~f:Reader.Local_subscriptions.create in
      let groups () = Option.value_exn ~message:"Configure group subscriptions state before startup" group_subscriptions in
      let db_lock = Eio.Mutex.create () in
      (* A late response must not replace a newer entry snapshot in storage.
         Atomics are read by the storage system thread under db_lock. *)
      let entry_versions = Stdlib.Hashtbl.create 16 in
      let storage f =
        let path=Option.value_exn !state_file in
        Eio.Mutex.lock db_lock;
        Exn.protect ~finally:(fun () -> Eio.Mutex.unlock db_lock) ~f:(fun () ->
          Eio_unix.run_in_systhread (fun () -> CS.with_db path f)) in
      let reply emit kind fields = emit (State_result (`Assoc (("kind",`String kind)::fields))) in
      let observe ?(replace=false) emit source group messages =
        if Option.is_some !state_file then begin
          let data=storage(fun db -> CS.observe ~replace db ~source ~group messages;
            CS.snapshots db ~source (List.map messages ~f:(fun m -> m.Reader.Content.id))) in
          reply emit "states" ["data",data]
        end in
      let workers=List.map hosts ~f:(fun host -> host.Reader_backend.create ~net ~now ~sleep
        ~groups:(fun ~source ~id -> storage(fun db -> CS.groups_for_item db ~source ~id))) in
      let worker source=List.find workers ~f:(fun w -> w.Reader_backend.matches_source source)
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
              let extract=List.find_map !Reader_backend.archive_extractors ~f:(fun f -> f source)
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
        reply emit "entries" ["rows",`List(List.map (List.filter (Reader.Local_subscriptions.followed (groups ()) ~backend) ~f:subscribable) ~f:(fun target ->
          `Assoc["name",`String(target.source^" · "^target.group);"source",`String target.source;"group",`String target.group]))]
      | Group_subscription(target,setting) ->
        if not(subscribable target)then invalid_arg "This backend does not offer a subscription for this group";
        Option.iter setting ~f:(fun setting -> Reader.Local_subscriptions.set (groups ()) target
          (match setting with "subscribed"->true | "unsubscribed"->false | _->invalid_arg "Invalid group subscription setting"));
        let subscribed=Reader.Local_subscriptions.get (groups ()) target in
        reply emit "subscription" ["source",`String target.source;"target",`String target.group;
          "setting",`String(if subscribed then "subscribed" else "unsubscribed");
          "choices",`List[`String "unsubscribed";`String "subscribed"]]
      | work ->
        if not(List.exists workers ~f:(fun worker ->
          worker.execute work ~observe:(fun ~source messages -> observe emit source None messages) ~emit)) then
          failwith "Unknown backend task"
    in
    let t = Runtime.create ~extension ~notify_ui () in
    backend_hosts := hosts;
    runtime := Some t; wake_fd := Some fd;
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
  | Data (r, State_result result) ->
    Value.list [sym "content-state";int r.id;owner r.owner;
      Reader_backend.of_json result]
  | Data (r,Reader_backend.Presentation (name,data)) ->
    Value.list [sym name;int r.id;owner r.owner;Reader_backend.of_json data]
  | Data (r,payload) ->
    let name,data=List.find_map !Reader_backend.payload_renderers ~f:(fun render -> render payload)
      |> Option.value_exn ~message:"Unknown backend event" in
    Value.list [sym name;int r.id;owner r.owner;Reader_backend.of_json data]
  | Finished (r, outcome) ->
    let state, message = match outcome with
      | Completed -> "completed", ""
      | Cancelled -> "cancelled", ""
      | Failed message -> "failed", message
    in
    Value.list [sym "finished"; int r.id; owner r.owner; sym state; Value.of_utf8_bytes message]

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
        let update = List.concat_map !Reader_backend.event_observers ~f:(fun f -> f ~barrier:!next_id event) in
        receive event;
        let acc = if publish_values then event_value event :: acc else acc in
        let acc = List.fold (if publish_values then update else []) ~init:acc ~f:(fun acc update -> update :: acc) in
        loop (remaining - 1) acc
  in
  List.concat_map !backend_hosts ~f:(fun host -> host.drain ()) @ loop 64 []

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
      List.iter !backend_hosts ~f:(fun host -> host.close ());
      backend_hosts:=[];wake_fd := None; runtime := None; true
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
let request_subscription source target setting = submit_work ~service:true (Reader_backend.Subscription(source,target,setting))
let configured_sources = Reader_backend.configured_sources


let followed_groups ~backend = submit_work ~service:true (Group_followed backend)
let subscribe_group ~backend ~source ~group ~setting =
  submit_work ~service:true (Group_subscription({Reader.Local_subscriptions.backend;source;group},setting))

let s=Value.of_utf8_bytes
let sym=Value.intern
let nil=Value.nil
let call name args=Value.funcallN (sym name) args
let run name args=ignore(call name args : Value.t)
let get name=Current_buffer.symbol_value (Symbol.intern name)
let set name value=run "set" [sym name;value]
let truth value=not(Value.eq value nil)
let receive = ref (fun (_:Reader.Protocol.event) -> ())
let publishing = ref false
let publish () =
  if not !publishing && not(truth(get "camulus-probe--hold")) && not(String.equal (status ()) "idle") then (
    publishing:=true;
    Exn.protect ~finally:(fun () -> publishing:=false) ~f:(fun () ->
      let publish_values=truth(get "camulus-probe-record-events") || truth(get "camulus-probe-event-hook") in
      let events=drain ~receive:!receive ~publish_values () in
      List.iter events ~f:(fun event ->
        if not(truth(call "camulus-host-handle-event" [event])) then (
          if truth(get "camulus-probe-record-events") then set "camulus-probe--events" (call "cons" [event;get "camulus-probe--events"]);
          run "run-hook-with-args" [sym "camulus-probe-event-hook";event];
          ()))))
let await predicate timeout =
  let deadline=Os.gettimeofday () +. timeout in
  while not(truth(Value.funcallN predicate [])) do
    if Float.(Os.gettimeofday () > deadline) then failwith ("Camulus worker timeout: "^status ());
    run "accept-process-output" [nil;Value.of_float 0.01]
  done
let start () =
  if not(String.equal (status ()) "idle") then failwith "A Camulus session is already active";
  set "camulus-probe--events" nil;set "camulus-probe--wakeups" (Value.of_int_exn 0);set "camulus-probe--hold" nil;
  let process=call "make-pipe-process" [sym ":name";s "camulus-events";sym ":buffer";nil;
    sym ":coding";sym "binary";sym ":noquery";sym "t";sym ":filter";sym "camulus-probe--receive"] in
  set "camulus-probe--process" process;
  try start_runtime process;
    set "camulus-probe--timer" (call "run-at-time" [Value.of_float 0.02;Value.of_float 0.02;sym "camulus-probe--publish"])
  with exn -> run "delete-process" [process];set "camulus-probe--process" nil;raise exn
let stop () =
  if not(String.equal (status ()) "idle") then (
    Reader.Runtime.request_stop (get_runtime ());
    let reap=Function.to_value (Defun.lambda_nullary [%here] (Returns Value.Type.bool) reap) in
    await reap 5.);
  if truth(call "timerp" [get "camulus-probe--timer"]) then run "cancel-timer" [get "camulus-probe--timer"];
  run "camulus-host-cancel-all" [];
  if truth(call "process-live-p" [get "camulus-probe--process"]) then run "delete-process" [get "camulus-probe--process"];
  set "camulus-probe--timer" nil;set "camulus-probe--process" nil
let install ~on_event ~on_start ~on_stop =
  List.iter ["camulus-probe--process";"camulus-probe--timer";"camulus-probe--events";"camulus-probe--hold";"camulus-probe-record-events";"camulus-probe-event-hook"]
    ~f:(fun name->Reader_backend.variable name nil);
  Reader_backend.variable "camulus-probe--wakeups"(Value.of_int_exn 0);
  receive:=on_event;
  let define name f=Defun.defun_nullary_nil (Symbol.intern name) [%here] ~docstring:"Camulus event-loop operation." f in
  define "camulus-probe--publish" publish;
  define "camulus-probe-session-start" (fun () -> start ();on_start ());
  define "camulus-probe-session-stop" (fun () -> on_stop ();stop ());
  run "add-hook"[sym "kill-emacs-hook";sym "camulus-probe-session-stop"];
  Defun.defun (Symbol.intern "camulus-probe--receive") [%here] ~docstring:"Drain worker events after a pipe wakeup."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open _process=required "process" value and _bytes=required "bytes" value in
     set "camulus-probe--wakeups" (Value.of_int_exn (Value.to_int_exn(get "camulus-probe--wakeups")+1));publish ());
  Defun.defun (Symbol.intern "camulus-probe-await") [%here] ~docstring:"Wait while pumping Emacs events; used by cleanup and integration tests."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open predicate=required "predicate" value and timeout=optional_with_nil "timeout" value in
     let timeout=if truth timeout then Value.to_float_exn(call "float" [timeout]) else 5. in await predicate timeout)
