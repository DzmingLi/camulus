open! Core
open! Ecaml
open Reader_backend
module Protocol = Reader.Protocol
module CS = Reader.Content_state
let mail_config = ref None
type Protocol.work += Mail of Mu_backend.operation
type Protocol.payload += Mail_result of Mu_backend.result
let mail_service = function
  | Mu_backend.Send _ | Attachment _ | Draft_new _ | Draft_save _ | Draft_send _ | Archive _ | Trash _ | Sync _ -> true
  | _ -> false
let source () = Option.map !mail_config ~f:(fun c -> "maildir:"^c.Mu_backend.root)
let submit_operation ~submit operation =
  if Option.is_none !mail_config then failwith "Configure maildir first";
  submit ~service:(mail_service operation) (Mail operation)
let process_bridge = ref None
let drain_processes () = match !process_bridge with
    | None -> []
    | Some bridge -> List.map (Mu_backend.Process.drain bridge) ~f:(function
      | Mu_backend.Process.Start r -> Value.list [Value.intern "host-start";Value.of_int_exn r.id;
          Value.list (List.map r.argv ~f:Value.of_utf8_bytes);Value.of_utf8_bytes r.input_path;Value.of_int_exn r.limit]
      | Cancel id -> Value.list [Value.intern "host-cancel";Value.of_int_exn id])

let prepare ~notify =
  Option.map !mail_config ~f:(fun config ->
    let bridge=Mu_backend.Process.create ~notify () in
    process_bridge:=Some bridge;
    {drain=drain_processes;close=(fun () -> process_bridge:=None);
     create=(fun ~net ~now ~sleep ~groups:_ ->
       let run ?(input="") ?timeout argv =
         let r=Mu_backend.Process.run bridge ~now ~sleep ~input ?timeout argv in
         {Mu_backend.status=r.status;stdout=r.stdout} in
       let submit ~profile ~sender ~recipients message =
         let smtp=List.Assoc.find config.Mu_backend.smtp_profiles profile ~equal:String.equal
           |> Option.value_exn ~message:"Configure an SMTP profile before sending" in
         Mu_backend.Smtp.send ~net ~sleep smtp ~sender ~recipients message in
       let backend=Mu_backend.create ~config ~run ~submit in
       {matches_source=String.equal ("maildir:"^config.root);
        page=(fun ~source:_ group ~after -> Mu_backend.list backend group ~after);
        archive=(fun ~source:_ ~refresh:_ target id ~native_thread:_ ~add ->
          let capture (a:Mu_backend.article) =
            let attachments=match Mu_backend.result_json (Article a) with
              | `Assoc fields -> List.Assoc.find_exn fields ~equal:String.equal "attachments" | _ -> assert false in
            add {CS.message=a.message;attachments;raw=Some(Mu_backend.raw_message backend a.message.id);files=[]} in
          match target with
          | CS.Article -> capture(Mu_backend.read backend id);true
          | CS.Thread -> let complete=ref false in
            Mu_backend.execute backend (Thread id) ~emit:(function
              | Article a -> capture a | Thread_batch b -> complete:=b.complete | _ -> ());!complete);
        execute=(fun work ~observe ~emit -> let observe=observe ~source:("maildir:"^config.root) in match work with
          | Mail operation -> Mu_backend.execute backend operation ~emit:(fun result ->
            let messages=match result with Mu_backend.Page(p,_) -> p.messages | Thread_batch b -> b.messages | Article a -> [a.message] | _ -> [] in
            if not(List.is_empty messages) then observe messages;
            emit(Mail_result result));true
          | _ -> false)})})
let install_protocol ~ensure_ui ~idle ~submit =
  worker_factories := prepare :: !worker_factories;
  source_providers := source :: !source_providers;
  payload_renderers := (function Mail_result r -> Some ("mail",Mu_backend.result_json r) | _ -> None) :: !payload_renderers;
  archive_extractors := (fun source -> if String.is_prefix source ~prefix:"maildir:" then Some Mu_backend.Mime.extract else None) :: !archive_extractors;
  defun (Symbol.intern "camulus-host-process-complete") [%here]
    ~docstring:"Complete an Emacs-owned subprocess for the OCaml worker."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open id = required "id" int
     and status = required "status" int
     and stdout = required "stdout" string in
     ensure_ui ();Option.iter !process_bridge ~f:(fun t -> Mu_backend.Process.complete t id {status;stdout}));
  defun (Symbol.intern "camulus-mail-configure") [%here]
    ~docstring:"Configure the local Maildir/mu/Colombe backend before starting the runtime."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open root = required "maildir-root" string
     and muhome = required "muhome" string
     and sender = required "sender" string
     and mu = required "mu-program" string
     and account = required "smtp-profile" string in
     ensure_ui ();
     if not(idle ()) then failwith "Stop the runtime before reconfiguring mail";
     let config={ (Mu_backend.config ~root ~muhome ~sender) with mu;account } in
     Mu_backend.validate_config config;mail_config:=Some config);
  defun (Symbol.intern "camulus-mail-configure-account") [%here]
    ~docstring:"Add an account route (Maildir prefix, sender, SMTP profile) before startup."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open prefix = required "prefix" string
     and address = required "address" string
     and smtp_account = required "smtp-account" string in
     ensure_ui ();
     if not(idle ()) then failwith "Stop runtime before configuring accounts";
     let c=Option.value_exn !mail_config in
     let config={c with accounts={Mu_backend.prefix;address;smtp_account} ::
       List.filter c.accounts ~f:(fun a -> not(String.equal a.prefix prefix))} in
     Mu_backend.validate_config config;mail_config:=Some config);
  defun (Symbol.intern "camulus-mail-configure-smtp") [%here]
    ~docstring:"Configure implicit-TLS SMTP. Password file is read only on explicit sending."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open name = required "profile" string
     and host = required "host" string
     and port = required "port" int
     and username = required "username" string
     and password_file = required "password-file" string
     and ca_file = required "ca-file" string in
     ensure_ui ();
     if not(idle ()) then failwith "Stop runtime before configuring SMTP";
     let smtp={Mu_backend.Smtp.host;port;username;password_file;ca_file} in
     Mu_backend.Smtp.validate smtp;
     let c=Option.value_exn !mail_config in
     mail_config:=Some {c with smtp_profiles=(name,smtp)::
       List.filter c.smtp_profiles ~f:(fun (n,_) -> not(String.equal n name))});
  defun (Symbol.intern "camulus-mail-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     submit_operation ~submit (Mu_backend.operation_of_json (Reader_backend.to_json request)));
  ()

let sync_timer = ref nil
let sync_request : int option ref = ref None
let stop_sync () =
  if truth !sync_timer then run "cancel-timer" [!sync_timer];
  Option.iter !sync_request ~f:(fun id -> run "remhash" [i id;get "camulus-reader--completions"]);
  sync_timer:=nil;sync_request:=None
let sync ~submit ~manual () =
  let command=values(get "camulus-mail-sync-command") |> List.map ~f:text in
  match command,!sync_request with
  | [],_ -> if manual then user_error "Configure camulus-mail-sync-command first"
  | _,Some _ -> if manual then message "Mail synchronization is already running" []
  | _,None ->
    let source=s ("maildir:"^text(get "camulus-mail-root")) in
    let id=submit (Mu_backend.Sync command) in
    sync_request:=Some id;
    let finish=Function.to_value(Defun.lambda [%here] (Returns Value.Type.value)
      (let open Defun.Let_syntax in let%map_open event=required "event" value in
       if Option.equal Int.equal !sync_request (Some id) then (
         sync_request:=None;
         if Value.eq (call "nth" [i 3;event]) (sym "completed") then (
           run "camulus-reader--refresh-source" [source];
           if manual then message "Mail synchronized and indexed" [])
         else message "Mail sync: %s" [call "nth" [i 4;event]]);
       nil)) in
    run "puthash" [i id;finish;get "camulus-reader--completions"]
let start_sync ~submit () =
  let interval=integer(get "camulus-mail-sync-interval") in
  if interval>0 && truth(get "camulus-mail-sync-command") && truth(get "camulus-mail-sender") then
    sync_timer:=call "run-at-time" [i 0;i interval;lambda0(fun () ->
      if List.mem ["starting";"running"] (text(call "camulus-probe-status" [])) ~equal:String.equal then
        (try sync ~submit ~manual:false () with ex -> message "Mail sync: %s" [s(Exn.to_string ex)]);
      nil)]
let install ~submit ~configured ~request_state () =
  Process_emacs.install ();
  Compose_emacs.install ();
  Attachments_emacs.install ~save_attachment:(fun ~archived ~source ~id ~part ~destination ->
    Value.of_int_exn (if archived then request_state (CS.Attachment(source,id,part,destination))
      else submit (Mu_backend.Attachment(id,part,destination))));
  custom ~kind:Customization.Type.Directory "camulus-mail-root" (call "expand-file-name" [s "~/Maildir"]) "Backend configuration option.";
  custom ~kind:Customization.Type.Directory "camulus-mail-muhome" (call "expand-file-name" [s "~/.cache/mu"]) "Backend configuration option.";
  custom ~kind:(Customization.Type.Option {doc_for_none="Unconfigured";t=String}) "camulus-mail-sender" nil "Default sender address.";
  custom ~kind:(Customization.Type.Repeat (List [String;String;Integer;String;File;String])) "camulus-mail-smtp-profiles" nil "SMTP profiles as name, host, port, username, password file and CA file (empty for system trust).";
  custom ~kind:(Customization.Type.Repeat (List [Directory;String;String])) "camulus-mail-accounts" nil "Account routes as Maildir prefix, sender and SMTP profile name.";
  custom ~kind:Customization.Type.String "camulus-mail-account" (s "") "Backend configuration option.";
  custom ~kind:Customization.Type.(Repeat String) "camulus-mail-sync-command" nil "Backend configuration option.";
  custom ~kind:Customization.Type.Integer "camulus-mail-sync-interval" (i 0) "Backend configuration option.";
  define0 ~interactive:true "camulus-mail-sync" (fun () ->
    run "camulus-reader-start" [];sync ~submit ~manual:true ());
  register_settings "mu"(fun j->
    let strings=string_settings ["root","camulus-mail-root";"muhome","camulus-mail-muhome";"sender","camulus-mail-sender";"account","camulus-mail-account"]j in
    let lists=List.filter_map ["accounts","camulus-mail-accounts",3;"smtp_profiles","camulus-mail-smtp-profiles",6]~f:(fun(k,var,width)->match json_field k j with
     |`Null->None
     |`List rows when List.for_all rows~f:(function `List cells when List.length cells=width ->
       List.for_alli cells~f:(fun index->function `Int port when width=6 && index=2->port>0 && port<65536|`String _->not(width=6 && index=2)|_->false)|_->false)->Some(var,of_json(`List rows))
     |_->failwith("Invalid mu setting: "^k))in
    let command=match json_field "sync_command" j with
      | `Null -> []
      | `List args when List.for_all args ~f:(function `String arg -> not(String.contains arg '\000') | _ -> false)
          && (match args with `String first::_ -> not(String.is_empty first) | [] -> true | _ -> false) ->
        ["camulus-mail-sync-command",of_json (`List args)]
      | _ -> failwith "mu.sync_command must be an argument array" in
    let interval=match json_field "sync_interval" j with
      | `Null -> []
      | `Int n when n>=0 -> ["camulus-mail-sync-interval",i n]
      | _ -> failwith "mu.sync_interval must be nonnegative seconds" in
    strings@lists@command@interval);
  register_local_groups ~option:"camulus-mu-subscriptions" ~source:(fun _->"maildir:"^text(get "camulus-mail-root")) ~accepts:all_groups { (base ~name:"Mu" ~matches:(String.is_prefix ~prefix:"maildir:") ~threaded:true
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Mu_backend.Thread id)))) with
    start=(fun()->if truth(get "camulus-mail-sender")then(
      run "camulus-mail-configure"[get "camulus-mail-root";get "camulus-mail-muhome";get "camulus-mail-sender";
       (let mu=call "executable-find"[s "mu"]in if truth mu then mu else s "mu");get "camulus-mail-account"];
      List.iter(values(get "camulus-mail-smtp-profiles"))~f:(fun row->run "apply"[sym "camulus-mail-configure-smtp";row]);
      List.iter(values(get "camulus-mail-accounts"))~f:(fun row->run "apply"[sym "camulus-mail-configure-account";row])));
    inline_attachments=false;
    on_refresh=(fun () -> if truth(get "camulus-mail-sender") then
      try sync ~submit ~manual:false () with ex -> message "Mail sync: %s" [s(Exn.to_string ex)]);
    attachment=Some (fun ~source:_ ~id ~group:_ ~part ~destination -> Value.of_int_exn (submit (Mu_backend.Attachment(id,part,destination))));
    events=["mail"];available=configured;
    delete=Some Compose_emacs.trash;
    controls=[{key="A";hint="A: attachments";command="camulus-mail-attachments"};{key="r";hint="r: reply";command="camulus-mail-reply"};{key="c";hint="c: compose";command="camulus-mail-compose"};{key="a";hint="a: archive";command="camulus-mail-archive"}];
    list_controls=[{key="r";hint="r: reply";command="camulus-mail-reply"};{key="c";hint="c: compose";command="camulus-mail-compose"};{key="a";hint="a: archive";command="camulus-mail-archive"};{key="D";hint="D: drafts";command="camulus-mail-drafts"}] }
