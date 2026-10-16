open! Core
open! Ecaml
open Application

let () =
  defun_nullary_nil (Symbol.intern "ef-discourse-clear-config") [%here]
    ~docstring:"Clear Discourse session configuration before startup."
    (fun () -> ensure_ui ();if Option.is_some !runtime then failwith "Stop runtime first";discourse_configs:=[]);
  defun (Symbol.intern "ef-discourse-site") [%here]
    ~docstring:"Validate and normalize a Discourse forum base URL."
    (Returns Value.Type.string)
    (let open Defun.Let_syntax in let%map_open site=required "site" string in Discourse.Decode.site site);
  defun (Symbol.intern "ef-discourse-configure") [%here]
    ~docstring:"Configure User API credentials before startup; empty key/client means anonymous."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open site=required "site" string
     and key=required "user-api-key" string
     and client=required "client-id" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop runtime before changing Discourse identity";
     let config=Discourse.config ~api_key:key ~client_id:client site in
     discourse_configs:=config::List.filter !discourse_configs ~f:(fun c -> not(String.equal c.site config.site)));
  defun (Symbol.intern "camulus-discourse-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     let j=Reader_backend.to_json request in request_discourse (Discourse.Decode.site (Discourse.Decode.str "site" j)) (Discourse.operation_of_json j));
  defun (Symbol.intern "ef-browser-profile-configure") [%here]
    ~docstring:"Set Firefox profile for Zhihu and Douban; cookies are read only on requests."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open path=required "profile" string in
     ensure_ui ();if Option.is_some !runtime then failwith "Stop the runtime before changing browser profile";
     firefox_profile := (if String.is_empty path then None else Some path));
  defun (Symbol.intern "camulus-rss-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     request_rss (Rss_backend.operation_of_json (Reader_backend.to_json request)));
  defun (Symbol.intern "camulus-jsonfeed-subscriptions-configure") [%here]
    ~docstring:"Set JSON Feed subscriptions storage before startup." (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open path=required "path" string in
     ensure_ui ();if Option.is_some !runtime then failwith "Stop runtime before configuring JSON Feed storage";
     if Stdlib.Filename.is_relative path then invalid_arg "Use an absolute subscriptions path";
     jsonfeed_subscriptions_file:=Some path);
  defun (Symbol.intern "camulus-jsonfeed-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     request_jsonfeed (Rss_backend.operation_of_json (Reader_backend.to_json request)));
  defun_nullary (Symbol.intern "camulus-jsonfeed-followed") [%here]
    ~docstring:"Discover JSON Feed subscriptions." (Returns Value.Type.int) followed_jsonfeed;
  defun (Symbol.intern "ef-rss-subscriptions-configure") [%here]
    ~docstring:"Set the RSS backend's own subscriptions file before startup."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open path = required "path" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop runtime before changing RSS storage";
     if Stdlib.Filename.is_relative path then invalid_arg "Use an absolute RSS subscriptions path";
     rss_subscriptions_file:=Some path);
  defun_nullary (Symbol.intern "ef-rss-followed") [%here]
    ~docstring:"Discover the RSS backend's followed feed entries, without accessing the content database."
    (Returns Value.Type.int) (fun () ->
      let t=get_runtime () in let id= !next_id+1 in
      if not(Runtime.submit t (Protocol.Submit {id;owner=Service;work=Rss_followed})) then failwith "Request not accepted";
      next_id:=id;id);
  defun (Symbol.intern "camulus-subscription-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     let j=Reader_backend.to_json request in
     let open Yojson.Safe.Util in
     request_subscription (j |> member "source" |> to_string) (j |> member "target" |> to_string)
       (match member "setting" j with `String s -> Some s | `Null -> None | _ -> invalid_arg "Invalid setting"));
  defun (Symbol.intern "ef-content-state-configure") [%here]
    ~docstring:"Set an absolute local state database path before startup; empty disables storage."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open path = required "path" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop runtime before changing state storage";
     if not(String.is_empty path) && Stdlib.Filename.is_relative path then failwith "Use an absolute state path";
     state_file := if String.is_empty path then None else Some path);
  defun_nullary (Symbol.intern "ef-content-sources") [%here]
    ~docstring:"Configured source identities."
    (Returns Value.Type.(list string)) (fun () -> ensure_ui ();
      Option.to_list (Option.map !reddit_config ~f:(fun c -> "reddit:"^c.Reddit_backend.username)) @
      Option.to_list (Option.map !mail_config ~f:(fun c -> "maildir:"^c.Maildir_backend.root)));
  defun (Symbol.intern "camulus-state-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     request_state (CS.operation_of_json (Reader_backend.to_json request)));
  defun (Symbol.intern "ef-reddit-vote") [%here]
    ~docstring:"Set an absolute vote with optimistic UI state; reject overlapping votes for the same item."
    (Returns Value.Type.int)
    (let open Defun.Let_syntax in
     let%map_open id = required "item-id" string
     and previous = required "previous-vote" int
     and direction = required "direction" int in
     let t = get_runtime () in
     if Option.is_none !reddit_config then failwith "Configure Reddit first";
     ignore (Reddit_backend.Decode.require_thing id : string);
     let previous = vote_of_int previous and direction = vote_of_int direction in
     if Voting.is_pending votes id then failwith "A vote for this item is still pending";
     let request = !next_id + 1 in
     if not (Runtime.submit t (Protocol.Submit {id=request;owner=Service;work=Reddit (Vote (id,direction))})) then
       failwith "Request not accepted: runtime stopping or queue full";
     Voting.start votes ~id ~request ~previous ~direction;
     next_id := request; request);
  defun (Symbol.intern "ef-reddit-vote-state") [%here]
    ~docstring:"Return the local vote overlay as (direction status message request-id), or nil."
    (Returns Value.Type.value)
    (let open Defun.Let_syntax in
     let%map_open id = required "item-id" string in
     ensure_ui ();
     match Voting.find votes id with
     | None -> Value.nil
     | Some update ->
       let state, message = vote_status update.status in
       Value.list [Value.of_int_exn (vote_code update.direction);Value.intern state;
         Value.of_utf8_bytes message;Value.of_int_exn update.request]);
  defun (Symbol.intern "ef-host-process-complete") [%here]
    ~docstring:"Complete an Emacs-owned subprocess for the OCaml worker."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open id = required "id" int
     and status = required "status" int
     and stdout = required "stdout" string in
     ensure_ui ();Option.iter !process_bridge ~f:(fun t -> Maildir_backend.Process.complete t id {status;stdout}));
  defun (Symbol.intern "ef-mail-configure") [%here]
    ~docstring:"Configure the local Maildir/mu/Colombe backend before starting the runtime."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open root = required "maildir-root" string
     and muhome = required "muhome" string
     and sender = required "sender" string
     and mu = required "mu-program" string
     and account = required "smtp-profile" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop the runtime before reconfiguring mail";
     let config={ (Maildir_backend.config ~root ~muhome ~sender) with mu;account } in
     Maildir_backend.validate_config config;mail_config:=Some config);
  defun (Symbol.intern "ef-mail-configure-account") [%here]
    ~docstring:"Add an account route (Maildir prefix, sender, SMTP profile) before startup."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open prefix = required "prefix" string
     and address = required "address" string
     and smtp_account = required "smtp-account" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop runtime before configuring accounts";
     let c=Option.value_exn !mail_config in
     let config={c with accounts={Maildir_backend.prefix;address;smtp_account} ::
       List.filter c.accounts ~f:(fun a -> not(String.equal a.prefix prefix))} in
     Maildir_backend.validate_config config;mail_config:=Some config);
  defun (Symbol.intern "ef-mail-configure-smtp") [%here]
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
     if Option.is_some !runtime then failwith "Stop runtime before configuring SMTP";
     let smtp={Maildir_backend.Smtp.host;port;username;password_file;ca_file} in
     Maildir_backend.Smtp.validate smtp;
     let c=Option.value_exn !mail_config in
     mail_config:=Some {c with smtp_profiles=(name,smtp)::
       List.filter c.smtp_profiles ~f:(fun (n,_) -> not(String.equal n name))});
  defun (Symbol.intern "camulus-mail-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     request_mail (Maildir_backend.operation_of_json (Reader_backend.to_json request)));
  defun (Symbol.intern "ef-reddit-configure") [%here]
    ~docstring:"Configure the OCaml Reddit backend before starting the runtime; token file is read only by the worker."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open username = required "username" string
     and token_file = required "token-file" string
     and client_id = required "client-id" string
     and redirect_uri = required "redirect-uri" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop the runtime before reconfiguring Reddit";
     let config = Reddit_backend.config ~username ~token_file in
     let config = {config with client_id;redirect_uri} in
     Reddit_backend.validate_config config;
     reddit_config := Some config);
  defun (Symbol.intern "camulus-reddit-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     request_reddit (Reddit_backend.operation_of_json (Reader_backend.to_json request)));
  defun (Symbol.intern "ef-probe-start") [%here] ~docstring:"Start the in-process Eio worker."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open p = required "pipe-process" value in start p);
  defun (Symbol.intern "ef-probe-submit") [%here] ~docstring:"Submit synthetic work without blocking Emacs."
    (Returns Value.Type.int)
    (let open Defun.Let_syntax in
     let%map_open ms = required "milliseconds" int
     and steps = required "steps" int
     and service = required "service" bool
     and fail = required "fail" bool
     and allocate = required "allocation-rounds" int in
     submit ms steps service fail allocate);
  defun (Symbol.intern "ef-probe-cancel") [%here] ~docstring:"Request cancellation of one job."
    (Returns Value.Type.bool)
    (let open Defun.Let_syntax in let%map_open id = required "id" int in
     Runtime.submit (get_runtime ()) (Protocol.Cancel id));
  defun_nullary (Symbol.intern "ef-probe-close-view") [%here]
    ~docstring:"Cancel view-owned work and invalidate late view results."
    (Returns Value.Type.int) (fun () ->
      let t = get_runtime () in
      if not (Runtime.submit t (Protocol.Close_view !current_generation)) then
        failwith "close-view not accepted";
      Int.incr current_generation; !current_generation);
  defun_nullary (Symbol.intern "ef-probe-drain") [%here]
    ~docstring:"Publish at most 64 results on the UI thread."
    (Returns Value.Type.(list value)) (fun () -> drain ());
  defun_nullary (Symbol.intern "ef-probe-status") [%here] ~docstring:"Read worker lifecycle."
    (Returns Value.Type.string) status;
  defun_nullary (Symbol.intern "ef-probe-stale-count") [%here] ~docstring:"Count discarded stale results."
    (Returns Value.Type.int) (fun () -> ensure_ui (); !dropped_stale);
  defun_nullary (Symbol.intern "ef-probe-signals-unchanged") [%here]
    ~docstring:"Check SIGPIPE and SIGCHLD ownership against the pre-worker snapshot."
    (Returns Value.Type.bool) (fun () -> ensure_ui (); Reader.Host.signals_unchanged ());
  defun_nullary_nil (Symbol.intern "ef-probe-stop") [%here] ~docstring:"Request asynchronous shutdown."
    (fun () -> Runtime.request_stop (get_runtime ()));
  defun_nullary (Symbol.intern "ef-probe-reap") [%here] ~docstring:"Release a finished worker; never wait for active jobs."
    (Returns Value.Type.bool) reap;
  defun_nullary_nil (Symbol.intern "ef-probe-gc") [%here] ~docstring:"Exercise Ecaml value finalization on the UI side."
    (fun () ->
      ensure_ui ();
      for i = 1 to 2000 do ignore (Stdlib.Sys.opaque_identity (Value.of_utf8_bytes (Int.to_string i)) : Value.t) done;
      Gc.full_major ());
  Reader_ui.register ();
  Runtime_ui.install ~on_event:(fun event -> if Current_buffer.variable_is_defined (Symbol.intern "ef-reader--requests") then Reader_ui.event (Application.event_value event));
  defun_nullary_nil (Symbol.intern "ef-reader-backends-install") [%here]
    ~docstring:"Register the linked backends' optional Emacs capabilities."
    (fun () ->
      let configured source=List.mem (configured_sources ()) source ~equal:String.equal in
      Reddit_emacs.install ~submit:request_reddit ~configured ();
      Discourse_emacs.install ~submit:request_discourse ();
      Rss_emacs.install ~submit:request_rss ~followed:followed_rss ();
      Jsonfeed_emacs.install ~submit:request_jsonfeed ~followed:followed_jsonfeed ();
      Maildir_emacs.install ~submit:request_mail ~configured ();
      let publication install source = install
        ~read:(fun id group -> request_publication source (Publication_read(id,group)))
        ~attachment:(fun id group part destination -> request_publication source (Publication_attachment(id,group,part,destination))) () in
      publication Atlantic_emacs.install "atlantic";publication Janestreet_emacs.install "janestreet";
      publication Zhihu_emacs.install "zhihu";publication Douban_emacs.install "douban";
      publication Netease_emacs.install "netease";publication Public_inbox_emacs.install "public-inbox";
      publication Blogger_emacs.install "blogger";publication Gcores_emacs.install "gcores");
  provide (Symbol.intern "camulus_native")
