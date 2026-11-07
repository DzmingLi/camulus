open! Core
open! Ecaml
open Runtime_ui

let request_mail = Maildir_emacs.submit_operation ~submit:submit_work
let request_reddit = Reddit_emacs.submit_operation ~submit:submit_work
let () =
  defun (Symbol.intern "camulus-firefox-cookies-get") [%here]
    ~docstring:"Return Firefox cookies for HTTPS URL as an alist of string pairs.\nRead camulus-firefox-profile on each call, without starting the reader.\nThis synchronous callback reads only the local database; it does not log in."
    (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
     ensure_ui ();
     let provider=Reader_backend.firefox_cookie_provider () in
     Value.list(List.map (Reader.Firefox_cookies.get_sync provider url) ~f:(fun(name,value)->
       Reader_backend.call "cons" [Value.of_utf8_bytes name;Value.of_utf8_bytes value])));
  let idle () = Option.is_none !runtime in
  Reddit_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Zhihu_emacs.install_protocol ();
  Douban_emacs.install_protocol ();
  Maildir_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Discourse_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Rss_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Jsonfeed_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  defun (Symbol.intern "camulus-subscription-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     let j=Reader_backend.to_json request in
     let open Yojson.Safe.Util in
     request_subscription (j |> member "source" |> to_string) (j |> member "target" |> to_string)
       (match member "setting" j with `String s -> Some s | `Null -> None | _ -> invalid_arg "Invalid setting"));
  defun (Symbol.intern "camulus-group-subscription-submit") [%here]
    ~docstring:"Read or change a backend's group subscription." (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     let source=Reader_backend.text(Reader_backend.field "source" request) in
     let group=Reader_backend.text(Reader_backend.field "group" request) in
     let setting=Reader_backend.field "setting" request in
     match Option.bind (Reader_backend.find source) ~f:(fun backend -> backend.group_subscription) with
     | None -> failwith "This backend does not offer group subscriptions"
     | Some submit -> submit ~source ~group ~setting:(if Value.eq setting Value.nil then None else Some(Value.to_utf8_bytes_exn setting)));
  defun (Symbol.intern "camulus-group-followed") [%here]
    ~docstring:"Discover a backend's locally followed groups." (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open source=required "source" string in
     match Reader_backend.find source with
     | Some backend when Option.is_some backend.group_subscription -> (match (Option.value_exn backend.followed) () with Reader_backend.Pending id->id | Immediate _->failwith "Not a local subscription provider")
     | _ -> failwith "This backend does not offer group subscriptions");
  defun (Symbol.intern "camulus-group-subscriptions-configure") [%here]
    ~docstring:"Set mutable local group subscription storage before startup." (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open path=required "path" string in
     ensure_ui ();if Option.is_some !runtime then failwith "Stop runtime before changing group subscription storage";
     if Stdlib.Filename.is_relative path then invalid_arg "Use an absolute state path";
     group_subscriptions_file:=Some path);
  defun (Symbol.intern "camulus-content-state-configure") [%here]
    ~docstring:"Set an absolute local state database path before startup; empty disables storage."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open path = required "path" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop runtime before changing state storage";
     if not(String.is_empty path) && Stdlib.Filename.is_relative path then failwith "Use an absolute state path";
     state_file := if String.is_empty path then None else Some path);
  defun_nullary (Symbol.intern "camulus-content-sources") [%here]
    ~docstring:"Configured source identities."
    (Returns Value.Type.(list string)) (fun () -> ensure_ui ();
      configured_sources ());
  defun (Symbol.intern "camulus-state-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     request_state (CS.operation_of_json (Reader_backend.to_json request)));
  defun (Symbol.intern "camulus-probe-start") [%here] ~docstring:"Start the in-process Eio worker."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open p = required "pipe-process" value in start_runtime p);
  defun (Symbol.intern "camulus-probe-submit") [%here] ~docstring:"Submit synthetic work without blocking Emacs."
    (Returns Value.Type.int)
    (let open Defun.Let_syntax in
     let%map_open ms = required "milliseconds" int
     and steps = required "steps" int
     and service = required "service" bool
     and fail = required "fail" bool
     and allocate = required "allocation-rounds" int in
     submit ms steps service fail allocate);
  defun (Symbol.intern "camulus-probe-cancel") [%here] ~docstring:"Request cancellation of one job."
    (Returns Value.Type.bool)
    (let open Defun.Let_syntax in let%map_open id = required "id" int in
     Runtime.submit (get_runtime ()) (Protocol.Cancel id));
  defun_nullary (Symbol.intern "camulus-probe-close-view") [%here]
    ~docstring:"Cancel view-owned work and invalidate late view results."
    (Returns Value.Type.int) (fun () ->
      let t = get_runtime () in
      if not (Runtime.submit t (Protocol.Close_view !current_generation)) then
        failwith "close-view not accepted";
      Int.incr current_generation; !current_generation);
  defun_nullary (Symbol.intern "camulus-probe-drain") [%here]
    ~docstring:"Publish at most 64 results on the UI thread."
    (Returns Value.Type.(list value)) (fun () -> drain ());
  defun_nullary (Symbol.intern "camulus-probe-status") [%here] ~docstring:"Read worker lifecycle."
    (Returns Value.Type.string) status;
  defun_nullary (Symbol.intern "camulus-probe-stale-count") [%here] ~docstring:"Count discarded stale results."
    (Returns Value.Type.int) (fun () -> ensure_ui (); !dropped_stale);
  defun_nullary (Symbol.intern "camulus-probe-signals-unchanged") [%here]
    ~docstring:"Check SIGPIPE and SIGCHLD ownership against the pre-worker snapshot."
    (Returns Value.Type.bool) (fun () -> ensure_ui (); Reader.Host.signals_unchanged ());
  defun_nullary_nil (Symbol.intern "camulus-probe-stop") [%here] ~docstring:"Request asynchronous shutdown."
    (fun () -> Runtime.request_stop (get_runtime ()));
  defun_nullary (Symbol.intern "camulus-probe-reap") [%here] ~docstring:"Release a finished worker; never wait for active jobs."
    (Returns Value.Type.bool) reap;
  defun_nullary_nil (Symbol.intern "camulus-probe-gc") [%here] ~docstring:"Exercise Ecaml value finalization on the UI side."
    (fun () ->
      ensure_ui ();
      for i = 1 to 2000 do ignore (Stdlib.Sys.opaque_identity (Value.of_utf8_bytes (Int.to_string i)) : Value.t) done;
      Gc.full_major ());
  Reader_ui.register ();
  Runtime_ui.install ~on_start:(Maildir_emacs.start_sync ~submit:request_mail) ~on_stop:Maildir_emacs.stop_sync
    ~on_event:(fun event -> if Current_buffer.variable_is_defined (Symbol.intern "camulus-reader--requests") then Reader_ui.event (Runtime_ui.event_value event));
  defun_nullary_nil (Symbol.intern "camulus-reader-backends-install") [%here]
    ~docstring:"Register the linked backends' optional Emacs capabilities."
    (fun () ->
      let configured source=List.mem (configured_sources ()) source ~equal:String.equal in
      Reader_backend.local_group_operations := Some {followed_groups;subscribe_group};
      Reddit_emacs.install ~submit:request_reddit ~configured ();
      Discourse_emacs.install ~submit:submit_work ();
      Rss_emacs.install ~submit:submit_work ();
      Jsonfeed_emacs.install ~submit:submit_work ();
      Maildir_emacs.install ~submit:request_mail ~configured ();
      Atlantic_emacs.install ~submit:submit_work ();Janestreet_emacs.install ~submit:submit_work ();
      Zhihu_emacs.install ~submit:submit_work ();Douban_emacs.install ~submit:submit_work ();
      Netease_emacs.install ~submit:submit_work ();Public_inbox_emacs.install ~submit:submit_work ();
      Blogger_emacs.install ~submit:submit_work ();Gcores_emacs.install ~submit:submit_work ());
  Reader_config.setup_variables ();
  Reader_config.register ~idle:(fun()->String.equal(Runtime_ui.status())"idle");
  Reader_ui.install ();
  provide (Symbol.intern "camulus_native")
