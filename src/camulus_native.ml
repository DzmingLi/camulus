open! Core
open! Ecaml
open Runtime_ui

let request_mail = Mu_emacs.submit_operation ~submit:submit_work
let request_reddit = Reddit_emacs.submit_operation ~submit:submit_work
let () =
  defun (Symbol.intern "camulus-firefox-cookies-get") [%here]
    ~docstring:"Return Firefox cookies for HTTPS URL as an alist of string pairs.\nRead camulus-firefox-profile on each call, without starting the reader.\nThis synchronous callback reads only the local database; it does not log in."
    (Returns Value.Type.value)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
     ensure_ui ();
     let provider=Reader_backend.firefox_cookie_provider () in
     Value.list(List.map (Reader.Credentials.Firefox.get_sync provider url) ~f:(fun(name,value)->
       Reader_backend.call "cons" [Value.of_utf8_bytes name;Value.of_utf8_bytes value])));
  let idle () = Option.is_none !runtime in
  Reddit_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Zhihu_emacs.install_protocol ();
  Douban_emacs.install_protocol ();
  Mu_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Github_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Discourse_emacs.install_protocol ~ensure_ui ~idle ~submit:submit_work;
  Rss_emacs.install_protocol ();
  Jsonfeed_emacs.install_protocol ();
  defun (Symbol.intern "camulus-content-state-configure") [%here]
    ~docstring:"Set an absolute local state database path before startup; empty disables storage."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open path = required "path" string in
     ensure_ui ();
     if Option.is_some !runtime then failwith "Stop runtime before changing state storage";
     if not(String.is_empty path) && Stdlib.Filename.is_relative path then failwith "Use an absolute state path";
     state_file := if String.is_empty path then None else Some path);
  defun (Symbol.intern "camulus-probe-cancel") [%here] ~docstring:"Request cancellation of one job."
    (Returns Value.Type.bool)
    (let open Defun.Let_syntax in let%map_open id = required "id" int in
     Runtime.submit (get_runtime ()) (Protocol.Cancel id));
  defun_nullary (Symbol.intern "camulus-probe-status") [%here] ~docstring:"Read worker lifecycle."
    (Returns Value.Type.string) status;
  Reader_ui.register ();
  Runtime_ui.install ~on_start:(Mu_emacs.start_sync ~submit:request_mail) ~on_stop:Mu_emacs.stop_sync
    ~on_event:(fun event -> if Current_buffer.variable_is_defined (Symbol.intern "camulus-reader--requests") then Reader_ui.event event);
  defun_nullary_nil (Symbol.intern "camulus-reader-backends-install") [%here]
    ~docstring:"Register the linked backends' optional Emacs capabilities."
    (fun () ->
      let configured source=List.mem (configured_sources ()) source ~equal:String.equal in
      Reddit_emacs.install ~submit:request_reddit ~configured ();
      Github_emacs.install ~submit:submit_work ();
      Discourse_emacs.install ~submit:submit_work ();
      Rss_emacs.install ~submit:submit_work ();
      Jsonfeed_emacs.install ~submit:submit_work ();
      Mu_emacs.install ~submit:request_mail ~configured ~request_state:Runtime_ui.request_state ();
      Atlantic_emacs.install ~submit:submit_work ();Janestreet_emacs.install ~submit:submit_work ();
      Zhihu_emacs.install ~submit:submit_work ();Douban_emacs.install ~submit:submit_work ();
      Netease_emacs.install ~submit:submit_work ();Public_inbox_emacs.install ~submit:submit_work ();
      Blogger_emacs.install ~submit:submit_work ());
  Reader_config.setup_variables ();
  Reader_config.register ~idle:(fun()->String.equal(Runtime_ui.status())"idle");
  Reader_ui.install ();
  provide (Symbol.intern "camulus_native")
