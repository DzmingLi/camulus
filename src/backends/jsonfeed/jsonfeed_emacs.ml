open! Core
open! Ecaml
open Reader_backend
let install ~submit ~followed () =
  variable "camulus-jsonfeed-subscriptions-file"(data_path "XDG_STATE_HOME" "~/.local/state" "jsonfeed-subscriptions.json");
  register { (base ~name:"JSON Feed" ~matches:Jsonfeed_backend.is_source ~threaded:false
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Rss_backend.Read(Jsonfeed_backend.url_of_source source,id))))) with
    attachment=Some (fun ~source ~id ~group:_ ~part ~destination ->
      Value.of_int_exn (submit (Rss_backend.Attachment(Jsonfeed_backend.url_of_source source,id,part,destination))));
    start=(fun()->run "camulus-jsonfeed-subscriptions-configure"[get "camulus-jsonfeed-subscriptions-file"]);
    events=["rss"];subscription_target=Some (fun ~source ~selected:_ -> Some(Jsonfeed_backend.url_of_source source));
    browser=Some("JSON Feed","camulus-jsonfeed");followed=Some(fun () -> Value.of_int_exn (followed ())) };
  Defun.defun (Symbol.intern "camulus-jsonfeed") [%here] ~docstring:"Browse a JSON Feed without subscribing."
    ~interactive:(Prompt "JSON Feed URL: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
      let source=Jsonfeed_backend.source_of_url url in
      run "ef-reader-browse-entry" [alist ["name",s ("JSON Feed: "^url);"source",s source;"group",s url]])
