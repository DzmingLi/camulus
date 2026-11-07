open! Core
open! Ecaml
open Reader_backend
module Connection = Rss_emacs.Connect(struct
  include Jsonfeed_backend.Live
  let name="jsonfeed"
  let label="JSON Feed"
  let is_source=Jsonfeed_backend.is_source
  let source_of_url=Jsonfeed_backend.source_of_url
  let url_of_source=Jsonfeed_backend.url_of_source
end)
let install_protocol = Connection.install_protocol
let install ~submit () =
  let submit=Connection.request ~submit in
  register_local_groups ~option:"camulus-jsonfeed-subscriptions" ~source:Jsonfeed_backend.source_of_url
    ~accepts:(fun ~source ~group->String.equal (Jsonfeed_backend.url_of_source source) group) { (base ~name:"JSON Feed" ~matches:Jsonfeed_backend.is_source ~threaded:false
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Rss_backend.Read(Jsonfeed_backend.url_of_source source,id))))) with
    attachment=Some (fun ~source ~id ~group:_ ~part ~destination ->
      Value.of_int_exn (submit (Rss_backend.Attachment(Jsonfeed_backend.url_of_source source,id,part,destination))));
    events=["rss"];
    browser=Some("JSON Feed","camulus-jsonfeed") };
  Defun.defun (Symbol.intern "camulus-jsonfeed") [%here] ~docstring:"Browse a JSON Feed without subscribing."
    ~interactive:(Prompt "JSON Feed URL: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open url=required "url" string in
      let source=Jsonfeed_backend.source_of_url url in
      run "camulus-reader-browse-entry" [alist ["name",s ("JSON Feed: "^url);"source",s source;"group",s url]])
