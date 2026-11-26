open! Core
open! Ecaml
open Reader_backend
module Connection = Rss_emacs.Connect(struct
  include Jsonfeed_backend.Live
  let id="jsonfeed"
  let all_pages=true
  let local_articles=false
  let page t url ~after = list t url ~after,[]
  let is_source=Jsonfeed_backend.is_source
  let source_of_url=Jsonfeed_backend.source_of_url
  let url_of_source=Jsonfeed_backend.url_of_source
end)
let install_protocol = Connection.install_protocol
let install ~submit:_ () =
  register_local_feeds ~groups:false ~option:"camulus-jsonfeed-subscriptions" ~key_for_group:Jsonfeed_backend.source_of_url
    ~accepts:(fun ~source ~group->String.equal (Jsonfeed_backend.url_of_source source) group) { (base ~id:"jsonfeed" ~name:"JSON Feed" ~threaded:false
    ~read:(fun ~source ~id ~thread_id:_ ~group:_ -> Connection.Request(Rss_emacs.Read(Jsonfeed_backend.url_of_source source,id)))) with
    render_media=Rss_emacs.render_media;
    events=["rss"] }
