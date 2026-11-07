open! Core
open! Ecaml
open Reader_backend
module B = Public_inbox_backend
let prepare ~notify:_ =
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
    let backend=B.Live.create () and source="public-inbox" in
    Rss_emacs.publication_worker ~source
      ~group:(fun _id requested -> match requested with Some group -> group | None -> "")
      ~page:(fun group ~after -> B.Live.list backend group ~after)
      ~read:(B.Live.read backend) ~archive:(B.Live.archive backend) ~attachment:(B.Live.attachment backend)
      ~refresh:(fun _ _ -> ())
      ~thread:(B.Live.stream_thread backend,B.Live.archive_article backend) ())}
let install ~submit () =
  worker_factories := prepare :: !worker_factories;
  let backend=Rss_emacs.publication_backend ~submit ~source:"public-inbox" ~threaded:true in
  register_local_groups ~option:"camulus-public-inbox-subscriptions" ~source:(fun _->"public-inbox") ~accepts:all_groups backend
