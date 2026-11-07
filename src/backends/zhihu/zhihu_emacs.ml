open! Core
open Reader_backend
module Z=Zhihu_backend

let prepare ~notify:_ =
  let browser=firefox_cookie_provider () in
  Some {drain=(fun()->[]);close=(fun()->());
    create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
      let backend=Z.create_live ~cookies:(Reader.Credentials.Firefox.get browser) () in
      Rss_emacs.publication_worker ~source:"zhihu" ~group:(fun _ _ -> "")
        ~page:(fun group ~after -> Z.Live.list backend group ~after)
        ~read:(Z.Live.read backend) ~attachment:(Z.Live.attachment backend)
        ~archive:(fun group id -> Z.Live.invalidate backend id;Z.Live.archive backend group id)
        ~refresh:(fun _ _ -> ()) ())}

let install_protocol () =
  worker_factories:=prepare::!worker_factories;
  source_providers:=(fun()->Some "zhihu")::!source_providers

let install ~submit () =
  register_local_groups ~option:"camulus-zhihu-subscriptions" ~source:(fun _->"zhihu") ~accepts:all_groups
    (Rss_emacs.publication_backend ~submit ~source:"zhihu" ~threaded:false)
