open! Core
open Reader_backend
module Z=Zhihu_backend

let prepare ~notify:_ =
  let browser=firefox_cookie_provider () in
  Some {drain=(fun()->[]);close=(fun()->());
    create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ~revisions:_ ~article:_ ->
      let backend=Z.create_live ~cookies:(Reader.Credentials.Firefox.get browser) () in
      Rss_emacs.publication_worker ~source:"zhihu" ~group:(fun _ _ -> "")
        ~page:(fun group ~after -> Z.Live.list backend group ~after,[])
        ~read:(Z.Live.read backend)
        ~archive:(fun group id -> Z.Live.invalidate backend id;Z.Live.archive backend group id)
        ~refresh:(fun _ _ -> ()) ())}

let install_protocol () =
  worker_factories:=prepare::!worker_factories;
  source_providers:=(fun()->Some "zhihu")::!source_providers

let install ~submit:_ () =
  let feed_name id =
    let kind,_,user=Z.parts id in
    user^" · "^(if String.equal kind "answers" then "回答" else "文章") in
  register_local_feeds ~groups:false ~feed_name ~option:"camulus-zhihu-subscriptions" ~accepts:all_groups
    (Rss_emacs.publication_backend ~source:"zhihu" ~threaded:false)
