open! Core
open! Ecaml
open Reader_backend
module B = Netease_backend
let prepare ~notify:_ =
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups ~revisions:_ ~article:_ ->
    let backend=B.Live.create () and source="netease" in
    Rss_emacs.publication_worker ~source
      ~group:(fun id requested -> match requested with Some group -> group | None -> groups ~source ~id |> List.hd |> Option.value_exn ~message:"Open the feed before reading its article")
      ~page:(fun group ~after -> B.Live.list backend group ~after,[])
      ~read:(B.Live.read backend) ~archive:(B.Live.archive backend) ~attachment:(B.Live.attachment backend)
      ~refresh:(fun group _ -> ignore(B.Live.list backend group ~after:None : Reader.Content.page)) ())}
let install ~submit () =
  worker_factories := prepare :: !worker_factories;
  let backend=Rss_emacs.publication_backend ~submit ~source:"netease" ~threaded:false in
  register_local_feeds ~groups:false ~feed_name:(fun id -> B.user id^" · 动态") ~option:"camulus-netease-subscriptions" ~accepts:all_groups backend
