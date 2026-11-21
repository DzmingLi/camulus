open! Core
open! Ecaml
open Reader_backend
module B = Atlantic_backend
let prepare ~notify:_ =
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups ~revisions:_ ~article ->
    let backend=B.Live.create () and source="atlantic" in
    let previous id=article ~source ~id in
    let cached id=Option.map (previous id) ~f:Rss_backend.article_of_json in
    Rss_emacs.publication_worker ~source
      ~group:(fun id requested -> match requested with Some group -> group | None -> groups ~source ~id |> List.hd |> Option.value_exn ~message:"Open this source group before reading its article")
      ~page:(fun group ~after ->
        let articles=B.Live.local_articles backend group ~after ~previous in
        {Reader.Content.messages=List.map articles ~f:(fun article -> article.Rss_backend.Decode.message);after=None},
        List.map articles ~f:Rss_backend.article_json)
      ~read:(fun group id -> B.Live.read_local backend group id ~previous)
      ~archive:(fun group id -> match cached id with
        | Some article -> B.Live.archive_local article
        | None -> B.Live.archive backend group id)
      ~attachment:(fun group id part destination -> match cached id with
        | Some article -> B.Live.attachment_local article part destination
        | None -> B.Live.attachment backend group id part destination)
      ~refresh:(fun group _ -> ignore(B.Live.list backend group ~after:None : Reader.Content.page)) ())}
let install ~submit () =
  worker_factories := prepare :: !worker_factories;
  let backend=Rss_emacs.publication_backend ~submit ~source:"atlantic" ~threaded:false in
  register_local_feeds ~option:"camulus-theatlantic-subscriptions" ~accepts:all_groups
    {backend with inline_attachments=false}
