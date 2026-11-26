open Reader_backend
module B = Bugzilla
module C = Reader.Content

let source = "bugzilla"
let prepare ~notify:_ =
  let create ~net:_ ~now:_ ~sleep:_ ~groups:_ ~revisions:_ ~article:_ =
    let group id _ = B.number_of_id id in
    let page number ~after =
      if Option.is_some after then invalid_arg "Bugzilla report has no next page";
      let messages = B.load number in
      {C.messages; after=None}, List.map (fun m -> Rss_backend.article_json (B.article m)) messages in
    let read number id =
      B.load number |> List.find (fun m -> m.C.id = id) |> B.article in
    let stream id ~emit_article ~emit =
      let thread = B.number_of_id id in
      let messages = B.load thread in
      List.iter (fun m -> emit_article (B.article m)) messages;
      emit {C.thread; messages; gaps=[]; complete=true} in
    Rss_emacs.publication_worker ~source ~group ~page ~read
      ~archive:(fun number id -> B.archive_article (read number id))
      ~refresh:(fun _ _ -> ()) ~thread:(stream,B.archive_article) () in
  Some {drain=(fun () -> []); close=(fun () -> ()); create}

let install () =
  worker_factories := prepare :: !worker_factories;
  let backend = Rss_emacs.publication_backend ~source ~threaded:true in
  register_local_feeds ~groups:false ~option:"camulus-bugzilla-subscriptions"
    ~accepts:(fun ~source:_ ~group -> B.valid_number group)
    ~feed_name:(fun number -> "Mozilla Bugzilla #" ^ number)
    {backend with name="Mozilla Bugzilla"}
