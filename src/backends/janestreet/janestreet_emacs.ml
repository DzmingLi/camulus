open! Core
open! Ecaml
open Reader_backend
module B = Janestreet_backend
let prepare ~notify:_ =
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
    let backend=B.Live.create () and source="janestreet" in
    Rss_emacs.publication_worker ~source
      ~group:(fun _id requested -> match requested with Some group -> group | None -> "posts")
      ~page:(fun group ~after ~query -> match query with None -> B.Live.list backend group ~after | Some query -> B.Live.search backend group ~query ~after)
      ~read:(B.Live.read backend) ~archive:(B.Live.archive backend) ~attachment:(B.Live.attachment backend)
      ~refresh:(fun group _ -> ignore(B.Live.list backend group ~after:None : Reader.Content.page)) ())}
let install ~submit () =
  worker_factories := prepare :: !worker_factories;
  let read,attachment=Rss_emacs.publication_commands ~submit "janestreet" in
  register_local_groups ~accepts:all_groups { (base ~name:"janestreet" ~matches:(String.equal "janestreet") ~threaded:false
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group -> Value.of_int_exn (read id group))) with
    attachment=Some (fun ~source:_ ~id ~group ~part ~destination -> Value.of_int_exn (attachment id group part destination));
    events=["rss"] }
