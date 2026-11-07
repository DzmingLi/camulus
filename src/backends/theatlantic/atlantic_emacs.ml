open! Core
open! Ecaml
open Reader_backend
module B = Atlantic_backend
let prepare ~notify:_ =
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups ->
    let backend=B.Live.create () and source="atlantic" in
    Rss_emacs.publication_worker ~source
      ~group:(fun id requested -> match requested with Some group -> group | None -> groups ~source ~id |> List.hd |> Option.value_exn ~message:"Open this source group before reading its article")
      ~page:(fun group ~after -> B.Live.list backend group ~after)
      ~read:(B.Live.read backend) ~archive:(B.Live.archive backend) ~attachment:(B.Live.attachment backend)
      ~refresh:(fun group _ -> ignore(B.Live.list backend group ~after:None : Reader.Content.page)) ())}
let install ~submit () =
  worker_factories := prepare :: !worker_factories;
  let read,attachment=Rss_emacs.publication_commands ~submit "atlantic" in
  register_local_groups ~option:"camulus-theatlantic-subscriptions" ~source:(fun _->"atlantic") ~accepts:all_groups { (base ~name:"atlantic" ~matches:(String.equal "atlantic") ~threaded:false
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group -> Value.of_int_exn (read id group))) with
    attachment=Some (fun ~source:_ ~id ~group ~part ~destination -> Value.of_int_exn (attachment id group part destination));
    events=["rss"] }
