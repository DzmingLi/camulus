open! Core
open! Ecaml
open Reader_backend
module B = Blogger_backend
let prepare ~notify:_ =
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups ->
    let backend=B.Live.create () and source="blogger" in
    Rss_emacs.publication_worker ~all_pages:true ~source
      ~group:(fun id requested -> match requested with Some group -> group | None -> groups ~source ~id |> List.hd |> Option.value_exn ~message:"Open this source group before reading its article")
      ~page:(fun group ~after -> B.Live.list backend group ~after)
      ~read:(B.Live.read backend) ~archive:(B.Live.archive backend) ~attachment:(B.Live.attachment backend)
      ~refresh:(fun group _ -> ignore(B.Live.refresh backend group : Reader.Content.page)) ())}
let install ~submit () =
  worker_factories := prepare :: !worker_factories;
  let backend=Rss_emacs.publication_backend ~submit ~source:"blogger" ~threaded:false in
  register_local_groups ~option:"camulus-blogger-subscriptions" ~source:(fun _->"blogger") ~accepts:all_groups {backend with name="Blogger";browser=Some("Blogger","camulus-blogger")};
  Defun.defun (Symbol.intern "camulus-blogger") [%here] ~docstring:"Browse a source without adding it to the timeline."
    ~interactive:(Prompt "Blogger host: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open target=required "target" string in
     let group="posts."^String.strip target in ignore(Blogger_backend.host group : string);
     run "camulus-reader-browse-entry" [alist["name",s ("Blogger: "^target);"source",s "blogger";"group",s group]])
