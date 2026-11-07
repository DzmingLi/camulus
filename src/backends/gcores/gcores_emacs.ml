open! Core
open! Ecaml
open Reader_backend
module B = Gcores_backend
let prepare ~notify:_ =
  Some {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now:_ ~sleep:_ ~groups:_ ->
    let backend=B.Live.create () and source="gcores" in
    Rss_emacs.publication_worker ~source
      ~group:(fun _id requested -> match requested with Some group -> group | None -> "")
      ~page:(fun group ~after ~query -> match query with None -> B.Live.list backend group ~after | Some query -> B.Live.search backend group ~query ~after)
      ~read:(B.Live.read backend) ~archive:(B.Live.archive backend) ~attachment:(B.Live.attachment backend)
      ~refresh:(fun _ id -> B.Live.invalidate backend id) ())}
let install ~submit () =
  worker_factories := prepare :: !worker_factories;
  let read,attachment=Rss_emacs.publication_commands ~submit "gcores" in
  register_local_groups ~accepts:all_groups { (base ~name:"机核" ~matches:(String.equal "gcores") ~threaded:false
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group -> Value.of_int_exn (read id group))) with
    attachment=Some (fun ~source:_ ~id ~group ~part ~destination -> Value.of_int_exn (attachment id group part destination));
    events=["rss"];browser=Some("机核","camulus-gcores") };
  Defun.defun (Symbol.intern "camulus-gcores") [%here] ~docstring:"Browse a source without adding it to the timeline."
    ~interactive:(Prompt "Gcores user ID: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open target=required "target" string in
     let group="talks."^String.strip target in ignore(Gcores_backend.user group : string);
     run "camulus-reader-browse-entry" [alist["name",s ("机核: "^target);"source",s "gcores";"group",s group]])
