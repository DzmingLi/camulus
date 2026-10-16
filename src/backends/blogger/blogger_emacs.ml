open! Core
open! Ecaml
open Reader_backend
let install ~read ~attachment () =
  register_local_groups { (base ~name:"Blogger" ~matches:(String.equal "blogger") ~threaded:false
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group -> Value.of_int_exn (read id group))) with
    attachment=Some (fun ~source:_ ~id ~group ~part ~destination -> Value.of_int_exn (attachment id group part destination));
    events=["rss"];browser=Some("Blogger","camulus-blogger") };
  Defun.defun (Symbol.intern "camulus-blogger") [%here] ~docstring:"Browse a source without adding it to the timeline."
    ~interactive:(Prompt "Blogger host: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open target=required "target" string in
     let group="posts."^String.strip target in ignore(Blogger_backend.host group : string);
     run "ef-reader-browse-entry" [alist["name",s ("Blogger: "^target);"source",s "blogger";"group",s group]])
