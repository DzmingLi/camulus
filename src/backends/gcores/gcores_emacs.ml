open! Core
open! Ecaml
open Reader_backend
let install ~read ~attachment () =
  register_local_groups { (base ~name:"机核" ~matches:(String.equal "gcores") ~threaded:false
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group -> Value.of_int_exn (read id group))) with
    attachment=Some (fun ~source:_ ~id ~group ~part ~destination -> Value.of_int_exn (attachment id group part destination));
    events=["rss"];browser=Some("机核","camulus-gcores") };
  Defun.defun (Symbol.intern "camulus-gcores") [%here] ~docstring:"Browse a source without adding it to the timeline."
    ~interactive:(Prompt "Gcores user ID: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open target=required "target" string in
     let group="talks."^String.strip target in ignore(Gcores_backend.user group : string);
     run "ef-reader-browse-entry" [alist["name",s ("机核: "^target);"source",s "gcores";"group",s group]])
