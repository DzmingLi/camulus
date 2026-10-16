open! Core
open! Ecaml
open Reader_backend
let install ~submit ~configured () =
  variable "ef-mail-root" (call "expand-file-name" [s "~/Maildir"]);
  variable "ef-mail-muhome" (call "expand-file-name" [s "~/.cache/mu"]);
  List.iter ["ef-mail-sender";"ef-mail-smtp-profiles";"ef-mail-accounts"] ~f:(fun name -> variable name Value.nil);
  variable "ef-mail-account" (s "");
  register { (base ~name:"Maildir" ~matches:(String.is_prefix ~prefix:"maildir:") ~threaded:true
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Maildir_backend.Thread id)))) with
    attachment=Some (fun ~source:_ ~id ~group:_ ~part ~destination -> Value.of_int_exn (submit (Maildir_backend.Attachment(id,part,destination))));
    events=["mail"];available=configured }
