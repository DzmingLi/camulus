open! Core
open! Ecaml
open Reader_backend
let install ~submit ~configured () =
  Process_emacs.install ();
  Compose_emacs.install ();
  variable "ef-mail-root" (call "expand-file-name" [s "~/Maildir"]);
  variable "ef-mail-muhome" (call "expand-file-name" [s "~/.cache/mu"]);
  List.iter ["ef-mail-sender";"ef-mail-smtp-profiles";"ef-mail-accounts"] ~f:(fun name -> variable name Value.nil);
  variable "ef-mail-account" (s "");
  register_settings "maildir"(fun j->
    let strings=string_settings ["root","ef-mail-root";"muhome","ef-mail-muhome";"sender","ef-mail-sender";"account","ef-mail-account"]j in
    let lists=List.filter_map ["accounts","ef-mail-accounts",3;"smtp_profiles","ef-mail-smtp-profiles",6]~f:(fun(k,var,width)->match json_field k j with
     |`Null->None
     |`List rows when List.for_all rows~f:(function `List cells when List.length cells=width ->
       List.for_alli cells~f:(fun index->function `Int port when width=6 && index=2->port>0 && port<65536|`String _->not(width=6 && index=2)|_->false)|_->false)->Some(var,of_json(`List rows))
     |_->failwith("Invalid maildir setting: "^k))in strings@lists);
  register { (base ~name:"Maildir" ~matches:(String.is_prefix ~prefix:"maildir:") ~threaded:true
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group:_ -> Value.of_int_exn (submit (Maildir_backend.Thread id)))) with
    start=(fun()->if truth(get "ef-mail-sender")then(
      run "ef-mail-configure"[get "ef-mail-root";get "ef-mail-muhome";get "ef-mail-sender";
       (let mu=call "executable-find"[s "mu"]in if truth mu then mu else s "mu");get "ef-mail-account"];
      List.iter(values(get "ef-mail-smtp-profiles"))~f:(fun row->run "apply"[sym "ef-mail-configure-smtp";row]);
      List.iter(values(get "ef-mail-accounts"))~f:(fun row->run "apply"[sym "ef-mail-configure-account";row])));
    attachment=Some (fun ~source:_ ~id ~group:_ ~part ~destination -> Value.of_int_exn (submit (Maildir_backend.Attachment(id,part,destination))));
    events=["mail"];available=configured;
    delete=Some Compose_emacs.trash;
    controls=[{key="r";hint="r: reply";command="ef-mail-reply"};{key="c";hint="c: compose";command="ef-mail-compose"};{key="a";hint="a: archive";command="ef-mail-archive"}];
    list_controls=[{key="r";hint="r: reply";command="ef-mail-reply"};{key="c";hint="c: compose";command="ef-mail-compose"};{key="a";hint="a: archive";command="ef-mail-archive"};{key="D";hint="D: drafts";command="ef-mail-drafts"}] }
