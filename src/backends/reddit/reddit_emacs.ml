open! Core
open! Ecaml
open Reader_backend
let install ~submit ~configured () =
  Interaction_emacs.install ();
  variable "ef-reddit-username" Value.nil;
  variable "ef-reddit-token-file" (call "expand-file-name" [s "~/News/nnreddit/refresh-token"]);
  variable "ef-reddit-client-id" (s "5oagOpX2_NKVDej_iuZjFA");
  variable "ef-reddit-redirect-uri" (s "http://127.0.0.1:17973");
  register_settings "reddit"(string_settings["username","ef-reddit-username";"token_file","ef-reddit-token-file"]);
  register { (base ~name:"Reddit" ~matches:(String.is_prefix ~prefix:"reddit:") ~threaded:true
    ~read:(fun ~source:_ ~id ~thread_id ~group:_ ->
      Value.of_int_exn (submit (Reddit_backend.Thread (Option.value thread_id ~default:id))))) with
    start=(fun()->if truth(get "ef-reddit-username")then run "ef-reddit-configure"[get "ef-reddit-username";get "ef-reddit-token-file";get "ef-reddit-client-id";get "ef-reddit-redirect-uri"]);
    controls=Interaction_emacs.controls;list_controls=Interaction_emacs.controls;
    article_marks=(fun m->"  ["^Interaction_emacs.vote_label m^"]");
    row_title=(fun row->s("["^Interaction_emacs.vote_label row^"] "^text(field "title"row)));
    events=["reddit"];available=configured;thread_contains_bodies=true;browser=Some ("Reddit", "ef-reader-browse-reddit") };
  Defun.defun (Symbol.intern "ef-reader-browse-reddit") [%here] ~docstring:"Browse a subreddit without following it."
    ~interactive:(Prompt "sSubreddit: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open subreddit=required "subreddit" string in
     run "ef-reader-start" [];
     let username=Current_buffer.symbol_value (Symbol.intern "ef-reddit-username") in
     if Value.eq username Value.nil then run "user-error" [s "Configure a Reddit account first"];
     let subreddit=String.strip subreddit in
     let subreddit=if String.is_prefix subreddit ~prefix:"r/" then String.drop_prefix subreddit 2 else subreddit in
     if String.is_empty subreddit || not (String.for_all subreddit ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')) then
       run "user-error" [s "Enter a subreddit name"];
     run "ef-reader-browse-entry" [alist ["name",s ("r/"^subreddit);"source",s ("reddit:"^Value.to_utf8_bytes_exn username);"group",s subreddit]])
