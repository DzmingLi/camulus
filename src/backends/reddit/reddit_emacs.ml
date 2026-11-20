open! Core
open! Ecaml
open Reader_backend
module Protocol = Reader.Protocol
module CS = Reader.Content_state
let reddit_config = ref None
type Protocol.work += Reddit of Reddit_backend.operation
type Protocol.payload += Reddit_result of Reddit_backend.result
let source () = Option.map !reddit_config ~f:(fun c -> "reddit:"^c.Reddit_backend.username)
let submit_operation ~submit operation =
  if Option.is_none !reddit_config then failwith "Configure reddit first";
  submit (Reddit operation)
module Voting = Reader.Voting
let votes = Voting.create ()
let vote_of_int = function
  | 1 -> Reader.Content.Upvote | -1 -> Downvote | 0 -> No_vote
  | _ -> invalid_arg "Vote must be -1, 0 or 1"
let vote_code = function Reader.Content.Upvote -> 1 | Downvote -> -1 | No_vote -> 0
let vote_status = function
  | Voting.Pending -> "pending", "" | Confirmed -> "confirmed", ""
  | Rejected message -> "rejected", message | Uncertain message -> "uncertain", message

let vote_update_value (update : Voting.update) =
  let state, message = vote_status update.status in
  Value.list [Value.intern "reddit-vote";Value.of_int_exn update.request;
    Value.of_utf8_bytes update.id;Value.of_int_exn (vote_code update.direction);
    Value.intern state;Value.of_utf8_bytes message]

let update_votes ~barrier event =
  let finish (r : Protocol.request) status = match r.work with
    | Reddit (Vote (id,_)) -> Option.to_list (Voting.finish votes ~id ~request:r.id ~barrier ~status)
    | _ -> [] in
  let observe request messages =
    List.filter_map messages ~f:(fun (m : Reader.Content.message) ->
      Voting.observe votes ~id:m.id ~request ~direction:m.my_vote) in
  match event with
  | Protocol.Data (r,Reddit_result (Voted _)) -> finish r Voting.Confirmed
  | Data (r,Reddit_result (Backend_error error)) ->
    let message = Reddit_backend.to_string error in
    let status = match error with
      | Reddit_backend.Uncertain_write | Invalid_response _ -> Voting.Uncertain message
      | _ -> Voting.Rejected message in
    finish r status
  | Finished (r,outcome) ->
    let message = match outcome with
      | Protocol.Completed -> "No vote confirmation received; refresh to check"
      | Cancelled -> "Vote cancelled; remote outcome is unknown"
      | Failed message -> message in
    finish r (Voting.Uncertain message)
  | Data (r,Reddit_result (Page page)) -> observe r.id page.messages
  | Data (r,Reddit_result (Thread_batch batch)) -> observe r.id batch.messages
  | _ -> []

let prepare ~notify:_ =
  Option.map !reddit_config ~f:(fun config ->
    Voting.clear votes;
    {drain=(fun () -> []);close=(fun () -> ());create=(fun ~net:_ ~now ~sleep ~groups:_ ~revisions:_ ~article:_ ->
      let backend=Reddit_backend.Live.create ~config ~now ~sleep in
      {matches_source=String.equal ("reddit:"^config.username);all_pages=false;refresh_membership=`Merge;
       page=(fun ~source:_ group ~after ->
         let group=if String.equal group "notifications" then Reddit_backend.Notifications
           else if String.is_prefix group ~prefix:"post:" then Reddit_backend.Post(String.drop_prefix group 5)
           else Reddit_backend.Subreddit group in
         Reddit_backend.Live.list backend group ~after,[]);
       archive=(fun ~source:_ ~refresh:_ target id ~native_thread ~add ->
         let root=if String.is_prefix id ~prefix:"t3_" then id else native_thread id in
         let complete=ref false in
         Reddit_backend.Live.thread backend root ~emit:(fun batch ->
           complete:=batch.complete;
           List.iter batch.messages ~f:(fun message ->
             if Poly.equal target CS.Thread || String.equal message.id id then
               add {CS.message;attachments=`List [];raw=None;files=[]}));!complete);
       execute=(fun work ~observe ~emit -> let observe=observe ~source:("reddit:"^config.username) in match work with
         | Reddit operation ->
           (try Reddit_backend.Live.execute backend operation ~emit:(fun result ->
              let messages=match result with Reddit_backend.Page p -> p.messages | Thread_batch b -> b.messages | _ -> [] in
              if not(List.is_empty messages) then observe messages;
              emit(Reddit_result result))
            with Reddit_backend.Error error as ex -> emit(Reddit_result(Reddit_backend.Backend_error error));raise ex);true
         | _ -> false)})})
let install_protocol ~ensure_ui ~idle ~submit =
  worker_factories := prepare :: !worker_factories;
  source_providers := source :: !source_providers;
  payload_renderers := (function Reddit_result r -> Some ("reddit",Reddit_backend.result_json r) | _ -> None) :: !payload_renderers;
  event_observers := (fun ~barrier event -> List.map (update_votes ~barrier event) ~f:vote_update_value) :: !event_observers;
  defun (Symbol.intern "camulus-reddit-vote") [%here]
    ~docstring:"Set an absolute vote with optimistic UI state; reject overlapping votes for the same item."
    (Returns Value.Type.int)
    (let open Defun.Let_syntax in
     let%map_open id = required "item-id" string
     and previous = required "previous-vote" int
     and direction = required "direction" int in
     ensure_ui ();
     if Option.is_none !reddit_config then failwith "Configure Reddit first";
     ignore (Reddit_backend.Decode.require_thing id : string);
     let previous = vote_of_int previous and direction = vote_of_int direction in
     if Voting.is_pending votes id then failwith "A vote for this item is still pending";
     let request=submit (Reddit (Vote (id,direction))) in
     Voting.start votes ~id ~request ~previous ~direction;
     request);
  defun (Symbol.intern "camulus-reddit-vote-state") [%here]
    ~docstring:"Return the local vote overlay as (direction status message request-id), or nil."
    (Returns Value.Type.value)
    (let open Defun.Let_syntax in
     let%map_open id = required "item-id" string in
     ensure_ui ();
     match Voting.find votes id with
     | None -> Value.nil
     | Some update ->
       let state, message = vote_status update.status in
       Value.list [Value.of_int_exn (vote_code update.direction);Value.intern state;
         Value.of_utf8_bytes message;Value.of_int_exn update.request]);
  defun (Symbol.intern "camulus-reddit-configure") [%here]
    ~docstring:"Configure the OCaml Reddit backend before starting the runtime; token file is read only by the worker."
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open username = required "username" string
     and token_file = required "token-file" string
     and client_id = required "client-id" string
     and redirect_uri = required "redirect-uri" string in
     ensure_ui ();
     if not(idle ()) then failwith "Stop the runtime before reconfiguring Reddit";
     let config = Reddit_backend.config ~username ~token_file in
     let config = {config with client_id;redirect_uri} in
     Reddit_backend.validate_config config;
     reddit_config := Some config);
  defun (Symbol.intern "camulus-reddit-submit") [%here]
    ~docstring:"Submit an operation described by an Emacs alist." (Returns Value.Type.int)
    (let open Defun.Let_syntax in let%map_open request=required "request" value in
     submit_operation ~submit (Reddit_backend.operation_of_json (Reader_backend.to_json request)));
  ()

let subreddit_name name =
  let name=String.strip name in
  let name=if String.is_prefix name ~prefix:"r/" then String.drop_prefix name 2 else name in
  if String.is_empty name || not(String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')) then None
  else Some name

let composer ~submit ~configured =
  let post source name () =
    Writing.post_draft ~destination:("Reddit · r/"^name)
      ~identity:(source^"/"^String.lowercase name)
      ~submit:(fun ~subject ~body ~complete ->
        Writing.request (submit(Reddit_backend.Submit(name,subject,body))) ~expect:"submit" ~complete) in
  let in_group entry =
    let source=text(field "source" entry) and group=text(field "group" entry) in
    if not(configured source) || String.equal group "notifications" then None
    else Option.map (subreddit_name group) ~f:(post source) in
  let destinations () =
    let source="reddit:"^text(get "camulus-reddit-username") in
    if not(configured source) then [] else
    let names=List.filter_map (values(get "camulus-groups")) ~f:(fun entry ->
      if String.equal (text(field "source" entry)) source then Some(text(field "group" entry)) else None)
      @ List.map (values(get "camulus-reddit-subscriptions")) ~f:text in
    let names=List.filter_map names ~f:(fun name ->
      if String.equal name "notifications" then None else subreddit_name name)
      |> List.dedup_and_sort ~compare:(fun a b -> String.compare (String.lowercase a) (String.lowercase b)) in
    List.map names ~f:(fun name -> {label="Reddit · r/"^name;open_=post source name})
    @ [{label="Reddit · Other subreddit…";open_=(fun () ->
      match subreddit_name(text(call "read-string" [s "Subreddit: "])) with
      | Some name -> post source name ()
      | None -> user_error "Enter a subreddit name")}] in
  {email=false;in_group;destinations}

let install ~submit ~configured () =
  Interaction_emacs.install ();
  custom ~kind:Customization.Type.(Option {doc_for_none="Unconfigured";t=String}) "camulus-reddit-username" Value.nil "Backend configuration option.";
  custom ~kind:Customization.Type.File "camulus-reddit-token-file" (call "expand-file-name" [s "~/News/nnreddit/refresh-token"]) "Backend configuration option.";
  custom ~kind:Customization.Type.String "camulus-reddit-client-id" (s "5oagOpX2_NKVDej_iuZjFA") "Backend configuration option.";
  custom ~kind:Customization.Type.String "camulus-reddit-redirect-uri" (s "http://127.0.0.1:17973") "Backend configuration option.";
  register_local_groups ~option:"camulus-reddit-subscriptions" ~source:(fun _->"reddit:"^text(get "camulus-reddit-username")) ~accepts:all_groups { (base ~name:"Reddit" ~matches:(String.is_prefix ~prefix:"reddit:") ~threaded:true
    ~read:(fun ~source:_ ~id ~thread_id ~group:_ ->
      Reddit(Reddit_backend.Thread (Option.value thread_id ~default:id)))) with
    start=(fun()->if truth(get "camulus-reddit-username")then run "camulus-reddit-configure"[get "camulus-reddit-username";get "camulus-reddit-token-file";get "camulus-reddit-client-id";get "camulus-reddit-redirect-uri"]);
    composer=Some(composer ~submit ~configured);
    controls=Interaction_emacs.controls;list_controls=Interaction_emacs.controls;
    article_marks=(fun m->"  ["^Interaction_emacs.vote_label m^"]");
    row_title=(fun row->s("["^Interaction_emacs.vote_label row^"] "^text(field "title"row)));
    events=["reddit"];available=configured;thread_contains_bodies=true;browser=Some ("Reddit", "camulus-reader-browse-reddit") };
  Defun.defun (Symbol.intern "camulus-reader-browse-reddit") [%here] ~docstring:"Browse a subreddit without following it."
    ~interactive:(Prompt "Subreddit: ") (Returns Value.Type.unit)
    (let open Defun.Let_syntax in let%map_open subreddit=required "subreddit" string in
     run "camulus-reader-start" [];
     let username=Current_buffer.symbol_value (Symbol.intern "camulus-reddit-username") in
     if Value.eq username Value.nil then run "user-error" [s "Configure a Reddit account first"];
     match subreddit_name subreddit with
     | None -> user_error "Enter a subreddit name"
     | Some name -> run "camulus-reader-browse-entry" [alist ["name",s ("r/"^name);"source",s ("reddit:"^Value.to_utf8_bytes_exn username);"group",s name]])
