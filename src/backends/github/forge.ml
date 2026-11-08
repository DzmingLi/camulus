(** Emacs-side navigation only. Forge owns remote/worktree associations and
    Git operations; the GitHub protocol never calls Emacs or consults its DB. *)
open! Core
open! Ecaml
open Reader_backend
module G = Github_backend

let require () =
  if not(truth(call "require" [sym "forge";nil;sym "t"])) then
    user_error "Install Forge to navigate between GitHub topics and local repositories"

let slot object_ name = call "eieio-oref" [object_;sym name]
let identity owner repo = Value.list [s "github.com";s owner;s repo]
let known owner repo =
  require ();
  (* :known? is a local lookup. Opening Camulus must not register or fetch a
     repository merely to find its worktree. *)
  call "forge-get-repository" [identity owner repo;nil;sym ":known?"]

let worktree owner repo =
  let repository=known owner repo in
  if truth repository then call "forge-get-worktree" [repository] else nil

let selected () =
  let row=call "camulus-reader--selected" [] in
  if not(String.equal(text(field "source" row)) G.source) then
    user_error "Select GitHub content";
  if truth(call "derived-mode-p" [sym "camulus-reader-article-mode"]) then
    let message=call "gethash" [field "id" row;get "camulus-reader--messages"] in
    if truth message then call "cons" [call "cons" [sym "source";s G.source];message] else row
  else row

let selected_topic () =
  match G.topic_of_url(text(field "url" (selected ()))) with
  | Some topic -> topic
  | None -> failwith "The selected GitHub item has no topic URL"

let local_directory topic =
  let directory=worktree topic.G.owner topic.repo in
  if not(truth directory) then
    user_error "No local worktree recorded by Forge; visit this repository in Magit/Forge first";
  directory

let current_repository () =
  require ();
  let repo=call "forge-get-repository" [sym ":stub?"] in
  if not(truth repo) || not(String.equal(text(slot repo "forge")) "github.com") then
    user_error "Open a local GitHub repository or one of its Forge buffers first";
  repo

let install ~browse =
  define0 ~interactive:true "camulus-github-forge-topic" (fun () ->
    let topic=selected_topic () in
    require ();
    (* Explicit navigation lets Forge fetch a missing topic. Its normal cached
       topic path continues to work offline. Strip comment fragments first. *)
    run "forge-visit-topic-from-url" [s(G.topic_url topic)]);
  define0 ~interactive:true "camulus-github-magit" (fun () ->
    let topic=selected_topic () in
    run "magit-status" [local_directory topic]);
  define0 ~interactive:true "camulus-github-repository" (fun () ->
    let repo=current_repository () in
    let directory=call "forge-get-worktree" [repo] in
    let section=text(call "completing-read" [s "GitHub topics: ";
      Value.list(List.map ["issues";"pulls";"discussions"] ~f:s);nil;sym "t"]) in
    browse (text(slot repo "owner")^"/"^text(slot repo "name")^"/"^section);
    if truth directory then local "default-directory" directory);
  define0 ~interactive:true "camulus-github-review-find-file" (fun () ->
    let data=get "camulus-github--review-data" in
    let topic=Option.value_exn(G.topic_of_url(text(field "url" (field "pr" data)))) in
    let directory=local_directory topic in
    let location=call "get-text-property" [call "point" [];sym "camulus-github-location"] in
    if not(truth location) then user_error "Move to a changed line first";
    let path=text(field "path" location) in
    let file=call "expand-file-name" [s path;directory] in
    if not(truth(call "file-in-directory-p" [file;directory])) then
      user_error "This diff path is outside the local worktree";
    (* A diff line is meaningful at its exact base/head commit, not necessarily
       at the current checkout. Magit visits the committed blob read-only. *)
    let key=if String.equal(text(field "side" location)) "LEFT" then "baseRefOid" else "headRefOid" in
    let revision=field key (field "pr" data) in
    Current_buffer.set_value_temporarily Sync
      (Var.create(Symbol.intern "default-directory") Value.Type.value) directory
      ~f:(fun () -> run "magit-find-file" [revision;s path]);
    run "goto-char" [call "point-min" []];
    run "forward-line" [i(max 0 (integer(field "line" location)-1))])
