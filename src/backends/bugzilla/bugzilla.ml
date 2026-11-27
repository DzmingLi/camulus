module C = Reader.Content
module R = Rss_backend
module J = Yojson.Safe.Util

let host = "https://bugzilla.mozilla.org"
let valid_number s =
  String.length s > 0 && s.[0] <> '0' &&
  String.for_all (function '0'..'9' -> true | _ -> false) s

let number_of_id id = List.hd (String.split_on_char ':' id)
let string field obj = J.member field obj |> J.to_string
let list field obj = J.member field obj |> J.to_list

let fetch number suffix =
  let url = host ^ "/rest/bug/" ^ number ^ suffix in
  let response = Reader.Http.request ~policy:Https_only (Reader.Http.get url) in
  if response.status <> 200 then
    failwith (Printf.sprintf "Mozilla Bugzilla #%s%s returned HTTP %d" number suffix response.status);
  Yojson.Safe.from_string response.body

let history_line change =
  let field = string "field_name" change in
  let field = match J.member "attachment_id" change with
    | `Int id -> Printf.sprintf "Attachment %d · %s" id field
    | _ -> field in
  let value key = match string key change with "" -> "—" | s -> s in
  Printf.sprintf "%s: %s → %s" field (value "removed") (value "added")

let load number =
  if not (valid_number number) then invalid_arg "Expected a Mozilla Bugzilla bug number";
  let first_bug json = match list "bugs" json with
    | [bug] -> bug | _ -> failwith ("Missing Bugzilla report #" ^ number) in
  let bug = first_bug (fetch number "") in
  let comments = fetch number "/comment" |> J.member "bugs" |> J.member number |> list "comments" in
  let history = fetch number "/history" |> first_bug |> list "history" in
  let url = host ^ "/show_bug.cgi?id=" ^ number in
  let message ~id ~title ~author ~body ~date ~link ~activity =
    let parent_id = if id = number then None else Some number in
    {C.id; thread_id=Some number; parent_id; references=Option.to_list parent_id;
     title; author; body; html=None; url=link; link_url=None; created=R.Decode.date date;
     activity_at=None;unavailable=false; score=0; my_vote=C.No_vote; activity} in
  let description, replies = List.partition (fun c -> J.member "count" c = `Int 0) comments in
  let body = String.concat "\n\n" (string "status" bug :: List.map (string "text") description) in
  let root = message ~id:number ~title:("#" ^ number ^ " · " ^ string "summary" bug)
      ~author:(string "creator" bug) ~body ~date:(string "creation_time" bug)
      ~link:url ~activity:(Some (string "last_change_time" bug)) in
  let reply c =
    let count = J.member "count" c |> J.to_int |> string_of_int in
    let id = number ^ ":comment:" ^ string_of_int (J.member "id" c |> J.to_int) in
    let body = string "text" c in
    message ~id ~title:("Comment #" ^ count) ~author:(string "creator" c) ~body
      ~date:(string "creation_time" c) ~link:(url ^ "#c" ^ count)
      ~activity:(Some (Digest.to_hex (Digest.string body))) in
  let event e =
    let date = string "when" e and author = string "who" e in
    let lines = List.map history_line (list "changes" e) in
    (* Bugzilla history records have no event ID. Use their contents so that
       adding later events or reordering a change list preserves identity. *)
    let fingerprint = String.concat "\000" (date :: author :: List.sort String.compare lines) in
    let id = number ^ ":history:" ^ Digest.to_hex (Digest.string fingerprint) in
    message ~id ~title:"Bug updated" ~author ~body:(String.concat "\n" lines)
      ~date ~link:(host ^ "/show_activity.cgi?id=" ^ number) ~activity:None in
  root :: List.stable_sort (fun a b -> Float.compare a.C.created b.C.created)
    (List.map reply replies @ List.map event history)

let article message = {R.Decode.message; enclosures=[]}
let archive_article (a:R.Decode.article) =
  {Reader.Content_state.message=a.message; attachments=`List []; raw=None; files=[]}
