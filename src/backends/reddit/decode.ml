open Errors
module M = Reader.Content

let get key = function `Assoc xs -> Option.value (List.assoc_opt key xs) ~default:`Null | _ -> `Null
let text = function `String s -> s | _ -> ""
let string key j = text (get key j)
let optional key j = match get key j with `String s when s <> "" -> Some s | _ -> None
let number = function `Int n -> float_of_int n | `Float n -> n | _ -> 0.
let integer j = match j with `Int n -> n | _ -> 0
let items = function `List xs -> xs | _ -> raise (Error (Invalid_response "expected array"))
let parse body = try Yojson.Safe.from_string body with Yojson.Json_error _ ->
  raise (Error (Invalid_response "JSON"))
let alphanumeric s = s <> "" && String.for_all (function 'a'..'z' | '0'..'9' -> true | _ -> false) s
let fullname prefix s = String.starts_with ~prefix:(prefix ^ "_") s
  && alphanumeric (String.sub s 3 (String.length s - 3))
let require_fullname prefix s =
  if not (fullname prefix s) then raise (Error (Invalid_input ("expected " ^ prefix ^ " fullname")));
  s
let require_thing s =
  if not (fullname "t1" s || fullname "t3" s) then raise (Error (Invalid_input "expected post/comment fullname")); s
let require_subreddit s =
  if s = "" || String.length s > 64 || not (String.for_all (function
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false) s) then
    raise (Error (Invalid_input "subreddit name")); s
let listing j =
  if string "kind" j <> "Listing" then raise (Error (Invalid_response "expected Listing"));
  items (get "children" (get "data" j))
let after j = match get "after" (get "data" j) with
  | `Null -> None
  | `String s when s <> "" -> Some s
  | _ -> raise (Error (Invalid_response "listing cursor"))
let action_data j = get "data" (get "json" j)
let things j = items (get "things" (action_data j))

let message ~root thing =
  let kind = string "kind" thing and d = get "data" thing in
  if kind <> "t1" && kind <> "t3" then raise (Error (Invalid_response "message kind"));
  let id = require_fullname kind (string "name" d) in
  let thread, parent = if kind = "t3" then id, None else begin
    let link = require_fullname "t3" (string "link_id" d) in
    if Option.is_some root && root <> Some link then raise (Error (Invalid_response "comment outside requested thread"));
    let parent = require_thing (string "parent_id" d) in
    if parent = id || (fullname "t3" parent && parent <> link) then
      raise (Error (Invalid_response "invalid parent relationship"));
    link, Some parent
  end in
  if kind = "t3" && Option.is_some root && root <> Some id then
    raise (Error (Invalid_response "wrong thread root"));
  let body = string (if kind = "t3" then "selftext" else "body") d in
  let unavailable = body = "[removed]" || body = "[deleted]"
    || (kind = "t3" && get "removed_by_category" d <> `Null) in
  let created = number (get "created_utc" d) in
  if not (Float.is_finite created) || created < 0. then raise (Error (Invalid_response "timestamp"));
  { M.id; thread_id = Some thread; parent_id = parent;
    title = string "title" d;
    author = Option.value (optional "author" d) ~default:"[deleted]";
    body = if unavailable then "" else body;
    html = if unavailable then None else optional (if kind = "t3" then "selftext_html" else "body_html") d;
    url = "https://www.reddit.com" ^ (Option.value (optional "permalink" d)
      ~default:("/comments/" ^ String.sub thread 3 (String.length thread-3) ^ "/"));
    link_url = (if kind = "t3" then optional "url" d else None);
    created; unavailable; score = integer (get "score" d);
    references = [];
    activity = (if kind = "t3" then Some (id ^ ":" ^ string_of_int (integer (get "num_comments" d))) else None);
    my_vote = (match get "likes" d with `Bool true -> M.Upvote | `Bool false -> M.Downvote | _ -> M.No_vote) }

let notifications j =
  listing j |> List.filter_map (fun thing ->
    let d = get "data" thing in
    let target = string "name" d and thread = string "link_id" d in
    if not (fullname "t1" target && fullname "t3" thread) then None else
    Some { M.event_id = target; target; thread;
      title = Option.value (optional "link_title" d) ~default:(string "subject" d);
      reason = (if string "subject" d = "username mention" then "mention" else "reply");
      created = number (get "created_utc" d) })
