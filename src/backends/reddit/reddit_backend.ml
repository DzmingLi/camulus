module Http = Http
module Curl_transport = Curl_transport
module Credentials = Credentials
module Decode = Decode
include Errors
module M = Reader.Content
module D = Decode

(* Integer encoding belongs to Reddit and the existing Emacs/CLI JSON boundary. *)
let vote_code = function M.Upvote -> 1 | M.Downvote -> -1 | M.No_vote -> 0

let default_client_id = "5oagOpX2_NKVDej_iuZjFA"
let default_redirect_uri = "http://127.0.0.1:17973"
let scopes = ["identity"; "read"; "privatemessages"; "submit"; "vote"]
type config = { username : string; token_file : string; client_id : string;
                redirect_uri : string; request_interval : float }
let config ~username ~token_file =
  { username; token_file; client_id = default_client_id;
    redirect_uri = default_redirect_uri; request_interval = 1. }

let validate_config config =
  let valid_name s = s <> "" && String.length s <= 128 && String.for_all (function
    'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' -> true | _ -> false) s in
  if not (valid_name config.username && valid_name config.client_id)
     || config.token_file = "" || not (Float.is_finite config.request_interval)
     || config.request_interval < 0. then raise (Error (Invalid_input "configuration"));
  let redirect = Uri.of_string config.redirect_uri in
  if Uri.scheme redirect <> Some "http" || Uri.host redirect <> Some "127.0.0.1"
     || Uri.userinfo redirect <> None || Uri.query redirect <> [] || Uri.fragment redirect <> None then
    raise (Error (Invalid_input "OAuth redirect must be a loopback HTTP URI"))
type group = Subreddit of string | Post of string | Notifications
type draft = Comment of { parent : string; text : string }
           | Text_post of { subreddit : string; title : string; text : string }
type receipt = Comment_created of M.message | Post_created of string
type operation =
  | Identity
  | List of group * string option
  | Search of group * string * string option
  | Thread of string
  | Vote of string * M.vote
  | Reply of string * string
  | Submit of string * string * string
  | Authorize
  | Complete_authorization of string
type result =
  | Identity_result of string
  | Page of M.page
  | Thread_batch of M.thread_batch
  | Voted of string * M.vote
  | Replied of M.message
  | Submitted of string
  | Authorization_url of string
  | Authorized
  | Backend_error of error

let form fields = Uri.encoded_of_query (List.map (fun (k,v) -> k,[v]) fields)
let with_lock mutex f =
  Eio.Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock mutex) f
let valid_header s = s <> "" && not (String.exists (function '\r' | '\n' -> true | _ -> false) s)
let cursor = function None -> [] | Some s ->
  if String.length s > 256 || not (String.for_all (function
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false) s) then
    raise (Error (Invalid_input "pagination cursor"));
  ["after",s]

module Make (Transport : Http.S) = struct
  type nonrec group = group
  type cursor = string
  type token = { bearer : string; expires : float; scopes : string list option }
  type t = { config : config; now : unit -> float; sleep : float -> unit;
             lock : Eio.Mutex.t; mutable token : token option;
             mutable next_request : float; mutable challenge : (string * float) option }
  let source = "reddit"

  let create ~config ~now ~sleep =
    validate_config config;
    { config; now; sleep; lock = Eio.Mutex.create (); token=None; next_request=0.; challenge=None }

  let user_agent t = "linux:camulus:0.1.0 (by /u/" ^ t.config.username ^ ")"
  let remember_token t json =
    let bearer = D.string "access_token" json and seconds = D.number (D.get "expires_in" json) in
    if not (valid_header bearer) || seconds <= 0. || not (Float.is_finite seconds)
       || String.lowercase_ascii (D.string "token_type" json) <> "bearer" then
      raise (Error (Authorization "token endpoint did not return a valid bearer token"));
    let scopes = Option.map (String.split_on_char ' ') (D.optional "scope" json) in
    let token = { bearer; expires=t.now () +. seconds; scopes } in
    t.token <- Some token; token

  let oauth t fields =
    let response = try Transport.request {
      Http.meth=Post; url="https://www.reddit.com/api/v1/access_token";
      headers=["Authorization", "Basic " ^ Base64.encode_exn (t.config.client_id ^ ":");
               "User-Agent",user_agent t; "Content-Type","application/x-www-form-urlencoded"];
      body=form fields }
    with Http.Transport_error _ -> raise (Error (Authorization "token exchange failed; retry later")) in
    if response.status <> 200 then raise (Error (Authorization (Printf.sprintf "token endpoint HTTP %d" response.status)));
    let json = try D.parse response.body with Error _ -> raise (Error (Authorization "invalid token response")) in
    if D.get "error" json <> `Null then raise (Error (Authorization "grant rejected; authorize again"));
    json

  let token t = match t.token with
    | Some token when token.expires > t.now () +. 60. -> token
    | _ ->
      let refresh = try Credentials.read t.config.token_file with Failure message ->
        raise (Error (Authorization message)) in
      oauth t ["grant_type","refresh_token"; "refresh_token",refresh] |> remember_token t

  let api t ~scope meth path fields = with_lock t.lock @@ fun () ->
    let fields = fields @ ["raw_json","1"; "api_type","json"] in
    let rec call refresh_retry read_retry =
      let access = token t in
      (match access.scopes with Some xs when not (List.mem scope xs || List.mem "*" xs) ->
         raise (Error (Missing_scope scope)) | _ -> ());
      let delay = t.next_request -. t.now () in
      if delay > 0. then t.sleep delay;
      t.next_request <- t.now () +. t.config.request_interval;
      let response =
        try Some (Transport.request {
          Http.meth; url="https://oauth.reddit.com" ^ path ^ (if meth=Get then "?" ^ form fields else "");
          headers=["Authorization","Bearer " ^ access.bearer; "User-Agent",user_agent t;
                   "Content-Type","application/x-www-form-urlencoded"];
          body=(if meth=Post then form fields else "") })
        with
        | Eio.Cancel.Cancelled _ as ex -> if meth=Post then raise (Error Uncertain_write) else raise ex
        | Http.Transport_error _ -> if meth=Post then raise (Error Uncertain_write) else None
      in
      match response with
      | None when read_retry -> t.sleep 0.25; call refresh_retry false
      | None -> raise (Error (Invalid_response "HTTPS transfer"))
      | Some r ->
        let header_float name = List.find_map (fun (k,v) ->
          if String.lowercase_ascii k = name then Float.of_string_opt v else None) r.headers in
        let safe_delay x = if Float.is_finite x then max 0. x else 60. in
        (match header_float "x-ratelimit-remaining", header_float "x-ratelimit-reset" with
         | Some n, Some reset when n < 1. ->
           t.next_request <- max t.next_request (t.now () +. safe_delay reset)
         | _ -> ());
        if r.status = 429 then begin
          let delay = Option.value (header_float "retry-after") ~default:60. |> safe_delay in
          t.next_request <- max t.next_request (t.now () +. delay);
          raise (Error (Rate_limited delay))
        end;
        if r.status = 401 && refresh_retry && meth=Get then
          (t.token <- None; call false read_retry)
        else if List.mem r.status [502;503;504] && meth=Get && read_retry then
          (t.sleep 0.25; call refresh_retry false)
        else if r.status >= 500 && meth=Post then raise (Error Uncertain_write)
        else if r.status < 200 || r.status >= 300 then raise (Error (Http_status r.status))
        else begin
          let json = try D.parse r.body with Error _ when meth=Post -> raise (Error Uncertain_write) in
          let errors = D.get "errors" (D.get "json" json) in
          (match errors with `Null | `List [] -> () | `List xs ->
             let codes = List.map (function `List (`String code::_) -> code | _ -> "UNKNOWN") xs in
             raise (Error (Rejected codes))
           | _ -> raise (Error (Invalid_response "API error envelope")));
          json
        end
    in call true true

  let list t group ~after =
    let fields = ["limit","25"] @ cursor after in
    match group with
    | Notifications ->
      let j = api t ~scope:"privatemessages" Get "/message/inbox.json" (("mark","false")::fields) in
      let notifications=D.notifications j in
      let messages=List.map(fun (n:M.notification) ->
        {M.id=n.target;thread_id=Some n.thread;parent_id=None;references=[];
         title=n.title;author="";body="";html=None;url="https://www.reddit.com/comments/"^String.sub n.thread 3 (String.length n.thread-3)^"/";
         link_url=None;created=n.created;unavailable=false;score=0;my_vote=M.No_vote;activity=Some n.event_id}) notifications in
      { M.messages; notifications; after=D.after j }
    | Subreddit name ->
      let name = D.require_subreddit name in
      let j = api t ~scope:"read" Get ("/r/" ^ name ^ "/new.json") fields in
      { M.messages=List.map (D.message ~root:None) (D.listing j); notifications=[]; after=D.after j }
    | Post id ->
      ignore (D.require_fullname "t3" id);
      if after <> None then raise (Error (Invalid_input "single post has no listing cursor"));
      let j = api t ~scope:"read" Get ("/by_id/" ^ id ^ ".json") [] in
      { M.messages=List.map (D.message ~root:(Some id)) (D.listing j); notifications=[]; after=None }

  let search t group ~query ~after = match group with
    | Notifications | Post _ -> raise (Error (Unsupported "search is scoped to a subreddit"))
    | Subreddit name ->
      let name = D.require_subreddit name in
      if String.trim query = "" then raise (Error (Invalid_input "empty search"));
      let j = api t ~scope:"read" Get ("/r/" ^ name ^ "/search.json")
        (["q",query;"restrict_sr","on";"sort","new";"limit","25"] @ cursor after) in
      { M.messages=List.map (D.message ~root:None) (D.listing j); notifications=[]; after=D.after j }

  (* A request at a time per session also enforces Reddit's morechildren limit.
     Every response is emitted immediately; no full-thread buffer is required. *)
  let thread t root ~emit =
    ignore (D.require_fullname "t3" root);
    let post = String.sub root 3 (String.length root-3) in
    let known = Hashtbl.create 256 and expanded = Hashtbl.create 128 in
    let pending = Queue.create () and unresolved = Hashtbl.create 128 in
    let pages = ref 0 in
    let next_page () =
      incr pages;
      if !pages > 200 then raise (Error (Invalid_response "comment pagination limit; partial results retained"))
    in
    let comment_page fields =
      next_page ();
      let j = api t ~scope:"read" Get ("/comments/" ^ post ^ ".json")
        (["sort","old";"limit","100"] @ fields) in
      match j with `List [posts;comments] ->
        let roots = D.listing posts in
        (match roots with [p] -> ignore (D.message ~root:(Some root) p) | _ ->
          raise (Error (Invalid_response "thread root")));
        roots @ D.listing comments
      | _ -> raise (Error (Invalid_response "comment tree"))
    in
    let gaps () =
      let xs = ref [] in
      Queue.iter (fun (parent,ids) -> xs := {M.parent; count=max 1 (List.length ids)} :: !xs) pending;
      Hashtbl.iter (fun id parent -> if not (Hashtbl.mem known id) then
        xs := {M.parent;count=1} :: !xs) unresolved;
      !xs
    in
    let ingest things =
      let changed = ref [] and count = ref 0 in
      let queue = Queue.create () in List.iter (fun x -> Queue.push x queue) things;
      while not (Queue.is_empty queue) do
        incr count;
        if !count > 10000 then raise (Error (Invalid_response "oversized comment page"));
        let thing = Queue.pop queue in
        let d = D.get "data" thing in
        match D.string "kind" thing with
        | "t1" | "t3" ->
          let m = D.message ~root:(Some root) thing in
          if Hashtbl.find_opt known m.id <> Some m then changed := m :: !changed;
          Hashtbl.replace known m.id m;
          Hashtbl.remove unresolved m.id;
          (match D.get "replies" d with `Null | `String "" -> ()
           | j -> List.iter (fun x -> Queue.push x queue) (D.listing j))
        | "more" ->
          let parent = D.require_thing (D.string "parent_id" d) in
          if D.fullname "t3" parent && parent <> root then raise (Error (Invalid_response "more outside thread"));
          let ids = D.items (D.get "children" d) |> List.map (fun j ->
            let id = D.text j in
            if not (D.alphanumeric id) then raise (Error (Invalid_response "more child id")); id) in
          Queue.push (parent,ids) pending
        | _ -> raise (Error (Invalid_response "comment kind"))
      done;
      emit { M.thread=root;messages=List.rev !changed;gaps=gaps ();complete=false }
    in
    ingest (comment_page []);
    while not (Queue.is_empty pending) do
      let parent,ids = Queue.pop pending in
      if ids = [] then begin
        let key = "continue:" ^ parent in
        if not (Hashtbl.mem expanded key) && D.fullname "t1" parent then begin
          Hashtbl.add expanded key ();
          ingest (comment_page ["comment",String.sub parent 3 (String.length parent-3);"context","0"])
        end else Hashtbl.replace unresolved key parent
      end else begin
        let ids = List.filter (fun id ->
          let key = "t1_" ^ id in
          if Hashtbl.mem known key || Hashtbl.mem expanded key then false
          else (Hashtbl.add expanded key (); true)) ids in
        let rec batches ids = match ids with [] -> () | _ ->
          let rec split n acc xs = match n,xs with
            | 0,_ | _,[] -> List.rev acc,xs
            | n,x::rest -> split (n-1) (x::acc) rest in
          let batch,rest = split 100 [] ids in
          List.iter (fun id -> Hashtbl.replace unresolved ("t1_" ^ id) parent) batch;
          next_page ();
          let j = api t ~scope:"read" Get "/api/morechildren.json"
            ["link_id",root;"children",String.concat "," batch;"sort","old"] in
          ingest (D.things j); batches rest
        in batches ids
      end
    done;
    Hashtbl.iter (fun _ (m:M.message) -> match m.parent_id with
      | Some parent when not (Hashtbl.mem known parent) -> Hashtbl.replace unresolved parent root
      | _ -> ()) known;
    let gaps = gaps () in
    emit {M.thread=root;messages=[];gaps;complete=(gaps=[])}

  let vote t ~id ~direction =
    ignore (D.require_thing id);
    ignore (api t ~scope:"vote" Post "/api/vote.json" ["id",id;"dir",string_of_int (vote_code direction)])

  let reply t ~parent ~text =
    ignore (D.require_thing parent);
    if String.trim text = "" then raise (Error (Invalid_input "empty reply"));
    let j = api t ~scope:"submit" Post "/api/comment.json" ["thing_id",parent;"text",text] in
    try match D.things j with
      | [thing] ->
        let m = D.message ~root:None thing in
        if m.parent_id <> Some parent then raise (Error Uncertain_write); m
      | _ -> raise (Error Uncertain_write)
    with Error _ -> raise (Error Uncertain_write)

  let submit t ~subreddit ~title ~text =
    ignore (D.require_subreddit subreddit);
    if String.trim title = "" || String.trim text = "" then raise (Error (Invalid_input "empty title or body"));
    let j = api t ~scope:"submit" Post "/api/submit.json"
      ["sr",subreddit;"kind","self";"title",title;"text",text;"sendreplies","true"] in
    try D.require_fullname "t3" (D.string "name" (D.action_data j)) with Error _ -> raise (Error Uncertain_write)

  let authorize t =
    let state = Credentials.random_state () in
    t.challenge <- Some (state,t.now () +. 600.);
    "https://www.reddit.com/api/v1/authorize?" ^ form [
      "client_id",t.config.client_id;"response_type","code";"state",state;
      "redirect_uri",t.config.redirect_uri;"duration","permanent";"scope",String.concat " " scopes]

  let complete_authorization t callback = with_lock t.lock @@ fun () ->
    let state,expires = match t.challenge with Some x -> x | None ->
      raise (Error (Authorization "start authorization first")) in
    if expires < t.now () then raise (Error (Authorization "authorization request expired"));
    let uri = Uri.of_string callback and redirect = Uri.of_string t.config.redirect_uri in
    let normalize_path p = if p = "/" then "" else p in
    if Uri.scheme uri <> Uri.scheme redirect || Uri.host uri <> Uri.host redirect
       || Uri.port uri <> Uri.port redirect || Uri.userinfo uri <> None
       || Uri.fragment uri <> None || normalize_path (Uri.path uri) <> normalize_path (Uri.path redirect) then
      raise (Error (Authorization "callback does not match redirect URI"));
    let single key = match List.filter (fun (k,_) -> k=key) (Uri.query uri) with
      | [_,[v]] -> v | _ -> "" in
    if single "state" <> state then raise (Error (Authorization "OAuth state mismatch"));
    let code = single "code" in
    if code = "" then raise (Error (Authorization "authorization declined or code missing"));
    t.challenge <- None;
    let j = oauth t ["grant_type","authorization_code";"code",code;"redirect_uri",t.config.redirect_uri] in
    let refresh = D.string "refresh_token" j in
    if refresh = "" then raise (Error (Authorization "no permanent grant returned"));
    let previous = t.token in
    (try
       ignore (remember_token t j);
       Credentials.save t.config.token_file refresh
     with ex -> t.token <- previous; raise ex)

  let execute t operation ~emit = match operation with
    | Identity ->
      let j = api t ~scope:"identity" Get "/api/v1/me" [] in
      let name = D.string "name" j in
      if name = "" then raise (Error (Invalid_response "identity"));
      emit (Identity_result name)
    | List (group,after) -> emit (Page (list t group ~after))
    | Search (group,query,after) -> emit (Page (search t group ~query ~after))
    | Thread root -> thread t root ~emit:(fun batch -> emit (Thread_batch batch))
    | Vote (id,direction) -> vote t ~id ~direction; emit (Voted (id,direction))
    | Reply (parent,text) -> emit (Replied (reply t ~parent ~text))
    | Submit (sr,title,text) -> emit (Submitted (submit t ~subreddit:sr ~title ~text))
    | Authorize -> emit (Authorization_url (authorize t))
    | Complete_authorization callback -> complete_authorization t callback; emit Authorized
end

module Live = Make(Curl_transport)
module Readable : Reader.Backend.S with type t = Live.t and type group = group
  and type cursor = string = Live
module Threading : Reader.Backend.Threading with type t = Live.t = Live
module Voting : Reader.Backend.Voting with type t = Live.t = Live
module Writing = struct
  type t = Live.t
  type nonrec draft = draft
  type nonrec receipt = receipt
  let send t = function
    | Comment {parent;text} -> Comment_created (Live.reply t ~parent ~text)
    | Text_post {subreddit;title;text} -> Post_created (Live.submit t ~subreddit ~title ~text)
end

let operation_of_json json =
  let s k = D.string k json in
  let after = D.optional "after" json in
  match s "op" with
  | "identity" -> Identity
  | "subreddit" -> List (Subreddit (s "name"),after)
  | "post" -> List (Post (s "id"),after)
  | "notifications" -> List (Notifications,after)
  | "search" -> Search (Subreddit (s "name"),s "query",after)
  | "thread" -> Thread (s "id")
  | "vote" ->
    let dir = match D.get "direction" json with
      | `Int 1 -> M.Upvote | `Int (-1) -> M.Downvote | `Int 0 -> M.No_vote
      | _ -> raise (Error (Invalid_input "vote direction must be -1, 0 or 1")) in
    Vote (s "id",dir)
  | "reply" -> Reply (s "parent",s "text")
  | "submit" -> Submit (s "name",s "title",s "text")
  | "authorize" -> Authorize
  | "authorize-complete" -> Complete_authorization (s "callback")
  | _ -> raise (Error (Unsupported "unknown request operation"))

let json_option = function None -> `Null | Some s -> `String s
let message_json (m:M.message) = `Assoc [
  "id",`String m.id; "thread_id",json_option m.thread_id; "parent_id",json_option m.parent_id;
  "title",`String m.title; "author",`String m.author; "body",`String m.body;
  "html",json_option m.html; "url",`String m.url; "link_url",json_option m.link_url;
  "created",`Float m.created; "unavailable",`Bool m.unavailable;
  "score",`Int m.score; "my_vote",`Int (vote_code m.my_vote)]
let result_json result =
  let messages xs = `List (List.map message_json xs) in
  let make kind fields = `Assoc (("kind",`String kind)::fields) in
  match result with
  | Identity_result name -> make "identity" ["name",`String name]
  | Page p -> make "page" ["messages",messages p.messages; "after",json_option p.after;
      "notifications",`List (List.map (fun (n:M.notification) -> `Assoc [
        "event_id",`String n.event_id;"thread",`String n.thread;"target",`String n.target;
        "title",`String n.title;"reason",`String n.reason;"created",`Float n.created]) p.notifications)]
  | Thread_batch b -> make "thread" ["thread",`String b.thread;"messages",messages b.messages;
      "complete",`Bool b.complete;"gaps",`List (List.map (fun (g:M.gap) ->
        `Assoc ["parent",`String g.parent;"count",`Int g.count]) b.gaps)]
  | Voted (id,dir) -> make "vote" ["id",`String id;"direction",`Int (vote_code dir)]
  | Replied m -> make "reply" ["message",message_json m]
  | Submitted id -> make "submit" ["id",`String id]
  | Authorization_url url -> make "authorize" ["url",`String url]
  | Authorized -> make "authorized" []
  | Backend_error e -> make "error" ["message",`String (Errors.to_string e);
      "uncertain_write",`Bool (e=Uncertain_write);
      "retry_after",(match e with Rate_limited seconds -> `Float seconds | _ -> `Null)]
