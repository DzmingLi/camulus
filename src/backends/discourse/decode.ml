module C = Reader.Content
let field k = function `Assoc xs -> Option.value(List.assoc_opt k xs) ~default:`Null | _ -> `Null
let string = function `String s -> s | _ -> ""
let str k j = string(field k j)
let integer = function `Int n -> n | _ -> 0
let num k j = integer(field k j)
let array = function `List xs -> xs | _ -> failwith "Discourse response is missing an array"
let list k j = array(field k j)
let optional_list k j = match field k j with `Null -> [] | x -> array x
let positive n = if n<=0 then invalid_arg "Expected positive Discourse ID";n
let number s =
  if s="" || String.exists (fun c -> c<'0' || c>'9') s then invalid_arg "Invalid Discourse ID";
  positive(int_of_string s)
let site url =
  ignore(Http.validate url);
  let u=Uri.of_string url in
  if Uri.query u<>[] || Uri.fragment u<>None then invalid_arg "Use a forum base URL without query or fragment";
  let path=Uri.path u in
  let parts=String.split_on_char '/' path |> List.filter((<>) "") in
  if List.exists(fun p -> p="." || p=".." || String.exists(fun c -> not((c>='a' && c<='z') || (c>='A' && c<='Z') || (c>='0' && c<='9') || c='-' || c='_')) p) parts then
    invalid_arg "Invalid forum mount path";
  let u=Uri.with_host u (Option.map String.lowercase_ascii (Uri.host u)) in
  let u=if Uri.scheme u=Some "https" && Uri.port u=Some 443 then Uri.with_port u None else u in
  Uri.with_path u (if parts=[] then "" else "/"^String.concat "/" parts) |> Uri.to_string
let topic_id n = "topic:"^string_of_int(positive n)
let article_id topic floor = topic_id topic ^ "/post:" ^ string_of_int(positive floor)
let parse_id id = match String.split_on_char '/' id with
  | [t] when String.starts_with ~prefix:"topic:" t -> number(String.sub t 6 (String.length t-6)),None
  | [t;p] when String.starts_with ~prefix:"topic:" t && String.starts_with ~prefix:"post:" p ->
    number(String.sub t 6 (String.length t-6)),Some(number(String.sub p 5 (String.length p-5)))
  | _ -> invalid_arg "Expected topic:ID or topic:ID/post:FLOOR"
let date s = match Ptime.of_rfc3339 s with Ok(t,_,_) -> Ptime.to_float_s t | Error _ -> 0.
let base_message ~site ~topic ~floor ~title ~author ~created =
  {C.id=article_id topic floor;thread_id=Some(topic_id topic);parent_id=(if floor=1 then None else Some(article_id topic 1));references=[];
   title;author;body="";html=None;url=Printf.sprintf "%s/t/%d/%d" site topic floor;link_url=None;created;unavailable=false;score=0;my_vote=C.No_vote;activity=None}
let summary ~site j =
  let topic=positive(num "id" j) in
  let title=str "title" j in if title="" then failwith "Discourse topic title is missing";
  let stamp=match str "last_posted_at" j with "" -> str "created_at" j | s -> s in
  { (base_message ~site ~topic ~floor:1 ~title ~author:(str "last_poster_username" j) ~created:(date stamp)) with
    activity=Some(Printf.sprintf "%d:%d:%s" (num "highest_post_number" j) (num "posts_count" j) stamp) }
type attachment = {part:int;url:string;name:string;size:int}
type article = {message:C.message;post_id:int;floor:int;liked:bool;attachments:attachment list}
let attachments ~site cooked =
  let found=ref [] in
  let add url name =
    if url<>"" then begin
      let url=Http.resolve (site^"/") url in
      ignore(Http.validate url);
      if not(List.exists(fun (u,_) -> u=url) !found) then found := !found@[url,name]
    end in
  let open Markup in
  string cooked |> parse_html |> signals |> iter(function
    | `Start_element ((_,tag),attrs) ->
      let get name=Option.value(List.assoc_opt ("",name) attrs) ~default:"" in
      let classes=String.split_on_char ' ' (get "class") in
      if tag="a" && (List.mem "attachment" classes || List.mem "lightbox" classes) then add (get "href") (get "data-original-filename")
      else if tag="img" && (List.mem "upload" classes || String.starts_with ~prefix:"/uploads/" (Uri.path(Uri.of_string(get "src"))) || get "data-base62-sha1"<>"") then
        add (get "src") (get "alt")
      else if (tag="audio" || tag="video" || tag="source") && get "src"<>"" then add (get "src") ""
    | _ -> ());
  if List.length !found>1000 then failwith "Too many Discourse attachments";
  List.mapi(fun part (url,name) ->
    let name=if name<>"" then name else Uri.path(Uri.of_string url) |> Filename.basename |> Uri.pct_decode in
    {part;url;name=(if name="" || name="." || name="/" then "attachment" else name);size=0}) !found
let post ~site ~topic ~title j =
  if num "topic_id" j<>topic then failwith "Discourse returned a post from another topic";
  let post_id=positive(num "id" j) and floor=positive(num "post_number" j) in
  let parent=num "reply_to_post_number" j in
  if parent<0 || parent>=floor then failwith "Invalid Discourse reply relationship";
  let cooked=match field "cooked" j with `String s -> s | _ -> failwith "Discourse post body is missing" in
  let actions=optional_list "actions_summary" j in
  let likes=List.find_opt(fun a -> num "id" a=2) actions in
  let liked=Option.fold ~none:false ~some:(fun a -> field "acted" a=`Bool true) likes in
  let score=Option.fold ~none:0 ~some:(num "count") likes in
  let message={ (base_message ~site ~topic ~floor ~title ~author:(str "username" j) ~created:(date(str "created_at" j))) with
    html=Some cooked;parent_id=(if floor=1 then None else Some(article_id topic (if parent=0 then 1 else parent)));
    unavailable=(field "cooked_hidden" j=`Bool true);score;
    activity=Some(string_of_int post_id^":"^str "updated_at" j^":"^Digest.to_hex(Digest.string cooked))} in
  {message;post_id;floor;liked;attachments=attachments ~site cooked}
let attachment_json a = `Assoc["part",`Int a.part;"url",`String a.url;"name",`String a.name;"size",`Int a.size]
let attachments_json a = `List(List.map attachment_json a.attachments)
let article_json a =
  let message=match Reader.Content_state.message_json a.message with
    | `Assoc fields -> `Assoc(("liked",`Bool a.liked)::fields) | x -> x in
  `Assoc["kind",`String "article";"message",message;"attachments",attachments_json a]
