module R = Rss_backend
module C = Reader.Content
module I = Jsonfeed.Item
module A = Jsonfeed.Attachment
let result = function Ok value -> value | Error message -> failwith message
let text = Option.value ~default:""
let resolve base value = if value="" then "" else R.Http.validate (R.Http.resolve base value)
let authors values = Option.value values ~default:[] |> List.filter_map (fun a ->
  List.find_opt ((<>) "") [text (Jsonfeed.Author.name a);text (Jsonfeed.Author.url a)])

let parse ~url:base input =
  let json = result (Jsont_bytesrw.decode_string Jsont.json input) in
  (* jsonfeed 1.1.0 normalizes the version during decoding: check the input first. *)
  if result (Jsont.Json.decode (Jsont.mem "version" Jsont.string) json)
     <> "https://jsonfeed.org/version/1.1" then failwith "Only JSON Feed 1.1 is supported";
  let feed = result (Jsont.Json.decode Jsonfeed.jsont json) in
  (match Jsonfeed.validate feed with Ok () -> () | Error errors -> failwith (String.concat "; " errors));
  let items = Jsonfeed.items feed in
  if List.length items>10000 then failwith "JSON Feed has too many items";
  let inherited = authors (Jsonfeed.authors feed) in
  let articles = List.map (fun item ->
    let id=I.id item in
    if String.contains id '\000' then failwith "JSON Feed item ID contains NUL";
    let text_body=text (I.content_text item) and html_body=text (I.content_html item) in
    let permalink=match text (I.url item) with "" -> base | value -> resolve base value in
    let html=if html_body="" then None else
      Reader.Html.parse html_body |> Reader.Html.clean ~base:permalink |> Option.map Reader.Html.render in
    let body=if Option.is_some html then "" else text_body in
    let author=String.concat ", " (match authors (I.authors item) with [] -> inherited | xs -> xs) in
    let title=match text (I.title item) with "" ->
      let summary=match text (I.summary item) with "" ->
        if text_body<>"" then text_body else Reader.Html.text (Reader.Html.parse html_body) | x -> x in
      (match String.split_on_char '\n' summary with ""::_ | [] -> "Untitled" | first::_ -> first) | x -> x in
    let attachments=Option.value (I.attachments item) ~default:[] in
    if List.length attachments>1000 then failwith "JSON Feed has too many attachments";
    let enclosures=List.mapi (fun part a ->
      let url=resolve base (A.url a) in
      if url="" then failwith "JSON Feed attachment has no URL";
      let name=match text (A.title a) with "" -> Filename.basename (Uri.path (Uri.of_string url)) |> Uri.pct_decode | x -> x in
      let size=match A.size_in_bytes a with
        | None -> 0
        | Some n when n>=0L && n<=Int64.of_int max_int -> Int64.to_int n
        | Some _ -> failwith "JSON Feed attachment size is out of range" in
      {R.Decode.part;url;name;size;mime=A.mime_type a}) attachments in
    let created=match I.date_published item, I.date_modified item with
      | Some t,_ | None,Some t -> Ptime.to_float_s t | None,None -> 0. in
    let activity=Jsont_bytesrw.encode_string I.jsont item |> result |> Digest.string |> Digest.to_hex in
    {R.Decode.message={C.id;thread_id=None;parent_id=None;references=[];title;author;body;html;
      url=permalink;link_url=Option.map (resolve base) (I.external_url item);created;
      unavailable=false;score=0;my_vote=No_vote;activity=Some activity};enclosures}) items in
  let next=match text (Jsonfeed.next_url feed) with "" -> None | value -> Some (resolve base value) in
  articles,next

let source = "jsonfeed"
let is_source = String.starts_with ~prefix:"jsonfeed:"
let source_of_url url = "jsonfeed:" ^ R.Http.validate url
let url_of_source source =
  if not(is_source source) then invalid_arg "Not a JSON Feed source";
  String.sub source 9 (String.length source-9) |> R.Http.validate
module Make(H:R.Http.S) = struct
  module Feed = R.Make(H)
  type t = { feed:Feed.t; lock:Eio.Mutex.t; next:(string,string option) Hashtbl.t;
    articles:((string*string),R.Decode.article) Hashtbl.t }
  type group = string
  type cursor = string
  let source=source
  let create () =
    let next=Hashtbl.create 16 in
    let decode ~url body=let articles,after=parse ~url body in Hashtbl.replace next url after;articles in
    {feed=Feed.create ~decode ();lock=Eio.Mutex.create ();next;articles=Hashtbl.create 64}
  let cursor root = function
    | None -> root,[]
    | Some value -> match Yojson.Safe.from_string value with
      | `List[`String scope; `String page; `List visited] when scope=root ->
        let visited=List.map(function `String s->s | _->invalid_arg "Invalid JSON Feed cursor")visited in
        if List.length visited>=256 then failwith "JSON Feed pagination limit exceeded";
        R.Http.validate page,visited
      | _ -> invalid_arg "JSON Feed cursor belongs to another feed"
  let list t root ~after =
    ignore(R.Http.validate root);
    let page_url,visited=cursor root after in
    Eio.Mutex.lock t.lock;
    Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.lock) (fun () ->
      if List.mem page_url visited then failwith "JSON Feed pagination loop";
      let endpoint,articles=Feed.fetch_articles t.feed page_url in
      let visited=List.sort_uniq String.compare (page_url::endpoint::visited) in
      List.iter(fun (a:R.Decode.article)->Hashtbl.replace t.articles (root,a.message.id) a) articles;
      let next=Option.join(Hashtbl.find_opt t.next endpoint) in
      let after=Option.map(fun next ->
        if List.mem next visited then failwith "JSON Feed pagination loop";
        Yojson.Safe.to_string (`List[`String root;
          `String next;`List(List.map(fun s->`String s)visited)])) next in
      {C.messages=List.map(fun(a:R.Decode.article)->a.message) articles;after})
  (* Saved items on later pages must be revalidated during archive refresh. *)
  let refresh t root =
    let page=list t root ~after:None in
    let fresh=List.map (fun (m:C.message)->m.id) page.messages in
    Hashtbl.filter_map_inplace (fun (feed,id) article ->
      if feed=root && not(List.mem id fresh) then None else Some article) t.articles;
    page
  let read t root id =
    let rec find after =
      let p=list t root ~after in
      match Hashtbl.find_opt t.articles(root,id),p.after with
      | Some a,_->a | None,Some next->find(Some next)
      | None,None->failwith "Article is no longer in this JSON Feed; use its saved copy" in
    match Hashtbl.find_opt t.articles(root,id) with Some a->a | None->find None
  let archive t root id = R.Article.archive ~download:Feed.download_url (read t root id)
  let attachment t root id part destination = R.Article.attachment ~download:Feed.download_url (read t root id) part destination
end
module Live = Make(R.Transport)
