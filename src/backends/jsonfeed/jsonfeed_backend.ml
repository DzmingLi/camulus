module Decode = Decode
module R = Rss_backend
module C = Reader.Content
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
  let create ?subscriptions_file () =
    let next=Hashtbl.create 16 in
    let decode ~url body=let articles,after=Decode.parse ~url body in Hashtbl.replace next url after;articles in
    {feed=Feed.create ?subscriptions_file ~decode ();lock=Eio.Mutex.create ();next;articles=Hashtbl.create 64}
  let cursor root query = function
    | None -> root,[]
    | Some value -> match Yojson.Safe.from_string value with
      | `List[`String scope; q; `String page; `List visited]
        when scope=root && q=(match query with None->`Null|Some q->`String q) ->
        let visited=List.map(function `String s->s | _->invalid_arg "Invalid JSON Feed cursor")visited in
        if List.length visited>=256 then failwith "JSON Feed pagination limit exceeded";
        R.Http.validate page,visited
      | _ -> invalid_arg "JSON Feed cursor belongs to another feed or search"
  let page t root query after =
    ignore(R.Http.validate root);
    let page_url,visited=cursor root query after in
    Eio.Mutex.lock t.lock;
    Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.lock) (fun () ->
      if List.mem page_url visited then failwith "JSON Feed pagination loop";
      let endpoint,articles=Feed.fetch_articles t.feed page_url in
      let visited=List.sort_uniq String.compare (page_url::endpoint::visited) in
      List.iter(fun (a:R.Decode.article)->Hashtbl.replace t.articles (root,a.message.id) a) articles;
      let next=Option.join(Hashtbl.find_opt t.next endpoint) in
      let after=Option.map(fun next ->
        if List.mem next visited then failwith "JSON Feed pagination loop";
        Yojson.Safe.to_string (`List[`String root;(match query with None->`Null|Some q->`String q);
          `String next;`List(List.map(fun s->`String s)visited)])) next in
      let contains text query =
        let text=String.lowercase_ascii text and query=String.lowercase_ascii query in
        let rec loop i = i+String.length query<=String.length text &&
          (String.sub text i (String.length query)=query || loop(i+1)) in loop 0 in
      let articles=match query with None->articles | Some query->List.filter(fun(a:R.Decode.article)->
        contains (a.message.title^" "^a.message.body^" "^Option.value a.message.html ~default:"") query) articles in
      {C.messages=List.map(fun(a:R.Decode.article)->a.message) articles;notifications=[];after})
  let list t root ~after = page t root None after
  let search t root ~query ~after = page t root (Some query) after
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
  let archive t root id = R.Article.archive ~download:(fun ~limit url -> Feed.download_url ~limit url) (read t root id)
  let attachment t root id part destination = R.Article.attachment ~download:(fun ~limit url -> Feed.download_url ~limit url) (read t root id) part destination
  module Subscription = struct
    type nonrec t=t
    type target=string
    type settings=bool
    let get t = Feed.Subscription.get t.feed
    let set t = Feed.Subscription.set t.feed
    let followed t = Feed.followed t.feed
  end
end
module Live = Make(R.Curl_transport)
module Subscription = Live.Subscription
let followed = Subscription.followed
