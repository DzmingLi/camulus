module R = Rss_backend
module C = Reader.Content
module D = R.Decode
let field key = function `Assoc xs -> Option.value (List.assoc_opt key xs) ~default:`Null | _ -> `Null
let string = function `String s -> s | _ -> ""
let text key j = string (field key j)
let array = function `List xs -> xs | _ -> []
let nonempty s = s<>"" && not(String.contains s '\000')
let authors j =
  let values=match field "authors" j with `List xs -> xs | _ ->
    match field "author" j with `Assoc _ as a -> [a] | _ -> [] in
  List.filter_map (fun a -> List.find_opt ((<>) "") [text "name" a;text "url" a]) values
let date s = match Ptime.of_rfc3339 s with Ok(t,_,_) -> Ptime.to_float_s t | _ -> 0.
let url base value = if value="" then "" else R.Http.validate (R.Http.resolve base value)
let parse ~url:base input =
  let j=Yojson.Safe.from_string input in
  if not(List.mem (text "version" j) ["https://jsonfeed.org/version/1";"https://jsonfeed.org/version/1.1"]) then
    failwith "Unsupported JSON Feed version";
  if text "title" j="" then failwith "JSON Feed title is missing";
  let items=match field "items" j with `List xs -> xs | _ -> failwith "JSON Feed items must be an array" in
  if List.length items>10000 then failwith "JSON Feed has too many items";
  let inherited=authors j and seen=Hashtbl.create 32 in
  let articles=List.filter_map (fun item ->
    let id=match field "id" item with `String s->s | `Int n->string_of_int n | `Intlit s->s
      | `Float f when Float.is_finite f -> string_of_float f | _->"" in
    if not(nonempty id) || Hashtbl.mem seen id then None else (
      Hashtbl.add seen id ();
      let text_body=text "content_text" item and html_body=text "content_html" item in
      if text_body="" && html_body="" then None else
      let permalink=match text "url" item with "" -> base | value->url base value in
      let html=if html_body="" then None else
        Reader.Html.parse html_body |> Reader.Html.clean ~base:permalink |> Option.map Reader.Html.render in
      let body=if Option.is_some html then "" else text_body in
      let author=String.concat ", " (match authors item with []->inherited | xs->xs) in
      let title=match text "title" item with "" ->
        let summary=match text "summary" item with "" ->
          if text_body<>"" then text_body else Reader.Html.text (Reader.Html.parse html_body) | x->x in
        (match String.split_on_char '\n' summary with first::_->first | []->"Untitled") | x->x in
      let attachment_values=array(field "attachments" item) in
      if List.length attachment_values>1000 then failwith "JSON Feed has too many attachments";
      let enclosures=List.mapi (fun part a ->
        let attachment_url=url base (text "url" a) in
        if attachment_url="" then failwith "JSON Feed attachment has no URL";
        let name=match text "title" a with ""->Filename.basename(Uri.path(Uri.of_string attachment_url)) |> Uri.pct_decode | x->x in
        let size=match field "size_in_bytes" a with `Int n when n>=0->n | _->0 in
        {D.part;url=attachment_url;name;size;mime=text "mime_type" a}) attachment_values in
      let published=date(text "date_published" item) in
      let created=if published>0. then published else date(text "date_modified" item) in
      let activity=Digest.to_hex(Digest.string(Yojson.Safe.to_string (`List[
        `String title;`String text_body;`String html_body;field "date_modified" item;field "attachments" item]))) in
      Some {D.message={C.id;thread_id=None;parent_id=None;references=[];title;author;body;html;
        url=permalink;link_url=(match text "external_url" item with ""->None | x->Some(url base x));created;
        unavailable=false;score=0;my_vote=No_vote;activity=Some activity};enclosures})) items in
  let next=match text "next_url" j with ""->None | x->Some(url base x) in
  articles,next
