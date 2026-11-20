module R = Rss_backend
module C = Reader.Content
module S = Reader.Content_state
module Html = Reader.Html
module Json = Yojson.Safe.Util

let check condition message = if not condition then failwith message
let source = Atlantic_backend.source
let group = "author.fixture"
let id = "tag:example.org,2026:article"
let cover_url = "https://cdn.example.org/cover.jpg"
let first_image = "\255\216\255first JPEG"
let second_image = "\255\216\255next JPEG"
let cover_data image = Option.get (Html.image_data_uri image)
let feed ~date ~text =
  Printf.sprintf {|<feed xmlns="http://www.w3.org/2005/Atom" xmlns:media="http://search.yahoo.com/mrss/">
  <id>https://example.org/</id><title>The Atlantic</title><updated>%s</updated><author><name>Fixture</name></author>
  <entry>
  <id>%s</id><title>%s</title><published>2026-10-10T10:00:00Z</published><updated>%s</updated>
  <link href="https://example.org/article"/>
  <content type="html">&lt;img src="%s"&gt;&lt;p&gt;%s&lt;/p&gt;</content>
  <media:content url="%s" medium="image"/>
  </entry></feed>|} date id text date cover_url text cover_url

let feed_text = ref (feed ~date:"2026-10-10T10:00:00Z" ~text:"First revision")
let image_response = ref (200,first_image)
let image_requests = ref 0
module B = Atlantic_backend.Make(struct
  let request (r:R.Http.request) =
    let status,body=if r.url=cover_url then (incr image_requests;!image_response)
      else if r.url=Atlantic_backend.feed_url group then 200,!feed_text
      else failwith ("Unexpected request to " ^ r.url) in
    {R.Http.status;headers=[];body}
end)

let with_database f =
  let path=Filename.temp_file "camulus-atlantic" ".sqlite3" in
  let directory=path^".html" in
  Fun.protect ~finally:(fun () ->
    if Sys.file_exists directory then begin
      Array.iter (fun name -> Sys.remove (Filename.concat directory name)) (Sys.readdir directory);
      Sys.rmdir directory
    end;
    Sys.remove path) (fun () -> f path directory)
let file directory =
  match Sys.readdir directory with
  | [|name|] when Filename.check_suffix name ".html" -> Filename.concat directory name
  | _ -> failwith "Expected exactly one HTML file"
let assert_article path expected_text expected_image =
  let bytes=In_channel.with_open_bin path In_channel.input_all in
  check (String.starts_with ~prefix:"<!doctype html>" bytes) "Not a standalone HTML document";
  let tree=Html.parse bytes in
  check (Html.find (fun n -> Html.tag n="p" && Html.text n=expected_text) tree <> None)
    "Article did not contain the requested revision";
  match Html.select (fun n -> Html.tag n="img") tree with
  | [img] -> check (Html.attr "src" img=cover_data expected_image) "Expected the embedded cover"
  | _ -> failwith "Expected exactly one cover image"

let check_states db =
  let state=S.snapshots db ~source [id] |> Json.member "items" |> Json.to_list |> List.hd in
  check (Json.member "read" state=`Bool true) "Lost read state";
  check (Json.member "favorite" state=`Bool true) "Lost favorite state"

let refresh_and_reopen () = with_database (fun path directory ->
  let browser=B.create () in
  let refresh () =
    let previous id=S.with_db path (fun db -> S.local_article db ~source ~id) in
    let articles=B.local_articles browser group ~after:None ~previous in
    S.with_db path (fun db ->
      S.cache_page_articles db ~source (List.map R.article_json articles);
      S.observe db ~source ~group:(Some group) (List.map (fun a -> a.R.Decode.message) articles)) in
  refresh ();
  let html=file directory in
  assert_article html "First revision" first_image;
  S.with_db path (fun db ->
    S.mark_read db ~source [id] true;
    let article=Option.get (S.local_article db ~source ~id) |> R.article_of_json in
    let archive=B.archive_local article in
    check (archive.files=[0,first_image]) "Archiving could not reuse the cover";
    S.save_archive db ~source ~target:S.Article ~id ~folder:"saved" [archive]);
  let inode=(Unix.stat html).st_ino in
  refresh ();
  check (!image_requests=1 && (Unix.stat html).st_ino=inode) "Unchanged revision fetched or rewrote the cover";
  feed_text := feed ~date:"2026-10-11T10:00:00Z" ~text:"Second revision";
  image_response := 503,"";
  refresh ();
  assert_article html "Second revision" first_image;
  image_response := 200,second_image;
  refresh ();
  check (file directory=html) "A revision created another article file";
  assert_article html "Second revision" second_image;
  check (!image_requests=3) "Failed cover was not retried";

  S.with_db path (fun db ->
    check_states db;
    let articles,complete=S.cached_content db ~source ~target:S.Article ~id in
    check complete "Missing offline article";
    let article=List.hd articles |> R.article_of_json in
    check ((B.archive_local article).files=[0,second_image]) "Offline cover did not use revised bytes";
    check (S.attachment_archive db ~source ~id ~part:0=Some second_image) "Favorite served an old cover";
    check (S.load_archive db ~source ~target:S.Article ~id |> Json.to_list=articles)
      "Favorite did not render the updated local HTML"))

let migrate_legacy_article () = with_database (fun path directory ->
  let legacy=feed ~date:"2026-10-10T10:00:00Z" ~text:"Legacy article" in
  let article=Atlantic_backend.decode ~url:(Atlantic_backend.feed_url group) legacy |> List.hd in
  S.with_db path (fun db ->
    S.observe db ~source ~group:(Some group) [article.message];
    S.mark_read db ~source [id] true);
  let db=Sqlite3.db_open path in
  Fun.protect ~finally:(fun () -> ignore(Sqlite3.db_close db)) (fun () ->
    let run sql values=
      let stmt=Sqlite3.prepare db sql in
      Fun.protect ~finally:(fun () -> ignore(Sqlite3.finalize stmt)) (fun () ->
        check (Sqlite3.bind_values stmt values=Sqlite3.Rc.OK && Sqlite3.step stmt=Sqlite3.Rc.DONE)
          "Could not create a v6 fixture") in
    let text s=Sqlite3.Data.TEXT s in
    run "PRAGMA user_version=6" [];
    run "INSERT INTO archives(source,id,payload,resources_complete) VALUES(?,?,?,1)"
      [text source;text id;text(Yojson.Safe.to_string (R.article_json article))];
    run "INSERT INTO archive_files VALUES(?,?,0,?)" [text source;text id;Sqlite3.Data.BLOB second_image];
    run "INSERT INTO folders VALUES('saved','')" [];
    run "INSERT INTO favorites VALUES('saved',?,'article',?)" [text source;text id]);
  S.with_db path check_states;
  assert_article (file directory) "Legacy article" second_image)

let () = Eio_posix.run (fun _ ->
  refresh_and_reopen ();
  migrate_legacy_article ();
  print_endline "The Atlantic inbox tests passed")
