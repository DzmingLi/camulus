module R=Rss_backend
module D=R.Decode
let check b why=if not b then failwith why
let fails f = try f();false with _ -> true
let url="https://example.test/feed"
let rss ?(body="<p>Hello &amp; world</p>") () =
  "<rss version='2.0' xmlns:content='http://purl.org/rss/1.0/modules/content/'><channel><title>News</title><item><guid isPermaLink='false'>stable</guid><title>First</title><link>/posts/1</link><pubDate>Fri, 16 Oct 2026 10:30:00 +0800</pubDate><description>Short</description><content:encoded><![CDATA["^body^"]]></content:encoded><enclosure url='/file.bin' length='4' type='application/octet-stream'/></item></channel></rss>"
let atom = {|<feed xmlns="http://www.w3.org/2005/Atom" xml:base="https://example.test/root/"><title>Feed</title><author><name>Alice</name></author><entry xml:base="../posts/"><id>urn:entry:1</id><title>Atom</title><updated>2026-10-16T02:30:00Z</updated><link href="one"/><content type="xhtml"><div xmlns="http://www.w3.org/1999/xhtml"><p>Hello <a href="../about">about</a></p></div></content><link rel="enclosure" href="file.bin" length="4" type="application/octet-stream"/></entry><entry><id>urn:entry:2</id><title>Plain</title><content type="text">literal &lt;b&gt;plain&lt;/b&gt;</content><updated>2026-10-16T02:30:00Z</updated></entry></feed>|}
let () =
  let a=List.hd(D.parse ~url (rss())) in
  check (a.message.id="stable" && a.message.thread_id=None && a.message.parent_id=None) "RSS identity, no thread";
  check (a.message.url="https://example.test/posts/1") "relative permalink";
  check (a.message.html=Some "<p>Hello &amp; world</p>") "CDATA full content preferred";
  let updated=List.hd(D.parse ~url (rss ~body:"updated" ())) in
  check(a.message.id=updated.message.id && a.message.activity<>updated.message.activity) "stable identity, changed revision";
  let entries=D.parse ~url atom in let b=List.hd entries in
  check(b.message.author="Alice" && b.message.url="https://example.test/posts/one") "Atom inheritance and xml:base";
  check(b.message.created=a.message.created) "timezone-independent dates";
  check(b.message.html=Some "<div><p>Hello <a href=\"https://example.test/about\">about</a></p></div>") "XHTML structure and links";
  check((List.nth entries 1).message.body="literal <b>plain</b>" && (List.nth entries 1).message.html=None) "Atom plain text stays plain";
  check((List.hd b.enclosures).url="https://example.test/posts/file.bin") "Atom enclosure";
  let rdf={|<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://purl.org/rss/1.0/" xmlns:dc="http://purl.org/dc/elements/1.1/"><item rdf:about="https://example.test/one"><title>RDF</title><description>Body</description><dc:creator>Bob</dc:creator><dc:date>2026-10-16T02:30:00Z</dc:date></item></rdf:RDF>|} in
  check((List.hd(D.parse ~url rdf)).message.author="Bob") "RSS 1.0";
  List.iter(fun body -> check(fails(fun () -> ignore(D.parse ~url body))) "reject malformed or non-feed XML")
    ["<rss>";"<html/>";"<!DOCTYPE rss SYSTEM 'file:///etc/passwd'><rss><channel/></rss>";
     "<rss><channel/></rss><rss/>";"<rss><channel>"^String.concat "" (List.init 130(fun _ -> "<x>"))];
  List.iter(fun u -> check(fails(fun () -> ignore(R.source_of_url u))) "URL scheme/credentials rejected")
    ["file:///etc/passwd";"https://user:password@example.test/feed";"https:///" ];
  check(D.date "invalid"=0.) "missing or bad dates have deterministic epoch";
  let queue=ref [] and requests=ref [] in
  let module H=struct let request r = requests:=r::!requests;match !queue with
    | x::xs -> queue:=xs;x | [] -> failwith "Unexpected network call" end in
  let module B=R.Make(H) in
  let response ?(headers=[]) status body : R.Http.response={status;headers;body} in
  Eio_posix.run(fun _ ->
    let file=Filename.temp_file "rss-subscriptions" ".json" in
    Sys.remove file;
    let subscribed=B.create ~subscriptions_file:file () in
    check(not(B.Subscription.get subscribed url)) "initial RSS following state";
    B.Subscription.set subscribed url true;
    let reopened=B.create ~subscriptions_file:file () in
    check(B.Subscription.get reopened url) "RSS subscriptions survive backend restart without SQLite";
    B.Subscription.set reopened url false;
    check(not(B.Subscription.get subscribed url)) "RSS accepts outside following changes";
    check(!requests=[]) "following an RSS feed does not fetch content";
    Sys.remove file;Sys.remove(file^".lock");
    let backend=B.create () in
    queue:=[response ~headers:["location","/actual"] 302 "";response ~headers:["etag","\"v1\""] 200 (rss())];
    let p=B.list backend url ~after:None in
    check(List.length p.messages=1 && p.after=None) "redirect list";
    queue:=[response ~headers:["location","/actual"] 302 "";response 304 ""];
    check((B.list backend url ~after:None).messages=p.messages) "304 keeps cached entries";
    check(List.assoc_opt "If-None-Match" (List.hd !requests).headers=Some "\"v1\"") "conditional request";
    check((B.read backend url "stable").message.id="stable") "read without another fetch";
    queue:=[response ~headers:["location","/actual"] 302 "";response 304 ""];
    check(List.length (B.search backend url ~query:"HELLO" ~after:None).messages=1) "feed-scoped search";
    queue:=[response 200 "A\000BC"];
    let article,files=B.archive backend url "stable" in
    check(files=[0,"A\000BC"]) "archive enclosure bytes";
    let path=Filename.temp_file "rss-archive" ".sqlite" in
    Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
      Reader.Content_state.with_db path (fun db ->
        let module S=Reader.Content_state in let source=R.source_of_url url in
        S.observe db ~source ~group:(Some url) p.messages;
        let rows=function `List xs -> xs | _ -> assert false in
        let first=List.hd(rows(S.feed db ~source ~group:url)) in
        let revision=Yojson.Safe.Util.(first |> member "revision" |> to_int) in
        S.dismiss db ~source ~group:url ~id:"stable" ~through:revision;
        S.observe db ~source ~group:(Some url) p.messages;
        check(rows(S.feed db ~source ~group:url)=[]) "refresh cannot resurrect dismissed same revision";
        S.observe db ~source ~group:(Some url) [updated.message];
        check(List.length(rows(S.feed db ~source ~group:url))=1) "changed content can reappear";
        S.save_archive db ~source ~target:S.Article ~id:"stable" ~folder:"rss/keep"
          [{S.message=article.message;attachments=D.attachments article;raw=None;files}];
        check(S.attachment_archive db ~source ~id:"stable" ~part:0=Some "A\000BC") "durable binary enclosure";
        S.remove_folder db "rss";
        check(S.attachment_archive db ~source ~id:"stable" ~part:0=None) "last folder removal prunes blobs"));
    queue:=[response ~headers:["location",url] 302 ""];
    check(fails(fun () -> ignore(B.list backend url ~after:None))) "redirect loop rejected";
    queue:=[response ~headers:["location","file:///etc/passwd"] 302 ""];
    check(fails(fun () -> ignore(B.list backend url ~after:None))) "redirect scheme validated";
    queue:=[response 200 "<broken>"];
    check(fails(fun () -> ignore(B.list backend url ~after:None))) "bad refresh fails";
    check((B.read backend url "stable").message.id="stable") "bad refresh preserves cache");
  print_endline "PASS: RSS/Atom parsing, namespaces, dates, identity, conditional GET, search, archives and enclosures"
