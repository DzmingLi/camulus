module H=Reader.Http
let check b why=if not b then failwith why
let rejected f=try ignore(f());false with Invalid_argument _|H.Transport_error _->true
let ()=
 check(rejected(fun()->H.validate ~policy:H.Https_only "http://localhost/a"))"HTTPS policy";
 check(H.validate ~policy:H.Https_or_loopback "http://127.0.0.1/a"<>"")"loopback test policy";
 List.iter(fun u->check(rejected(fun()->H.validate ~policy:H.Https_or_loopback u))"reject unsafe URL")
  ["http://example.test/a";"https://user:secret@example.test/a";"file:///etc/passwd";"https://example.test/a\r\nX-Test: a"];
 Eio_posix.run(fun env->Eio.Switch.run(fun sw->
  let socket=Eio.Net.listen ~sw ~backlog:10(Eio.Stdenv.net env)(`Tcp(Eio.Net.Ipaddr.V4.loopback,0))in
  let port=match Eio.Net.listening_addr socket with `Tcp(_,p)->p|_->assert false in
  let calls=Hashtbl.create 8 in
  let handle flow _=
   let input=Eio.Buf_read.of_flow flow ~max_size:8192 in
   let method_,path=match String.split_on_char ' '(Eio.Buf_read.line input)with meth::path::_->meth,path|_->failwith "Invalid fixture request"in
   let rec headers acc=match Eio.Buf_read.line input with ""->acc|line->
    let n=String.index line ':'in headers((String.lowercase_ascii(String.sub line 0 n),String.trim(String.sub line(n+1)(String.length line-n-1)))::acc)in
   let headers=headers[]in
   let length=Option.fold ~none:0 ~some:int_of_string(List.assoc_opt "content-length"headers) in
   let body=Eio.Buf_read.take length input in
   Hashtbl.replace calls path(1+Option.value(Hashtbl.find_opt calls path)~default:0);
   let status,extra,body=match path with
    |"/redirect"->302,"Location: /target\r\n","redirect"
    |"/echo"->200,"",method_^":"^body
    |"/large"->200,"","1234"
    |"/headers"->200,"X-Filler: "^String.make 70000 'x'^"\r\n","headers"
    |"/slow"->Eio.Time.Mono.sleep(Eio.Stdenv.mono_clock env)0.3;200,"","late"
    |_->200,"","ok"in
   Eio.Flow.copy_string(Printf.sprintf "HTTP/1.1 %d Fixture\r\n%sContent-Length: %d\r\nConnection: close\r\n\r\n%s" status extra(String.length body)body)flow in
  Eio.Fiber.first
   (fun()->while true do Eio.Net.accept_fork ~sw socket ~on_error:(fun _->())handle done)
   (fun()->
    let url path=Printf.sprintf "http://127.0.0.1:%d%s" port path in
    let get ?(limit=4096)path=H.request ~timeout_ms:1000(H.get ~limit(url path))in
    check((get "/ok").body="ok")"loopback HTTP";
    check((get "/redirect").status=302 && not(Hashtbl.mem calls "/target"))"no implicit credential-bearing redirects";
    List.iter(fun(meth,label)->let r=H.request ~timeout_ms:1000{(H.get(url "/echo"))with meth;body="A\000B"}in
      check(r.body=label^":A\000B")"binary write body and method") [H.Post,"POST";H.Delete,"DELETE";H.Patch,"PATCH"];
    check(Hashtbl.find calls "/echo"=3)"writes are never retried";
    check(rejected(fun()->get ~limit:3 "/large"))"decoded response size limit";
    check(rejected(fun()->get "/headers"))"response header size limit";
    check(rejected(fun()->H.request{(H.get(url "/ok"))with headers=["Authorization","fixture\r\nInjected: value"]}))"reject injected request headers";
    check(rejected(fun()->H.request ~timeout_ms:30(H.get(url "/slow"))))"transfer timeout";
    let cancelled=Eio.Fiber.first(fun()->ignore(get "/slow");false)(fun()->Eio.Time.Mono.sleep(Eio.Stdenv.mono_clock env)0.02;true)in
    check cancelled "Eio cancellation remains responsive";
    check((get "/ok").body="ok")"transport remains usable after cancellation")));
 print_endline "PASS shared HTTP: URL policies, no redirects/retries, binary methods, limits, timeout and cancellation"
