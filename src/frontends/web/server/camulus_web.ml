module Settings=Reader.Settings
module Service=Reader.Service
module CS=Reader.Content_state
let field=Settings.field
let str key j=Settings.string(field key j)
let state_dir ()=Filename.concat (match Sys.getenv_opt "XDG_STATE_HOME"with
 |Some p when not(Filename.is_relative p)->p|_->Filename.concat(Sys.getenv "HOME")".local/state")"camulus"
let read_file path=let ch=open_in_bin path in Fun.protect ~finally:(fun()->close_in_noerr ch)(fun()->really_input_string ch (in_channel_length ch))
let revision settings=Digest.to_hex(Digest.string(Yojson.Safe.to_string(Settings.to_json settings)))
(* Only this allowlisted HTML reaches inner_html in the browser. No IDs, styles,
   SVG, forms, executable URLs or event attributes survive the boundary. *)
let clean_html ~base html =
 let open Reader.Html in
 let tags=["p";"div";"span";"br";"hr";"h1";"h2";"h3";"h4";"h5";"h6";"strong";"b";"em";"i";"u";"s";"del";"blockquote";"pre";"code";"ul";"ol";"li";"dl";"dt";"dd";"a";"img";"figure";"figcaption";"table";"thead";"tbody";"tr";"td";"th";"sup";"sub";"video";"audio";"source"]in
 let rec clean=function
 |Text _ as n->render n
 |Element(tag,attrs,children)->
  if List.mem tag ["script";"style";"iframe";"object";"embed";"svg";"math";"form";"template"]then ""
  else let body=String.concat ""(List.map clean children)in
  if not(List.mem tag tags)then body else
  let attrs=List.filter_map(fun(k,v)->
   if List.mem k ["href";"src";"poster"]then Option.map(fun v->k,v)(safe_link ~base k v)
   else if List.mem k ["alt";"title";"type"]then Some(k,v)else None)attrs in
  let attrs=attrs@(if tag="a"then["target","_blank";"rel","noopener noreferrer"]else[])
    @(if List.mem tag ["video";"audio"]then["controls","";"preload","metadata"]else[]) in
  "<"^tag^String.concat ""(List.map(fun(k,v)->" "^k^"=\""^escape v^"\"")attrs)^">"^body^
    (if List.mem tag ["br";"hr";"img";"source"]then""else"</"^tag^">")in
 clean(parse html)
let rec presentation = function
 |`Assoc fields as j->`Assoc(List.map(fun(k,v)->k,(match k,v with
   |"html",`String html->`String(clean_html ~base:(match field "url"j with `String s->s|_->"")html)
   |_->presentation v))fields)
 |`List xs->`List(List.map presentation xs)|x->x

type job={mutable events:Yojson.Safe.t list;mutable size:int;mutable finished:bool;mutable error:string option;created:float}
let () =
 let port=ref 8765 and config_path=ref(Settings.path()) and state_path=ref(state_dir())and assets=ref "src/frontends/web/client/_build/default"in
 Arg.parse ["--port",Arg.Set_int port,"Loopback HTTP port (8765)";
  "--config",Arg.Set_string config_path,"Shared settings file";"--state-dir",Arg.Set_string state_path,"State directory";
  "--assets",Arg.Set_string assets,"Directory containing index.html, style.css and app.js"]
  (fun _->raise(Arg.Bad "Unexpected argument"))"camulus-web";
 if !port<1 || !port>65535 then invalid_arg "Invalid port";
 Eio_main.run(fun env->Eio.Switch.run(fun sw->
  let settings=ref(Settings.load !config_path)in
  Web_backend.validate !settings;
  let make settings =
   let backends=Web_backend.create ~state_dir:!state_path settings in
   let dispatch=Service.create ~state_file:(Some(Filename.concat !state_path "state.sqlite3"))
    ~subscriptions_file:(Some(Filename.concat !state_path "group-subscriptions.json")) ~subscribable:(fun _->false)
    ~factories:backends.factories ~archive_extractors:[] ~net:env#net
    ~now:(fun()->Eio.Time.now env#clock) ~sleep:(Eio.Time.sleep env#clock)in backends,dispatch in
  let backends,dispatch=make !settings in
  let backends=ref backends and dispatch=ref dispatch in
  let jobs=Hashtbl.create 32 and next=ref 0 in
  let settings_lock=Eio.Mutex.create()in
  let active ()=Hashtbl.fold(fun _ j n->if j.finished then n else n+1)jobs 0 in
  let run work =let xs=ref[]in !dispatch work ~emit:(function
   |Service.State_result j|Service.Presentation(_,j)->xs:=j::!xs|_->());List.rev !xs in
  let subscriptions () =
   let operations,remote=Web_backend.discover !backends in
   let local=List.concat_map(fun work->List.concat_map(fun j->match field "rows"j with
    |`List xs->List.map(fun x->{Settings.name=str "name"x;source=str "source"x;group=str "group"x})xs|_->[]) (run work))operations in
   List.map(fun (g:Settings.group)->match List.find_opt(fun (r:Settings.group)->r.source=g.source&&r.group=g.group)(!settings).groups with
    |Some r->r|None->g)(local@remote)in
  let payload j=match str "kind"j with
   |"state"->let op=CS.operation_of_json(field "request"j)in
     (match op with CS.Attachment _->invalid_arg "Filesystem downloads are not exposed by the web API"|_->());Service.Local op
   |"read"->Web_backend.read ~source:(str "source"j) ~id:(str "id"j)
   |"subscription"->let source=str "source"j and target=str "target"j in
     if not(Web_backend.supported source)then invalid_arg "Unsupported backend";
     Service.Subscription(source,target,(match field "setting"j with `Null->None|`String s->Some s|_->invalid_arg "Invalid setting"))
   |"tracking"->let source=str "source"j in
     if not(Discourse_backend.is_source source)then invalid_arg "Topic tracking is unavailable";
     Discourse_service.Discourse(Discourse_backend.site_of_source source,
       Discourse_backend.Subscriptions(Discourse_backend.tracking_of_string(str "filter"j),
        (match field "after"j with `String s->Some s|_->None)))
   |_->invalid_arg "Unknown operation"in
  let base_headers=["Cache-Control","no-store";"X-Content-Type-Options","nosniff";"Referrer-Policy","no-referrer";
   "Content-Security-Policy","default-src 'none'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src https: http: data:; media-src https: http:; connect-src 'self'; font-src 'self'; frame-src 'none'; object-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'"]in
  let respond ?(status=`OK) content body=Cohttp_eio.Server.respond_string ~status
    ~headers:(Http.Header.of_list(("Content-Type",content)::base_headers))~body()in
  let json ?status j=respond ?status "application/json; charset=utf-8"(Yojson.Safe.to_string j)in
  let callback _ request body =
   try
    let headers=Http.Request.headers request in
    let host=Http.Header.get headers "host"in
    let origins=[Printf.sprintf "http://127.0.0.1:%d" !port;Printf.sprintf "http://localhost:%d" !port]in
    if not(List.mem host [Some(Printf.sprintf "127.0.0.1:%d" !port);Some(Printf.sprintf "localhost:%d" !port)])then
     invalid_arg "Unexpected Host";
    (match Http.Header.get headers "origin"with Some o when not(List.mem o origins)->invalid_arg "Cross-origin request rejected"|_->());
    let meth=Http.Request.meth request and path=Uri.path(Uri.of_string(Http.Request.resource request))in
    if meth<>`GET && Http.Header.get headers "x-camulus-request"<>Some "1"then invalid_arg "Missing request header";
    let input ()=Eio.Time.with_timeout_exn env#clock 10.(fun()->
      Eio.Buf_read.of_flow body ~max_size:(1024*1024) |> Eio.Buf_read.take_all |> Yojson.Safe.from_string)in
    if active()=0 && Sys.file_exists !config_path then (
     let disk=Eio_unix.run_in_systhread(fun()->Settings.load !config_path)in
     if revision disk<>revision !settings then(
      Web_backend.validate disk;let b,d=make disk in settings:=disk;backends:=b;dispatch:=d));
    match meth,path with
    |`GET,"/api/bootstrap"->
     let subs=subscriptions()in
     json(`Assoc["settings",Settings.to_json !settings;"revision",`String(revision !settings);
       "subscriptions",`List(List.map Settings.group_json subs);
       "backends",`List(List.map(fun s->`String s)["rss";"jsonfeed";"discourse"])])
    |`PUT,"/api/settings"->
     let j=input()in
     Eio.Mutex.use_rw ~protect:true settings_lock(fun()->
       if active()>0 then json ~status:`Conflict (`Assoc["error",`String "Wait for active operations to finish before changing settings"])
       else if str "revision"j<>revision !settings then json ~status:`Conflict(`Assoc["error",`String "Settings changed; reload before saving"])
       else let proposed=Settings.of_json(field "settings"j)in
        Web_backend.validate proposed;
        let new_backends,new_dispatch=make proposed in
        ignore(Eio_unix.run_in_systhread(fun()->Settings.update !config_path(fun existing->
          if revision existing<>str "revision"j then invalid_arg "Settings changed on disk; reload the service";proposed)));
        settings:=proposed;backends:=new_backends;dispatch:=new_dispatch;
        json(`Assoc["revision",`String(revision proposed)]))
    |`POST,"/api/jobs"->
     let operation=payload(input())in
     if active()>=16 then json ~status:`Too_many_requests(`Assoc["error",`String "Too many active operations"])
     else begin
      let now=Eio.Time.now env#clock in
      Hashtbl.filter_map_inplace(fun _ j->if j.finished && now-.j.created>300. then None else Some j)jobs;
      if Hashtbl.length jobs>=128 then (let old=Hashtbl.fold(fun id j acc->if j.finished then(id,j.created)::acc else acc)jobs[]
        |>List.sort(fun(_,a)(_,b)->compare a b)in match old with(id,_)::_->Hashtbl.remove jobs id|_->());
      incr next;let id=string_of_int !next in
      let job={events=[];size=0;finished=false;error=None;created=now}in Hashtbl.add jobs id job;
      let dispatch= !dispatch in
      Eio.Fiber.fork ~sw(fun()->
       (try Eio.Time.with_timeout_exn env#clock 120.(fun()->dispatch operation ~emit:(function
         |Service.State_result j|Service.Presentation(_,j)->
          let j=presentation j in job.size<-job.size+String.length(Yojson.Safe.to_string j);
          if job.size>32*1024*1024 then failwith "Response exceeds 32 MiB";
          job.events<-j::job.events|_->()))
        with ex->job.error<-Some(Printexc.to_string ex));job.finished<-true);
      json ~status:`Accepted(`Assoc["id",`String id])end
    |`GET,path when String.starts_with ~prefix:"/api/jobs/" path->
     let id=String.sub path 10 (String.length path-10)in
     (match Hashtbl.find_opt jobs id with
      |None->json ~status:`Not_found(`Assoc["error",`String "Operation expired"])
      |Some j->json(`Assoc["done",`Bool j.finished;"error",(match j.error with None->`Null|Some s->`String s);"events",`List(List.rev j.events)]))
    |`GET,"/api/timeline"->
      let entries=subscriptions()|>List.filter(fun (g:Settings.group)->Settings.visible !settings ~source:g.source ~group:g.group)
       |>List.map(fun(g:Settings.group)->g.source,g.group)in
      json(`Assoc["events",`List(run(Service.Local(CS.Timeline entries)))])
    |`GET,("/"|"/index.html")->respond "text/html; charset=utf-8"(read_file(Filename.concat !assets "index.html"))
    |`GET,"/favicon.ico"->respond ~status:`No_content "image/x-icon" ""
    |`GET,"/style.css"->respond "text/css; charset=utf-8"(read_file(Filename.concat !assets "style.css"))
    |`GET,"/app.js"->
      let path=Filename.concat !assets "app.js"in
      let path=if Sys.file_exists path then path else Filename.concat !assets "main.bc.js"in
      respond "text/javascript; charset=utf-8"(read_file path)
    |_->json ~status:`Not_found(`Assoc["error",`String "Not found"])
   with ex->json ~status:`Bad_request(`Assoc["error",`String(Printexc.to_string ex)])in
  let socket=Eio.Net.listen env#net ~sw ~backlog:64 ~reuse_addr:true (`Tcp(Eio.Net.Ipaddr.V4.loopback,!port))in
  Printf.printf "Camulus: http://127.0.0.1:%d\nSettings: %s\n%!" !port !config_path;
  Cohttp_eio.Server.run ~max_connections:64 socket (Cohttp_eio.Server.make ~callback ()) ~on_error:(fun ex->prerr_endline(Printexc.to_string ex)) ))
