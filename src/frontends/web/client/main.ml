open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml
module J=Yojson.Safe
let field key=function `Assoc xs->Option.value(List.Assoc.find xs key ~equal:String.equal)~default:`Null|_->`Null
let text=function `String s->s|_->""
let str k j=text(field k j)
let array=function `List xs->xs|_->[]
let integer=function `Int n->n|_->0
let bool=function `Bool b->b|_->false
let assoc xs=`Assoc xs
let s x=`String x
let delay seconds =
 let iv=Async_kernel.Ivar.create()in
 ignore(Dom_html.window##setTimeout(Js.wrap_callback(fun()->Async_kernel.Ivar.fill_exn iv ()))(seconds*.1000.) : Dom_html.timeout_id);
 Async_kernel.Ivar.read iv
let http meth path data =
 let iv=Async_kernel.Ivar.create()and xhr=XmlHttpRequest.create()in
 let finish result=if not(Async_kernel.Ivar.is_full iv)then Async_kernel.Ivar.fill_exn iv result in
 xhr##_open(Js.string meth)(Js.string path)Js._true;
 xhr##setRequestHeader(Js.string "X-Camulus-Request")(Js.string "1");
 xhr##setRequestHeader(Js.string "Content-Type")(Js.string "application/json");
 xhr##.onreadystatechange:=Js.wrap_callback(fun()->if Poly.equal xhr##.readyState XmlHttpRequest.DONE then
  let raw=Js.Opt.case xhr##.responseText (fun()->"") Js.to_string in
  try let j=J.from_string raw in
   if xhr##.status>=200 && xhr##.status<300 then finish(Ok j)
   else finish(Error(if String.is_empty(str "error"j)then "Request failed"else str "error"j))
  with _->finish(Error "Cannot reach Camulus service"));
 xhr##send(match data with None->Js.null|Some j->Js.some(Js.string(J.to_string j)));
 ignore(Dom_html.window##setTimeout(Js.wrap_callback(fun()->if not(Async_kernel.Ivar.is_full iv)then(
   finish(Error "Request timed out");xhr##abort)))(15000.) : Dom_html.timeout_id);
 Async_kernel.Ivar.read iv
let job request progress =
 let open Async_kernel.Deferred.Let_syntax in
 let%bind started=http "POST" "/api/jobs" (Some request)in
 match started with Error e->return(Error e)|Ok result->
 let rec poll seen =
  let%bind result=http "GET" ("/api/jobs/"^str "id"result) None in
  match result with Error e->return(Error e)|Ok snapshot->
   let events=array(field "events"snapshot)in
   List.iter(List.drop events seen)~f:progress;
   if bool(field "done"snapshot)then
     (match field "error"snapshot with `String e->return(Error e)|_->return(Ok snapshot))
   else let%bind ()=delay 0.2 in poll(List.length events)
 in poll 0
let state fields=assoc["kind",s "state";"request",assoc fields]
let bootstrap ()=http "GET" "/api/bootstrap" None
let dispatch effect=Vdom.Effect.Expert.handle_non_dom_event_exn effect

type view=Home|Groups|Subscriptions|Folders|Folder of string|Group of J.t|Settings
let empty_settings=assoc["version",`Int 1;"groups",`List [];"priorities",`List [];"default_priority",`Int 2;"home_priorities",`List [];"backend_settings",assoc[]]
type model={view:view;config:J.t;revision:string;subscriptions:J.t list;rows:J.t list;articles:J.t list;
 selected:J.t option;folders:string list;error:string;notice:string;busy:int;after:J.t;
 settings_text:string;query:string;save_dialog:bool;save_item:J.t option;save_folder:string;new_name:string;new_url:string;new_kind:string;levels_text:string}
let initial={view=Home;config=empty_settings;revision="";subscriptions=[];rows=[];articles=[];selected=None;
 folders=[];error="";notice="";busy=0;after=`Null;settings_text="";query="";save_dialog=false;save_item=None;save_folder="";
 new_name="";new_url="";new_kind="rss";levels_text=""}
let generation=ref 0 and read_generation=ref 0
let read_seen=Hash_set.create(module String)
let scan_read=ref(fun()->())
let read_state=Hashtbl.create(module String)
let jumped=ref false
let cls=Vdom.Attr.class_
let attr=Vdom.Attr.create
let node=Vdom.Node.create
let label=Vdom.Node.text
let button ?(attrs=[]) title action=Vdom.Node.button ~attrs:(Vdom.Attr.on_click(fun _->action)::attrs)[label title]
let input ?(kind="text") ?(placeholder="") value update=Vdom.Node.input
 ~attrs:[attr "type"kind;attr "placeholder"placeholder;Vdom.Attr.value value;Vdom.Attr.on_input(fun _ x->update x)]()
let change key value j=match j with `Assoc xs->assoc((key,value)::List.Assoc.remove xs key ~equal:String.equal)|_->j
let priority config entry=List.find_map(array(field "priorities"config))~f:(fun p->
 if String.equal(str "source"p)(str "source"entry)&&String.equal(str "group"p)(str "group"entry)then Some(integer(field "level"p))else None)
 |>Option.value ~default:(integer(field "default_priority"config))
let eligible config entry=let levels=array(field "home_priorities"config)in
 List.is_empty levels || List.mem levels (`Int(priority config entry))~equal:Poly.equal
let safe_url url=if List.exists["https://";"http://";"mailto:"]~f:(fun prefix->String.is_prefix url ~prefix)then url else ""
let backend source=String.take_while source ~f:(fun c->not(Char.equal c ':'))
let supported source=List.mem["rss";"jsonfeed";"discourse"](backend source)~equal:String.equal
let date j=match j with `Float n->Js.to_string((new%js Js.date_fromTimeValue(n*.1000.))##toLocaleString)|_->""
let merge_message articles message =
 let id=str "id"message in
 match List.find articles ~f:(fun m->String.equal(str "id"m)id)with
 |None->articles@[message]
 |Some old->let message=if String.is_empty(str "body"message)&&String.is_empty(str "html"message)
   then change "body"(field "body"old)(change "html"(field "html"old)message)else message in
  List.map articles ~f:(fun m->if String.equal(str "id"m)id then message else m)
let tree articles =
 let compare a b=Float.compare (match field "created"a with `Float f->f|_->0.)(match field "created"b with `Float f->f|_->0.)in
 let articles=List.sort articles ~compare in
 let ids=List.map articles ~f:(str "id")and seen=Hash_set.create(module String)in
 let rec visit depth m=let id=str "id"m in if Hash_set.mem seen id then[]else(
  Hash_set.add seen id;(depth,m)::List.concat_map articles ~f:(fun child->if String.equal(str "parent_id"child)id then visit(depth+1)child else[]))in
 let roots=List.filter articles ~f:(fun m->not(List.mem ids(str "parent_id"m)~equal:String.equal))in
 List.concat_map roots ~f:(visit 0)@List.concat_map articles ~f:(visit 0)

let component graph =
 let model,inject=Bonsai.state_machine0 graph ~default_model:initial ~apply_action:(fun _ m f->f m)in
 let activate=let%map inject=inject in Effect.of_deferred_thunk(fun()->
  let open Async_kernel.Deferred.Let_syntax in
  let%bind result=bootstrap()in
  (match result with Error e->dispatch(inject(fun m->{m with error=e}))|Ok j->dispatch(inject(fun m->{m with
   config=field "settings"j;revision=str "revision"j;subscriptions=array(field "subscriptions"j);settings_text=J.pretty_to_string(field "settings"j)})));
  let%map result=http "GET" "/api/timeline" None in
  match result with Error e->dispatch(inject(fun m->{m with error=e}))|Ok j->
   List.iter(array(field "events"j))~f:(fun e->if String.equal(str "kind"e)"feed"then dispatch(inject(fun m->{m with rows=array(field "rows"e)}))))in
 Bonsai.Edge.lifecycle ~on_activate:activate ~after_display:(Bonsai.return(Effect.of_sync_fun(fun()-> !scan_read())())) graph;
 let%map m=model and inject=inject in
 let update f=inject f in
 let apply f=dispatch(update f)in
 let async f=Effect.of_deferred_thunk(fun()->
  apply(fun m->{m with busy=m.busy+1;error=""});
  let open Async_kernel.Deferred.Let_syntax in let%map result=f()in
  apply(fun m->{m with busy=Int.max 0(m.busy-1);error=(match result with Ok _->m.error|Error e->e)}))in
 let run request progress=async(fun()->job request progress)in
 let reload_bootstrap ()=
  let open Async_kernel.Deferred.Let_syntax in let%map result=bootstrap()in
  (match result with Ok j->apply(fun m->{m with config=field "settings"j;revision=str "revision"j;
   subscriptions=array(field "subscriptions"j);settings_text=J.pretty_to_string(field "settings"j)})|_->());result in
 let feed_event token e=if token= !generation && String.equal(str "kind"e)"feed"then
   apply(fun m->{m with rows=array(field "rows"e);after=field "after"e})in
 let timeline token ()=
  let open Async_kernel.Deferred.Let_syntax in let%map result=http "GET" "/api/timeline" None in
  (match result with Ok j->List.iter(array(field "events"j))~f:(feed_event token)|_->());result in
 let navigate view=Int.incr generation;Int.incr read_generation;
  apply(fun m->{m with view;rows=[];selected=None;articles=[];error="";query="";after=`Null});!generation in
 let home ()=let token=navigate Home in async(timeline token)in
 let browse entry ?after ?query ()=
  let token=match after,query with None,None->navigate(Group entry)|_-> !generation in
  run(state["op",s "refresh";"source",field "source"entry;"group",field "group"entry;
    "after",Option.value after ~default:`Null;"query",Option.value_map query ~default:`Null ~f:s])(feed_event token)in
 let reload_folders ()=job(state["op",s "folders"])(fun e->if String.equal(str "kind"e)"folders"then
  apply(fun m->{m with folders=List.map(array(field "rows"e))~f:text}))in
 let folder name=let token=navigate(Folder name)in
  run(state["op",s "favorites";"folder",s name])(fun e->if token= !generation&&String.equal(str "kind"e)"favorites"then apply(fun m->{m with rows=array(field "rows"e)}))in
 let open_row row=
  Int.incr read_generation;let token= !read_generation in Hash_set.clear read_seen;Hashtbl.clear read_state;jumped:=false;
  apply(fun m->{m with selected=Some row;articles=[]});
  ignore(Dom_html.window##setTimeout(Js.wrap_callback(fun()->
   Js.Opt.iter(Dom_html.document##querySelector(Js.string ".reader"))(fun e->(Js.Unsafe.coerce e)##focus)))(80.) : Dom_html.timeout_id);
  let request=match m.view with Folder _->state["op",s "archive";"source",field "source"row;"id",field "id"row;"target",field "target"row]
   |_->assoc["kind",s "read";"source",field "source"row;"id",field "id"row]in
  run request(fun e->if token= !read_generation then (
   if String.equal(str "kind"e)"states"then List.iter(array(field "items"(field "data"e)))~f:(fun item->
     Hashtbl.set read_state ~key:(str "id"item) ~data:(bool(field "read"item)));
   apply(fun m->
   let messages=match str "kind"e with
    |"article"|"liked"|"sent"->[field "message"e]
    |"thread"->array(field "messages"e)
    |"archive"->List.map(array(field "articles"e))~f:(field "message")|_->[]in
   let articles=List.fold messages ~init:m.articles ~f:(fun xs msg->if String.is_empty(str "id"msg)then xs else merge_message xs msg)in
   let read=not(List.is_empty articles)&&List.for_all articles ~f:(fun msg->Option.value(Hashtbl.find read_state(str "id"msg))~default:false)in
   {m with articles;rows=List.map m.rows ~f:(fun item->if String.equal(str "source"item)(str "source"row)&&String.equal(str "id"item)(str "id"row)
     then change "unread"(`Bool(not read))item else item)})))in
 let dismiss row=let token= !generation in run(state["op",s "dismiss-timeline";"source",field "source"row;"id",field "id"row;"revision",field "revision"row])
  (fun e->if token= !generation&&String.equal(str "kind"e)"changed"then apply(fun m->{m with rows=List.filter m.rows ~f:(fun x->not(String.equal(str "id"x)(str "id"row)&&String.equal(str "source"x)(str "source"row)))}))in
 let save_dialog row=Effect.Many[update(fun m->{m with save_dialog=true;save_item=Some row});async reload_folders]in
 let save row name=run(state["op",s "favorite";"source",field "source"row;"id",field "id"row;
   "target",s(if String.is_empty(str "thread_id"row)then "article"else "thread");"folder",s name])
  (fun e->if String.equal(str "kind"e)"saved"then apply(fun m->{m with save_dialog=false;notice="已收藏并持久保存";
   rows=List.map m.rows ~f:(fun x->if String.equal(str "id"x)(str "id"row)&&String.equal(str "source"x)(str "source"row)then change "favorite"(`Bool true)x else x)}))in
 let save_config config=async(fun()->let open Async_kernel.Deferred.Let_syntax in
   let%bind result=http "PUT" "/api/settings"(Some(assoc["settings",config;"revision",s m.revision]))in
   match result with Error _->return result|Ok _->
     let%bind result=reload_bootstrap()in apply(fun m->{m with notice="设置已保存"});
     if Poly.equal m.view Home then timeline !generation ()else return result)in
 let subscribe entry value=async(fun()->let open Async_kernel.Deferred.Let_syntax in
  let%bind result=job(assoc["kind",s "subscription";"source",field "source"entry;"target",field "group"entry;"setting",s value])ignore in
  match result with Error _->return result|Ok _->reload_bootstrap())in
 let subscribed entry=List.exists m.subscriptions ~f:(fun x->String.equal(str "source"x)(str "source"entry)&&String.equal(str "group"x)(str "group"entry))in
 let menu view title=button ~attrs:[cls(if Poly.equal m.view view then "nav active"else"nav")] title
  (match view with Home->Effect.of_sync_fun(fun()->dispatch(home()))()|Folders->Effect.of_sync_fun(fun()->ignore(navigate Folders:int);dispatch(async reload_folders))()
   |_->Effect.of_sync_fun(fun()->ignore(navigate view:int))())in
 let settings_view ()=
  let add ()=try
    let source=match m.new_kind with "discourse"->"discourse:"^String.rstrip m.new_url ~drop:(Char.equal '/')|kind->kind^":"^m.new_url in
    let entry=assoc["name",s m.new_name;"source",s source;"group",s(if String.equal m.new_kind "discourse"then"latest"else m.new_url)]in
    save_config(change "groups"(`List(array(field "groups"m.config)@[entry]))m.config)
   with ex->update(fun m->{m with error=Exn.to_string ex})in
  node "section" ~attrs:[cls "settings"][node "h2"[label "来源与显示设置"];
   node "p"[label "注册来源后即可浏览；订阅在组内单独选择。Discourse 的通知与会话追踪由站点管理。"];
   node "div" ~attrs:[cls "add-source"][input ~placeholder:"来源名称" m.new_name(fun x->update(fun m->{m with new_name=x}));
    node "select" ~attrs:[Vdom.Attr.value m.new_kind;Vdom.Attr.on_change(fun _ x->update(fun m->{m with new_kind=x}))]
     (List.map["rss","RSS / Atom";"jsonfeed","JSON Feed 1.1";"discourse","Discourse"]~f:(fun(k,v)->node "option" ~attrs:[attr "value"k][label v]));
    input ~placeholder:"https://…" m.new_url(fun x->update(fun m->{m with new_url=x}));button "注册来源"(add())];
   node "h3"[label "共享设置"];
   node "p"[label "高级设置包含组、优先级和后端连接参数。凭据使用文件路径；订阅状态不写入这里。"];
   Vdom.Node.textarea ~attrs:[attr "aria-label""共享设置 JSON";Vdom.Attr.value m.settings_text;
    Vdom.Attr.on_input(fun _ x->update(fun m->{m with settings_text=x}))][];
   button "保存设置"(try save_config(J.from_string m.settings_text)with _->update(fun m->{m with error="设置不是有效 JSON"}))]in
 let entries=match m.view with Subscriptions->m.subscriptions|_->array(field "groups"m.config)in
 let groups_view ()=node "section" ~attrs:[cls "groups"][node "h2"[label(if Poly.equal m.view Subscriptions then"订阅来源"else"已注册的组")];
  node "p"[label "注册用于浏览，订阅决定哪些更新进入主页。"];
  node "div" ~attrs:[cls "cards"](List.map entries ~f:(fun e->let source=str "source"e in
   node "article" ~attrs:[cls "group-card"][node "small"[label(backend source)];node "h3"[label(str "name"e)];node "p"[label(str "group"e)];
    (if supported source then button "浏览"(Effect.of_sync_fun(fun()->dispatch(browse e()))())else node "small"[label "此后端尚未接入网页"]);
    (if List.mem["rss";"jsonfeed"](backend source)~equal:String.equal then button(if subscribed e then"取消订阅"else"订阅")
      (subscribe e (if subscribed e then"unsubscribed"else"subscribed"))else Vdom.Node.none);
    (if Poly.equal m.view Subscriptions then node "label"[label "优先级 ";input ~kind:"number"(Int.to_string(priority m.config e))(fun v->
      match Int.of_string_opt v with None->Effect.Ignore|Some level->
       let rules=List.filter(array(field "priorities"m.config))~f:(fun p->not(String.equal(str "source"p)source&&String.equal(str "group"p)(str "group"e)))in
       save_config(change "priorities"(`List(assoc["source",s source;"group",field "group"e;"level",`Int level]::rules))m.config))]else Vdom.Node.none)]))]in
 let title=match m.view with Home->"个人时间线"|Groups->"组"|Subscriptions->"订阅"|Folders->"收藏夹"|Folder name->name|Group e->str "name"e|Settings->"设置"in
 let refresh=match m.view with
 |Home->async(fun()->let open Async_kernel.Deferred.Let_syntax in
   let token= !generation in
   let rec loop errors=function []->let%map result=timeline token()in if List.is_empty errors then result else Error(String.concat ~sep:"；"(List.rev errors))
    |e::rest->let%bind result=job(state["op",s "refresh";"source",field "source"e;"group",field "group"e])ignore in
      loop(match result with Ok _->errors|Error err->(str "name"e^": "^err)::errors)rest in
   loop[](List.filter m.subscriptions ~f:(fun e->supported(str "source"e)&&eligible m.config e)))
 |Group e->Effect.of_sync_fun(fun()->dispatch(browse e()))()
 |_->async reload_bootstrap in
 let list_view ()=node "section" ~attrs:[cls "summary"][
  (match m.view with Group e->node "div" ~attrs:[cls "search"][input ~placeholder:"在此组搜索"m.query(fun x->update(fun m->{m with query=x}));
    button "搜索"(Effect.of_sync_fun(fun()->dispatch(browse e ~query:m.query()))())]|_->Vdom.Node.none);
  (if List.is_empty m.rows then node "p" ~attrs:[cls "empty"][label(if m.busy>0 then "正在加载…"else"这里还没有内容。可以浏览已注册的组，或刷新订阅。")]
   else node "div"(List.map m.rows ~f:(fun row->
    let selected=Option.exists m.selected ~f:(fun r->String.equal(str "source"r)(str "source"row)&&String.equal(str "id"r)(str "id"row))in
    let favorite=bool(field "favorite"row)&&not(match m.view with Folder _->true|_->false)in
    node "div" ~attrs:[cls("summary-row"^(if favorite then" favorite"else if bool(field "unread"row)then""else" read")^(if selected then" selected"else""))]
     [button ~attrs:[cls "row-open"] (str "title"row) (Effect.of_sync_fun(fun()->dispatch(open_row row))());
      node "div" ~attrs:[cls "row-meta"][label(str "author"row^" · "^date(field "occurred"row));
       node "span"[label(backend(str "source"row))]];
      node "div" ~attrs:[cls "row-actions"][button "收藏"(save_dialog row);
       (match m.view with Home->button "移除"(dismiss row)|Folder name->button "取消此收藏"(run(state["op",s "unfavorite";"source",field "source"row;
        "id",field "id"row;"target",field "target"row;"folder",s name])(fun e->if String.equal(str "kind"e)"changed"then dispatch(folder name)))|_->Vdom.Node.none)]])));
  (match m.view,m.after with Group e,(`String _ as after)->button "加载更多"(Effect.of_sync_fun(fun()->dispatch(browse e ~after()))())|_->Vdom.Node.none)]in
 let article_view ()=node "section" ~attrs:[cls "reader";attr "tabindex""0";attr "aria-label""阅读区";Vdom.Attr.on_scroll(fun _->Effect.of_sync_fun(fun()-> !scan_read())())]
  (match m.selected with None->[node "p" ~attrs:[cls "empty"][label "选择一篇文章开始阅读"]]|Some selected->
   node "div" ~attrs:[cls "reader-toolbar"][button "收藏…"(save_dialog selected);button "关闭阅读"(update(fun m->{m with selected=None;articles=[]}))]::
   (if List.is_empty m.articles then[node "p"[label "正在加载正文…"]]else List.map(tree m.articles)~f:(fun(depth,msg)->
    node "article" ~key:(str "id"msg) ~attrs:[cls "article";attr "data-message"(str "id"msg);attr "data-source"(str "source"selected);
       attr "style"(Printf.sprintf "margin-left:%dpx"(Int.min 6 depth*20))]
      [node "header"[node "h2"[label(str "title"msg)];node "p" ~attrs:[cls "byline"][label(str "author"msg^" · "^date(field "created"msg))]];
       (match field "html"msg with `String html when not(String.is_empty html)->Vdom.Node.inner_html ~tag:"div"~attrs:[cls "article-body"]
         ~this_html_is_sanitized_and_is_totally_safe_trust_me:html ()|_->node "div" ~attrs:[cls "article-body plain"][label(str "body"msg)]);
       node "footer"[node "a" ~attrs:[attr "href"(safe_url(str "url"msg));attr "target""_blank";attr "rel""noopener noreferrer"][label "打开原文"];
        button "收藏此楼"(save_dialog(change "source"(field "source"selected)(change "thread_id"`Null msg)))]])))in
 (* Mark only visible article bodies, not the summaries or background batches. *)
 let scan ()=
  let elements=Dom_html.document##querySelectorAll(Js.string ".reader .article")in
  if not !jumped && elements##.length>0 then (
   let target=ref None in
   for i=0 to elements##.length-1 do Js.Opt.iter(elements##item i)(fun element->
    let id=Js.Opt.case(element##getAttribute(Js.string "data-message"))(fun()->"")Js.to_string in
    if Option.is_none !target && not(Option.value(Hashtbl.find read_state id)~default:false)then target:=Some element)done;
   jumped:=Option.is_some !target;Option.iter !target ~f:(fun element->ignore(Js.Unsafe.meth_call element "scrollIntoView"[|Js.Unsafe.inject Js._true|] : Js.Unsafe.any)));

  let bodies=Dom_html.document##querySelectorAll(Js.string ".reader .article-body")in
  let visible=ref []in
  for i=0 to bodies##.length-1 do Js.Opt.iter(bodies##item i)(fun element->
   let body:Dom_html.element Js.t=Js.Unsafe.coerce element in
   let el:Dom_html.element Js.t=Js.Unsafe.coerce(Js.Opt.get body##.parentNode (fun()->assert false))in
   let rect=body##getBoundingClientRect in
   let id=Js.Opt.case(el##getAttribute(Js.string "data-message"))(fun()->"")Js.to_string in
   if Float.(rect##.top < of_int Dom_html.document##.documentElement##.clientHeight && rect##.bottom > 150. && rect##.bottom > rect##.top) && not(Hash_set.mem read_seen id)then(
    Hash_set.add read_seen id;visible:=id::!visible))done;
  match m.selected,!visible with
  |Some row,(_::_ as ids)->let source=str "source"row in
   dispatch(run(state["op",s "read";"source",s source;"ids",`List(List.map ids ~f:s);"value",`Bool true])
    (fun e->if String.equal(str "kind"e)"states"then (
     List.iter ids ~f:(fun id->Hashtbl.set read_state ~key:id ~data:true);
     apply(fun m->let read=List.for_all m.articles ~f:(fun msg->Option.value(Hashtbl.find read_state(str "id"msg))~default:false)in
      {m with rows=List.map m.rows ~f:(fun item->if String.equal(str "source"item)source&&String.equal(str "id"item)(str "id"row)
       then change "unread"(`Bool(not read))item else item)}))))
  |_->()in
 scan_read:=scan;
 node "div" ~attrs:[cls "app";Vdom.Attr.on_keydown(fun ev->
  let tag=Js.Opt.case ev##.target (fun()->"")(fun target->Js.to_string target##.tagName) in
  if List.mem["INPUT";"TEXTAREA";"SELECT"]tag ~equal:String.equal then Effect.Ignore else
  match Js.Optdef.case ev##.key (fun()->"") Js.to_string,m.selected with
  |"m",Some row->save_dialog row
  |"d",Some row when Poly.equal m.view Home->dismiss row
  |_->Effect.Ignore)][node "aside" ~attrs:[cls "sidebar"][node "h1"[label "Camulus"];node "p" ~attrs:[cls "subtitle"][label "阅读 · 讨论 · 收藏"];
  menu Home "时间线";menu Groups "组浏览";menu Subscriptions "订阅";menu Folders "收藏夹";menu Settings "设置";
  node "div" ~attrs:[cls "sidebar-foot"][label "OCaml · Bonsai"]];
  node "main"[node "header" ~attrs:[cls "topbar"][node "h2"[label title];
   node "span" ~attrs:[cls "status";attr "role""status"][label(if m.busy>0 then Printf.sprintf "正在处理 %d 项…"m.busy else m.notice)];button "刷新"refresh];
   (if String.is_empty m.error then Vdom.Node.none else node "div" ~attrs:[cls "error";attr "role""alert"][label m.error;button "关闭"(update(fun m->{m with error=""}))]);
   (match m.view with Home->node "div" ~attrs:[cls "priorities"][label "首页优先级";
     input ~placeholder:"全部，或 0,1,2"m.levels_text(fun x->update(fun m->{m with levels_text=x}));
     button "应用"(try let levels=if String.is_empty(String.strip m.levels_text)then[]else
       String.split m.levels_text ~on:',' |>List.map ~f:(fun x->`Int(Int.of_string(String.strip x)))in
       save_config(change "home_priorities"(`List levels)m.config)
       with _->update(fun m->{m with error="请输入逗号分隔的非负整数"}));
     label("当前："^(let xs=array(field "home_priorities"m.config)in if List.is_empty xs then"全部"else String.concat ~sep:","(List.map xs ~f:(fun x->Int.to_string(integer x)))))]|_->Vdom.Node.none);
   (match m.view with Settings->settings_view()|Groups|Subscriptions->groups_view()
    |Folders->node "section" ~attrs:[cls "groups"][node "h2"[label "收藏夹"];node "div" ~attrs:[cls "cards"](List.map m.folders ~f:(fun name->button name(Effect.of_sync_fun(fun()->dispatch(folder name))())))]
    |_->node "div" ~attrs:[cls(if Option.is_some m.selected then"workspace split"else"workspace");Vdom.Attr.on_scroll(fun _->Effect.of_sync_fun scan())]
      (list_view()::(if Option.is_some m.selected then[article_view()]else[])))];
  (if not m.save_dialog then Vdom.Node.none else node "div" ~attrs:[cls "modal-backdrop"][node "section" ~attrs:[cls "modal";attr "role""dialog";attr "aria-label""收藏到文件夹"]
   [node "h2"[label "收藏到文件夹"];node "p"[label "收藏会持久保存正文。同一篇内容可以加入多个文件夹。"];
    node "div"(List.map m.folders ~f:(fun name->button name(match m.save_item with None->Effect.Ignore|Some row->save row name)));
    input ~placeholder:"新文件夹，支持 父文件夹/子文件夹" m.save_folder(fun x->update(fun m->{m with save_folder=x}));
    button "保存"(match m.save_item with None->Effect.Ignore|Some row->save row m.save_folder);button "取消"(update(fun m->{m with save_dialog=false}))]])]
let ()=Bonsai_web.Start.start component
