(* Frontend-independent source registry and display preferences. Subscription
   state belongs to backend services and is deliberately absent from this file. *)
type group = { name : string; source : string; group : string }
type priority = { source : string; group : string; level : int }
type t = { groups : group list; priorities : priority list; default_priority : int;
           home_priorities : int list; backend_settings : Yojson.Safe.t }
let empty = {groups=[];priorities=[];default_priority=2;home_priorities=[];backend_settings=`Assoc []}
let field name = function `Assoc xs -> Option.value (List.assoc_opt name xs) ~default:`Null | _ -> `Null
let string = function `String s when s<>"" && not(String.contains s '\000') -> s | _ -> invalid_arg "Expected a nonempty string"
let level = function `Int n when n>=0 -> n | _ -> invalid_arg "Priority must be a nonnegative integer"
let array = function `List xs when List.length xs<=1000 -> xs | _ -> invalid_arg "Expected an array of at most 1000 entries"
let of_json j =
 (match j with
 |`Assoc fields->List.iter(fun(k,_)->if not(List.mem k ["version";"groups";"priorities";"default_priority";"home_priorities";"backend_settings"])
   then invalid_arg("Unknown settings field: "^k))fields
 |_->invalid_arg "Settings must be an object");
 if field "version" j<>`Int 1 then invalid_arg "Unsupported settings version";
 let groups=array(field "groups" j) |> List.map(fun j ->
   if field "timeline" j<>`Null then invalid_arg "Group registration does not subscribe";
   {name=string(field "name" j);source=string(field "source" j);group=string(field "group" j)}) in
 let names=Hashtbl.create 16 and keys=Hashtbl.create 16 in
 List.iter(fun (g:group) ->
   if Hashtbl.mem names g.name || Hashtbl.mem keys (g.source,g.group) then invalid_arg "Duplicate group";
   Hashtbl.add names g.name ();Hashtbl.add keys (g.source,g.group) ()) groups;
 let priorities=array(field "priorities" j) |> List.map(fun j ->
   {source=string(field "source" j);group=string(field "group" j);level=level(field "level" j)}) in
 let seen=Hashtbl.create 16 in
 List.iter(fun (p:priority) -> if Hashtbl.mem seen (p.source,p.group) then invalid_arg "Duplicate priority rule";
   Hashtbl.add seen (p.source,p.group) ()) priorities;
 let backend_settings=match field "backend_settings" j with `Assoc _ as x->x | _ -> invalid_arg "backend_settings must be an object" in
 {groups;priorities;default_priority=level(field "default_priority" j);
  home_priorities=List.sort_uniq compare (List.map level (array(field "home_priorities" j)));backend_settings}
let group_json (g:group) = `Assoc["name",`String g.name;"source",`String g.source;"group",`String g.group]
let to_json t = `Assoc["version",`Int 1;"groups",`List(List.map group_json t.groups);
 "priorities",`List(List.map(fun (p:priority)->`Assoc["source",`String p.source;"group",`String p.group;"level",`Int p.level])t.priorities);
 "default_priority",`Int t.default_priority;"home_priorities",`List(List.map(fun n->`Int n)t.home_priorities);
 "backend_settings",t.backend_settings]
let priority t ~source ~group = match List.find_opt(fun (p:priority)->p.source=source && p.group=group)t.priorities with
 |Some p->p.level|None->t.default_priority
let visible t ~source ~group = t.home_priorities=[] || List.mem(priority t ~source ~group)t.home_priorities
let path () = Filename.concat (match Sys.getenv_opt "XDG_CONFIG_HOME" with
 |Some p when not(Filename.is_relative p)->p|_->Filename.concat(Sys.getenv "HOME")".config") "camulus/settings.json"
let rec directory p = if not(Sys.file_exists p)then(directory(Filename.dirname p);try Unix.mkdir p 0o700 with Unix.Unix_error(Unix.EEXIST,_,_)->())
let load path = if Sys.file_exists path then of_json(Yojson.Safe.from_file path) else empty
let save path t =
 let data=to_json t in ignore(of_json data);
 directory(Filename.dirname path);
 let temp,ch=Filename.open_temp_file ~temp_dir:(Filename.dirname path) ~perms:0o600 ".settings-" ".json" in
 Fun.protect ~finally:(fun()->close_out_noerr ch;try Sys.remove temp with Sys_error _->()) (fun()->
  output_string ch (Yojson.Safe.pretty_to_string data^"\n");flush ch;Unix.fsync(Unix.descr_of_out_channel ch);close_out ch;
  Unix.rename temp path)
(* Lock the entire read-modify-write, including between frontend processes. *)
let update path f =
 directory(Filename.dirname path);
 let fd=Unix.openfile(path^".lock")[Unix.O_CREAT;Unix.O_RDWR;Unix.O_CLOEXEC]0o600 in
 Fun.protect ~finally:(fun()->Unix.close fd)(fun()->
  Unix.lockf fd Unix.F_LOCK 0;let result=f(load path)in save path result;result)
