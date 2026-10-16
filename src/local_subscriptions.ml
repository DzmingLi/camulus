(* Shared persistence for backends that opt into local group subscriptions.
   It is mutable runtime state, independent of the browsing registry and cache. *)
type target = { backend : string; source : string; group : string }
type settings = bool
type t = { path : string; lock : Eio.Mutex.t }
let create path =
  if Filename.is_relative path then invalid_arg "Subscriptions state path must be absolute";
  { path; lock=Eio.Mutex.create () }
let validate target =
  List.iter (fun value -> if value="" || String.contains value '\000' then
    invalid_arg "Invalid group subscription identity") [target.backend;target.source;target.group]
let json target = `Assoc ["backend",`String target.backend;"source",`String target.source;"group",`String target.group]
let decode value =
  let open Yojson.Safe.Util in
  let target={backend=member "backend" value |> to_string;source=member "source" value |> to_string;group=member "group" value |> to_string} in
  validate target;target
let access t f =
  Eio.Mutex.lock t.lock;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.lock) (fun () ->
    Eio_unix.run_in_systhread (fun () ->
      let rec mkdir path = if not(Sys.file_exists path) then (
        mkdir(Filename.dirname path);try Unix.mkdir path 0o700 with Unix.Unix_error(Unix.EEXIST,_,_)->()) in
      mkdir(Filename.dirname t.path);
      let fd=Unix.openfile (t.path^".lock") [Unix.O_CREAT;Unix.O_RDWR;Unix.O_CLOEXEC] 0o600 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        Unix.lockf fd Unix.F_LOCK 0;
        let targets=if Sys.file_exists t.path then
          match Yojson.Safe.from_file t.path with `List xs -> List.map decode xs
          | _ -> failwith "Invalid group subscriptions state" else [] in
        f targets)))
let followed t ~backend = access t (List.filter (fun target -> target.backend=backend))
let get t target = validate target;access t (List.mem target)
let set t target subscribed =
  validate target;
  access t (fun targets ->
    let targets=List.filter ((<>)target) targets in
    let targets=List.sort_uniq compare (if subscribed then target::targets else targets) in
    let temp,ch=Filename.open_temp_file ~temp_dir:(Filename.dirname t.path) ".subscriptions-" ".json" in
    Fun.protect ~finally:(fun () -> close_out_noerr ch;try Sys.remove temp with Sys_error _ -> ()) (fun () ->
      Yojson.Safe.to_channel ch (`List(List.map json targets));output_char ch '\n';
      flush ch;Unix.fsync(Unix.descr_of_out_channel ch);close_out ch;Unix.rename temp t.path))
