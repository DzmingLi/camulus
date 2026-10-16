(** Emacs owns subprocesses and SIGCHLD. Only ordinary OCaml data crosses domains.
    Queue has one producer (worker domain) and one consumer (UI thread). *)
type response = { status : int; stdout : string }
type request = {
  id : int; argv : string list; input_path : string; limit : int;
  cancelled : bool Atomic.t; response : response option Atomic.t;
}
type t = {
  queue : request Reader.Spsc_queue.t;
  active : (int, request) Hashtbl.t; (* UI-owned *)
  mutable next_id : int;           (* worker-owned *)
  notify : unit -> unit;
}
type event = Start of request | Cancel of int
exception Failed of int
exception Timeout
let create ~notify () =
  {queue=Reader.Spsc_queue.create 64;active=Hashtbl.create 32;next_id=0;notify}

let run t ~now ~sleep ?(timeout=60.) ?(limit=16*1024*1024) ?(input="") argv =
  if argv=[] || timeout<=0. || limit<=0 then invalid_arg "Invalid process request";
  t.next_id <- t.next_id+1;
  (* Redirect from a private file rather than process-send-string on the UI thread:
     a child waiting on the network may not consume stdin for a long time. *)
  let input_path=if input="" then "" else begin
    let path,ch=Filename.open_temp_file ~mode:[Open_binary] ~perms:0o600 "ecaml-input-" ".eml" in
    (try output_string ch input;close_out ch with ex -> close_out_noerr ch;Unix.unlink path;raise ex);
    path
  end in
  let r={id=t.next_id;argv;input_path;limit;cancelled=Atomic.make false;response=Atomic.make None} in
  let deadline=now () +. timeout in
  let pause () = if now () >= deadline then raise Timeout else sleep 0.01 in
  Fun.protect ~finally:(fun () ->
    Atomic.set r.cancelled true;t.notify ();
    if input_path<>"" then (try Unix.unlink input_path with Unix.Unix_error(Unix.ENOENT,_,_) -> ())) @@ fun () ->
  while not (Reader.Spsc_queue.push t.queue r) do pause () done;
  t.notify ();
  let rec await () = match Atomic.get r.response with
    | Some response -> response
    | None -> pause ();await () in
  await ()

let drain t =
  let events=ref [] and remove=ref [] in
  Hashtbl.iter (fun id r -> if Atomic.get r.cancelled then begin
    remove:=id::!remove; events:=Cancel id::!events end) t.active;
  List.iter (Hashtbl.remove t.active) !remove;
  let rec loop () = match Reader.Spsc_queue.pop t.queue with
    | None -> ()
    | Some r ->
      if not (Atomic.get r.cancelled) then begin
        Hashtbl.add t.active r.id r;events:=Start r::!events end;
      loop () in
  loop (); List.rev !events

let complete t id response = match Hashtbl.find_opt t.active id with
  | None -> ()
  | Some r ->
    Hashtbl.remove t.active id;
    let response = if String.length response.stdout>r.limit
      then {status= -2;stdout=""} else response in
    Atomic.set r.response (Some response)

let cancel_all t =
  Hashtbl.iter (fun _ r -> Atomic.set r.cancelled true) t.active;
  drain t
