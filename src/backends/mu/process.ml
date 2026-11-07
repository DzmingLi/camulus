(** Emacs owns subprocesses and SIGCHLD. Only ordinary OCaml data crosses domains.
    Queue has one producer (worker domain) and one consumer (UI thread). *)
module Queue = Saturn.Single_prod_single_cons_queue

type response = { status : int; stdout : string }
type request = {
  id : int; argv : string list;
  cancelled : bool Atomic.t; response : response option Atomic.t;
}
type t = {
  queue : request Queue.t;
  active : (int, request) Hashtbl.t; (* UI-owned *)
  mutable next_id : int;           (* worker-owned *)
  notify : unit -> unit;
}
type event = Start of request | Cancel of int
exception Timeout
let create ~notify () =
  {queue=Queue.create ~size_exponent:6;active=Hashtbl.create 32;next_id=0;notify}

let run t ~now ~sleep ?(timeout=60.) argv =
  if argv=[] || timeout<=0. then invalid_arg "Invalid process request";
  t.next_id <- t.next_id+1;
  let r={id=t.next_id;argv;cancelled=Atomic.make false;response=Atomic.make None} in
  let deadline=now () +. timeout in
  let pause () = if now () >= deadline then raise Timeout else sleep 0.01 in
  Fun.protect ~finally:(fun () ->
    Atomic.set r.cancelled true;t.notify ()) @@ fun () ->
  while not (Queue.try_push t.queue r) do pause () done;
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
  let rec loop () = match Queue.pop_opt t.queue with
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
    Atomic.set r.response (Some response)
