open Protocol

exception User_cancelled
exception Shutdown

type handler = work -> emit:(payload -> unit) -> unit
type extension = net:Eio_unix.Net.t -> now:(unit -> float) -> sleep:(float -> unit) -> handler

type t = {
  commands : command Spsc_queue.t;
  events : event Spsc_queue.t;
  stop_requested : bool Atomic.t;
  lifecycle : lifecycle Atomic.t;
  finished : bool Atomic.t;
  command_read : Unix.file_descr;
  command_write : Unix.file_descr;
  notify_ui : unit -> unit;
  extension : extension;
  mutable domain : unit Domain.t option;
}

let wake fd =
  try ignore (Unix.write fd (Bytes.of_string "!") 0 1) with
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) -> ()

let drain fd =
  let bytes = Bytes.create 1024 in
  let rec loop () =
    try if Unix.read fd bytes 0 1024 > 0 then loop () with
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in loop ()

let publish t event =
  let rec put () =
    if Atomic.get t.stop_requested then ()
    else if Spsc_queue.push t.events event then t.notify_ui ()
    else (Eio_unix.sleep 0.001; put ())
  in put ()

let worker t () =
  Fun.protect ~finally:(fun () -> Atomic.set t.finished true; t.notify_ui ()) @@ fun () ->
  try
    Eio_posix.run ~manage_signals:false @@ fun env ->
    let clock = Eio.Stdenv.mono_clock env in
    let epoch = Eio.Time.Mono.now clock in
    let handle = t.extension ~net:(Eio.Stdenv.net env) ~now:(fun () -> Mtime.Span.to_float_ns (Mtime.span epoch (Eio.Time.Mono.now clock)) /. 1e9)
      ~sleep:(Eio.Time.Mono.sleep clock) in
    let jobs = Hashtbl.create 32 in
    let start_job sw request =
      if Hashtbl.mem jobs request.id then
        publish t (Finished (request, Failed "duplicate request id"))
      else if Hashtbl.length jobs >= 32 then
        publish t (Finished (request, Failed "worker busy: 32 active jobs"))
      else begin
        let cancelled, cancel = Eio.Promise.create () in
        Hashtbl.add jobs request.id (request.owner, cancelled, cancel);
        Eio.Fiber.fork ~sw (fun () ->
          let outcome =
            try
              Eio.Fiber.first
                (fun () ->
                  match request.work with
                  | Fail -> failwith "synthetic backend failure"
                  | Delay { seconds; steps } ->
                    for step = 1 to steps do
                      Eio.Time.Mono.sleep clock (seconds /. float_of_int steps);
                      publish t (Progress (request, step))
                    done
                  | Allocate rounds ->
                    for step = 1 to rounds do
                      let batch = Array.init 500 (fun i -> String.make (128 + i mod 64) 'x') in
                      ignore (Sys.opaque_identity batch);
                      if step mod 10 = 0 then Gc.full_major ();
                      Eio.Fiber.yield ()
                    done
                  | work -> handle work ~emit:(fun data -> publish t (Data (request,data))))
                (fun () -> Eio.Promise.await cancelled; raise User_cancelled);
              Completed
            with
            | User_cancelled -> Cancelled
            | Eio.Cancel.Cancelled _ as ex -> raise ex
            | ex -> Failed (Printexc.to_string ex)
          in
          (* A fiber waiting to publish its terminal result still consumes a slot. *)
          publish t (Finished (request, outcome));
          Hashtbl.remove jobs request.id)
      end
    in
    Atomic.set t.lifecycle Running;
    publish t (Ready (Domain.self () :> int));
    (try Eio.Switch.run @@ fun sw ->
      while not (Atomic.get t.stop_requested) do
        Eio_unix.await_readable t.command_read;
        drain t.command_read;
        let rec consume budget =
          if Atomic.get t.stop_requested then raise Shutdown;
          if budget = 0 then (Eio.Fiber.yield (); consume 32)
          else match Spsc_queue.pop t.commands with
          | None -> ()
          | Some (Submit request) -> start_job sw request; consume (budget - 1)
          | Some (Cancel id) ->
            (match Hashtbl.find_opt jobs id with
             | None -> ()
             | Some (_, promise, resolver) ->
               if not (Eio.Promise.is_resolved promise) then Eio.Promise.resolve resolver ());
            consume (budget - 1)
          | Some (Close_view generation) ->
            Hashtbl.iter (fun _ (owner, promise, resolver) ->
              if owner = View generation && not (Eio.Promise.is_resolved promise) then
                Eio.Promise.resolve resolver ()) jobs;
            consume (budget - 1)
        in consume 32
      done;
      raise Shutdown
    with Shutdown -> ());
    Atomic.set t.lifecycle Stopped
  with ex -> Atomic.set t.lifecycle (Crashed (Printexc.to_string ex))

let create ?(extension = fun ~net:_ ~now:_ ~sleep:_ _ ~emit:_ -> failwith "Unsupported backend task") ~notify_ui () =
  let command_read, command_write = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock command_read;
  Unix.set_nonblock command_write;
  let t = {
    commands = Spsc_queue.create 256; events = Spsc_queue.create 512;
    stop_requested = Atomic.make false; lifecycle = Atomic.make Starting;
    finished = Atomic.make false; command_read; command_write; notify_ui; extension;
    domain = None;
  } in
  (try t.domain <- Some (Domain.spawn (worker t)) with ex ->
     Unix.close command_read; Unix.close command_write; raise ex);
  t

let submit t command =
  if Atomic.get t.stop_requested || Atomic.get t.finished then false
  else if Spsc_queue.push t.commands command then (wake t.command_write; true)
  else false

let request_stop t =
  if not (Atomic.get t.finished) then begin
    Atomic.set t.stop_requested true;
    wake t.command_write
  end

let take_event t = Spsc_queue.pop t.events
let status t = Atomic.get t.lifecycle

let reap t =
  if not (Atomic.get t.finished) then false
  else begin
    (match t.domain with
     | None -> ()
     | Some domain ->
       Domain.join domain;
       t.domain <- None;
       Unix.close t.command_read;
       Unix.close t.command_write);
    true
  end
