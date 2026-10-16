open Reader
open Protocol

let check condition message = if not condition then failwith message
let wait_until f =
  let deadline = Unix.gettimeofday () +. 10. in
  let rec loop () =
    if f () then ()
    else if Unix.gettimeofday () > deadline then failwith "test timeout"
    else (Thread.delay 0.001; loop ())
  in loop ()

let spsc_queue_test () =
  let q = Spsc_queue.create 3 in
  check (Spsc_queue.push q 1 && Spsc_queue.push q 2 && Spsc_queue.push q 3) "spsc_queue capacity";
  check (not (Spsc_queue.push q 4)) "spsc_queue overflow must reject";
  check (Spsc_queue.pop q = Some 1 && Spsc_queue.pop q = Some 2 && Spsc_queue.pop q = Some 3) "FIFO";
  check (Spsc_queue.pop q = None) "empty spsc_queue";
  let producer = Domain.spawn (fun () ->
    for i = 1 to 100_000 do
      while not (Spsc_queue.push q i) do Domain.cpu_relax () done
    done) in
  for i = 1 to 100_000 do
    let rec take () = match Spsc_queue.pop q with
      | None -> Domain.cpu_relax (); take () | Some x -> x in
    check (take () = i) "cross-domain FIFO"
  done;
  Domain.join producer

let closed_notification_test () =
  let previous = Sys.signal Sys.sigpipe Sys.Signal_default in
  let rd, wr = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock wr;
  Unix.close rd;
  Fun.protect ~finally:(fun () -> Unix.close wr; Sys.set_signal Sys.sigpipe previous) (fun () ->
    check (not (Reader.Host.notify wr)) "closed channel must report failure, not raise SIGPIPE")

let runtime_test () =
  Reader.Host.snapshot_signals ();
  let t = Runtime.create ~notify_ui:(fun () -> ()) () in
  let events = ref [] in
  let collect () =
    let rec loop () = match Runtime.take_event t with
      | None -> () | Some x -> events := x :: !events; loop () in loop () in
  let finished id = List.find_opt (function Finished (r, _) -> r.id = id | _ -> false) !events in
  let submit id owner work =
    check (Runtime.submit t (Submit {id; owner; work})) "submit rejected" in
  Fun.protect ~finally:(fun () -> Runtime.request_stop t; wait_until (fun () -> Runtime.reap t)) (fun () ->
    wait_until (fun () -> collect (); Runtime.status t = Running);
    check (Reader.Host.signals_unchanged ()) "worker changed host signal handlers";
    submit 1 (View 0) (Delay {seconds = 3.; steps = 3});
    submit 2 Service (Delay {seconds = 0.04; steps = 2});
    submit 3 Service Fail;
    check (Runtime.submit t (Close_view 0)) "close view rejected";
    wait_until (fun () -> collect (); Option.is_some (finished 1) && Option.is_some (finished 2) && Option.is_some (finished 3));
    check (match finished 1 with Some (Finished (_, Cancelled)) -> true | _ -> false) "view cancellation";
    check (match finished 2 with Some (Finished (_, Completed)) -> true | _ -> false) "service survives view close";
    check (match finished 3 with Some (Finished (_, Failed _)) -> true | _ -> false) "exception isolation";
    submit 4 Service (Allocate 100);
    wait_until (fun () -> collect (); Option.is_some (finished 4));
    check (Runtime.status t = Running) "runtime died after task failure";
    for id = 5 to 36 do submit id Service (Delay {seconds = 60.; steps = 1}) done;
    submit 37 Service Fail;
    wait_until (fun () -> collect (); Option.is_some (finished 37));
    check (match finished 37 with
      | Some (Finished (_, Failed "worker busy: 32 active jobs")) -> true
      | _ -> false) "active job limit";
    Runtime.request_stop t;
    wait_until (fun () -> Runtime.reap t);
    check (Runtime.status t = Stopped) "shutdown lifecycle";
    check (Reader.Host.signals_unchanged ()) "shutdown changed signals")

let backpressure_shutdown_test () =
  let t = Runtime.create ~notify_ui:(fun () -> ()) () in
  wait_until (fun () -> Runtime.status t = Running);
  check (Runtime.submit t (Submit {id=1; owner=Service; work=Delay {seconds=0.; steps=1000}})) "submit";
  Thread.delay 0.15;
  Runtime.request_stop t;
  wait_until (fun () -> Runtime.reap t);
  check (Runtime.status t = Stopped) "full event queue prevented shutdown"

let () =
  spsc_queue_test ();
  closed_notification_test ();
  runtime_test ();
  backpressure_shutdown_test ();
  print_endline "PASS: spsc_queue, cancellation, service ownership, failure isolation, GC, signals, shutdown under backpressure"
