(* Opt-in, read-only validation. Token arrives over stdin and is never logged. *)
let () =
 let token=read_line () in
 Eio_posix.run(fun _ ->
  let backend=Github_backend.Live.create(Github_backend.config token) in
  Array.to_list Sys.argv |> List.tl |> List.iter(fun group ->
   let p=Github_backend.Live.list backend (Github_backend.group_of_string group) ~after:None in
   Printf.printf "%s: %d topics\n%!" group (List.length p.messages);
   match p.messages with []->()|m::_->
    let count=ref 0 and complete=ref false in
    Github_backend.Live.thread backend m.id ~emit:(fun b->count:= !count+List.length b.messages;complete:=b.complete);
    if not !complete then failwith "Incomplete thread";
    ignore(Github_backend.Live.read backend m.id);
    Printf.printf "  thread: %d messages, complete\n%!" !count))
