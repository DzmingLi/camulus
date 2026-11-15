module M=Mu_backend
let check value message=if not value then failwith message
let contains text pattern =
 let n=String.length pattern in
 let rec loop i=i+n<=String.length text && (String.sub text i n=pattern || loop(i+1))in loop 0
let response ?(stdout="") status : M.process_result={status;stdout}
let ()=Eio_posix.run(fun _->
 let root=Filename.temp_file "camulus-trash-" "" in
 Sys.remove root;Unix.mkdir root 0o700;
 let rec mkdir path=if not(Sys.file_exists path)then(mkdir(Filename.dirname path);Unix.mkdir path 0o700)in
 let rec remove path=if Sys.is_directory path then(Array.iter(fun name->remove(Filename.concat path name))(Sys.readdir path);Unix.rmdir path)else Sys.remove path in
 Fun.protect ~finally:(fun()->remove root)(fun()->
  let path group name=let dir=root^group^"/cur" in mkdir dir;let p=dir^"/"^name in let ch=open_out p in close_out ch;p in
  let a=path "/work/Inbox" "a" and b=path "/work/Inbox" "b" and sent=path "/work/Sent" "copy" in
  let header id path group=`Assoc["message-id",`String id;"path",`String path;"maildir",`String group]in
  let rows=`List[header "a@example.test" a "/work/Inbox";header "b@example.test" b "/work/Inbox";header "a@example.test" sent "/work/Sent"]in
  let started,start=Eio.Promise.create()and released,release=Eio.Promise.create()in
  let moves=ref 0 and indexed=ref false in
  let run ?timeout:_ argv =match argv with
   | "fixture-sync"::_->Eio.Promise.resolve start ();Eio.Promise.await released;response 0
   | "mu"::"index"::_->indexed:=true;response 0
   | "mu"::"find"::_->check !indexed "Move queried before sync/index released the mutation lock";response ~stdout:(Yojson.Safe.to_string rows) 0
   | "mu"::"move"::_->
     if List.mem "--dry-run" argv then response 0 else(
      incr moves;check(List.hd(List.rev argv)="/work/Trash") "Account Trash destination";
      check(not(List.mem sent argv)) "Sent copy must remain";
      response(if !moves=1 then 0 else 1))
   | _->failwith "Unexpected command" in
  let config=M.config ~root ~muhome:(root^"/mu") ~sender:"reader@example.test" in
  let backend=M.create ~config ~run ~submit:(fun ~profile:_ ~sender:_ ~recipients:_ _->assert false)in
  let progress=ref []and result=ref None in
  let queued,queue=Eio.Promise.create()in
  Eio.Fiber.both
   (fun()->M.execute backend (M.Sync["fixture-sync"])~emit:ignore)
   (fun()->
    Eio.Promise.await started;
    Eio.Fiber.both
     (fun()->M.execute backend (M.Trash("/work/Inbox","mail:a@example.test",true))~emit:(function
       | M.Progress message->progress:= !progress@[message];if not(Eio.Promise.is_resolved queued)then Eio.Promise.resolve queue ()
       | M.Trashed value->result:=Some value
       | _->failwith "Unexpected move event"))
     (fun()->
      Eio.Promise.await queued;
      check(!moves=0 && not !indexed) "Move ran concurrently with synchronization";
      check(List.length !progress=1 && contains(List.hd !progress)"Waiting") "No progress while queued";
      Eio.Promise.resolve release ()));
  let moved=Option.get !result in
  check(moved.moved=["mail:a@example.test"] && moved.failed=1) "Partial move must report actual success/failure";
  check(List.exists(fun x->contains x "(1/2)")!progress && List.exists(fun x->contains x "(2/2)")!progress) "Per-message progress";
  let json=M.result_json(M.Progress "Waiting")in
  check(List.assoc "kind"(Yojson.Safe.Util.to_assoc json)=`String "progress") "Progress event wire format";
  Printf.printf "PASS Trash reports lock waits, stays asynchronous, preserves Sent and reports partial failure\n"))
