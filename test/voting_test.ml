open Reader
open Content
module V = Voting
let check condition message = if not condition then failwith message
let state t id = Option.get (V.find t id)
let finish t request barrier status = V.finish t ~id:"post" ~request ~barrier ~status

let () =
  let t = V.create () in
  V.start t ~id:"post" ~request:10 ~previous:Downvote ~direction:Upvote;
  check ((state t "post").direction = Upvote && V.is_pending t "post") "optimistic vote";
  (match V.start t ~id:"post" ~request:11 ~previous:Upvote ~direction:No_vote with
   | () -> failwith "overlapping vote accepted" | exception Invalid_argument _ -> ());
  V.start t ~id:"other" ~request:11 ~previous:No_vote ~direction:Downvote;
  check (V.is_pending t "other") "independent items may proceed";
  ignore (V.observe t ~id:"post" ~request:12 ~direction:Downvote);
  check ((state t "post").direction=Upvote) "read during write cannot erase intent";
  ignore (finish t 10 12 V.Confirmed);
  check (not (V.is_pending t "post")) "confirmed operation releases pending guard";
  ignore (V.observe t ~id:"post" ~request:12 ~direction:Downvote);
  check ((state t "post").direction=Upvote) "in-flight stale read cannot overwrite confirmation";
  ignore (V.observe t ~id:"post" ~request:14 ~direction:No_vote);
  ignore (V.observe t ~id:"post" ~request:13 ~direction:Downvote);
  check ((state t "post").direction=No_vote) "later remote state wins across views";
  V.start t ~id:"post" ~request:15 ~previous:No_vote ~direction:Downvote;
  check (finish t 10 15 (V.Rejected "old failure")=None) "late result cannot settle new vote";
  check (V.is_pending t "post") "late result leaves current operation pending";
  ignore (finish t 15 15 (V.Rejected "missing permission"));
  check ((state t "post").direction=No_vote) "definitive rejection restores previous vote";
  check (finish t 15 15 (V.Uncertain "finished after error")=None) "duplicate final event ignored";
  V.start t ~id:"post" ~request:16 ~previous:No_vote ~direction:Upvote;
  ignore (finish t 16 16 (V.Uncertain "connection lost"));
  check ((state t "post").direction=Upvote && (state t "post").status=V.Uncertain "connection lost")
    "unknown outcome must not pretend remote rollback";
  ignore (V.observe t ~id:"post" ~request:17 ~direction:Downvote);
  check ((state t "post").direction=Downvote && (state t "post").status=V.Confirmed)
    "explicit refresh reconciles unknown outcome";
  ignore (V.observe t ~id:"unmodified" ~request:18 ~direction:No_vote);
  check (V.find t "unmodified"=None) "ordinary browsing does not grow overlay state";
  V.clear t;
  check (V.find t "post"=None && V.find t "other"=None) "session reset";
  print_endline "PASS: optimistic votes, overlap guard, stale reads/results, rejection, uncertainty and reconciliation"
