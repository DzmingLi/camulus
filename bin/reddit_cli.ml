module R = Reddit_backend

let () =
  try
    let operation = match Array.to_list Sys.argv with
      | [_;"identity"] -> R.Identity
      | [_;"subreddit";name] -> R.List (R.Subreddit name,None)
      | [_;"thread";id] -> R.Thread id
      | [_;"search";name;query] -> R.Search (R.Subreddit name,query,None)
      | [_;"notifications"] -> R.List (R.Notifications,None)
      | _ -> failwith "Usage: reddit_cli identity | subreddit NAME | thread t3_ID | search NAME QUERY | notifications"
    in
    let config = R.config ~username:(Sys.getenv "REDDIT_USERNAME")
      ~token_file:(Sys.getenv "REDDIT_TOKEN_FILE") in
    Eio_posix.run ~manage_signals:false @@ fun env ->
    let clock = Eio.Stdenv.mono_clock env in
    let epoch = Eio.Time.Mono.now clock in
    let t = R.Live.create ~config ~now:(fun () -> Mtime.Span.to_float_ns (Mtime.span epoch (Eio.Time.Mono.now clock)) /. 1e9)
      ~sleep:(Eio.Time.Mono.sleep clock) in
    R.Live.execute t operation ~emit:(fun result ->
      if Sys.getenv_opt "REDDIT_SUMMARY" = Some "1" then
        (match result with
         | R.Page p -> Printf.printf "page: posts=%d notifications=%d has_next=%b\n%!"
             (List.length p.messages) (List.length p.notifications) (Option.is_some p.after)
         | R.Thread_batch b -> Printf.printf "thread batch: messages=%d gaps=%d complete=%b\n%!"
             (List.length b.messages) (List.length b.gaps) b.complete
         | R.Identity_result _ -> print_endline "identity: authenticated"
         | _ -> print_endline "result received")
      else print_endline (Yojson.Safe.to_string (R.result_json result)))
  with
  | Not_found -> prerr_endline "Set REDDIT_USERNAME and REDDIT_TOKEN_FILE (file path, never token text)"; exit 2
  | ex -> prerr_endline (Printexc.to_string ex); exit 1
