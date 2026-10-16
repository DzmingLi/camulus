(** Read only the explicitly configured Firefox profile, without persisting credentials in the reader configuration.
    The caller chooses the destination; cookies never accompany media downloads. *)
let with_snapshot file f =
 let directory=Filename.temp_file "ecaml-browser-session-" ".private" in
 Sys.remove directory;Unix.mkdir directory 0o700;
 Fun.protect ~finally:(fun()->
  Array.iter(fun name->try Sys.remove(Filename.concat directory name) with _->())(Sys.readdir directory);
  Unix.rmdir directory)(fun()->
   let target=Filename.concat directory "cookies.sqlite" in
   let stamp path=try let s=Unix.stat path in Some(s.Unix.st_ino,s.st_size,s.st_mtime,s.st_ctime) with Unix.Unix_error(Unix.ENOENT,_,_)->None in
   let copy source destination=
    let ic=open_in_bin source in
    Fun.protect ~finally:(fun()->close_in_noerr ic)(fun()->
     let fd=Unix.openfile destination [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC;Unix.O_CLOEXEC] 0o600 in
     let oc=Unix.out_channel_of_descr fd in
     Fun.protect ~finally:(fun()->close_out_noerr oc)(fun()->
      let bytes=Bytes.create 65536 and total=ref 0 in
      let rec loop()=let n=input ic bytes 0 (Bytes.length bytes) in
       if n>0 then (total:= !total+n;if !total>128*1024*1024 then failwith "Cookie database exceeds snapshot limit";
        output oc bytes 0 n;loop()) in loop())) in
   let rec attempt remaining=
    let before=List.map(fun suffix->stamp(file^suffix))["";"-wal"] in
    List.iter(fun suffix->if Sys.file_exists(file^suffix) then copy(file^suffix)(target^suffix)
      else if Sys.file_exists(target^suffix) then Sys.remove(target^suffix))["";"-wal"];
    let after=List.map(fun suffix->stamp(file^suffix))["";"-wal"] in
    if before<>after then (if remaining>0 then attempt(remaining-1) else failwith "Firefox cookie database is changing; retry shortly")
    else f target in attempt 2)
let get ~profile url =
 let u=Uri.of_string url in
 let host=Option.value (Uri.host u) ~default:"" |> String.lowercase_ascii in
 let path=match Uri.path u with ""->"/" | p->p in
 if Uri.scheme u<>Some "https" || host="" then invalid_arg "Cookies require an HTTPS destination";
 Eio_unix.run_in_systhread(fun () ->
  let file=Filename.concat profile "cookies.sqlite" in
  if not(Sys.file_exists file) then failwith "Firefox cookies.sqlite not found in the configured profile";
  let read file =
  let db=Sqlite3.db_open ~mode:`READONLY file in
  Fun.protect ~finally:(fun()->ignore(Sqlite3.db_close db)) (fun () ->
   Sqlite3.busy_timeout db 1000;
   let st=Sqlite3.prepare db "SELECT host,path,name,value,expiry FROM moz_cookies WHERE originAttributes='' ORDER BY length(path) DESC,creationTime ASC" in
   Fun.protect ~finally:(fun()->ignore(Sqlite3.finalize st)) (fun () ->
    let rec read count acc = match Sqlite3.step st with
    | Sqlite3.Rc.ROW ->
      if count>100000 then failwith "Firefox cookie row limit exceeded";
      let str i=Sqlite3.column_text st i in
      let domain=str 0 and cp=str 1 in
      let domain_ok=domain=host || (String.starts_with ~prefix:"." domain &&
        (host=String.sub domain 1 (String.length domain-1) || String.ends_with ~suffix:domain host)) in
      let path_ok=path=cp || (String.starts_with ~prefix:cp path &&
        (String.ends_with ~suffix:"/" cp || (String.length path>String.length cp && path.[String.length cp]='/'))) in
      let expiry=Int64.to_float(Sqlite3.column_int64 st 4) in
      let live=expiry=0. || expiry>Unix.gettimeofday () in
      let acc=if domain_ok && path_ok && live then (str 2,str 3)::acc else acc in
      read(count+1)acc
    | Sqlite3.Rc.DONE -> List.rev acc
    | _ -> failwith "Cannot read Firefox cookies" in read 0 [])) in
  try read file with Sqlite3.Error _ -> with_snapshot file read)
let header pairs =
 List.iter(fun(k,v)->if String.exists(fun c->c='\r'||c='\n'||c='\000') (k^v) then failwith "Invalid cookie")pairs;
 String.concat "; " (List.map(fun(k,v)->k^"="^v)pairs)
