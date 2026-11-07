(** Shared credential storage; authorization protocols remain backend-owned. *)
module Firefox = Firefox_cookies
exception Error of string
let fail s=raise(Error s)
let read_file ?(private_file=false) ~limit path =
  if limit<=0 then invalid_arg "Invalid credential size limit";
  try
    let fd=Unix.openfile path [Unix.O_RDONLY;Unix.O_CLOEXEC;Unix.O_NONBLOCK] 0 in
    Fun.protect ~finally:(fun()->Unix.close fd) @@ fun()->
    let st=Unix.fstat fd in
    if st.Unix.st_kind<>Unix.S_REG || st.st_size>limit then fail "Credential must be a regular file within its size limit";
    if private_file && st.st_perm land 0o077<>0 then fail "Credential file must not be accessible to other users";
    let bytes=Bytes.create st.st_size in
    let rec read n=if n<Bytes.length bytes then
      let count=Unix.read fd bytes n(Bytes.length bytes-n) in
      if count=0 then fail "Credential file changed while reading" else read(n+count) in
    read 0;Bytes.to_string bytes
  with Unix.Unix_error _->fail "Cannot read credential file"
let read_secret_file ?private_file ~limit path =
  let value=read_file ?private_file ~limit path |> String.trim in
  if value="" || String.contains value '\000' then fail "Empty or invalid credential file";
  value
let nonce () =
  let ch=open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun()->close_in_noerr ch)(fun()->
    really_input_string ch 32 |> String.to_seq |> Seq.map(fun c->Printf.sprintf "%02x"(Char.code c)) |> List.of_seq |> String.concat "")
let write_secret_file path value =
  if value="" || String.exists(fun c->c='\r'||c='\n'||c='\000')value then invalid_arg "Invalid credential";
  (try if (Unix.lstat path).st_kind=Unix.S_LNK then fail "Cannot replace a symlink with a stored credential"
   with Unix.Unix_error(Unix.ENOENT,_,_)->());
  let dir=Filename.dirname path in
  if not(Sys.file_exists dir)then Unix.mkdir dir 0o700;
  let temp=Filename.concat dir(".credential-"^nonce()) in
  let fd=Unix.openfile temp [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_EXCL;Unix.O_CLOEXEC] 0o600 in
  let closed=ref false in
  Fun.protect ~finally:(fun()->if not !closed then Unix.close fd;
    if Sys.file_exists temp then Sys.remove temp) @@ fun()->
  let bytes=Bytes.of_string(value^"\n")in
  let rec write n=if n<Bytes.length bytes then
    let count=Unix.write fd bytes n(Bytes.length bytes-n)in
    if count=0 then fail "Cannot store credential" else write(n+count) in
  write 0;Unix.fsync fd;Unix.close fd;closed:=true;Unix.rename temp path
