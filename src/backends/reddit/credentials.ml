let read path =
  try
    let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
    let st = Unix.fstat fd in
    if st.Unix.st_kind <> Unix.S_REG || st.st_size > 4096 || st.st_perm land 0o077 <> 0 then
      failwith "Refresh token must be a private regular file (mode 0600)";
    let bytes = Bytes.create st.st_size in
    let rec loop offset =
      if offset < Bytes.length bytes then
        let n = Unix.read fd bytes offset (Bytes.length bytes-offset) in
        if n = 0 then failwith "Could not read refresh token" else loop (offset+n)
    in loop 0;
    let token = String.trim (Bytes.to_string bytes) in
    if token = "" || String.exists (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false) token then
      failwith "Invalid refresh token file";
    token
  with Unix.Unix_error _ -> failwith "Cannot read refresh token; configure a token file or authorize Reddit"

let random_state () =
  let ch = open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let bytes = really_input_string ch 32 in
  String.to_seq bytes |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat ""

let save path token =
  if token = "" || String.exists (function '\r' | '\n' -> true | _ -> false) token then
    invalid_arg "Invalid refresh token";
  (try if (Unix.lstat path).st_kind = Unix.S_LNK then
     failwith "Choose a writable token file; authorization cannot replace a symlink"
   with Unix.Unix_error (Unix.ENOENT,_,_) -> ());
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  let temp = Filename.concat dir (".reddit-token-" ^ random_state ()) in
  let fd = Unix.openfile temp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
  let closed = ref false in
  Fun.protect ~finally:(fun () ->
    if not !closed then Unix.close fd;
    if Sys.file_exists temp then Sys.remove temp) @@ fun () ->
  let bytes = Bytes.of_string (token ^ "\n") in
  let rec write n = if n < Bytes.length bytes then
    let count = Unix.write fd bytes n (Bytes.length bytes-n) in
    if count = 0 then failwith "Cannot save refresh token" else write (n+count) in
  write 0; Unix.fsync fd; Unix.close fd; closed := true;
  Unix.rename temp path
