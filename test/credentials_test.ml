module C=Reader.Credentials
let check b why=if not b then failwith why
let rejected f=try ignore(f());false with C.Error _|Invalid_argument _->true
let ()=
 let dir=Filename.temp_file "camulus-credentials" ""in Sys.remove dir;Unix.mkdir dir 0o700;
 Fun.protect ~finally:(fun()->Array.iter(fun name->Sys.remove(Filename.concat dir name))(Sys.readdir dir);Unix.rmdir dir)(fun()->
  let path=Filename.concat dir "secret"in
  C.write_secret_file path "fixture-token";
  check((Unix.stat path).Unix.st_perm land 0o777=0o600)"private file mode";
  check(C.read_secret_file ~private_file:true ~limit:4096 path="fixture-token")"read saved secret";
  check(rejected(fun()->C.read_secret_file ~limit:3 path))"bounded read";
  Unix.chmod path 0o644;
  check(rejected(fun()->C.read_secret_file ~private_file:true ~limit:4096 path))"private-file access policy";
  check(C.read_secret_file ~limit:4096 path="fixture-token")"managed credential permissions are caller policy";
  let alias=Filename.concat dir "managed"in Unix.symlink path alias;
  check(C.read_secret_file ~limit:4096 alias="fixture-token")"read managed symlink";
  check(rejected(fun()->C.write_secret_file alias "replacement"))"never overwrite managed symlink";
  check(C.read_secret_file ~limit:4096 path="fixture-token")"symlink target retained";
  C.write_secret_file path "second-token";
  check(C.read_secret_file ~private_file:true ~limit:4096 path="second-token")"atomic replacement restores private permissions";
  check(rejected(fun()->C.write_secret_file path "bad\nvalue"))"reject line injection";
  let fifo=Filename.concat dir "fifo"in Unix.mkfifo fifo 0o600;
  check(rejected(fun()->C.read_secret_file ~limit:4096 fifo))"reject non-regular file without blocking";
  check(Array.length(Sys.readdir dir)=3)"temporary credential files cleaned up";
  let a=C.nonce()and b=C.nonce()in check(String.length a=64 && a<>b)"fresh random challenges");
 print_endline "PASS shared credentials: bounded reads, permission policy, managed symlinks, atomic writes and cleanup"
