(** Local Maildir operations. Files contain RFC822, including all MIME parts. *)
exception Conflict
let valid_group group =
  String.starts_with ~prefix:"/" group &&
  List.for_all (fun s -> s<>"." && s<>".." && not(String.contains s '\000'))
    (String.split_on_char '/' group)
let folder ~root group =
  if not(valid_group group) then invalid_arg "Invalid Maildir group";
  let root=Unix.realpath root in
  let rec ensure path =
    if not(Sys.file_exists path) then begin ensure(Filename.dirname path);Unix.mkdir path 0o700 end;
    let actual=Unix.realpath path in
    if actual<>root && not(String.starts_with ~prefix:(root^"/") actual) then
      invalid_arg "Maildir escapes its configured root";
    if (Unix.stat path).Unix.st_kind<>Unix.S_DIR then invalid_arg "Not a directory"
  in
  let path=root ^ group in
  ensure path;List.iter(fun p -> ensure(Filename.concat path p)) ["cur";"new";"tmp"];path
let fsync_dir path =
  let fd=Unix.openfile path [Unix.O_RDONLY;Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd)(fun () -> Unix.fsync fd)
let read path =
  let ch=open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ch) @@ fun () ->
  let len=in_channel_length ch in
  if len>2*1024*1024 then invalid_arg "Draft exceeds 2 MiB";
  really_input_string ch len
let revision source = Digest.to_hex(Digest.string source)
let write_atomic ~dir ~path ~expected source =
  (* No effect suspension between checking the revision and replacing the file.
     Backend serialization orders its own writers; revision detects other editors. *)
  (match expected with
   | None -> if Sys.file_exists path then raise Conflict
   | Some rev -> if not(Sys.file_exists path) || revision(read path)<>rev then raise Conflict);
  let tmp,ch=Filename.open_temp_file ~temp_dir:(Filename.concat dir "tmp")
    ~mode:[Open_binary] ~perms:0o600 "ecaml-" "" in
  try
    output_string ch source;flush ch;Unix.fsync(Unix.descr_of_out_channel ch);close_out ch;
    (match expected with
     | None -> Unix.link tmp path;Unix.unlink tmp
     | Some _ -> Unix.rename tmp path);
    fsync_dir(Filename.dirname path);
    revision source
  with ex -> close_out_noerr ch;(try Unix.unlink tmp with _ -> ());raise ex

let draft_path ~root key =
  let group=Filename.dirname(Filename.dirname key) in
  if Filename.basename group<>"Drafts" || Filename.basename(Filename.dirname key)<>"cur"
     || Filename.basename key="" || not(valid_group key) then invalid_arg "Invalid draft key";
  let dir=folder ~root group in
  let path=dir^"/cur/"^Filename.basename key in
  if Sys.file_exists path && Unix.realpath path<>path then invalid_arg "Draft symlinks are not supported";
  group,dir,path
