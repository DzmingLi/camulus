(** Bounded file access and private atomic credential writes. File reads follow
    symlinks (for managed secrets); writes refuse to replace symlinks. These
    functions block: Eio callers should use [Eio_unix.run_in_systhread]. They
    never include secret values in errors. Cookie selection is provided by
    [Firefox_cookies], and Emacs auth-source by [Credentials_emacs]. *)
module Firefox = Firefox_cookies
exception Error of string
val read_file : ?private_file:bool -> limit:int -> string -> string
val read_secret_file : ?private_file:bool -> limit:int -> string -> string
val write_secret_file : string -> string -> unit
val nonce : unit -> string
