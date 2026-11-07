(** Camulus-owned, read-only Firefox credential provider. Creation does not
    access the browser; [get] runs filesystem/SQLite work off the Eio domain.
    No browser profile is discovered implicitly. *)
type t
val create : ?profile:string -> unit -> t

val get : t -> string -> (string * string) list
(** [get provider url] returns only unexpired cookies matching the HTTPS
    destination's domain and path, from Firefox's default container. *)

val header : (string * string) list -> string

val get_sync : t -> string -> (string * string) list
(** Blocking variant for synchronous external consumers such as Emacs cookie
    callbacks. Eio backends should use [get] to keep their domain responsive. *)
