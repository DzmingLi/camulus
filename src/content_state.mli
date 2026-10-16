(** Durable local state. All database work runs off the Emacs thread. *)
type t
type target = Article | Thread
type archived = { message : Content.message; attachments : Yojson.Safe.t; raw : string option; files : (int * string) list }
val with_db : string -> (t -> 'a) -> 'a
(* [group=None] only records content identity. [Some group] caches a page
    explicitly fetched from that backend entry; it never subscribes to it. *)
val observe : ?replace:bool -> t -> source:string -> group:string option -> Content.message list -> unit
val snapshots : t -> source:string -> string list -> Yojson.Safe.t
val mark_read : t -> source:string -> string list -> bool -> unit
val legacy_groups : t -> (string * string) list
val feed : t -> source:string -> group:string -> Yojson.Safe.t
val timeline : t -> entries:(string * string) list -> Yojson.Safe.t
val dismiss_timeline : t -> source:string -> id:string -> through:int -> unit
val search_rows : t -> source:string -> Content.message list -> Yojson.Safe.t
val dismiss : t -> source:string -> group:string -> id:string -> through:int -> unit
val folders : t -> string list
val mkdir : t -> string -> unit
val remove_folder : t -> string -> unit
val save_archive : t -> source:string -> target:target -> id:string -> folder:string -> archived list -> unit
val favorites : t -> folder:string -> Yojson.Safe.t
val memberships : t -> (string * target * string * string) list
val remove_favorite : t -> source:string -> target:target -> id:string -> folder:string -> unit
val load_archive : t -> source:string -> target:target -> id:string -> Yojson.Safe.t
val raw_archive : t -> source:string -> id:string -> string
val message_json : Content.message -> Yojson.Safe.t

type operation =
  | States of string * string list | Read of string * string list * bool
  | Legacy_groups | Feed of string * string
  | Timeline of (string * string) list
  | Dismiss_timeline of string * string * int
  | Refresh of string * string * string option * string option | Refresh_archives
  | Dismiss of string * string * string * int
  | Folders | Mkdir of string | Remove_folder of string | Favorites of string
  | Favorite of string * target * string * string
  | Unfavorite of string * target * string * string
  | Archive of string * target * string
  | Attachment of string * string * int * string
val operation_of_json : Yojson.Safe.t -> operation
val native_thread : t -> source:string -> id:string -> string option
val copy_favorite : t -> source:string -> target:target -> id:string -> folder:string -> bool
val refresh_archive : t -> source:string -> target:target -> id:string -> archived list -> unit

val attachment_archive : t -> source:string -> id:string -> part:int -> string option

val groups_for_item : t -> source:string -> id:string -> string list
