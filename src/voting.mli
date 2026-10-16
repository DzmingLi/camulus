(** Optimistic vote state owned by the UI domain, with no Ecaml values.
    This is session state, not a persistent offline queue. *)
type status = Pending | Confirmed | Rejected of string | Uncertain of string
type update = { id : string; request : int; direction : Content.vote; status : status }
type t
val create : unit -> t
val clear : t -> unit
val find : t -> string -> update option
val is_pending : t -> string -> bool
val start : t -> id:string -> request:int -> previous:Content.vote -> direction:Content.vote -> unit

(** Only the matching pending request may settle. [barrier] is the latest
    issued request ID, so reads already in flight cannot overwrite this result. *)
val finish : t -> id:string -> request:int -> barrier:int -> status:status -> update option

(** A read issued after settlement updates the overlay shared by all views.
    Unmodified items do not allocate state. *)
val observe : t -> id:string -> request:int -> direction:Content.vote -> update option
