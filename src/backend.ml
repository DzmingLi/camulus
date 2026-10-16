(** Backend-owned session, group and pagination types. Native stable thread IDs
    are not imposed on backends that reconstruct conversations (such as mail). *)
module type S = sig
  type t
  type group
  type cursor
  val source : string
  val list : t -> group -> after:cursor option -> Content.page
  val search : t -> group -> query:string -> after:cursor option -> Content.page
end

(** Optional capabilities: a backend need not implement them to be readable. *)
module type Threading = sig
  type t
  val thread : t -> string -> emit:(Content.thread_batch -> unit) -> unit
end

module type Voting = sig
  type t
  val vote : t -> id:string -> direction:Content.vote -> unit
end

(** A one-sided like, independent of up/down voting. Set the desired state
    explicitly: replaying a toggle would reverse the user's intent. *)
module type Liking = sig
  type t
  val set_like : t -> id:string -> liked:bool -> unit
end

module type Writing = sig
  type t
  type draft
  type receipt
  val send : t -> draft -> receipt
end

(** Optional backend-owned following policy. Targets and settings deliberately
    remain abstract: an RSS URL and a forum topic need different policies.
    [get] reads the backend's current authority, including remote changes;
    [set] sets an absolute policy, never toggles it. Neither creates a group.
    Listing personal content uses [S.list], independently of this capability. *)
module type Subscription = sig
  type t
  type target
  type settings
  val get : t -> target -> settings
  val set : t -> target -> settings -> unit
end
