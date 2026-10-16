(** Persistent group following for backends that explicitly choose local state.
    This is independent of group registration and remote topic tracking. *)
type target = { backend : string; source : string; group : string }
type t
include Backend.Subscription with type t := t and type target := target and type settings = bool
val create : string -> t
val followed : t -> backend:string -> target list
