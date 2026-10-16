(** Bounded, nonblocking single-producer/single-consumer queue.
    Use one producer domain and one consumer domain; payloads are immutable
    after publication. Fibers must not yield within push/pop. *)
type 'a t
val create : int -> 'a t
val push : 'a t -> 'a -> bool
val pop : 'a t -> 'a option
