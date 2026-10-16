type t
type handler = Protocol.work -> emit:(Protocol.payload -> unit) -> unit
type extension = net:Eio_unix.Net.t -> now:(unit -> float) -> sleep:(float -> unit) -> handler

(** Frontend-owned handle. Call all functions below from one owner thread.
    [notify_ui] runs on the worker domain, must not call Emacs, block, or raise. *)
val create : ?extension:extension -> notify_ui:(unit -> unit) -> unit -> t

(** [false] means no command was accepted: caller must not update its local state. *)
val submit : t -> Protocol.command -> bool

(** Out-of-band shutdown: remains available when either queue is full. *)
val request_stop : t -> unit
val take_event : t -> Protocol.event option
val status : t -> Protocol.lifecycle

(** Returns [false] while work/cleanup is active. Once finished, joins the
    terminating domain and closes descriptors. Idempotent, owner-thread only. *)
val reap : t -> bool
