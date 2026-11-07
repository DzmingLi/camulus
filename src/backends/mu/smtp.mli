(** Colombe SMTP over authenticated implicit TLS, driven by Eio on the worker.
    This is a sending capability of the Maildir backend, not a core requirement. *)
type config = { host : string; port : int; username : string; password_file : string; ca_file : string }
exception Error of string
val validate : config -> unit
val send : net:Eio_unix.Net.t -> sleep:(float -> unit) -> ?timeout:float -> config ->
  sender:string -> recipients:string list -> string -> unit
(** A single attempt. A failure after DATA can be uncertain; callers must not
    automatically replay it. Credentials are read only here, never logged. *)
