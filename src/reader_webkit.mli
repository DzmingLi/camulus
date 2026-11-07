(** Shared Emacs-thread WebKit capability. Each page has an isolated browser
    context, no persistent cookies, and a bounded, fixed-name message channel. *)
type t
val available : unit -> bool
val open_page :
  title:string -> url:string -> channel:string -> javascript:string ->
  on_message:(string -> unit) -> on_close:(unit -> unit) ->
  on_error:(string -> unit) -> t
val close : t -> unit
