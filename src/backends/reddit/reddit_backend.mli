module Http : module type of Http
module Curl_transport : Http.S
module Credentials : sig
  val read : string -> string
  val save : string -> string -> unit
  val random_state : unit -> string
end
module Decode : module type of Decode
include module type of Errors

val default_client_id : string
val default_redirect_uri : string
val scopes : string list
type config = {
  username : string; token_file : string; client_id : string;
  redirect_uri : string; request_interval : float;
}
val config : username:string -> token_file:string -> config
val validate_config : config -> unit
type group = Subreddit of string | Post of string | Notifications
type draft = Comment of { parent : string; text : string }
           | Text_post of { subreddit : string; title : string; text : string }
type receipt = Comment_created of Reader.Content.message | Post_created of string
type operation =
  | Identity
  | List of group * string option
  | Search of group * string * string option
  | Thread of string
  | Vote of string * Reader.Content.vote
  | Reply of string * string
  | Submit of string * string * string
  | Authorize
  | Complete_authorization of string
type result =
  | Identity_result of string
  | Page of Reader.Content.page
  | Thread_batch of Reader.Content.thread_batch
  | Voted of string * Reader.Content.vote
  | Replied of Reader.Content.message
  | Submitted of string
  | Authorization_url of string
  | Authorized
  | Backend_error of error

(** A session belongs to one Eio domain. Tokens, identity and OAuth state are
    encapsulated; concurrent fibers share refresh and request pacing.
    [Http.S] is injectable for deterministic protocol tests. *)
module Make (_ : Http.S) : sig
  type t
  type nonrec group = group
  type cursor = string
  include Reader.Backend.S with type t := t and type group := group and type cursor := cursor
  include Reader.Backend.Threading with type t := t
  include Reader.Backend.Voting with type t := t
  val reply : t -> parent:string -> text:string -> Reader.Content.message
  val submit : t -> subreddit:string -> title:string -> text:string -> string
  val create : config:config -> now:(unit -> float) -> sleep:(float -> unit) -> t
  val authorize : t -> string
  val complete_authorization : t -> string -> unit
  val execute : t -> operation -> emit:(result -> unit) -> unit
end
module Live : module type of Make(Curl_transport)
module Readable : Reader.Backend.S with type t = Live.t and type group = group and type cursor = string
module Threading : Reader.Backend.Threading with type t = Live.t
module Voting : Reader.Backend.Voting with type t = Live.t
module Writing : Reader.Backend.Writing with type t = Live.t
  and type draft = draft and type receipt = receipt

(** JSON is used only at the Emacs/CLI boundary, not for cross-domain messages. *)
val operation_of_json : Yojson.Safe.t -> operation
val result_json : result -> Yojson.Safe.t
