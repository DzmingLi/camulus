(** RSS 2.0, RSS 1.0/RDF and Atom 1.0. Each feed URL is a source;
    entries are independent articles, without a Threading capability. *)
module Article = Article
module Http = Http
module Curl_transport = Curl_transport
module Decode = Decode
val source : string
val source_of_url : string -> string
val url_of_source : string -> string
val is_source : string -> bool
type group = string
type cursor = string
type article = Decode.article
val article_json : article -> Yojson.Safe.t
type operation = List of group | Search of group * string | Read of group * string | Attachment of group * string * int * string
val operation_of_json : Yojson.Safe.t -> operation
val operation_url : operation -> string
module Make (_:Http.S) : sig
  type t
  include Reader.Backend.S with type t := t and type group = group and type cursor = cursor
  module Subscription : Reader.Backend.Subscription with type t = t and type target = string and type settings = bool
  val create : ?subscriptions_file:string -> ?decode:(url:string -> string -> article list) -> unit -> t
  val followed : t -> string list
  val fetch_articles : t -> string -> string * article list
  val download_url : ?limit:int -> string -> string
  val read : t -> group -> string -> article
  val archive : t -> group -> string -> article * (int * string) list
  val attachment : t -> group -> string -> int -> string -> unit
end
module Live : module type of Make(Curl_transport)

module Subscription : Reader.Backend.Subscription with type t = Live.t and type target = string and type settings = bool

val followed : Live.t -> string list
