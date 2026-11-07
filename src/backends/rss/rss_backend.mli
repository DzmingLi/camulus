(** RSS 2.0, RSS 1.0/RDF and Atom 1.0. Each feed URL is a source;
    entries are independent articles, without a Threading capability. *)
module Article = Article
module Http = Reader.Http
module Transport : Http.S
module Decode = Decode
module Feed_xml = Feed_xml
val source : string
val source_of_url : string -> string
val url_of_source : string -> string
val is_source : string -> bool
type group = string
type cursor = string
type article = Decode.article
val article_json : article -> Yojson.Safe.t
module Make (_:Http.S) : sig
  type 'a cache
  type t
  include Reader.Backend.S with type t := t and type group = group and type cursor = cursor
  val create : ?decode:(url:string -> string -> article list) -> unit -> t

  (** Cache the complete decoded response, including any pagination metadata. *)
  val create_cache : decode:(url:string -> string -> 'a) -> unit -> 'a cache
  val fetch_cached : 'a cache -> string -> string * 'a
  val download_url : string -> string
  val read : t -> group -> string -> article
  val archive : t -> group -> string -> Reader.Content_state.archived
  val attachment : t -> group -> string -> int -> string -> unit
end
module Live : module type of Make(Transport)
