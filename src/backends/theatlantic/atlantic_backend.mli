val source : string
type group = string
type cursor = string
val feed_url : group -> string
val decode : url:string -> string -> Rss_backend.Decode.article list
module Make (_:Rss_backend.Http.S) : sig
 type t
 include Reader.Backend.S with type t := t and type group = group and type cursor = cursor
 val create : unit -> t
 val local_articles : t -> group -> after:cursor option ->
  previous:(string -> Yojson.Safe.t option) -> Rss_backend.Decode.article list
 val read_local : t -> group -> string ->
  previous:(string -> Yojson.Safe.t option) -> Rss_backend.Decode.article
 val archive_local : Rss_backend.Decode.article -> Reader.Content_state.archived
 val attachment_local : Rss_backend.Decode.article -> int -> string -> unit
 val read : t -> group -> string -> Rss_backend.Decode.article
 val archive : t -> group -> string -> Reader.Content_state.archived
 val attachment : t -> group -> string -> int -> string -> unit
end
module Live : module type of Make(Rss_backend.Transport)
