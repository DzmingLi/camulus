val source : string
type group = string
type cursor = string
val feed_url : string
val full_post : Rss_backend.Decode.article -> string -> Rss_backend.Decode.article
module Make (_:Rss_backend.Http.S) : sig
 type t
 include Reader.Backend.S with type t := t and type group = group and type cursor = cursor
 val create : unit -> t
 val read : t -> group -> string -> Rss_backend.Decode.article
 val archive : t -> group -> string -> Reader.Content_state.archived
 val attachment : t -> group -> string -> int -> string -> unit
end
module Live : module type of Make(Rss_backend.Curl_transport)
module Readable : Reader.Backend.S with type t=Live.t and type group=group and type cursor=cursor
