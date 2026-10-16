module Http = Http
module Decode = Decode
val source : string
val source_of_site : string -> string
val site_of_source : string -> string
val is_source : string -> bool
type config = {site:string;api_key:string;client_id:string}
val config : ?api_key:string -> ?client_id:string -> string -> config
type group = Latest | Category of int | Topic of int | Watched_topics | Tracked_topics | Notifications | Replies
type cursor = string
val group_of_string : string -> group
val group_string : group -> string
type draft = Reply of {parent:string;text:string} | New_topic of {category:int;title:string;text:string}
type receipt = Decode.article
type tracking = Regular | Tracking | Watching | Muted
val tracking_code : tracking -> int
val tracking_of_string : string -> tracking
type operation = Categories | List of group * cursor option | Search of group * string * cursor option
 | Thread of string | Read of string | Like of string * bool | Send of draft
 | Track of string * tracking | Topic_state of string | Attachment of string * int * string
val service : operation -> bool
type result = Page of Reader.Content.page | Thread_batch of Reader.Content.thread_batch | Article of Decode.article
 | Category_list of Yojson.Safe.t | Liked of Decode.article | Sent of Decode.article
 | Tracked of string * tracking | Topic_state_result of string * int | Saved of string
exception Uncertain_write
module Make (_:Http.S) : sig
 type t
 include Reader.Backend.S with type t := t and type group = group and type cursor = cursor
 include Reader.Backend.Threading with type t := t
 include Reader.Backend.Liking with type t := t
 val create : config -> t
 val categories : t -> Yojson.Safe.t
 val read : t -> string -> Decode.article
 val stream_thread : t -> string -> emit_article:(Decode.article -> unit) -> emit:(Reader.Content.thread_batch -> unit) -> unit
 val archive_article : t -> Decode.article -> Reader.Content_state.archived
 val attachment : t -> string -> int -> string -> unit
 val download : t -> ?limit:int -> string -> string
 val send : t -> draft -> receipt
 val track : t -> string -> tracking -> unit
 val topic_state : t -> string -> int
 module Subscription : Reader.Backend.Subscription with type t = t and type target = string and type settings = tracking
 val execute : t -> operation -> emit:(result -> unit) -> unit
end
module Live : module type of Make(Curl_transport)
module Readable : Reader.Backend.S with type t = Live.t and type group = group and type cursor = cursor
module Threading : Reader.Backend.Threading with type t = Live.t
module Liking : Reader.Backend.Liking with type t = Live.t
module Writing : Reader.Backend.Writing with type t = Live.t and type draft = draft and type receipt = receipt
val operation_of_json : Yojson.Safe.t -> operation
val result_json : result -> Yojson.Safe.t

module Subscription : Reader.Backend.Subscription with type t = Live.t and type target = string and type settings = tracking
