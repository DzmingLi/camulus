(** MIME parsing and generation through Mrmime. No host or subprocess dependency. *)
type attachment = { part : int; name : string; mime : string; cid : string; size : int }
type parsed = {
  subject : string; author : string; date : string; message_id : string; in_reply_to : string; references : string;
  texts : (string * string) list; attachments : attachment list;
}
val parse : string -> parsed
val extract : string -> int -> string
val envelope : string -> string list * string list * string list
val ids : string -> string list
val body : string -> parsed -> string
val compose : sender:string -> to_:string list -> cc:string list -> subject:string ->
  date:string -> message_id:string -> references:string list -> body:string -> string
