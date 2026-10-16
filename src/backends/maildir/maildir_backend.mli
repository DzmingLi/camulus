module Process : module type of Process
module Mime : module type of Mime
module Smtp : module type of Smtp
type account_settings = { prefix : string; address : string; smtp_account : string }
type config = {
  root : string; muhome : string; mu : string;
  sender : string; account : string; page_size : int; max_headers : int;
  accounts : account_settings list; smtp_profiles : (string * Smtp.config) list;
}
val config : root:string -> muhome:string -> sender:string -> config
val validate_config : config -> unit
exception Error of string
type group = string
type cursor = string
type attachment = Mime.attachment
type article = { message : Reader.Content.message; attachments : attachment list }
type draft = { to_ : string list; cc : string list; subject : string; body : string; reply_to : string option }
type receipt = { message_id : string }
type saved_draft = { key : string; group : string; revision : string; source_text : string; indexed : bool }
type archive_result = { destination : string; moved : string list; failed : int }
type operation = List of group * cursor option | Search of group * string * cursor option
  | Thread of string | Read of string | Attachment of string * int * string | Send of draft
  | Draft_new of string * string option | Draft_open of string
  | Draft_save of string * string * string | Draft_list of string
  | Draft_send of string * string
  | Archive of string * string * bool
type result = Page of Reader.Content.page * bool | Thread_batch of Reader.Content.thread_batch
  | Article of article | Saved of string | Sent of receipt
  | Draft of saved_draft | Drafts of saved_draft list | Archived of archive_result
  | Draft_sent of string * bool
type process_result = { status : int; stdout : string }
type run = ?input:string -> string list -> process_result
type submit = profile:string -> sender:string -> recipients:string list -> string -> unit
type t
val create : config:config -> run:run -> submit:submit -> t
include Reader.Backend.S with type t := t and type group := group and type cursor := cursor
include Reader.Backend.Threading with type t := t
val read : t -> string -> article
val raw_message : t -> string -> string
val save_attachment : t -> string -> int -> string -> unit
val prepare : t -> draft -> string * string
val send : t -> draft -> receipt
module Readable : Reader.Backend.S with type t=t and type group=group and type cursor=cursor
module Threading : Reader.Backend.Threading with type t=t
module Writing : Reader.Backend.Writing with type t=t and type draft=draft and type receipt=receipt
val operation_of_json : Yojson.Safe.t -> operation
val result_json : result -> Yojson.Safe.t
val execute : t -> operation -> emit:(result -> unit) -> unit

val new_draft : t -> group -> string option -> saved_draft
val open_draft : t -> string -> saved_draft
val save_draft : t -> string -> string -> string -> saved_draft
val list_drafts : t -> group -> saved_draft list
val archive : t -> group -> string -> bool -> archive_result
