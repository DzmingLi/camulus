module R = Mrmime

type attachment = { part : int; name : string; mime : string; cid : string; size : int }
type parsed = {
  subject : string; author : string; date : string; message_id : string; in_reply_to : string; references : string;
  texts : (string * string) list; attachments : attachment list;
}
let fail () = failwith "Invalid or unsupported MIME message"
let get = function Ok x -> x | Error _ -> fail ()
let ids s =
  let rec loop pos acc =
    match String.index_from_opt s pos '<' with
    | None -> List.rev acc
    | Some a -> match String.index_from_opt s (a+1) '>' with
      | None -> List.rev acc
      | Some b -> let id=String.sub s (a+1) (b-a-1) in
        loop (b+1) (if id="" then acc else id::acc) in
  loop 0 []
let collect stream =
  let b=Buffer.create 256 in
  let rec loop () = match stream () with None -> Buffer.contents b
    | Some s -> Buffer.add_string b s; loop () in loop ()
let header_value h name =
  let fields=R.Header.assoc (R.Field_name.v name) h in
  let value = function
    | R.Field.Field(_,R.Field.MessageID,(local,domain)) ->
      "<"^R.Mailbox.Local.to_string local^"@"^R.MessageID.Domain.to_string domain^">"
    | R.Field.Field(_,R.Field.Unstructured,text) -> R.Unstructured.to_string text
    | field ->
      let s=collect(R.Header.to_stream(R.Header.of_list [field])) in
      match String.index_opt s ':' with None->"" | Some i->String.sub s (i+1) (String.length s-i-1) in
  String.concat " " (List.map value fields) |> String.trim

let utf8 charset bytes =
  match Rosetta.to_utf_8_string ~charset:(String.uppercase_ascii charset) bytes with
  | Some text -> text
  | None -> (* Preserve decodable UTF-8, visibly replace malformed octets. *)
    Option.value (Rosetta.to_utf_8_string ~charset:"UTF-8" bytes) ~default:bytes
let decode_words text =
  let tokens=get (Angstrom.parse_string ~consume:Prefix
    (R.Unstructured_with_encoded.Decoder.unstructured_with_encoded ()) (text^"\r\n")) in
  let b=Buffer.create 64 in
  let rec loop previous = function
    | [] -> ()
    | ((`WSP _ | `FWS _ | `CR | `LF) as x)::rest when previous ->
      let rec whitespace acc = function
        | ((`WSP _ | `FWS _ | `CR | `LF) as x)::rest -> whitespace (x::acc) rest
        | rest -> List.rev acc,rest in
      let spaces,rest=whitespace [x] rest in
      (match rest with `Encoded _::_ -> loop true rest
       | _ -> List.iter (fun x -> Buffer.add_string b (R.Unstructured.to_string [x])) spaces;
         loop false rest)
    | `Encoded (charset,raw)::rest ->
      let bytes=match raw with Emile.Base64 x | Emile.Quoted_printable x -> get x in
      Buffer.add_string b (utf8 charset bytes);loop true rest
    | (#R.Unstructured.elt as x)::rest ->
      Buffer.add_string b (R.Unstructured.to_string [x]);loop false rest in
  loop false tokens;Buffer.contents b |> String.trim
let parameter ct key =
  List.find_map (fun (k,v) -> if String.lowercase_ascii k=key then
    Some(match v with `String s | `Token s -> s) else None) (R.Content_type.parameters ct)
let extended_parameter ct name =
  let decode s = match String.split_on_char '\'' s with
    | charset::_lang::rest -> utf8 charset (Uri.pct_decode (String.concat "'" rest))
    | _ -> Uri.pct_decode s in
  match parameter ct (name^"*") with
  | Some s -> Some(decode s)
  | None ->
    let rec parts i acc encoded =
      match parameter ct (name^"*"^string_of_int i^"*"),parameter ct (name^"*"^string_of_int i) with
      | Some s,_ -> parts (i+1) (s::acc) (encoded || i=0)
      | _,Some s -> parts (i+1) (s::acc) encoded
      | _ -> if i=0 then parameter ct name else
        let s=String.concat "" (List.rev acc) in Some(if encoded then decode s else s) in
    parts 0 [] false
let disposition h =
  let text=header_value h "Content-Disposition" in
  if text="" then None else
  match Angstrom.parse_string ~consume:All R.Content_type.Decoder.content ("application/"^text) with
  | Ok ct -> Some ct | Error _ ->
    let unfolded=String.split_on_char '\n' text |> List.map String.trim |> String.concat " " in
    match Angstrom.parse_string ~consume:All R.Content_type.Decoder.content ("application/"^unfolded) with
    | Ok ct -> Some ct | Error _ -> fail ()
let mime_type h = let ct=R.Header.content_type h in
  String.lowercase_ascii (R.Content_type.Type.to_string ct.ty^"/"^R.Content_type.Subtype.to_string ct.subty)
let normalize raw =
  if String.length raw>32*1024*1024 then failwith "Mail exceeds 32 MiB MIME limit";
  (* Message-mode drafts use LF. RFC messages already using CRLF are untouched,
     including raw binary attachment octets. *)
  match String.index_opt raw '\n' with
  | Some i when i=0 || raw.[i-1]<>'\r' ->
    let lower=String.lowercase_ascii raw in
    if List.exists (fun line -> String.trim line="content-transfer-encoding: binary")
         (String.split_on_char '\n' lower) then
      failwith "LF-only binary MIME requires CRLF headers; octets were not modified";
    let b=Buffer.create(String.length raw) in
    String.iteri(fun i c ->
      if c='\n' && (i=0 || raw.[i-1]<>'\r') then Buffer.add_char b '\r';
      Buffer.add_char b c) raw;
    Buffer.contents b
  | _ -> raw

let parse_tree raw =
  let raw=normalize raw in
  let count=ref 0 and size=ref 0 in
  let emitters _ =
    incr count;if !count>4096 then failwith "Too many MIME parts";
    let b=Buffer.create 256 in
    ((function None -> () | Some s -> size:= !size+String.length s;
       if !size>32*1024*1024 then failwith "Decoded MIME exceeds limit";
       Buffer.add_string b s),b) in
  get(Angstrom.parse_string ~consume:All (R.Mail.stream emitters) raw)
let buffer_stream s = let once=ref false in fun () ->
  if !once then None else (once:=true;Some(s,0,String.length s))
let render mail =
  let stream=R.Mt.to_stream mail in collect(fun () ->
    Option.map(fun(s,o,n)->String.sub s o n)(stream()))
let rec part depth header tree =
  if depth>64 then failwith "MIME nesting exceeds limit";
  match tree with
  | R.Mail.Leaf bytes -> R.Mt.part ~header (buffer_stream (Buffer.contents bytes))
  | R.Mail.Multipart children ->
    let children=List.filter_map(fun (h,b) -> Option.map (part (depth+1) h) b) children in
    R.Mt.multipart ~rng:R.Mt.rng ~header ?boundary:(R.Content_type.boundary(R.Header.content_type header)) children
    |> R.Mt.multipart_as_part
  | R.Mail.Message(h,b) ->
    let bytes=render (R.Mt.make R.Header.empty R.Mt.simple (part (depth+1) h b)) in
    R.Mt.part ~header (buffer_stream bytes)
let leaves header tree =
  let n=ref 0 in
  let rec walk depth h tree =
    if depth>64 then failwith "MIME nesting exceeds limit";
    match tree with
    | R.Mail.Multipart parts -> List.concat_map (fun(h,b)->match b with None->[] | Some b->walk (depth+1) h b) parts
    | R.Mail.Leaf b -> incr n;[!n,h,Buffer.contents b,false]
    | R.Mail.Message(h',b) -> incr n;
      let bytes=render(R.Mt.make R.Header.empty R.Mt.simple (part (depth+1) h' b)) in [!n,h,bytes,true] in
  walk 0 header tree
let inspect raw =
  let header,tree=parse_tree raw in
  let texts=ref [] and attachments=ref [] and data=ref [] in
  List.iter(fun (part,h,bytes,message) ->
    let mime=mime_type h and disp=disposition h in
    let name=match Option.bind disp (fun ct -> extended_parameter ct "filename") with
      | Some s -> s | None -> Option.value (extended_parameter (R.Header.content_type h) "name") ~default:"" in
    let attached=message || name<>"" || Option.fold ~none:false
      ~some:(fun ct -> R.Content_type.Subtype.to_string ct.R.Content_type.subty="attachment") disp in
    if not attached && (mime="text/plain" || mime="text/html") then begin
      let charset=Option.value(parameter (R.Header.content_type h) "charset") ~default:"UTF-8" in
      texts:=(mime,utf8 charset bytes)::!texts
    end else begin
      let cid=header_value h "Content-ID" in
      let cid=match ids cid with [s]->s | _->cid in
      attachments:={part;name=(if name="" then if message then "message.eml" else "attachment" else decode_words name);
        mime;cid;size=String.length bytes}::!attachments;
      data:=(part,bytes)::!data
    end) (leaves header tree);
  let message_id=match ids(header_value header "Message-ID") with [s]->s | _->"" in
  {subject=decode_words(header_value header "Subject");author=decode_words(header_value header "From");date=header_value header "Date";message_id;
   in_reply_to=header_value header "In-Reply-To";references=header_value header "References";
   texts=List.rev !texts;attachments=List.rev !attachments},!data
let parse raw = fst(inspect raw)
let extract raw part = match List.assoc_opt part (snd(inspect raw)) with
  | Some bytes -> bytes | None -> failwith "Not an attachment part"
let envelope raw =
  let h,_=parse_tree raw in
  if R.Header.exists (R.Field_name.v "Bcc") h then failwith "Bcc is not supported by this composer";
  let mailbox (m:R.Mailbox.t) = R.Mailbox.Local.to_string m.local^"@"^R.Mailbox.Domain.to_string(fst m.domain) in
  let addresses name =
    let fields=R.Header.assoc (R.Field_name.v name) h in
    List.concat_map(function
      | R.Field.Field(_,R.Field.Mailboxes,ms) -> List.map mailbox ms
      | R.Field.Field(_,R.Field.Mailbox,m) -> [mailbox m]
      | R.Field.Field(_,R.Field.Addresses,xs) -> List.concat_map(function
        | `Mailbox m -> [mailbox m] | `Group g -> List.map mailbox g.Emile.mailboxes) xs
      | _ -> fail ()) fields in
  addresses "From",addresses "To",addresses "Cc"
let body kind parsed =
  List.filter_map(fun(mime,text)->if mime=kind then Some text else None) parsed.texts |> String.concat "\n"

let compose ~sender ~to_ ~cc ~subject ~date ~message_id ~references ~body =
  let field name witness value = R.Field.Field(R.Field_name.v name,witness,value) in
  let mailbox s=get(R.Mailbox.of_string s) in
  let unstructured s=(get(R.Unstructured.of_string (s^"\r\n")) :> R.Unstructured.t) in
  let rec words pos =
    if pos=String.length subject then [] else
    let stop=ref(min (pos+42) (String.length subject)) in
    while !stop<String.length subject && Char.code subject.[!stop] land 0xc0=0x80 do decr stop done;
    if !stop=pos then fail ();
    let word=R.Unstructured_with_encoded.Craft.(e ~encoding:b (String.sub subject pos (!stop-pos))) in
    word :: (if !stop=String.length subject then [] else R.Unstructured_with_encoded.Craft.sp 1 :: words !stop) in
  let subject=R.Unstructured_with_encoded.Craft.compile (words 0) in
  let headers=[field "From" R.Field.Mailboxes [mailbox sender];
    field "To" R.Field.Addresses (List.map(fun s->`Mailbox(mailbox s)) to_);
    field "Subject" R.Field.Unstructured_with_encoded subject;
    field "Date" R.Field.Unstructured (unstructured date);
    field "Message-ID" R.Field.MessageID (get(R.MessageID.of_string("<"^message_id^">")));
    field "MIME-Version" R.Field.Unstructured (unstructured "1.0")] @
    (if cc=[] then [] else [field "Cc" R.Field.Addresses(List.map(fun s->`Mailbox(mailbox s)) cc)]) @
    (match List.rev references with []->[] | parent::_->
      [field "In-Reply-To" R.Field.Unstructured (unstructured("<"^parent^">"));
       field "References" R.Field.Unstructured (unstructured(String.concat " " (List.map(fun s->"<"^s^">") references)))]) in
  let content=get(Angstrom.parse_string ~consume:All R.Content_type.Decoder.content "text/plain; charset=UTF-8") in
  let header=R.Header.of_list [field "Content-Type" R.Field.Content content;field "Content-Transfer-Encoding" R.Field.Encoding `Base64] in
  render(R.Mt.make (R.Header.of_list headers) R.Mt.simple (R.Mt.part ~header (buffer_stream body)))
