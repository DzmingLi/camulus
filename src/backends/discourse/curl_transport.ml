(* Initialized once when the native module loads, before spawning the worker.
   Never globally clean up libcurl inside a host shared with other modules. *)
let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let request (r : Http.request) =
  if not (List.mem "asynchdns" (Curl.version_info ()).features) then
    raise (Http.Transport_error "libcurl must support asynchronous DNS for the Eio transport");
  ignore (Http.validate r.url);
  let easy = Curl.init () and multi = Curl.Multi.create () in
  let attached = ref false in
  Fun.protect ~finally:(fun () ->
    if !attached then Curl.Multi.remove multi easy;
    Curl.Multi.cleanup multi; Curl.cleanup easy) @@ fun () ->
  let body = Buffer.create 4096 and headers = ref [] and header_size = ref 0 in
  Curl.set_url easy r.url;
  Curl.set_nosignal easy true;
  Curl.set_encoding easy Curl.CURL_ENCODING_ANY;
  Curl.set_useragent easy "camulus/0.1 (Discourse)";
  Curl.set_followlocation easy false;
  Curl.set_protocols easy [Curl.CURLPROTO_HTTP; Curl.CURLPROTO_HTTPS];
  Curl.set_sslverifypeer easy true;
  Curl.set_sslverifyhost easy Curl.SSLVERIFYHOST_HOSTNAME;
  (* Honour the Nix shell's CA bundle without disabling verification. *)
  Option.iter (Curl.set_cainfo easy) (Sys.getenv_opt "NIX_SSL_CERT_FILE");
  Curl.set_connecttimeoutms easy 10000;
  Curl.set_timeoutms easy 30000;
  Curl.set_httpheader easy (List.map (fun (k,v) -> k ^ ": " ^ v) r.headers);
  Curl.set_writefunction easy (fun chunk ->
    if Buffer.length body + String.length chunk > r.limit then 0
    else (Buffer.add_string body chunk; String.length chunk));
  Curl.set_headerfunction easy (fun line ->
    header_size := !header_size + String.length line;
    if !header_size > 65536 then 0 else begin
      (match String.index_opt line ':' with
       | None -> ()
       | Some i ->
         let key = String.sub line 0 i |> String.lowercase_ascii in
         let value = String.sub line (i+1) (String.length line-i-1) |> String.trim in
         headers := (key,value) :: !headers);
      String.length line
    end);
  (match r.meth with
   | Http.Get -> ()
   | Post | Delete ->
     Curl.set_postfields easy r.body;
     Curl.set_postfieldsize easy (String.length r.body);
     Curl.set_customrequest easy (match r.meth with Post -> "POST" | _ -> "DELETE"));
  Curl.Multi.add multi easy; attached := true;
  let rec poll () =
    ignore (Curl.Multi.perform multi);
    match Curl.Multi.remove_finished multi with
    | Some (_, code) ->
      attached := false;
      if code <> Curl.CURLE_OK then
        raise (Http.Transport_error "Discourse transfer failed or timed out")
    | None -> Eio_unix.sleep 0.01; poll ()
  in
  (try poll () with
   | Curl.CurlException _ | Curl.Multi.Error _ | Curl.Multi.CError _ ->
     raise (Http.Transport_error "Discourse transport failure"));
  { Http.status = Curl.get_responsecode easy; headers = !headers; body = Buffer.contents body }
