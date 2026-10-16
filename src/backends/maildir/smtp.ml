(* Colombe's scheduler-independent SMTP client, driven directly by Eio effects.
   Only implicit TLS is exposed: no plaintext fallback or credential subprocess. *)
type config = { host : string; port : int; username : string; password_file : string; ca_file : string }
exception Error of string
let fail s = raise(Error s)
let get = function Ok x -> x | Error _ -> fail "Invalid SMTP configuration"
let validate c =
  if c.host="" || c.port<1 || c.port>65535 then fail "Invalid SMTP endpoint";
  ignore(get(Result.bind (Domain_name.of_string c.host) Domain_name.host));
  if c.username<>"" && (c.password_file="" || Filename.is_relative c.password_file) then
    fail "SMTP authentication requires an absolute password-file path";
  if String.contains c.username '\000' then fail "Invalid SMTP username"
let read_file ~limit path =
  let ch=open_in_bin path in
  Fun.protect ~finally:(fun()->close_in_noerr ch) @@ fun () ->
  let n=in_channel_length ch in if n>limit then fail "SMTP configuration file exceeds limit";
  really_input_string ch n
let rng = lazy (Mirage_crypto_rng_unix.use_default ())
module Identity = Colombe.Sigs.Make(struct type 'a t = 'a end)
let scheduler : Identity.t Colombe.Sigs.impl = {
  bind=(fun x f->f(Identity.prj x));return=(fun x->Identity.inj x) }
let rdwr : (Tls_eio.t, Identity.t) Colombe.Sigs.rdwr = {
  rd=(fun flow bytes off len ->
    let buffer=Cstruct.create len in
    let n=try Eio.Flow.single_read flow buffer with End_of_file->0 in
    Cstruct.blit_to_bytes buffer 0 bytes off n;
    Identity.inj (if n=0 then `End else `Len n));
  wr=(fun flow bytes off len ->
    Eio.Flow.copy_string (String.sub bytes off len) flow;Identity.inj ()) }
let send ~net ~sleep ?(timeout=60.) c ~sender ~recipients source =
  validate c;
  let sender=fst(Colombe.Reverse_path.Decoder.of_string("<"^sender^">")) in
  let recipients=List.map(fun s->fst(Colombe.Forward_path.Decoder.of_string("<"^s^">"))) recipients in
  (* One line per chunk is required for Colombe's SMTP dot transparency. *)
  let lines=ref (String.split_on_char '\n' source) in
  let stream () = Identity.inj (match !lines with
    | [] | [""] -> lines:=[];None
    | line::rest -> lines:=rest;
      let line=if String.ends_with ~suffix:"\r" line then line^"\n" else line^"\r\n" in
      Some(line,0,String.length line)) in
  try Eio.Fiber.first (fun () ->
    Lazy.force rng;
    let authenticator=if c.ca_file="" then get(Ca_certs.authenticator ()) else
      let certs=get(X509.Certificate.decode_pem_multiple(read_file ~limit:(4*1024*1024) c.ca_file)) in
      X509.Authenticator.chain_of_trust ~time:(fun()->Some(Ptime_clock.now())) certs in
    let tls=get(Tls.Config.client ~authenticator ()) in
    let host=get(Result.bind (Domain_name.of_string c.host) Domain_name.host) in
    Eio.Switch.run @@ fun sw ->
    let addresses=Eio.Net.getaddrinfo_stream ~service:(string_of_int c.port) net c.host in
    let rec connect = function
      | [] -> fail "Cannot connect to SMTP server"
      | addr::rest -> try Eio.Net.connect ~sw net addr
        with Eio.Io _ when rest<>[] -> connect rest in
    let flow=Tls_eio.client_of_flow tls ~host (connect addresses) in
    let authentication=if c.username="" then None else
      let password=read_file ~limit:8192 c.password_file |> String.trim in
      if password="" || String.contains password '\000' then fail "Invalid SMTP password file";
      Some {Sendmail.username=c.username;password;mechanism=Sendmail.PLAIN} in
    let ctx=Colombe.State.Context.make () in
    let result=Identity.prj (Sendmail.sendmail scheduler rdwr flow ctx ?authentication
      ~domain:(Colombe.Domain.of_string_exn "camulus.local") sender recipients stream) in
    match result with Ok () -> () | Error _ -> fail "SMTP submission failed; inspect before retrying")
    (fun () -> sleep timeout;fail "SMTP submission timed out; outcome may be uncertain")
  with
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | Error _ as ex -> raise ex
  | _ -> fail "SMTP connection, TLS verification or submission failed"
