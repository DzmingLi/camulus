(* Test-only native entry points; not linked into the installed module. *)
open! Core
open! Ecaml
open Reader_backend
let view=ref None
let () =
  define0 "camulus-test-webkit-open" (fun()->
    view:=Some(Reader_webkit.open_page ~title:"*Camulus WebKit test*"
      ~url:"https://example.test/page" ~channel:"testChannel" ~javascript:"test-script"
      ~on_message:(fun payload->set "camulus-test-webkit-message" (s payload))
      ~on_close:(fun()->set "camulus-test-webkit-closed" (call "1+" [get "camulus-test-webkit-closed"]))
      ~on_error:(fun reason->set "camulus-test-webkit-error" (s reason))));
  define0 "camulus-test-webkit-close" (fun()->
    Option.iter !view ~f:Reader_webkit.close;view:=None);
  provide(Symbol.intern "webkit_probe")
