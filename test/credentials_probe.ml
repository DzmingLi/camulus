open! Core
open! Ecaml
open Reader_backend
let () =
 define0 "camulus-test-credential" (fun()->
  let result=Credentials_emacs.with_sources ["fixture.authinfo"](fun()->
    Credentials_emacs.auth_source ~user:"fixture-user" ~port:"fixture-api" "fixture.test")in
  set "camulus-test-credential-result"(Option.value_map result ~default:nil ~f:(fun c->Value.list[s c.user;s c.secret])));
 provide(Symbol.intern "credentials_probe")
