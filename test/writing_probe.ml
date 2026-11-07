open! Core
open! Ecaml
open Reader_backend
let () =
  Writing.install ();
  define0 "camulus-test-writing-attach" (fun () ->
    Writing.attach {
      snapshot=(fun () -> text(call "buffer-string" []));
      save=(fun ~source ~complete ->
        run "camulus-test-writing-save" [s source;lambda2(fun ok reason ->
          complete(if truth ok then Ok () else Error(text reason));nil)]);
      send=(fun ~complete ->
        run "camulus-test-writing-send" [lambda2(fun close message ->
          complete(if Value.eq close(sym "failed") then Error(text message)
            else Ok {Writing.close=truth close;message=text message});nil)])
    });
  provide(Symbol.intern "writing_probe")
