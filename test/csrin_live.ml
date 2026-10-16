(* Explicit read-only probe; never runs as part of the regression suite. *)
let ()=Eio_posix.run ~manage_signals:false(fun _->
 let module B=Csrin_backend in let module L=B.Live in
 let t=L.create()in
 let forums=L.forums t in Printf.printf "Visible forums: %d\n%!"(List.length forums);
 let group=Option.value(Sys.getenv_opt "CSRIN_GROUP")~default:(fst(List.hd forums))in
 let page=L.list t group ~after:None in Printf.printf "Recent posts: %d\n%!"(List.length page.messages);
 match page.messages with []->()|m::_->
  let topic=B.topic_of_id m.id in
  let posts,next=B.topic_page ~base:t.base ~topic ~start:0(L.get t("viewtopic.php?t="^topic^"&start=0"))in
  Printf.printf "First topic page: %d posts; has next page: %b\n%!"(List.length posts)(Option.is_some next))
