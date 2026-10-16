open! Core
open! Ecaml
open Reader_backend
let install ~read ~attachment () =
  register_local_groups { (base ~name:"zhihu" ~matches:(String.equal "zhihu") ~threaded:false
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group -> Value.of_int_exn (read id group))) with
    attachment=Some (fun ~source:_ ~id ~group ~part ~destination -> Value.of_int_exn (attachment id group part destination));
    events=["rss"] }
