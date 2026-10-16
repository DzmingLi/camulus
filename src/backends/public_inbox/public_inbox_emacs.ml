open! Core
open! Ecaml
open Reader_backend
let install ~read ~attachment () =
  register_local_groups { (base ~name:"public-inbox" ~matches:(String.equal "public-inbox") ~threaded:true
    ~read:(fun ~source:_ ~id ~thread_id:_ ~group -> Value.of_int_exn (read id group))) with
    attachment=Some (fun ~source:_ ~id ~group ~part ~destination -> Value.of_int_exn (attachment id group part destination));
    events=["rss"] }
