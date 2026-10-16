type status = Pending | Confirmed | Rejected of string | Uncertain of string
type update = { id : string; request : int; direction : Content.vote; status : status }
type entry = { update : update; previous : Content.vote; barrier : int }
type t = (string, entry) Hashtbl.t
let create () = Hashtbl.create 32
let clear = Hashtbl.clear
let find t id = Option.map (fun entry -> entry.update) (Hashtbl.find_opt t id)
let is_pending t id = match find t id with Some {status=Pending;_} -> true | _ -> false
let start t ~id ~request ~previous ~direction =
  if is_pending t id then invalid_arg "A vote for this item is still pending";
  Hashtbl.replace t id {update={id;request;direction;status=Pending};previous;barrier=request}

let finish t ~id ~request ~barrier ~status =
  if status = Pending then invalid_arg "Voting.finish: pending is not an outcome";
  match Hashtbl.find_opt t id with
  | Some entry when entry.update.request = request && entry.update.status = Pending ->
    let direction = match status with Rejected _ -> entry.previous | _ -> entry.update.direction in
    let update = {entry.update with direction;status} in
    Hashtbl.replace t id {entry with update;barrier=max request barrier};
    Some update
  | _ -> None

let observe t ~id ~request ~direction =
  match Hashtbl.find_opt t id with
  | Some {update={status=Pending;_};_} -> None
  | Some entry when request > entry.barrier ->
    let update = {entry.update with direction;status=Confirmed} in
    Hashtbl.replace t id {update;previous=direction;barrier=request};
    Some update
  | _ -> None
