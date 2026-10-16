(** The current vote; [No_vote] also cancels an existing vote. *)
type vote = Upvote | Downvote | No_vote

type message = {
  id : string;
  thread_id : string option;
  parent_id : string option;
  references : string list;
  title : string;
  author : string;
  body : string;
  html : string option;
  url : string;
  link_url : string option;
  created : float;
  unavailable : bool;
  score : int;
  my_vote : vote;
  activity : string option;
}

type notification = {
  event_id : string;
  thread : string;
  target : string;
  reason : string;
  title : string;
  created : float;
}

type page = {
  messages : message list;
  notifications : notification list;
  after : string option;
}

type gap = { parent : string; count : int }
type thread_batch = {
  thread : string;
  messages : message list;
  gaps : gap list;
  complete : bool;
}

(* Stable sibling order, independent of arrival order and Reddit scores.
   Preserve orphan nodes rather than inventing a parent relationship. *)
let tree_order (messages : message list) =
  let by_id = Hashtbl.create 128 and children = Hashtbl.create 128 in
  List.iter (fun m -> Hashtbl.replace by_id m.id m) messages;
  Hashtbl.iter (fun _ m ->
    let parent = match m.parent_id with
      | Some id when Hashtbl.mem by_id id -> Some id | _ -> None in
    let old = Option.value (Hashtbl.find_opt children parent) ~default:[] in
    Hashtbl.replace children parent (m :: old)) by_id;
  let sorted xs = List.sort (fun (a:message) (b:message) ->
    let c = Float.compare a.created b.created in
    if c = 0 then String.compare a.id b.id else c) xs in
  let visited = Hashtbl.create 128 and result = ref [] in
  let rec walk depth m =
    if not (Hashtbl.mem visited m.id) then begin
      Hashtbl.add visited m.id ();
      result := (depth, m) :: !result;
      Option.iter (fun xs -> List.iter (walk (min 256 (depth + 1))) (sorted xs))
        (Hashtbl.find_opt children (Some m.id))
    end
  in
  List.iter (walk 0) (sorted (Option.value (Hashtbl.find_opt children None) ~default:[]));
  Hashtbl.fold (fun _ m xs -> m :: xs) by_id [] |> sorted |> List.iter (walk 0);
  List.rev !result
