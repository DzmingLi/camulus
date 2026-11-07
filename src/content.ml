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
  (** Opaque, backend-defined token identifying an observable update to this
      message, such as a content revision or a remote notification event.

      Keep the token stable across repeated fetches of the same update. After
      the user dismisses an item from the feed, fetching that token again must
      not bring it back. A different token lets the feed recognize new activity
      on the same message, without changing its [id] or its read state.

      For example, a backend may use a remote event ID, a revision identifier,
      or a content fingerprint. The core compares tokens for equality; it does
      not parse them. [None] makes the timeline use [id] as the token, so changes
      to that same message cannot by themselves create another activity.

      This is neither a fetch timestamp nor a sorting key: [created] supplies
      source time, and storage assigns its own sequence for dismissal bounds.
      Some backends also compare this token to invalidate cached full bodies. *)
  activity : string option;
}

type page = {
  messages : message list;
  after : string option;
}

type gap = { parent : string; count : int }
type thread_batch = {
  thread : string;
  messages : message list;
  gaps : gap list;
  complete : bool;
}
