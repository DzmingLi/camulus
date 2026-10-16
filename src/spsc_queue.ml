(* Bounded SPSC queue. Exactly one producer and one consumer domain.
   Publication is atomic; neither side waits for space or data.
   Payloads must not be mutated after publication. *)
type 'a t = {
  slots : 'a option Atomic.t array;
  read : int Atomic.t;
  write : int Atomic.t;
}

let create capacity =
  if capacity < 1 then invalid_arg "Spsc_queue.create";
  { slots = Array.init (capacity + 1) (fun _ -> Atomic.make None);
    read = Atomic.make 0; write = Atomic.make 0 }

let push t item =
  let w = Atomic.get t.write in
  let next = (w + 1) mod Array.length t.slots in
  if next = Atomic.get t.read then false
  else begin
    Atomic.set t.slots.(w) (Some item);
    Atomic.set t.write next;
    true
  end

let pop t =
  let r = Atomic.get t.read in
  if r = Atomic.get t.write then None
  else begin
    let item = Atomic.get t.slots.(r) in
    Atomic.set t.slots.(r) None;
    Atomic.set t.read ((r + 1) mod Array.length t.slots);
    item
  end
