type work = ..
type payload = ..
type request = { id : int; work : work }
type command = Submit of request | Cancel of int
type outcome = Completed | Cancelled | Failed of string
type event =
  | Ready of int
  | Data of request * payload
  | Finished of request * outcome
type lifecycle = Starting | Running | Stopped | Crashed of string
