type owner = View of int | Service
type work = ..
type work += Delay of { seconds : float; steps : int } | Fail | Allocate of int
type payload = ..
type request = { id : int; owner : owner; work : work }
type command = Submit of request | Cancel of int | Close_view of int
type outcome = Completed | Cancelled | Failed of string
type event =
  | Ready of int
  | Progress of request * int
  | Data of request * payload
  | Finished of request * outcome
type lifecycle = Starting | Running | Stopped | Crashed of string
