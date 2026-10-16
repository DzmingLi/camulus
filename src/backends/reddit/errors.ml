type error =
  | Invalid_input of string
  | Invalid_response of string
  | Authorization of string
  | Missing_scope of string
  | Http_status of int
  | Rate_limited of float
  | Rejected of string list
  | Uncertain_write
  | Unsupported of string

exception Error of error

let to_string = function
  | Invalid_input s -> "Invalid Reddit input: " ^ s
  | Invalid_response s -> "Incomplete or invalid Reddit response: " ^ s
  | Authorization s -> "Reddit authorization: " ^ s
  | Missing_scope s -> "Reauthorize Reddit with OAuth scope: " ^ s
  | Http_status n -> Printf.sprintf "Reddit HTTP %d" n
  | Rate_limited n -> Printf.sprintf "Reddit rate limit; retry after %.0f seconds" n
  | Rejected codes -> "Reddit rejected operation: " ^ String.concat ", " codes
  | Uncertain_write -> "Reddit write outcome is unknown; check Reddit before sending again"
  | Unsupported s -> "Unsupported Reddit operation: " ^ s

let () = Printexc.register_printer (function Error e -> Some (to_string e) | _ -> None)
