type command = 
  |Fold 
  |Call
  |Raise of int
  |Check
  |Exit

exception Empty

exception Malformed

let parse_amount parts = 
  if List.length parts <> 1 then raise Malformed
  else int_of_string (List.hd parts)

let parse_command parts = 
  match parts with 
  |[] -> raise Empty
  | h::t -> if h = "raise" then (Raise (parse_amount t))
    else if t <> [] then raise Malformed
    else if h = "fold" then Fold
    else if h = "call" then Call
    else if h = "check" then Check
    else if h = "exit" then Exit
    else raise Malformed

let parse str =
  parse_command (List.filter(fun x -> x <> "") (String.split_on_char ' ' str))
