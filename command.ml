type command = 
  |Fold 
  |Call
  |Raise of int
  |Check
  |Exit
  |Quit

exception Empty

exception Malformed

(** [parse_amount parts] parse the sting form of a number into int type. 
    Requires: [parts]'s length is 1 and the only element in it is a string form 
    number. Raises: Malformed when the length of [parts] is not 1. *)
let parse_amount parts = 
  if List.length parts <> 1 then raise Malformed
  else int_of_string (List.hd parts)

(** [parse_command parts] parse string list [parts] into commands along with 
    inputs if needed. 
    Raises: Empty when [parts] is an empty list. 
    Malformed when the parts corresbond to no command. *)
let parse_command parts = 
  match parts with 
  |[] -> raise Empty
  | h::t -> if h = "raise" then (Raise (parse_amount t))
    else if t <> [] then raise Malformed
    else if h = "fold" then Fold
    else if h = "call" then Call
    else if h = "check" then Check
    else if h = "exit" then Exit
    else if h = "quit" then Quit
    else raise Malformed

let parse str =
  parse_command (List.filter(fun x -> x <> "") (String.split_on_char ' ' str))
