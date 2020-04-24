open State
open Command
(* this is the implementation of main interface with user*)


(** [InvalidCommand] signifies the usage of a not valid Command*)
exception InvalidCommand of string





(** the padding between displaying information of current room*)
let padding = "=====================================================\n" 



(* the following section is the actual parsing of the game*)

(** [parse_fold st] parses a fold command for the current player on [st]*)
let parse_fold (st: State.t) =
  fold st

(** [parse_call st] parses a call command for the current player on [st]*)
let parse_call st = 
  call st
(** [parse_raise st value] parses a raise with value [value] for the
    current player on [st]*)
let parse_raise st value =
  raise value st

(** [parse_check st] parses a check command for the current player on [st]*)
let parse_check st = 
  check st

(** [parse_exit st] parses a exit command for the current player on [st]*)
let parse_exit st = 
  exit st


(** [game_command st cmd] is the reulting state of 
    the current state [st] after currentplayer choose to play [cmd]

    raises BlindFold, NotEnoughMoney
*)
let game_command (st: State.t) (cmd: Command.command) = 
  match cmd with
  |Command.Fold ->  parse_fold st
  |Command.Call -> parse_call st
  |Command.Raise(x) -> parse_raise st x  
  |Command.Check -> parse_check st
  |Command.Exit -> parse_exit st
  |Command.Quit -> print_string "bye\n"; Stdlib.exit 0 

(** [valid_command cmd] is the resulting Command.command generated from
    [Command.parse cmd]. 

    raises: InvalidCommand if [cmd] is considered Empty or Malformed by
    [Command.parse]
*)
let valid_command (cmd: string) = 
  try (Command.parse cmd) with
  |Command.Empty -> Stdlib.raise  (InvalidCommand "\nan empty command\n")
  |Command.Malformed -> Stdlib.raise (InvalidCommand "\ncommand is malformed\n")


(**[out_put_description st] outputs the description of the curren tstate[st]*)
let out_put_description st = 
  failwith "unimplemented"

(** [rec_game st] recursively display the description of the current state
    [st] and asks for the next command.  If the command is failing under the
    current state, ask again for another command
*)

let rec rec_game  (st: State.t) = 
  print_string padding;
  print_string (out_put_description st ^ "\n");
  print_string  "> ";
  try read_line () |> valid_command |> game_command st |> rec_game 
  with
  | End_of_file -> ()
  | InvalidCommand str -> print_string (str^ "\n") ; rec_game st

(** [game nl] initialize the state with the name list [nl],
    and then starts playing the game 
*)
let game (nl: string)= 
  rec_game (State.init_state nl)




(** [main ()] prompts for the game to play, ask for the name of players,
    then starts it. 
*)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 TexasHold'em game\n");
  print_endline "Please enter the player names separated by space\n";
  print_string  "> ";
  try  read_line() |>  game  with 
  | End_of_file -> ()

(* Execute the game engine. *)
let () = main ()
