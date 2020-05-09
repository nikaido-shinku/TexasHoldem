open State
open Command
(* this is the implementation of main interface with user*)


(** [InvalidCommand] signifies the usage of a not valid Command*)
exception InvalidCommand of string





(** the padding between displaying information of current room*)
let padding = "=====================================================\n" 

let instruct = "valid commands are: check call raise fold exit quit \n"

(* the following section is the actual parsing of the game*)

(** [parse_fold st] parses a fold command for the current player on [st]*)
let parse_fold (st: State.t) =
  try fold st with 
  |BlindFold -> 
    Stdlib.raise
      (InvalidCommand "as one of the blinds, you cannot fold at the 
      start of the game."
      )

(** [parse_call st] parses a call command for the current player on [st]*)
let parse_call st = 
  try call st with 
  |NotEnoughMoney -> 
    Stdlib.raise
      (InvalidCommand "You don't have enough money to make the call.")

(** [parse_raise st value] parses a raise with value [value] for the
    current player on [st]*)
let parse_raise st value =
  try raise value st with
  |BlindRaise -> 
    Stdlib.raise
      (InvalidCommand "as one of the blinds, you cannot raise at the start of 
      the game.")
  |NotEnoughMoney -> 
    Stdlib.raise
      (InvalidCommand "You don't have enough money to make the raise.")

(** [parse_check st] parses a check command for the current player on [st]*)
let parse_check st = 
  try check st with 
  |BlindCheck -> 
    Stdlib.raise
      (InvalidCommand "as one of the blinds, you cannot check at the start of 
      the game.")
  |CannotCheck ->
    Stdlib.raise 
      (InvalidCommand "you have not matched your bet yet, cannot check") 

(** [parse_exit st] parses a exit command for the current player on [st]*)
let parse_exit st = 
  try exit st with TimeToQuit -> print_string "bye\n"; Stdlib.exit 0


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
  State.string_of st

(** [rec_game st] recursively display the description of the current state
    [st] and asks for the next command.  If the command is failing under the
    current state, ask again for another command
*)

let rec rec_game  (st: State.t) = 
  print_string padding;
  ANSITerminal.(print_string [green] (out_put_description st ^ "\n"));
  ANSITerminal.(print_string [blue] instruct);
  print_string  "> ";
  try read_line () |> valid_command |> game_command st |> rec_game 
  with
  | End_of_file -> ()
  | InvalidCommand str -> ANSITerminal.(print_string [red] (str^ "\n") );
    rec_game st

(** [game nl] initialize the state with the name list [nl],
    and then starts playing the game 
*)
let game (st: string)= 
  rec_game (State.init_state st)




(** [main ()] prompts for the game to play, ask for the name of players,
    then starts it. 
*)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 TexasHold'em game\n");

  let rec get_input b = 
    if not b then
      ANSITerminal.(print_string [red] "\n You have entered empty names\n")
    else (); 
    print_endline "Please enter the player names separated by space\n";
    print_string  "> ";
    let names = read_line() in 
    print_endline "Please enter the initial bid for every player\n";
    print_string "> ";
    let init_bid = read_line() in 
    try init_bid ^ " " ^ names |>  game  with 
    | End_of_file -> ()
    | EmptyPlayers -> get_input false
  in 
  get_input true

(* Execute the game engine. *)
let () = main ()
