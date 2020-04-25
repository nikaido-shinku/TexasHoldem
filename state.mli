(**[role] represents a player's role in a game *)
type role = 
  |BigBlind
  |SmallBlind
  |Normal

(**[player_id] is the name of a player. *)
type player_id = string

(**[t] is the type of the state of a game. *)
type t

(**Raised when a blind tries to fold at the start of a game. *)
exception BlindFold

(**Raised when a blind tries to check at the start of a game. *)
exception BlindCheck

(**Raised when a blind tries to raise at the start of a game. *)
exception BlindRaise

(**Raised when a players bets more than they own. *)
exception NotEnoughMoney

(**Raised when a player tries to check when the current bet is not 0. *)
exception CannotCheck

(** Raised when initialized with an empty player list*)
exception EmptyPlayers

(**[fold t] steps the current state [t] of the game when the current player 
   folds. *)
val fold : t -> t

(**[call t] steps the current state [t] of the game when the current player 
   calls the current bet. *)
val call : t -> t

(**[raise n t] steps the current state [t] of the game when the current player 
   raises the current bet by [n]. *)
val raise : int -> t -> t

(**[check t] steps the current state [t] of the game when the current player 
   checks. *)
val check : t-> t

(**[exit t] steps the current state [t] of the game when the current player 
   exits the game. *)
val exit : t -> t 

(** [init_state str] is the initial state with player names as listed 
    in the string [str].
    Space is used to separate each name in [str]
    Raises EmptyPlayers if [str] is full of spaces
*)
val init_state: string -> t

(** [string_of st] is the string representation of state [st]*)
val string_of : t -> string
