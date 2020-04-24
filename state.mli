(**[role] represents a player's role in a game *)
type role = 
  |BigBlind
  |SmallBlind
  |Normal

(**[player_id] is the name of a player. *)
type player_id = string

(**[t] is the type of the state of a game. *)
type t

(**Raised when small blind tries to fold at the start of a game. *)
exception BlindFold

(**Raised when small blind tries to check at the start of a game. *)
exception BlindCheck

(**Raised when a players bets more than they own. *)
exception NotEnoughMoney

(**Raised when a player tries to check when the current bet is not 0. *)
exception CannotCheck

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