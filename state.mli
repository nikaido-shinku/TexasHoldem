(**[role] represents a player's role in a game *)
type role = 
  |BigBlind
  |SmallBlind
  |Normal


type player_id = string

type t

exception BlindFold
exception BlindCheck
exception NotEnoughMoney
exception CannotCheck

val fold : t -> t

val call : t -> t

val raise : int -> t -> t

val check : t-> t

val exit : t -> t 