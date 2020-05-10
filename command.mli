(**[command] represents all possible commands a player can make during 
   their turn*)
type command = 
  |Fold 
  |Call
  |Raise of int
  |Check
  |Exit
  |Quit

(** Raised when no command is parsed. *)
exception Empty

(** Raises when an invalid command is parsed. *)
exception Malformed

(** [parse str] parses a player's text input into a [command].
    Raises: Malformed if parsed  wrong command *)
val parse : string -> command