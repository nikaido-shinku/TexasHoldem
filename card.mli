(** the type for a poker's suite*)
type suite = SPADE | HEART | DIAMOND | CLUB

(** the type for a poker's rank*)
type rank = 
  |NUMBER of int 
  |KING
  |QUEEN
  |JACK
  |ACE

(** the order between any two cards*)
type order = LT | GT | EQ 

(** the type for a card *)
type t


(** [rank_of_card c] is the rank of card [c]*)
val rank_of_card : t -> rank

(** [suite_of_card c] is the suite of card [c]*)
val suite_of_card : t -> suite

(** [make_card suite rank] create a card with rank [rank] and suite [suite]*)
val make_card :  suite -> rank -> t

(** [compare card1 card2] compares the order of [card1] relative to [card2]
    For example : [compare (club ace) (club 2)] is  [GT]
    [compare (club 2) (heat 2)] is [EQ]*)
val compare : t -> t -> order 