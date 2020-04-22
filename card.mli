(** the type for a poker's suite*)
type suite = SPADE | HEART | DIAMOND | CLUB

(** the type for a poker's rank*)
type rank = int



(** the type for a card *)
type t


(** [rank_of_card c] is the rank of card [c]*)
val rank_of_card : t -> rank

(** [suite_of_card c] is the suite of card [c]*)
val suite_of_card : t -> suite

(** [make_card suite rank] create a card with rank [rank] and suite [suite]*)
val make_card :  suite -> rank -> t

(** [compare card1 card2] compares the order of [card1] relative to [card2]
    if [card1] is greater than [card2], then it returns [1]
    For example : [compare (club ace) (club 2)] is  [1]
    [compare (club 2) (heat 2)] is [0]*)
val compare : t -> t -> int 

(** [string_of card] is the string representation of [card]*)
val string_of : t -> string