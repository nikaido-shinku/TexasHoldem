open Card
(** the type of a deck*)
type t

(** [empty] returns an empty deck*)
val empty : t

(** [is_empty d] is whether deck [d] is empty deck*)
val is_empty : t -> bool

(** [deal d] deal a card from the current deck [d]
    returns [None] if it is an empty deck
    [Some c] if there are cards left 
*)
val deal : t -> Card.t option

(** [shuffle d] is the deck [d] after shuffling*)
val shuffle : t -> t

(** [mem d r s] is whether the card with rank [r] suite [s] 
    is contained in the deck [d]*)
val mem : t -> Card.rank -> Card.suite -> bool

(** [remove d r s] is the deck[d] without any card with the combination of 
    rank [r] and suite [s]*)
val remove : t -> Card.rank -> Card.suite -> t

(** [insert c d] is the deck [d] after inserting the card [c]*)
val insert : Card.t -> t -> t