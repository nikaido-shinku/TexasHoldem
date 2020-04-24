open Card

(** the type for a hand*)
type t

type category = 
  |Unknown
  |HighCard
  |Pair
  |TwoPair
  |ThreeOfAKind
  |Straight
  |Flush
  |FullHouse
  |FourOfAKind
  |StraightFlush
  |RoyalFlush 


(** an empty hand*)
val empty : t

(** [size hand] is the number of cards in [hand]*)
val size: t -> int

(** [mem card hand] is whether [card] is in [hand]*)
val mem: Card.t -> t -> bool

(** [insert card hand] is [hand] after inserting [card] to it
    if [card] is already in there, do nothing
    Raises: failure if more than five are in [hand] after this operation*)
val insert : Card.t -> t -> t



(** [combine hand1 hand2] is
    the list of possible hands with size 5 that can form using the 
    cards in [hand1] and [hand2]

    the list with one hand that include all the cards in [hand1] and [hand2]
    when [size hand1 + size hand2 < 5] 

    Requires: [hand1] and [hand2] don't have repeated cards
*)
val combine : t -> t -> t

(* (** [union hand1 hand2] is the hand that include all the card in hand1 
    and hand2, excluding repeated cards
    Raises: failure if more than five are in [hand] after this operation*)
   val union : t -> t -> t *)


(** [category_of hand] is the category of [hand] in typical texasholdem rule
    Raises: failure if hand has size <> 5*)
val category_of : t -> category

(** [string_of hand] is the string representation of [hand]*)
val string_of : t -> string 

(** [to_list hand] is the list representation of [hand] *)
val to_list : t -> Card.t list


(** [compare hand1 hand2] compares the order of [hand1] [hand2]*)
val compare : t -> t -> int


(** [highest_hand community hands] is the highest possible hand 
    created by [hands] and [community] cars 
    tupled with the winner's number(s) 

    Requires: all the hand in [hands] have the same size of 2
    And [community] has size at least 3

*)
val highest_hand : Card.t list -> t list -> (int list* category)