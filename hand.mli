open Card

(** the type for a hand*)
type t

(** an empty hand*)
val empty : t

(** [insert card hand] is [hand] after inserting [card] to it*)
val insert : Card.t -> t -> t

(** [to_list h] is an list representation of the same hand [h]*)
val to_list : t -> (string * string) list

