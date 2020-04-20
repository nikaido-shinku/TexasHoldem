type suite = SPADE | HEART | DIAMOND | CLUB


type rank = 
  |NUMBER of int 
  |KING
  |QUEEN
  |JACK
  |ACE

type order = LT | GT | EQ 


(** AF : A card [(s, r)] represent a card with rank [r] and suit [s]
    RI: If [r] is a Number, its rank should be between 2 and 10
*)
type t = suite * rank

(** whether we are in debug mode*)
let debug= false

(** [rep_ok c] returns [c] if it satisfies the representation invariant
    Raises: Failure if RI is violated*)
let rep_ok c = 
  if debug then 
    match snd c with
    |NUMBER (x) when (x < 2 || x > 10) -> 
      failwith "representation invariant for card failed"
    |_ -> c
  else c 

let rank_of_card = snd

let suite_of_card = fst

let make_card s r = 
  rep_ok (s,r)

(** [compare_rank r1 r2] compares the order of two ranks*)
let compare_rank r1 r2 = 
  match r1 , r2 with 
  |a, b when a = b -> EQ
  |ACE , _ -> GT
  |_,ACE -> LT
  |KING , _ -> GT
  |_ , KING -> LT
  |QUEEN , _ -> GT
  |_ , QUEEN -> LT
  |JACK , _ -> GT
  |_ ,JACK -> LT
  |NUMBER(x) , NUMBER(y) -> if x > y then GT
    else if x< y then LT
    else EQ


let compare c1 c2 = 
  let r1 = rank_of_card c1 in 
  let r2 = rank_of_card c2 in 
  compare_rank r1 r2