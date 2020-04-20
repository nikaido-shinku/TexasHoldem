type suite = SPADE | HEART | DIAMOND | CLUB


type rank = int 

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
    |x when (x < 1 || x > 13) -> 
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
  |1,1 -> 0
  |1,_ -> 1
  |_,1 -> -1
  |_ -> (if r1<r2 then -1 
         else if r1 = r2 then 0
         else 1)


let compare c1 c2 = 
  let r1 = rank_of_card c1 in 
  let r2 = rank_of_card c2 in 
  compare_rank r1 r2