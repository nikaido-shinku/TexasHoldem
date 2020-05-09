open Card
(** AF: a card list [c1 ; c2 ; c3 ....] represents a hand with cards 
    {c1,c2,c3...}
    RI: The size of the list should not be greater than 5*)


type t = Card.t list



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

let empty = []

let size = List.length


let rep_ok h = 
  if (size h > 5) then failwith "RI violation in Hand: more than 5 cards" 
  else h

let mem  = List.mem

let insert c h = (if mem c h then h else c::h) |> rep_ok

let combine h1 h2 = h1 @ h2

(* 
let rec union h1 h2 = 
  match h1 with 
  |[] -> h2
  |h :: tl -> if mem h h2 then union tl h2 
    else union tl (h :: h2) *)





(** [sort hand] is the [hand] after sorted according to the rule of the card*)
let sort h = List.sort Card.compare h

(** [same_suite sl] checks whether all the suite in suite list [sl] are the 
    same*)
let same_suite (sl: suite list) = 
  let target = List.hd sl in 
  sl 
  |>List.filter ( (<>) target) 
  |> (=) []


(** [is_flush hand] is whether [hand] qualifies for a flush 

    Requires: [size hand = 5]*)
let is_flush h = 
  h
  |> List.map Card.suite_of_card 
  |> same_suite

(** [straight_ranks rl] checks whether the ranks in rank list [rl] satisfies
    a straight

    Requires: [rl] is sorted in increasing order according to card
*)
let straight_ranks (rl: rank list) = 
  let init = List.hd rl - 1 in 
  if init = 1 then rl = [2;3;4;5;1] || rl = [2;3;4;5;6]
  else 
    rl 
    |> List.fold_left
      (fun (p,b) r -> 
         if not b then (p,b) 
         else if (r-1) mod 13 = (p mod 13) then (r,b) 
         else (p,false)) (init, true) 
    |> snd



(** [is_straight hand] is whether [hand] qualifies for straight

    Requires: [size hand = 5]*)
let is_straight h = 
  h 
  |> List.map Card.rank_of_card
  |> straight_ranks


(** [find_straight_flush h] is the category of [h] if it is a straight
    or flush or both. If it is none, then HighCard

    Requires: [h] is sorted in increasing order and of size 5*)
let find_straight_flush h = 
  match (is_straight h) , (is_flush h) with
  |true,true -> if rank_of_card (List.hd h) = 10 then RoyalFlush
    else StraightFlush
  |true, _ -> Straight
  |_, true -> Flush
  |_, _ -> HighCard


(** [same_ranks rl] is the category of rank list [rl] if it is four of a kind
    /full house/ three of a kind/ two pairs/ pair. If it is none, then HighCard

    Requires: [rl] is sorted in increasing order and of size 5*)
let same_ranks = 
  function
  |[r1 ; r2 ; r3 ; r4 ;r5] -> 
    if (r3 = r2) && (r3 = r4) && ((r3 =r1 )|| (r3 = r5)) 
    then FourOfAKind
    else if (r1 = r2) && (r4 = r5) && ((r3 =r1 )|| (r3 = r5)) 
    then FullHouse
    else if (r2 = r3 && (r1 = r2 || r3 = r4)) || (r3 = r4 && r4 = r5) 
    then ThreeOfAKind
    else if (r1 = r2 && (r3 = r4 || r4 = r5)) || (r2 = r3 && r4 = r5)
    then TwoPair
    else if r1 = r2 || r2 = r3 || r3 = r4 || r4 = r5 
    then Pair
    else HighCard
  |_ -> Unknown


(** [find_same_cards hand] is the category of [hand] if it is 
    four of a kind /full house/ three of a kind/ two pairs/ pair.
    If it is none of the above, then HighCard.

    Requires: [rl] is sorted in increasing order and of size 5*)
let find_same_cards h = 
  h 
  |> List.map Card.rank_of_card
  |> same_ranks

let category_of h = 
  if size h <> 5 
  then failwith "cannot find category for a nonstandard hand"
  else let new_h = sort h in
    match find_straight_flush new_h with 
    | HighCard -> find_same_cards new_h
    | c -> c




let string_of h = 
  List.fold_left (fun s c -> s ^ (Card.string_of c) ^ "\n ")  "" h

let to_list h = h


let compare h1 h2 = 
  let c1 = List.hd (sort h1) in 
  let c2 = List.hd (sort h2) in 
  match category_of h1 , category_of h2 with 
  |a,b when a = b -> Card.compare c1 c2 
  |RoyalFlush, _ -> 1
  |_ , RoyalFlush -> -1
  |StraightFlush, _ -> 1
  |_, StraightFlush -> -1
  |FourOfAKind, _ -> 1
  |_, FourOfAKind -> -1
  |FullHouse, _ -> 1
  |_ ,FullHouse -> -1 
  |Flush, _-> 1
  |_,Flush -> -1
  |Straight, _ -> 1
  |_,Straight -> -1
  |ThreeOfAKind,_ -> 1
  |_, ThreeOfAKind -> -1
  |TwoPair, _ -> 1
  |_, TwoPair -> -1
  |Pair,_ -> 1
  |_,Pair -> -1
  |HighCard,_ -> 1
  |_, HighCard -> -1
  |_ -> 0




(** [possible_hands cards hands s]
    All the possible hands of size [s] in [Cards]
*)
let rec possible_hands 
    (cards: Card.t list)
    (s : int) = 
  let size = List.length cards in 
  if size = s then [cards]
  else if size < s then []
  else match cards with
    |[] -> []
    |h :: tl -> 
      let new_hands = 
        List.map (fun hand -> h:: hand)(possible_hands tl (s-1)) in 
      new_hands @ possible_hands tl s

(** [highest_hand_helper hands highest] is
    the list of highest hands and their index in [hands]
    and then compare with [highest], take the union if they equal to each other
    or choose the list with higher order*)
let rec highest_hand_helper hands highest index=  
  match hands, highest with
  |[] , _ -> highest
  |h :: tl, [] -> highest_hand_helper tl [(h,index)] (index+1)
  |h1 :: t1 , (h2,i) :: t2 -> 
    if compare h1 h2 = 0 
    then highest_hand_helper t1 ((h1,index)::highest) (index + 1)
    else if compare h1 h2 > 0 
    then highest_hand_helper t1 [(h1,index)] (index + 1) 
    else highest_hand_helper t1 highest (index + 1)


let highest_hand (community: Card.t list) (hands: t list) = 
  let high_hands = 
    hands
    |>
    List.map (fun hand -> 
        (possible_hands (community @ hand) 5)
        |>List.sort compare 
        |> List.rev
        |> List.hd
      ) in
  let highest_hands = 
    (highest_hand_helper high_hands [] 0) in 
  let highest_category = 
    highest_hands
    |> List.hd
    |> fst
    |> category_of
  in 
  (List.map (snd) highest_hands , highest_category) 
